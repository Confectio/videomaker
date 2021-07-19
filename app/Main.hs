module Main where

import Lib
import Data.STBImage
import System.Environment
import Data.Char
import Data.Maybe
import Options.Applicative
import System.Process
import qualified Reanimate.Render as RR

type Amount = Int
type FormatWriter = (FilePath -> Image RGBColor -> IO ())
type FileName = String
type DirectoryPath = String
type Format = String
type Width = Int
type Height = Int

formatsClassic :: [(String, FormatWriter)]
formatsClassic = [("png", writePNG),
                  ("tga", writeTGA),
                  ("bmp", writeBMP)]

formatsReanimate :: [(String, RR.Format)]
formatsReanimate = [("mp4", RR.RenderMp4),
                    ("gif", RR.RenderGif),
                    ("webm", RR.RenderWebm)]

dirpath :: Parser String
dirpath = strOption
  (long "dir"
  <> metavar "Path"
  <> help "Path of the directory, where the frames/animation should be saved")

name :: Parser String
name = strOption
    (long "name"
    <> short 'n'
    <> metavar "Name"
    <> help "Name of the frames/animation. Every frame will be named according to the pattern [Name][Framenr].[extension]")

validateFormatClassic :: String -> Either String Format
validateFormatClassic s =  if isNothing (lookup format formatsClassic) then
                        Left "Error: Desired Format not supported. Format must be either png, tga or bmp."
                    else   
                        Right format
    where format = map toLower s

validateFormatReanimate :: String -> Either String Format
validateFormatReanimate s =  if isNothing (lookup format formatsReanimate) then
                        Left "Error: Desired Format not supported. Format must be either mp4, gif or webm."
                    else   
                        Right format
    where format = map toLower s

format :: Parser Format
format = strOption
    (long "format"
    <> short 'f'
    <> metavar "Format of Frames (N) | Format of Animation (T)"
    <> help "Format of the frames, when in noise mode. Only PNG, TGA and BMP are supported, default is PNG. Format of the animation, when in triangle mode. Only mp4, gif and webm are supported.")

amount :: Parser Int
amount = option auto
    (long "amount"
    <> short 'a'
    <> metavar "Amount of Frames (N) | Amount of Triangles (T)"
    <> help "In noise mode, this is the amount of frames that should be created, in triangle mode, this is the amount of triangles per 'wave' in the animation")

width :: Parser Int
width = option auto
    (long "width"
    <> short 'w'
    <> metavar "Width"
    <> help "Width of the frames in px.")

height :: Parser Int
height = option auto
    (long "height"
    <> short 'h'
    <> metavar "Height"
    <> help "Height of the frames in px.")

render :: Parser Bool
render = option auto
    (long "render"
    <> short 'r'
    <> help ("Whether the created frames should be rendered into a video. Only used in noise mode. This executes a standard (windows specific?) call to ffmpeg, so if"
    ++ "you have specific needs, you need to do the video rendering yourself (e.g. by providing the right arguments to a ffmpeg call)."
    ++ "Requires ffmpeg to be installed and in PATH.")
    <> value False
    <> metavar "Render or not (N)")

noise :: Parser Bool
noise = option auto
    (long "noise"
    <> help "Whether you want to use the noise-generating function or the moving triangle generating function."
    <> metavar "Noise or Triangle")

duration :: Parser Double
duration = option auto
    (long "duration"
    <> short 'd'
    <> metavar "Duration (T)"
    <> value 27
    <> help "Duration of the Animation (in seconds). Only used in triangle mode. The duration in noise mode is given by the amount of frames.")

fps :: Parser Int
fps = option auto
    (long "fps"
    <> metavar "FPS (T)"
    <> value 60
    <> help "Frames per Second. Only used in triangle mode.")

data Options = Options
    { optNoise :: Bool,
      optDir :: String,
      optName :: String,
      optFormat :: Format,
      optAmount :: Int,
      optWidth :: Int,
      optHeight :: Int,
      optRender :: Bool,
      optDuration :: Double,
      optFps :: Int
    }

opts :: Parser Options
opts = Options <$> noise <*> dirpath <*> name <*> format <*> amount <*> width <*> height <*> render <*> duration <*> fps

finalParser :: ParserInfo Options
finalParser = info (opts <**> helper)
    (fullDesc
     <> progDesc ("Generate a certain amount of frames, which can be used to generate a video.\n"
                  ++ "The frames are generated in a way, that the difference between each consecutive frame is high.\n"
                  ++ "Therefore the resulting video will take up relatively much space.\n"
                  ++ "There are two modes: Noise mode and Triangle mode.\n"
                  ++ "Noise mode creates an animation of...noise.\n"
                  ++ "Triangle mode creates an animation of moving triangles.\n"
                  ++ "Do note, that even though the arguments might still have the same abbreviation,\n"
                  ++ "they might do different things in noise and triangle mode, respectively.\n"
                  ++ "Also note, that for the animation in Triangle mode, you need an ffmpeg-version installed, that has an svg decoder.\n"
                  ++ "It might also be that the Triangle mode contains errors, since I was not able to extensively test it.\n"
                  ++ "By using the argument '--help', you can retrieve additional information about the arguments.")
     <> header ("Generate a certain amount of frames, which can be used to generate a video.\n"
                ++ "The resulting video is made to take up relatively much space."))

main :: IO ()
main = do 
    opts <- execParser finalParser
    if (optNoise opts) then do

        formatWriter <- return (lookup (optFormat opts) formatsClassic)
        generateFrames (optDir opts) (optName opts) (optFormat opts) (optAmount opts) (optWidth opts) (optHeight opts) (fromJust formatWriter)
        putStrLn("Finished generating the frames.")
        if (optRender opts) then do
            putStrLn("Trying to spawn ffmpeg process now...")
            r <- createProcess (shell $ "ffmpeg -i " ++ (patternForFrames (optAmount opts) (optName opts) (optFormat opts)) ++ " " ++ (optName opts) ++ ".mp4"){cwd = Just (optDir opts), create_new_console = True}
            return ()
        else
            return ()
    else do
        formatRR <- return (lookup (optFormat opts) formatsReanimate)
        filepath <- return ((optDir opts) ++ "\\" ++ (optName opts) ++ "." ++ (optFormat opts))
        renderAnimation (optDuration opts) (optAmount opts) filepath (fromJust formatRR) (optWidth opts) (optHeight opts) (optFps opts)


patternForFrames :: Amount -> FileName -> Format -> String
patternForFrames n name format = name ++ "%0" ++  show (magnitude n) ++ "d." ++ format

magnitude :: Amount -> Integer
magnitude n = (floor (logBase 10.0 (fromIntegral n))) + 1

generateFrames :: DirectoryPath -> FileName -> Format -> Amount -> Width -> Height -> FormatWriter -> IO ()
generateFrames dirpath filename format n w h writer = do
                if n == 0 then
                    return ()
                else
                    if (magnitude n) < (magnitude (n + 1)) then do
                        -- This fills the following Framenumbers with zeros. F.ex. if the original amount is 3623
                        -- then the first frame will be named filename3623.format but the last will be named filename0001.format
                        writer (dirpath ++ "\\" ++ (filename ++ "0") ++ show n ++ "." ++ format) (buildImage w h n)
                        generateFrames dirpath (filename ++ "0") format (n - 1) w h writer
                    else do
                        writer (dirpath ++ "\\" ++ filename ++ show n ++ "." ++ format) (buildImage w h n)
                        generateFrames dirpath filename format (n - 1) w h writer