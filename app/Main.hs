module Main where

import Lib
import Data.STBImage
import System.Environment
import Data.Char
import Data.Maybe
import Options.Applicative
import System.Process

type Amount = Int
type FormatWriter = (FilePath -> Image RGBColor -> IO ())
type FileName = String
type DirectoryPath = String
type Format = String
type Width = Int
type Height = Int

formats :: [(String, FormatWriter)]
formats = [("png", writePNG),
           ("tga", writeTGA),
           ("bmp", writeBMP)]


dirpath :: Parser String
dirpath = strOption
  (long "directory"
  <> short 'd'
  <> metavar "Path"
  <> help "Path of the directory, where the frames should be saved")

name :: Parser String
name = strOption
    (long "name"
    <> short 'n'
    <> metavar "Name"
    <> help "Name of the frames. Every frame will be named according to the pattern [Name][Framenr].[extension]")

validateFormat :: String -> Either String Format
validateFormat s =  if isNothing (lookup format formats) then
                        Left "Error: Desired Format not supported. Format must be either png, tga or bmp."
                    else   
                        Right format
    where format = map toLower s

format :: Parser Format
format = option (eitherReader validateFormat) (long "format"
    <> short 'f'
    <> metavar "Format"
    <> value "png"
    <> help "Format of the frames. Only PNG, TGA and BMP are supported, default is PNG.")

amount :: Parser Int
amount = option auto
    (long "amount"
    <> short 'a'
    <> metavar "Amount"
    <> help "The amount of frames that should be created")

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
render = switch
    (long "render"
    <> short 'r'
    <> help ("Whether the created frames should be rendered into a video. This executes a standard call to ffmpeg, so if"
    ++ "you have specific needs, you need to do the video rendering yourself (e.g. by providing the right arguments to ffmpeg)."
    ++ "Requires ffmpeg to be installed and in PATH."))

data Options = Options
    { optDir :: String,
      optName :: String,
      optFormat :: Format,
      optAmount :: Int,
      optWidth :: Int,
      optHeight :: Int,
      optRender :: Bool
    }

opts :: Parser Options
opts = Options <$> dirpath <*> name <*> format <*> amount <*> width <*> height <*> render

finalParser :: ParserInfo Options
finalParser = info (opts <**> helper)
    (fullDesc
     <> progDesc ("Generate a certain amount of frames, which can be used to generate a video.\n"
                  ++ "The frames are generated in a way, that the difference between each consecutive frame is high.\n"
                  ++ "Therefore the resulting video will take up relatively much space.\n"
                  ++ "To be more precise, the frames will look like noise in an old tv, but in red instead of white.")
     <> header ("Generate a certain amount of frames, which can be used to generate a video.\n"
                ++ "The resulting video is made to take up relatively much space."))

--The call to the executable must contain the following arguments:
--First Argument: Path of the directory, where the frames should be saved.
--Second Argument: Name of the frames.
--(Every frame will be named according to the pattern [Name][Framenr].[extension])
--Third Argument (optional): Format of the frames. Only the formats PNG, TGA, BMP are supported. PNG is the default value.
--Fourth Argument: How many frames should be created.
--Fifth Argument: Width of the frames
--Sixth Argument: Height of the frames
--Seventh Argument (optional): Flag whether ffmpeg should be used after creating the frames.
main :: IO ()
main = do 
    opts <- execParser finalParser
    formatWriter <- return (lookup (optFormat opts) formats)
    generateFrames (optDir opts) (optName opts) (optFormat opts) (optAmount opts) (optWidth opts) (optHeight opts) (fromJust formatWriter)
    putStrLn("Finished generating the frames.")
    if (optRender opts) then do
        putStrLn("Start generating the video now.")
        r <- createProcess (proc "ffmpeg" ["--i", (patternForFrames (optAmount opts) (optName opts) (optFormat opts)), "video.mp4"]){cwd = Just (optDir opts)}
        return ()
    else
        return ()

patternForFrames :: Amount -> FileName -> Format -> String
patternForFrames n name format = name ++ "%0" ++  show (magnitude n) ++ "d." ++ format

magnitude :: Amount -> Integer
magnitude n = (floor (logBase 10.0 (fromIntegral n))) + 1

generateFrames :: DirectoryPath -> FileName -> Format -> Amount -> Width -> Height -> FormatWriter -> IO ()
generateFrames dirpath filename format n w h writer = do
                if n == 0 then
                    return ()
                else do
                    writer (dirpath ++ "\\" ++ filename ++ show n ++ "." ++ format) (buildImage w h n)
                    if (magnitude n) < (magnitude (n + 1)) then
                        -- This fills the following Framenumbers with zeros. F.ex. if the original amount is 3623
                        -- then the first frame will be named filename3623.format but the last will be named filename0001.format
                        generateFrames dirpath (filename ++ "0") format (n - 1) w h writer
                    else 
                        generateFrames dirpath filename format (n - 1) w h writer