module Main where

import Lib
import Data.STBImage
import System.Environment
import Data.Char
import Data.Maybe
import Options.Applicative

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
    <> help "Width of the frames")

height :: Parser Int
height = option auto
    (long "height"
    <> short 'h'
    <> metavar "Height"
    <> help "Height of the frames")

data Options = Options
    { optDir :: String,
      optName :: String,
      optFormat :: Format,
      optAmount :: Int,
      optWidth :: Int,
      optHeight :: Int
    }

opts :: Parser Options
opts = Options <$> dirpath <*> name <*> format <*> amount <*> width <*> height

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
--Third Argument: Format of the frames. Only the formats PNG, TGA, BMP are supported.
--Fourth Argument: How many frames should be created.
--Fifth Argument: Width of the frames
--Sixth Argument: Height of the frames
main :: IO ()
main = do 
    opts <- execParser finalParser
    formatWriter <- return (lookup (optFormat opts) formats)
    generateFrames (optDir opts) (optName opts) (optFormat opts) (optAmount opts) (optWidth opts) (optHeight opts) (fromJust formatWriter)

generateFrames :: DirectoryPath -> FileName -> Format -> Amount -> Width -> Height -> FormatWriter -> IO ()
generateFrames dirpath filename format n w h writer = do
                if n == 0 then
                    return ()
                else do
                    writer (dirpath ++ "\\" ++ filename ++ show n ++ "." ++ format) (buildImage w h n)
                    generateFrames dirpath filename format (n - 1) w h writer
