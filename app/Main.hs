module Main where

import Lib
import Data.STBImage
import System.Environment
import Data.Char
import Data.Maybe

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

--The call to the executable must contain the following arguments:
--First Argument: Filepath of the folder, where the frames should be saved.
--Second Argument: Name of the frames.
--(Every frame will be named according to the pattern NameFramenr.extension)
--Third Argument: Format of the frames. Only the formats PNG, TGA, BMP are supported.
--Fourth Argument: How many frames should be created.
--Fifth Argument: Width of the frames
--Sixth Argument: Height of the frames
main :: IO ()
main = do
    args <- getArgs
    let formatDesired = map toLower (args !! 2)
    format <- return (lookup formatDesired formats)
    if isNothing format then
        putStrLn "Error: Desired Format not found. Format must be either png, tga or bmp."
    else do
        generateFrames (args !! 0) (args !! 1) formatDesired (read (args !! 3)) (read (args !! 4)) (read (args !! 5)) (fromJust format)

generateFrames :: DirectoryPath -> FileName -> Format -> Amount -> Width -> Height -> FormatWriter -> IO ()
generateFrames dirpath filename format n w h writer = do
                if n == 0 then
                    return ()
                else do
                    writer (dirpath ++ "\\" ++ filename ++ show n ++ "." ++ format) (buildImage w h n)
                    generateFrames dirpath filename format (n - 1) w h writer
