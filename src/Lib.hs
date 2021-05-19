module Lib
    ( buildImage
    ) where

import Data.STBImage
import qualified Data.Word as W
import qualified Data.Vector.Storable as V

type Coordinates = (Int, Int)
type Frame = Int
type Height = Int
type Width = Int

buildImage :: Width -> Height -> Frame -> Image RGBColor
buildImage w h fr = Image (pixels w h fr) w h where
                        pixels w h fr = V.fromList [generateColor (x, y) fr | x <- [0..w], y <- [0..h]]

-- This is the Generator function for the pixels.
generateColor :: Coordinates -> Frame -> RGBColor
generateColor (x, y) fr = RGBColor (generateRed (x, y) fr) 0 0

generateRed :: Coordinates -> Frame -> W.Word8
generateRed (x, y) fr = (fromIntegral . round) (sin(fromIntegral (x*y*fr))*128.0 + 128.0)