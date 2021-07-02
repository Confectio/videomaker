module Lib
    ( buildImage,
      renderAnimation
    ) where

import Data.STBImage
import qualified Data.Word as W
import qualified Data.Vector.Storable as V
import Reanimate.Render as RR
import Reanimate as R

-- ======= methods for pixel-by-pixel image generation ===========

type Coordinates = (Int, Int)
type Frame = Int
type Height = Int
type Width = Int

buildImage :: Lib.Width -> Lib.Height -> Lib.Frame -> Image RGBColor
buildImage w h fr = Image (pixels w h fr) w h where
                        pixels w h fr = V.fromList [generateColor (x, y) fr | x <- [0..w], y <- [0..h]]

-- This is the Generator function for the pixels.
generateColor :: Coordinates -> Lib.Frame -> RGBColor
generateColor (x, y) fr = RGBColor (generateRed (x, y) fr) 0 0

generateRed :: Coordinates -> Lib.Frame -> W.Word8
generateRed (x, y) fr = (fromIntegral . round) (sin(fromIntegral (x*y*fr))*128.0 + 128.0)


-- ============= methods for 'reanimate' ============

type Amount = Int
type Frequency = Int
type Direction = (Double, Double)
type DirectionGenerator = Int -> Direction

--for Debugging
--renderAnimation :: Duration -> Amount -> FilePath -> Format -> RR.Width -> RR.Height -> FPS -> IO ()
--renderAnimation t a fp form w h fps = renderSvgs fp 10 False (jojoWaveOfMovingSquares t a)
renderAnimation :: Duration -> Amount -> FilePath -> Format -> RR.Width -> RR.Height -> FPS -> IO ()
renderAnimation t a fp form w h fps = render (jojoWaveOfMovingSquares t a) fp RasterAuto form w h fps False

--jojoWavesOfMovingSquares :: Duration -> Amount -> Frequency -> Animation
--jojoWavesOfMovingSquares t a = 

-- It takes about 4.5 seconds until a wave of moving squares hits the border
jojoWaveOfMovingSquares :: Duration -> Amount -> Animation
jojoWaveOfMovingSquares t a = setDuration t (playThenReverseA (signalA (curveS 2) (waveOfAmountMovingSquares gen a 4.5)))
                                where gen = evenlySpacedCircleDirectionGenerator a

waveOfAmountMovingSquares :: DirectionGenerator -> Amount -> Duration -> Animation
waveOfAmountMovingSquares gen leftOverAmount t | leftOverAmount >= 1 = (movingSquare t (getX direction) (getY direction)) 
                                                                `parA` waveOfAmountMovingSquares gen (leftOverAmount - 1) t
                                                                where direction = gen leftOverAmount
waveOfAmountMovingSquares gen leftOverAmount t | leftOverAmount == 0 = (movingSquare t (getX direction) (getY direction))
                                                                where direction = gen leftOverAmount

evenlySpacedCircleDirectionGenerator :: Amount -> DirectionGenerator
evenlySpacedCircleDirectionGenerator amountOfPossibleSpawns = (\currentSpawnNumber -> let angle = 2.0*pi*((fromIntegral currentSpawnNumber)/(fromIntegral amountOfPossibleSpawns)) in (cos(angle) , sin(angle)))

getX :: Direction -> Double
getX (x, y) = x

getY :: Direction -> Double
getY (x, y) = y

movingSquare :: Duration -> Double -> Double -> Animation
movingSquare t directionx directiony = (mkAnimation t
                                        (\t -> translate (directionx * t) (directiony * t)
                                            (rotate (360 * t)
                                                littleSquare)))

littleSquare :: SVG
littleSquare =
  withStrokeWidth 0
    (withFillOpacity 1
      (withFillColor "Black"
        (mkRect 0.25 0.25)))