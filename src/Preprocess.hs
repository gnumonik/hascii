{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
module Preprocess 
        (stretch
        ,prepareImage) where 

import Utils ( averageImages, avgLuma, getRegionAvg ) 

import Prelude hiding (sum)
import Graphics.Image
    ( VS,
      X,
      Bit,
      Y,
      Pixel(PixelY),
      isOff,
      isOn,
      off,
      on,
      toImageBinaryUsing,
      gaussianBlur,
      laplacianFilter,
      logFilter,
      sobelOperator,
      resize,
      dims,
      Image,
      Border(Reflect),
      RPU,
      Filter(applyFilter),
      Nearest(Nearest) )
import Graphics.Image.Interface
    ( Array(izipWith), Pixel, BaseArray(dims, Image), Border(Reflect) )
import Graphics.Image.Interface.Vector ( VS ) 
import Graphics.Image.Interface.Repa
    ( RPU, fromRepaArrayP, toRepaArray )
import Graphics.Image.ColorSpace
    ( X, Bit, Y, Pixel(PixelY), isOff, isOn, off, on )
import Graphics.Image.Processing.Binary ( toImageBinaryUsing )  

import Data.Array.Repa ( type (:.)((:.)), Source, DIM2 ) 
import Data.Array.Repa.Stencil
import qualified Data.Array.Repa as R
import Data.Array.Repa.Stencil.Dim2
import qualified Data.Text as T 
import qualified Data.Vector as V

-- stuff for testing. probably the only way to make this efficient is to convert 
-- everything to repa arrays and try to combine multiple operations into a single
-- step. just playing with repa here

hipLap :: (Array arr cs e, Array arr X e) => Image arr cs e -> Image arr cs e
hipLap i = applyFilter (laplacianFilter Reflect) i


repAvg :: Array arr Y Double => Image arr Y Double -> Image RPU Y Double
repAvg i = fromRepaArrayP . mapStencil2 BoundClamp testAvg $ toRepaArray i 

lap :: Graphics.Image.Interface.Array arr Y Double => Image arr Y Double -> Image RPU Y Double
lap i =  fromRepaArrayP .myLaplacian $ toRepaArray i


myLaplacian :: Source r (Pixel Y Double) => R.Array r DIM2 (Pixel Y Double) -> R.Array PC5 DIM2 (Pixel Y Double)
myLaplacian i = mapStencil2 BoundClamp testLaplacian i
{-# INLINE myLaplacian #-}

testLaplacian :: Stencil DIM2 (Pixel Y Double)
testLaplacian = [stencil2| 3 2 3 
                           2 -12 -2 
                           3  2  3 |]

testAvg :: Stencil DIM2 (Pixel Y Double)
testAvg = [stencil2|  2 1 2 
                      1 0 1
                      2 1 2|]
-- end testing --


-- stretches an image by the first arg (widge is always scaled by double heigh even
-- when the first arg is 1) and rounds each dimension to the nearest 13 (makes 
-- working with bounds easier; the sample images used for training the nn 
-- algorithm were 13x13)
stretch :: Int -> Int -> Image VS Y Double -> Image VS Y Double
stretch !scaleFac !r' !i = resize Nearest Reflect (go $ dims i) i
  where 
    go :: (Int,Int) -> (Int,Int)
    go !(x,y) = (findX r' $ x * scaleFac  ,findX r' $ y * (scaleFac * 2))

    findX :: Int -> Int -> Int 
    findX !r !n = if n `mod` r == 0 
                  then n
                  else findX r (n + 1)
{-# INLINE stretch #-}

-- takes the image, blurs it, combines 3 version of edge-detected 
-- filters (LoG, sobelOperator, laplacianFilter)
-- then binarizes the image using the edge-detected
-- variants and some special sauce
-- ridiculously inefficient, needs optimized
prepareImage :: Image VS Y Double -> Image VS X Bit 
prepareImage !grayI = go grayI merged 
  where
    !blurred = applyFilter (gaussianBlur 1.25) grayI 
    !avgI   = avgLuma $ grayI 
    !avgE   = avgLuma $ grayI  
    !merged = toImageBinaryUsing (\(PixelY d) -> d < (avgE * 1.2))
           $ averageImages 0.35 0.7 (e') 
           $ (averageImages 0.75 0.25 (sobelOperator grayI) edged) 
    !e'     =  applyFilter (logFilter Reflect) 
             $ blurred 
    !edged  = applyFilter (laplacianFilter Reflect) 
             $ blurred 
    go !grayed !neg = Graphics.Image.Interface.izipWith 
                (\ loc (PixelY d) e -> 
                  if | isOff e ->    --if d < avgI then on else off <-- change back to this if can't improve
                         let !regAvg = getRegionAvg 3 loc  grayI
                         in if d < regAvg 
                            then off 
                            else on
                     | isOn e ->    --if d < avgI then on else off <-- change back to this if can't improve
                         let !regAvg = getRegionAvg 5 loc  grayI
                         in if d < ((regAvg + (0.8 * avgI)) / 2)
                            then on
                            else off)
                grayed 
                neg 
{-# INLINE prepareImage #-}

