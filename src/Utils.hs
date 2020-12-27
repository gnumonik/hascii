{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE AllowAmbiguousTypes, RankNTypes, FlexibleInstances, TemplateHaskell, BangPatterns, OverloadedStrings #-}

module Utils 
        (binarize
        ,averageImages
        ,avgLuma
        ,getRegionAvg
        ,getArea
        ,countBool
        ,countSlice
        ,mapArray) where 


import Prelude hiding (sum)
import Graphics.Image
    ( VS,
      X,
      Bit,
      Y,
      Array(map),
      MArray,
      Pixel(PixelY, PixelX),
      sum,
      bit2bool,
      index,
      toImageBinaryUsing,
      dims,
      Image )
import Graphics.Image.Interface
    ( Array(zipWith), MArray, Pixel, index, BaseArray(dims, Image) )
import Graphics.Image.Interface.Vector ( VS ) 
import Graphics.Image.ColorSpace
    ( X, Bit, Y, Pixel(PixelY, PixelX), bit2bool )
import Graphics.Image.Processing.Binary ( toImageBinaryUsing )  
import qualified Data.Vector as V


-- utility function 
mapArray :: (a -> b) -> V.Vector (V.Vector a) ->  V.Vector (V.Vector b)
mapArray f !vec = V.force $ V.map (\x -> V.force $ V.map f x) vec
{-# INLINE mapArray #-}
-- utility function 
mapArrayM_ :: Monad m => (a -> m ()) -> V.Vector (V.Vector a) -> m () 
mapArrayM_ f !vec = V.mapM_ (\x -> V.mapM_ f x) vec 
{-# INLINE mapArrayM_ #-}
-- utility function 
flatten :: V.Vector (V.Vector a) -> V.Vector a
flatten !vec
  | V.null vec = V.empty 
  | otherwise = V.head vec V.++ flatten (V.tail vec)

-- utility function 
sumTup :: Num a => (a,a) -> (a,a) -> (a,a)
sumTup (a,b) (c,d) = (a + c, b + d)

-- given start coordinates and end coordinates, returns a list 
-- of the coordinates that lie between them (rectangular)
toSearch :: (Int,Int) -> (Int,Int) -> [(Int,Int)]
toSearch (a,b) (a',b') = pure (\x y -> (x,y)) <*> [a..a'] <*> [b..b']

-- gets a rectangular selection of pixels from an image
getArea' :: MArray arr cs e => (Int, Int) -> (Int, Int) -> Image arr cs e -> V.Vector (Pixel cs e)
getArea' !start !end !img = V.fromList 
                          . Prelude.map (\x -> index img x)
                          $ toSearch start end 
{-# INLINE getArea' #-}
-- gets a rectangular selection of pixels from a binarized image
-- then converts the word8s to bools 
getArea :: (Int,Int) -> (Int,Int) -> Image VS X Bit -> V.Vector Bool
getArea !start !end !img = V.fromList 
                        . Prelude.map (\x -> pixXToBool $ index img x) 
                        $ toSearch start end  
  where
    pixXToBool :: Pixel X Bit -> Bool
    pixXToBool !(PixelX b) = bit2bool b

-- gets standard deviation of luma values for a grayscale image
lumaStdDev :: forall  e. (Array VS Y e, Fractional e, Floating e) => Image VS Y e -> e
lumaStdDev !i = 
  let !(h,w)       = dims i 
      !(PixelY s)  = sum i
      !mean1       = s / (fromIntegral h * fromIntegral w)
      !apMn        = Graphics.Image.map (\(PixelY x) -> PixelY (x - mean1)) i
      !(PixelY s') = sum apMn 
      !std         = sqrt $ s' /  (fromIntegral h * fromIntegral w)
  in std

-- gets average luma value for a grayscale image
avgLuma :: forall  e. (Array VS Y e, Fractional e) => Image VS Y e -> e
avgLuma !i = 
  let !(PixelY s) = sum i
      size       = (\(x,y) -> (fromIntegral x * fromIntegral y)) $ dims i 
  in s / size 

-- counts occurrences of True in a vector 
countBool :: V.Vector Bool -> Int 
countBool !v = V.foldr (\x acc -> if x then acc + 1 else acc) 0 v

-- counts occurrences of True in a slice of a vector
-- 2nd arg is the end point, not the number of elements 
-- to take after the start point (i.e. this is different
-- than the way V.slice works)
countSlice :: Int -> Int ->  V.Vector Bool -> Int
countSlice !x !y !v = countBool $ V.slice x (y-x)  v

-- Takes 2 doubles and 2 images. Multiplies the value of each pixel
-- in the first image by the first arg, multiplies the value of each pixel in the 
-- second image by the second arg, then takes the average
-- Used to combine different edge detection "negative" images 
averageImages :: Double -> Double -> Image VS Y Double -> Image VS Y Double -> Image VS Y Double
averageImages !m !m' !i1 !i2 = 
  Graphics.Image.Interface.zipWith 
    (\(PixelY d) (PixelY d') -> PixelY $ ((d * m) + (d' * m')) / 2)
    i1 
    i2 
-- makes a threshold function based on the average luma in an image 
mkThresh :: Image VS Y Double -> (Pixel Y Double -> Bool)
mkThresh !i = 
  let !avg = avgLuma i 
  in \(PixelY x) -> if x < avg  then True else False 

binarize :: Image VS Y Double -> Image VS X Bit
binarize !i = toImageBinaryUsing (mkThresh i) i 

-- same thing as mkThresh but takes arguments that are supposed to be the stDev and avg 
-- of a second image. used to construct "negatives" to help with 
-- edge detection
mkThresh' :: Double -> Double -> Image VS Y Double -> (Pixel Y Double -> Bool)
mkThresh' !pAvg !pStd !i = 
  let !std = lumaStdDev i
      !avg = avgLuma i 
  in \(PixelY x) ->  --if x < ((avg + prnt) / 2)  then True else False 
      if | abs (avg - pAvg) >  pStd * 2.5 -> 
              if x < avg then True else False 
         | abs (avg - pAvg) >  pStd * 1.5 -> 
              if x < (0.7 * avg + 0.3 * pAvg) 
              then True else False 
         | otherwise -> 
              if x < (0.9 * pAvg + 0.1 * avg) then True else False 

binarize' :: Double -> Double -> Image VS Y Double -> Image VS X Bit
binarize' !avg !std !i = toImageBinaryUsing (mkThresh' avg std i) i 

-- determines the average luma for a region
-- first arg is the distance from the starting pixel 
-- second arg is the location of the target pixel
getRegionAvg :: Int -> (Int,Int) -> Image VS Y Double -> Double
getRegionAvg !apSize !loc !refImg = go loc refDims refImg 
  where

    !refDims = dims refImg

    size :: ((Int,Int),(Int,Int)) -> Double 
    size ((a,b),(x,y)) = fromIntegral $ (a + x) * (b + y)

    go :: (Int,Int) -> (Int,Int) -> Image VS Y Double -> Double
    go (locX,locY) (boundX,boundY) img =
      let !(s,e)  = aperture apSize (locX,locY) (boundX,boundY)
          !d      = V.sum . V.map (\(PixelY d') -> d') $ getArea' s e img
      in d /  (size (s,e))
    -- Given an int (ap), a start coordinate, and the boundaries for an image, 
    -- get the start and end coordinates for the largest rectnagle that can be produced
    -- by adding and subtracting ap from the start coordinate
    -- that doesn't exceed the dimensions of the boundary
    aperture :: Int -> (Int,Int) -> (Int,Int) -> ((Int,Int),(Int,Int))
    aperture ap (x,y) (bX,bY) = (getStart,getEnd)
      where
        !getStart =
          if | x - ap <  0 && y - ap >= 0   -> (0,y-ap)
             | x - ap <  0 && y - ap < 0    -> (0,0)
             | x - ap >= 0 && y - ap < 0    -> (x - ap,0)
             | otherwise                    -> (x - ap, y - ap)

        !getEnd =
          if | x+ap >= bX  && y + ap < bY    -> (bX-1,y+ap)
             | x + ap >= bX  && y + ap >= bY -> (bX-1,bY-1)
             | x + ap <= bX && y + ap >= bY  -> (x+ap,bY-1)
             | otherwise                     -> (x+ap,y+ap) 