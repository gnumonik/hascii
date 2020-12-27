{-# LANGUAGE BangPatterns #-}




module IO (displayAscii) where

import Utils ( mapArray ) 
import AsciiChars ( charFeatures ) 
import Cropper ( runSlice )
import Features ( findNeighbor )
import Preprocess (stretch, prepareImage) 
import Graphics.Image.IO ( readImage' )
import Data.Maybe ( fromMaybe )
import Graphics.Image.Interface
    ( Array, BaseArray(Image), Pixel, ColorSpace )
import Graphics.Image.Interface.Vector ( VS ) 
import Data.Word ( Word8 ) 
import Graphics.Image.ColorSpace
    ( Word8, toImageY, ToY, Y, YCbCr, Pixel )
import qualified Data.Vector as V






printAscii :: V.Vector (V.Vector Char) -> String
printAscii vec = concat . V.toList $ V.map (\x -> V.toList x <> "\n")  vec 



getImage :: FilePath  -> IO (Image VS Y Double) 
getImage fPath  = do
  readImage' fPath :: IO (Image VS Y Double)


displayAscii :: FilePath 
             -> Maybe Int 
             -> Maybe FilePath 
             ->  IO ()
displayAscii fpath scale' outpath' = do
  let scale = fromMaybe 1 scale'
  i <- getImage fpath 
  let !stretched = prepareImage $ stretch scale 13 i
  let !sliced    = runSlice stretched
  case outpath' of
    Just outpath -> 
      writeFile outpath $ (printAscii $ mapArray (\x -> findNeighbor x charFeatures) sliced)
    Nothing -> do 
      putStrLn $ "\n" 
                  <> (printAscii $ mapArray (\x -> findNeighbor x charFeatures) sliced)







