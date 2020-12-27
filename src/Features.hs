{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE AllowAmbiguousTypes, RankNTypes, FlexibleInstances, TemplateHaskell, BangPatterns, OverloadedStrings #-}

module Features 
         (evalFeatureGrid
         ,featureGrid
         ,findNeighbor) where 

import Utils 
import Control.Lens hiding (imap, index)
import Graphics.Image
import Graphics.Image.IO
import Graphics.Image.Interface hiding (mapM, mapM_)
import Graphics.Image.Interface.Vector 
import Data.Word 
import Graphics.Image.ColorSpace
import Graphics.Image.IO.Histogram 
import Graphics.Image.Processing.Binary
import Control.Monad.State.Strict 

import qualified Data.Vector as V 


-- For feature extraction
data Quadrant = Quadrant {_location    :: ((Int,Int),(Int,Int)) 
                          ,_subQuadA   :: V.Vector Bool 
                          ,_subQuadB   :: V.Vector Bool 
                          ,_subQuadC   :: V.Vector Bool 
                          ,_subQuadD   :: V.Vector Bool 
                          ,_bisectorsH :: (V.Vector Bool, V.Vector Bool) 
                          ,_bisectorsV :: (V.Vector Bool, V.Vector Bool) 
                          } deriving (Show, Eq) 
makeLenses ''Quadrant 

-- For feature extraction
data FeatureGrid = FeatureGrid {_quadA      :: Quadrant
                               ,_quadB      :: Quadrant
                               ,_quadC      :: Quadrant
                               ,_quadD      :: Quadrant
                               ,_bisectorH  :: V.Vector Bool
                               ,_bisectorV  :: V.Vector Bool
                               ,_center6    :: V.Vector Bool
                               ,_black      :: V.Vector Bool} deriving (Show, Eq)
makeLenses ''FeatureGrid 

-- best measure of distance. really. fucking. slow.
euclideanDistance :: V.Vector Int -> V.Vector Int -> Double
euclideanDistance v1 v2 = sqrt $ go v1 v2
  where
    go :: V.Vector Int -> V.Vector Int -> Double
    go v v' = fromIntegral . V.sum $ V.zipWith (\x y -> (x - y) ^ 2 ) v v' 

-- sufficiently good measure of distance
manhattanDistance :: V.Vector Int -> V.Vector Int -> Double
manhattanDistance v1 v2 = 
  fromIntegral (V.sum (V.zipWith (\x y -> abs (x - y)) v1 v2)) 
  / fromIntegral (V.length v1)   

-- takes a start and end point for part of an image and 
-- makes a feature quadrant 
mkQuad :: (Int,Int) -> (Int,Int) -> Image VS X Bit -> Quadrant
mkQuad l@(x,y) l'@(x',y') img = Quadrant (l,l') qA qB qC qD (biH1,biH2) (biV1,biV2) 
 where
  qA   = getArea  (x ,y)    (x+2, y+2) img 
  qB   = getArea  (x,y+3)   (x+2,y+5)  img 
  qC   = getArea  (x+3,y)   (x+5,y+2)  img 
  qD   = getArea  (x+3,y+3) (x+5,y+5)  img 
  biH1 = getArea  (x+2,y)   (x+2,y+5)  img
  biH2 = getArea  (x+3,y)   (x+3,y+5)  img 
  biV1 = getArea  (x,y+2)   (x+5,y+2)  img
  biV2 = getArea  (x,y+3)   (x+5,y+3)  img

-- same thing as mkQuad but makes the entire feature grid 
mkFeatureGrid :: ((Int,Int),(Int,Int)) -> Image VS X Bit -> FeatureGrid
mkFeatureGrid l@((h,w),(h',w')) i = FeatureGrid mkA mkB mkC mkD biH biV mkCenter mkBlack
  where

    mkA = mkQuad (h,w) (h+5,w+5) i

    mkB = mkQuad (h,w+7) (h+5,w+12) i 

    mkC = mkQuad (h+7,w) (h+12,w+5) i 
 
    mkD = mkQuad (h+7,w+7) (h+12,w+12) i 

    biH = getArea (h+6,w) (h+6,w+12) i 

    biV = getArea (h,w+6) (h+12,w+6) i 

    mkCenter = getArea (h+5,w+5) (h+7,w+7) i

    mkBlack = getArea (0,0) (12,12) i

-- helper function. use this. 
featureGrid :: Image VS X Bit -> FeatureGrid
featureGrid = mkFeatureGrid ((0,0),(0,0))

-- reduces a quadrant to a vector of ints representing the score 
-- for each component of the quadrant 
evalQuadrant :: Quadrant -> V.Vector Int
evalQuadrant q = V.fromList $ [scoreA
                             , scoreB
                             , scoreC 
                             , scoreD
                             , center
                             , down 
                             , up 
                             , left 
                             , right]
  where 
    scoreA = countBool $ q ^. subQuadA
    scoreB = countBool $ q ^. subQuadB
    scoreC = countBool $ q ^. subQuadC
    scoreD = countBool $ q ^. subQuadD

    getC = (\(a,b) -> (countBool $ V.slice 2 2 a) 
                +  (countBool $ V.slice 2 2 b))
    center = getC (q ^. bisectorsH) + getC (q ^. bisectorsV)


    countSlices x y (a,b) = countSlice x y a + countSlice x y b 

    down = countSlices 3 5  $ (q ^. bisectorsV)

    up  = countSlices 0 3   $ q ^. bisectorsV 

    left = countSlices 0 3  $ q ^. bisectorsH

    right = countSlices 3 5 $ q ^. bisectorsH 

-- same thing as evalQuadrant but creates the feature vector 
-- for the entire grid 
evalFeatureGrid :: FeatureGrid -> V.Vector Int 
evalFeatureGrid fg = V.concat [a,b,c,d] V.++ 
                      V.fromList 
                        [
                          center
                         ,bTop
                         ,bLeft
                         ,bRight
                         ,bBot
                        ]
                      
  where
    !a = evalQuadrant $ fg ^. quadA
    !b = evalQuadrant $ fg ^. quadB
    !c = evalQuadrant $ fg ^. quadC
    !d = evalQuadrant $ fg ^. quadD

    !center = countBool $ fg ^. center6 

    !bTop   = countSlice 0 5  $ fg ^. bisectorV 
    !bLeft  = countSlice 0 5  $ fg ^. bisectorH
    !bRight = countSlice 7 12 $ fg ^. bisectorH
    !bBot   = countSlice 7 12 $ fg ^. bisectorH

    !cDot   = countSlice 6 6 $ fg ^. bisectorH 

    !black' = countBool (fg ^. black) 
{-# INLINE evalFeatureGrid #-}

-- Neighbor search. Only searches for one nearnest neighbor. Uses manhattan distance
-- Can replace with euclidean if desired but results seem identical
findNeighbor :: Image VS X Bit -> [(V.Vector Int,Char)] -> Char
findNeighbor !img !letters = 
  let !imgFeatures = evalFeatureGrid .  featureGrid $ img  
  in go imgFeatures '.' 170 letters
 where
   go :: V.Vector Int -> Char -> Double -> [(V.Vector Int, Char)] -> Char
   go _ c _ [] = c 
   go !score c !old !(x:xs) = 
     let !xScore = x ^. _1 
         !diff   = manhattanDistance score xScore 
     in if diff < old then go score (x ^. _2) diff xs else go score c old xs 
{-# INLINE findNeighbor #-}