{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE AllowAmbiguousTypes, RankNTypes, FlexibleInstances, TemplateHaskell, BangPatterns #-}

module Cropper (runSlice) where 

import Control.Lens
    ( (^.), view, over, makeLenses, Ixed(ix), Field2(_2) )
import Graphics.Image ( VS, X, Bit, crop, dims, Image )
import Graphics.Image.Interface ( BaseArray(dims, Image) )
import Graphics.Image.Interface.Vector ( VS ) 
import Graphics.Image.ColorSpace ( X, Bit )
import Control.Monad.State.Strict
    ( State, modify, execState, MonadState(get) ) 

import qualified Data.Vector as V 


data Cropper 
  = Cropper {_pos       :: !(Int,Int)
            ,_bounds    :: !(Int,Int)
            ,_cropSize  :: !(Int,Int)
            ,_activeRow :: !Int 
            ,_srcImg    :: !(Image VS X Bit) 
            ,_acc       :: !(V.Vector (V.Vector (Image VS X Bit )))} deriving (Show, Eq)
makeLenses ''Cropper

-- Takes a binarized image and creates the corresponding
-- cropper record type, for chopping it up 
initCropper :: Image VS X Bit  -> Cropper
initCropper !i = 
  let !hw    = dims i
      !start = (0,0)
      !newAcc = V.singleton (V.empty) 
  in Cropper (0,0) hw (13,13) 0 i newAcc 

slice :: State Cropper () 
slice = do
  !s <- get
  let !(h,w)     =  s ^. pos
  let !(crH,crW) =  s ^. cropSize 
  let !(bH,bW)   = s ^. bounds 
  if | (crH + h) > bH -> return () 
     | (crW + w) > bW -> do
       modify $ over pos (\(x,y) -> (h + crH,0))
       modify $ over activeRow (+1)
       modify $ over acc (\x -> V.snoc x V.empty)
       slice
     | otherwise -> do
        let !newCrop = crop (h,w) (crH,crW) (s ^. srcImg)
        modify $ over (acc . ix (s ^. activeRow)) (\z -> V.snoc z newCrop)
        modify $ over (pos . _2) (+ crW)
        slice 
{-# INLINE slice #-}

runSlice :: Image VS X Bit  -> V.Vector (V.Vector (Image VS X Bit ))
runSlice !i = view acc $ execState slice (initCropper i)
{-# INLINE runSlice #-}