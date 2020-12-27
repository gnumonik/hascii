{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE AllowAmbiguousTypes, RankNTypes, FlexibleInstances, TemplateHaskell, BangPatterns, OverloadedStrings #-}

module AsciiChars (charFeatures) where 

import Utils ( binarize ) 
import Features ( evalFeatureGrid, featureGrid ) 

import Control.Lens
    ( (^.), over, makeLenses, Field1(_1), Field2(_2) )
import Graphics.Image
    ( VS, X, Bit, Y, toImageY, readImageExact, PNG(PNG), Image )
import Graphics.Image.IO ( readImageExact, PNG(PNG) )
import Graphics.Image.Interface ( BaseArray(Image) )
import Graphics.Image.Interface.Vector ( VS ) 
import Graphics.Image.ColorSpace ( X, Bit, Y, toImageY )
import qualified Data.Vector as V



-- data type for an image of a char and the associated char 
data AsciiChar = AsciiChar {_chImg  :: Image VS X Bit
                            ,_letter :: Char} deriving (Show, Eq)
makeLenses ''AsciiChar 


-- gets images of characters + the char the image is an image of 
asciiChars :: IO [AsciiChar]
asciiChars = chars <> symbols 
  where
    dir = "/home/gnumonic/Chars/"
    fatChars = mapM (\x -> getAsciiChar ("/home/gnumonic/Chars" <> (x : "1.png")))
                $ ['a'..'z']
                <> ['A'..'Z']

    chars = mapM (\x -> getAsciiChar ("/home/gnumonic/Chars/" <> (x : ".png")) x) 
           $  ['a'..'z']
           <> ['A'..'Z']
           <> ['1'..'9']
    getAsciiChar :: String -> Char -> IO AsciiChar
    getAsciiChar fpath ch = do
      i <- readImageExact PNG fpath :: IO (Either String (Image VS Y Double)) 
      case i of
        Left err -> fail $ err
        Right anImage ->  return $ AsciiChar (binarize . toImageY $ anImage) ch
    
    t str ch = (str,ch)

    symbols = mapM (\(x,y) -> getAsciiChar ("/home/gnumonic/Chars/" <> x <> ".png") y )
              $ [
                t "question"  '?'
               ,t "rslash"    '/'
               ,t "period"    '.'
               ,t "rpointy"   '>'
               ,t "lpointy"   '<'
               ,t "comma"     ','
               ,t "quote"     '"'
               ,t "tick"      '\''
               ,t "colon"     ':'
               ,t "semicolon" ';'
               ,t "pipe"      '|'
               ,t "lslash"    '\\'
               ,t "rsqbrac"   ']'
               ,t "lsqbrac"   '['
               ,t "rbrac"     '}'
               ,t "lbrac"     '{'
               ,t "backtick"  '`'
               ,t "squiggle"  '~'
               ,t "plus"      '+'
               ,t "minus"     '-'
               ,t "rparen"    ')'
               ,t "lparen"    '('
               ,t "star"      '*' 
               ,t "amp"       '&'
               ,t "carot"     '^'
               ,t "percent"   '%'
               ,t "dollar"    '$'
               ,t "hash"      '#'
               ,t "at"        '@'
               ,t "excl"      '!'
               ,t "question1"  '?'
               ,t "rslash1"    '/'
               ,t "period1"    '.'
               ,t "rpointy1"   '>'
               ,t "lpointy1"   '<'
               ,t "comma1"     ','
               ,t "quote1"     '"'
               ,t "tick1"      '\''
               ,t "colon1"     ':'
               ,t "semicolon1" ';'
               ,t "pipe1"      '|'
               ,t "lslash1"    '\\'
               ,t "rsqbrac1"   ']'
               ,t "lsqbrac1"   '['
               ,t "rbrac1"     '}'
               ,t "lbrac1"     '{'
               ,t "backtick1"  '`'
               ,t "squiggle1"  '~'
               ,t "plus1"      '+'
               ,t "minus1"     '-'
               ,t "rparen1"    ')'
               ,t "lparen1"    '('
               ,t "star1"      '*' 
               ,t "amp1"       '&'
               ,t "carot1"     '^'
               ,t "percent1"   '%'
               ,t "dollar1"    '$'
               ,t "hash1"      '#'
               ,t "at1"        '@'
               ,t "excl1"      '!'               ]


-- gets character features. see the Features module
getCharFeatures :: IO [(V.Vector Int, Char)] 
getCharFeatures = do
  myChars <- asciiChars
  let charFeatures = Prelude.map (\x -> (featureGrid (x ^. chImg),x ^. letter)) myChars
  return $  Prelude.map (\(x,y) -> (evalFeatureGrid x,y)) charFeatures 

-- used for writing the 'features' to a file so recalculating them isn't 
-- necessary each time the program is run.
-- technically useless after 'training' is complete once 
-- but i'm leaving it in for educational purposes
writeCharFeatures :: FilePath -> IO () 
writeCharFeatures fpath = do 
  myChars <- getCharFeatures
  let dec = toHaskellDec myChars
  writeFile fpath dec
 where
   toHaskellDec :: [(V.Vector Int, Char)] -> String
   toHaskellDec fs = 
     let  valDec =  "charFeatures :: [(V.Vector Int, Char)]"
                <> "\n"
                <> "charFeatures = map (over _1 V.fromList) [ "
                <> "\n"
          listElem x =   "    ("
                     <> (show . V.toList $ x ^. _1) 
                     <> ","
                     <> ['\'',x ^. _2,'\'']
                     <> ")"
     in   valDec 
       <> (Prelude.foldr (\x acc -> if null acc 
                            then listElem x 
                            else listElem x <> ",\n" <> acc) [] fs) 
       <> "]"

-- the 'training data'. this is what the program actually uses to match
-- parts of an image to a char
charFeatures :: [(V.Vector Int, Char)]
charFeatures = Prelude.map (over _1 V.fromList) [ 
    ([0,0,0,3,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,4,0,3,4,1,2,0,2,4,0,3,0,0,0,0,3,0,3,1,0,1,1],'a'),
    ([0,2,0,5,4,2,2,0,2,0,0,2,0,0,0,0,0,0,0,3,0,4,4,2,3,0,3,3,0,2,0,2,0,3,2,0,0,1,1,1,1],'b'),
    ([0,0,0,2,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,3,0,2,2,0,3,0,2,0,0,2,0,0,0,0,1,0,0,1,1,0,0],'c'),
    ([0,0,0,2,0,0,0,0,0,2,0,5,0,4,2,2,2,0,0,3,0,2,2,0,3,0,2,3,0,4,0,4,2,3,3,0,0,1,1,1,1],'d'),
    ([0,0,0,2,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,5,0,2,2,0,3,0,2,3,0,3,0,2,1,1,1,0,3,1,1,1,1],'e'),
    ([0,1,0,4,0,0,0,0,0,1,0,1,0,0,0,0,0,0,0,3,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,2,0,0,0],'f'),
    ([0,0,0,2,0,0,0,0,0,0,0,3,0,0,0,0,0,0,0,3,0,1,0,0,0,0,0,4,0,2,0,0,0,0,3,0,0,1,1,1,1],'g'),
    ([0,2,0,4,4,2,2,0,2,0,0,1,0,0,0,0,0,0,0,3,0,2,4,2,3,0,2,3,0,2,0,0,0,0,2,0,3,0,2,1,1],'h'),
    ([0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,2,0,0,0],'i'),
    ([0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,2,0,0,0],'j'),
    ([0,2,0,3,0,0,0,0,2,0,0,2,0,0,0,0,0,0,0,4,0,1,0,0,0,0,2,1,0,1,0,0,0,0,2,0,3,0,1,0,0],'k'),
    ([0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,4,0,0,0],'l'),
    ([0,0,1,3,0,0,0,0,0,0,0,2,1,0,0,0,0,0,3,0,2,0,0,0,0,2,0,0,3,0,2,0,0,0,0,2,4,0,2,2,2],'m'),
    ([0,0,0,4,0,1,0,0,0,0,0,4,0,0,0,0,0,0,0,3,0,2,4,2,3,0,2,3,0,2,0,4,2,3,2,0,0,1,1,1,1],'n'),
    ([0,0,0,2,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,3,0,2,2,0,3,0,2,3,0,2,0,2,0,3,2,0,0,1,1,1,1],'o'),
    ([0,0,0,3,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,4,0,2,0,0,0,0,2,3,0,0,0,0,0,0,1,0,0,1,1,1,1],'p'),
    ([0,0,0,2,0,0,0,0,0,0,0,3,0,0,0,0,0,0,0,3,0,0,0,0,0,0,0,4,0,2,0,0,0,0,3,0,0,1,1,1,1],'q'),
    ([0,0,0,1,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,3,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0],'r'),
    ([0,0,0,2,0,0,0,0,0,0,0,3,0,0,0,0,0,0,0,2,0,2,0,0,0,0,1,4,0,3,0,4,1,2,2,0,2,1,1,0,0],'s'),
    ([0,0,0,4,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,3,0,2,0,0,0,0,0,0,0,1,0,0,0,0,0,0,5,0,0,0,0],'t'),
    ([0,0,0,2,0,1,0,0,0,0,0,2,0,0,1,0,0,0,0,3,0,4,4,1,3,0,3,3,0,4,0,4,2,3,3,0,0,0,1,1,1],'u'),
    ([0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,3,0,1,0,0,0,0,0,3,0,1,0,0,0,0,2,0,0,0,1,1,1],'v'),
    ([0,0,2,0,0,1,0,0,0,0,0,0,2,0,1,0,0,0,0,6,0,2,2,0,3,0,2,6,0,2,0,2,0,3,3,0,4,1,1,1,1],'w'),
    ([0,0,0,3,2,1,0,0,1,0,0,3,0,2,1,0,1,0,0,3,0,0,2,0,1,0,1,3,0,0,0,2,0,1,1,0,5,0,0,0,0],'x'),
    ([0,0,0,2,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,1,0,2,0,0,0,0,1,1,0,0,0,0,0,0,0,0,4,0,0,1,1],'y'),
    ([0,0,0,2,0,0,0,0,0,0,0,4,0,0,1,0,0,0,0,1,0,3,0,0,0,0,1,1,0,3,0,0,1,0,0,0,1,1,0,1,1],'z'),
    ([0,1,0,3,0,0,0,0,0,1,0,3,0,0,0,0,2,0,1,4,2,0,4,1,3,2,0,4,1,0,2,4,1,3,0,2,0,2,1,1,1],'A'),
    ([3,3,3,3,4,2,4,2,0,3,2,4,2,4,3,3,0,2,3,0,2,3,4,3,3,2,0,0,3,3,0,4,1,3,1,1,3,1,1,2,2],'B'),
    ([0,3,3,0,4,2,1,1,1,4,1,0,0,0,0,3,0,0,2,1,0,2,2,0,3,0,2,0,0,4,1,4,3,0,1,1,0,1,1,0,0],'C'),
    ([4,3,3,0,0,0,2,2,0,4,0,0,3,4,2,1,1,1,3,0,3,3,0,2,0,2,0,1,2,3,0,2,0,3,3,0,0,1,1,1,1],'D'),
    ([3,3,3,3,4,2,4,2,0,3,0,3,0,0,0,1,0,0,3,0,2,3,4,3,3,2,0,0,0,3,0,0,1,0,0,0,3,1,1,0,0],'E'),
    ([0,5,0,5,4,2,3,0,2,3,0,2,0,0,0,1,0,0,0,3,0,2,4,2,3,0,2,0,0,0,0,0,0,0,0,0,3,1,1,0,0],'F'),
    ([1,3,3,0,2,0,2,2,0,4,2,0,0,2,0,4,0,1,3,0,0,3,4,1,1,1,1,0,3,3,1,4,2,3,0,2,1,1,1,4,4],'G'),
    ([2,0,3,0,4,2,2,2,0,0,2,0,3,4,2,2,0,2,3,0,2,0,4,2,3,2,0,0,3,0,2,4,2,3,0,2,3,0,3,4,4],'H'),
    ([0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,5,0,0,0],'I'),
    ([0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,4,0,0,0],'J'),
    ([3,0,3,3,4,2,3,2,0,3,0,0,0,0,0,1,1,0,3,1,2,0,4,2,3,2,0,1,0,2,0,0,1,0,2,0,1,1,2,0,0],'K'),
    ([0,3,0,3,4,2,3,0,2,0,0,0,0,0,0,0,0,0,0,3,0,4,4,2,3,0,2,0,0,3,0,0,1,0,0,0,0,0,1,0,0],'L'),
    ([5,1,3,3,4,2,3,2,2,1,5,3,3,4,2,3,2,2,3,2,2,0,0,0,0,2,0,2,3,0,2,0,0,0,0,2,2,0,2,2,2],'M'),
    ([3,3,3,1,4,2,4,2,1,0,3,0,3,0,0,0,0,2,3,0,2,0,4,2,3,2,0,3,3,1,3,4,2,1,2,2,2,1,1,2,2],'N'),
    ([1,3,3,0,2,0,2,2,0,3,1,0,3,2,0,2,0,2,3,0,0,3,4,1,1,1,1,0,3,3,0,4,1,1,1,1,0,1,1,1,1],'O'),
    ([3,3,3,0,4,2,4,2,0,3,0,3,0,4,2,1,2,0,3,0,2,0,4,2,3,2,0,0,0,0,0,0,0,0,0,0,3,1,3,1,1],'P'),
    ([1,3,3,0,2,0,2,2,0,3,1,0,3,2,0,2,0,2,3,0,0,3,4,1,1,1,1,0,3,4,1,6,2,1,1,2,0,1,1,1,1],'Q'),
    ([3,3,3,0,4,2,4,2,0,3,0,3,0,4,2,1,2,0,3,0,2,0,4,2,3,2,0,3,0,0,2,4,2,2,1,1,3,1,3,2,2],'R'),
    ([0,4,0,4,4,2,2,0,2,4,0,0,0,0,0,1,0,0,0,1,0,4,4,1,1,0,3,1,3,3,1,6,2,4,1,2,4,1,0,3,3],'S'),
    ([1,3,0,0,0,0,2,0,0,3,1,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,5,0,0,0],'T'),
    ([2,0,3,0,4,2,2,2,0,0,2,0,3,4,2,2,0,2,3,0,0,3,4,1,3,1,1,0,3,3,0,4,1,3,1,1,0,0,1,1,1],'U'),
    ([2,0,2,1,2,2,0,2,0,0,2,1,2,2,2,0,0,2,0,3,0,2,0,0,0,0,1,3,0,2,0,0,0,0,2,0,0,0,1,1,1],'V'),
    ([2,0,3,2,0,0,0,2,0,0,2,2,3,0,0,0,0,1,3,3,1,2,4,2,3,2,2,3,3,2,1,4,2,3,2,2,4,3,1,2,2],'W'),
    ([1,3,0,2,0,0,3,0,2,3,1,2,0,0,0,3,2,0,0,3,1,1,2,2,0,0,2,3,0,1,1,2,2,0,2,0,4,0,0,0,0],'X'),
    ([1,2,0,2,0,0,2,0,2,2,1,2,0,0,0,2,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,0,0,0,0],'Y'),
    ([1,3,0,0,0,0,2,0,0,3,2,3,0,4,1,3,1,1,0,3,2,3,4,3,1,1,1,0,0,3,1,0,2,0,0,0,3,1,0,0,0],'Z'),
    ([1,6,0,3,4,0,3,1,2,2,0,3,0,0,0,0,2,0,0,3,0,5,0,0,0,0,0,3,0,5,1,0,0,0,2,0,9,4,0,1,1],'1'),
    ([2,9,0,0,4,0,5,1,2,8,1,6,2,8,4,3,5,2,0,5,3,9,6,4,1,1,4,3,0,6,2,0,2,0,0,0,3,2,0,3,3],'2'),
    ([2,8,0,2,4,0,5,1,2,7,1,6,2,8,4,3,3,2,0,0,2,7,4,4,0,1,1,4,3,8,2,8,4,6,4,2,6,2,1,3,3],'3'),
    ([0,1,0,7,0,0,0,0,1,9,0,9,0,4,2,3,6,0,5,7,0,0,4,0,6,2,2,9,4,9,0,6,2,5,6,2,4,4,3,3,3],'4'),
    ([0,8,0,8,4,2,3,0,4,6,0,5,0,0,0,2,0,0,0,0,2,8,4,4,0,1,2,5,3,8,1,8,3,6,5,2,4,3,2,4,4],'5'),
    ([0,6,3,7,6,4,1,1,4,7,2,3,0,4,0,5,1,1,3,7,0,8,6,2,6,1,4,4,5,8,2,8,4,6,3,3,7,3,3,4,4],'6'),
    ([2,6,0,0,0,0,4,0,0,8,3,8,0,6,2,6,4,1,0,1,0,5,0,0,0,0,0,5,0,1,0,0,0,0,2,0,5,2,0,2,2],'7'),
    ([1,8,2,8,8,4,3,2,4,8,2,6,2,8,4,5,3,2,3,7,2,9,8,4,6,2,4,5,5,9,2,8,4,6,4,3,8,2,2,3,3],'8'),
    ([1,7,3,4,8,4,3,2,3,7,1,4,3,8,4,3,3,2,0,2,0,8,2,2,0,0,2,5,3,8,1,8,3,6,5,2,6,2,2,4,4],'9'),
    ([0,2,0,0,0,0,0,0,0,4,0,5,0,4,2,2,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,1,0,1,1],'?'),
    ([0,0,0,0,0,0,0,0,0,2,0,2,0,0,0,0,2,0,0,2,0,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,0,0,0,0],'/'),
    ([0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],'.'),
    ([0,0,1,3,2,2,0,1,0,0,0,2,0,0,0,0,0,0,1,3,0,0,2,0,2,1,0,2,0,0,0,0,0,0,0,0,4,0,0,1,1],'>'),
    ([0,0,0,3,0,0,0,0,0,0,0,3,0,2,1,0,1,0,0,3,0,0,0,0,1,0,0,3,0,0,0,2,0,1,1,0,2,1,1,0,0],'<'),
    ([0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],','),
    ([0,3,0,1,0,0,0,0,0,3,0,1,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],'"'),
    ([0,3,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],'\''),
    ([0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0],':'),
    ([0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0],';'),
    ([0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,5,0,0,0],'|'),
    ([0,4,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,3,0,0,0,0,2,0,3,0,0,0,0],'\\'),
    ([0,0,0,0,0,0,0,0,0,2,0,3,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,3,0,2,0,0,0,0,2,0,3,1,0,1,1],']'),
    ([0,2,0,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,1,0,0,0],'['),
    ([0,2,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,3,0,2,2],'}'),
    ([0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,3,3,1,0,0],'{'),
    ([0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0],'`'),
    ([0,0,0,3,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,1,3,3],'~'),
    ([0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,3,3,4,4],'+'),
    ([0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,0,0,1,1],'-'),
    ([0,1,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,2,0,0,0,0,0,0,0,0,3,3,0,1,1],')'),
    ([0,0,0,2,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,3,3,0,0,0],'('),
    ([0,0,0,3,0,0,0,0,1,0,0,3,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,3,1,1,1],'*'),
    ([0,1,0,4,0,0,0,0,1,0,0,1,0,0,0,0,1,0,0,4,0,2,2,0,3,0,3,4,1,2,1,4,2,2,3,1,3,1,1,1,1],'&'),
    ([0,1,0,2,0,1,0,0,1,0,0,2,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0],'^'),
    ([1,1,3,3,4,0,2,2,2,1,0,2,0,0,0,0,2,0,0,3,0,1,2,1,0,0,2,3,3,2,0,4,1,3,3,1,5,0,2,2,2],'%'),
    ([0,0,0,4,0,1,0,0,1,0,0,3,0,0,1,0,2,0,0,2,0,3,2,1,0,0,1,5,0,3,0,4,1,2,2,0,5,4,2,0,0],'$'),
    ([0,1,1,5,0,0,0,0,0,1,0,5,1,0,0,0,2,0,2,5,0,2,4,2,3,0,2,5,0,0,0,0,0,1,1,0,5,0,1,1,1],'#'),
    ([0,2,2,2,2,2,0,0,2,2,0,3,3,4,3,0,3,1,3,3,1,3,2,2,0,2,0,5,2,3,0,2,1,2,3,0,3,1,2,2,2],'@'),
    ([0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,2,0,0,0],'!'),
    ([0,9,0,0,2,0,3,0,2,9,1,8,2,8,4,4,5,2,0,3,0,2,0,0,0,0,0,3,0,2,0,0,0,0,1,0,8,3,0,2,2],'?'),
    ([0,0,0,1,0,0,0,0,0,6,0,4,0,0,0,0,4,0,0,5,0,8,0,1,0,0,2,0,0,0,0,0,0,0,0,0,8,3,0,1,1],'/'),
    ([0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],'.'),
    ([4,4,2,5,8,2,4,4,4,0,0,8,5,0,2,0,2,0,2,5,4,4,8,4,2,4,4,8,5,0,0,0,0,4,2,0,2,3,0,3,3],'>'),
    ([0,0,4,8,0,2,0,0,1,3,6,6,2,8,3,3,6,4,4,8,0,0,0,0,4,0,1,6,2,3,6,8,3,3,6,4,4,2,4,0,0],'<'),
    ([0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,0,0,0,0,0,1,0,2,0,0,0,0,2,0,0,0,0,0,0],','),
    ([0,4,0,6,0,0,0,0,2,4,0,6,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0],'"'),
    ([0,4,0,6,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0],'\''),
    ([0,1,0,1,0,0,0,0,0,1,0,1,0,0,0,0,2,0,0,0,0,2,0,0,0,0,0,0,0,2,0,0,0,0,1,0,0,2,0,0,0],':'),
    ([0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0],';'),
    ([0,3,0,3,0,0,0,0,0,3,0,3,0,0,0,0,2,0,0,3,0,3,0,0,0,0,0,3,0,3,0,0,0,0,2,0,9,5,0,1,1],'|'),
    ([0,7,0,4,0,0,1,0,2,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,7,0,0,0,0,4,0,8,3,0,1,1],'\\'),
    ([0,2,0,0,0,0,0,0,0,3,0,3,0,0,0,0,2,0,0,0,0,2,0,0,0,0,0,3,0,3,0,0,0,0,2,0,6,5,0,1,1],']'),
    ([0,6,0,6,0,0,0,0,2,1,0,0,0,0,0,0,0,0,0,6,0,6,0,0,0,0,2,0,0,1,0,0,0,0,0,0,3,1,1,0,0],'['),
    ([0,3,0,0,0,0,0,0,0,4,0,7,0,0,0,0,4,0,0,0,0,3,0,0,0,0,0,7,0,4,0,0,0,1,4,0,5,5,0,4,4],'}'),
    ([0,5,0,7,0,0,0,0,2,3,0,0,0,0,0,0,0,0,0,7,0,5,0,0,1,0,2,0,0,3,0,0,0,0,0,0,5,5,3,0,0],'{'),
    ([1,7,0,0,2,0,4,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],'`'),
    ([0,0,2,3,0,0,0,0,0,0,0,3,5,0,0,0,0,0,5,3,0,0,0,0,2,0,0,3,2,0,0,0,0,2,0,0,9,0,5,5,5],'~'),
    ([0,3,3,5,0,0,0,0,0,3,0,5,3,0,0,0,2,0,0,3,0,3,0,0,0,0,0,3,0,3,0,0,0,0,2,0,9,5,5,5,5],'+'),
    ([0,0,1,3,0,0,0,0,0,0,0,3,1,0,0,0,0,0,1,3,0,0,0,0,2,0,0,3,1,0,0,0,0,2,0,0,9,0,3,4,4],'-'),
    ([0,0,0,0,0,0,0,0,0,5,0,8,0,2,2,0,5,0,0,0,0,0,0,0,0,0,0,8,0,5,0,2,0,3,5,0,0,2,0,2,2],')'),
    ([0,3,0,6,0,0,0,0,1,2,0,0,0,0,0,0,0,0,0,6,0,2,0,0,0,0,1,0,0,2,0,0,0,0,0,0,3,4,1,0,0],'('),
    ([0,0,2,7,4,4,0,1,2,0,0,7,2,4,4,0,2,1,2,5,0,0,0,0,4,0,0,5,2,0,0,0,0,4,0,0,9,3,1,2,2],'*'),
    ([2,7,3,7,8,4,5,2,3,3,0,0,0,0,0,0,0,0,6,2,3,6,8,4,4,4,3,8,4,6,3,8,4,6,6,2,6,2,4,3,3],'&'),
    ([0,5,5,6,6,4,1,1,4,5,0,6,6,6,4,1,6,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0],'^'),
    ([6,4,7,7,6,2,4,5,4,5,1,5,0,2,0,3,5,0,0,7,2,3,6,4,1,1,4,7,7,5,5,4,4,2,5,3,8,1,5,4,4],'%'),
    ([0,6,3,5,6,4,2,1,3,5,0,0,0,2,0,1,3,0,0,1,1,5,4,3,0,1,2,7,2,5,0,6,1,5,5,1,8,5,2,2,2],'$'),
    ([0,4,4,8,4,4,0,2,3,4,2,8,6,6,4,5,5,2,6,8,3,4,6,4,5,1,4,8,4,4,0,0,0,4,4,0,5,4,2,2,2],'#'),
    ([4,7,7,7,6,2,5,5,2,6,6,8,6,4,2,5,3,3,7,7,4,7,6,4,3,5,2,8,7,6,2,4,2,5,3,2,0,4,4,3,3],'@'),
    ([0,2,0,3,0,0,0,0,0,2,0,3,0,0,0,0,2,0,0,2,0,2,0,0,0,0,0,2,0,2,0,0,0,0,0,0,9,4,0,1,1],'!')]