{-# LANGUAGE TemplateHaskell #-} 

module Opts  (inFile, opts, outFile, scaleFac) where 

import System.Console.GetOpt
    ( getOpt, ArgDescr(ReqArg), ArgOrder(RequireOrder), OptDescr(..) )

import Control.Lens ( makeLenses ) 
import System.Environment ( getArgs ) 
import Data.Maybe ( fromMaybe )
import Text.Read ( readMaybe )
import Control.Lens ( makeLenses ) 

data Options = Options {_inFile   :: FilePath
                       ,_outFile  :: Maybe FilePath
                       ,_scaleFac :: Maybe Int  } 
makeLenses ''Options 

data Flag = Input FilePath 
          | Output FilePath
          | Scale  Int 
            deriving Show 
        
isInput :: Flag -> Bool
isInput (Input _) = True
isInput _         = False 

isOutput :: Flag -> Bool
isOutput (Output x) = True 
isOutput _          = False 

isScale :: Flag -> Bool
isScale (Scale x) = True
isScale _         = False 


opts :: IO Options
opts = do
  (opts,nopts,errs) <- getOptions
  let (Input input)  = 
          case filter isInput opts of
            [] -> 
              error $ "Error: No input file." 
                    <> "Use -i /path/to/file to designate an image as input"
            [x] -> x
            xs  -> error "Error: Please only use one input file at a time"

  let  output = 
          case filter isOutput opts of
            []         -> Nothing 
            [Output o] -> Just o 
            xs  -> error "Error: Please use only one output file at a time"      
  let  scale  = 
        case filter isScale opts of
            []        -> Nothing 
            [Scale s] -> Just s 
            ss  -> error "Error: Please use only one scale at a time" 
  return $ Options input output scale 




getOptions :: IO ([Flag], [String], [String])
getOptions = 
  getArgs >>= \args ->  pure $ getOpt RequireOrder options args 

testOpts :: IO () 
testOpts = do 
  args <- getOptions
  print args 

scale :: String -> Flag
scale x = Scale . fromMaybe 1 $ (readMaybe x :: Maybe Int)

options :: [OptDescr Flag]
options = 
  [ Option ['i'] ["source"] (ReqArg Input "FILE") "input FILE"
  , Option ['o'] ["output"] (ReqArg Output "FILE")  "output FILE"
  , Option ['s'] ["scale"]  (ReqArg scale "SCALE") "integer scale"]