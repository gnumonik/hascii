module Main where


import IO (displayAscii)
import Opts ( inFile, opts, outFile, scaleFac ) 
import Control.Lens ( (^.) ) 


 
main ::  IO ()
main  = do
  myOpts <- opts 
  let input  = myOpts ^. inFile 
  let output = myOpts ^. outFile
  let scale  = myOpts ^. scaleFac 
  displayAscii input scale output    