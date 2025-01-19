module Main where

import System.Environment
import System.IO

-- This is a "fake" version of alex, so it should take the command line arguments
-- as alex.
main :: IO ()
main = do
  (_:"-o":target:source:_) <- getArgs
  let f '0' = '1'
      f c = c
  writeFile target . map f  =<< readFile source
