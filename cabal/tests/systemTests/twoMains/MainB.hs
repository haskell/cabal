module Main where

import System.Environment (getArgs)
import Control.Monad (when)

main = do print 'b'
          args <- getArgs
          let isB = head args
          when (isB /= "isB") (error "B is not B!")
