module Main where
import System
import Control.Monad(when)
main = do print 'a'
          args <- getArgs
          let isB = head args
          when (isB /= "isA") (error "A is not A!")
