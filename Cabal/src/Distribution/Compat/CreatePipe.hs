module Distribution.Compat.CreatePipe
  {-# DEPRECATED "Use System.Process from package process directly" #-}
  (createPipe) where

import System.Process (createPipe)
