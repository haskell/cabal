{-# LANGUAGE CPP #-}
module Main where

import Control.Monad.Catch ()
import Control.Monad.Trans.Class ()
import Data.ByteString ()
import Data.Set ()
import Data.Time ()
import Distribution.Simple ()
import Distribution.Simple.SetupHooks ()
import Distribution.Types.Version ()
import System.Directory ()
import System.FilePath ()
import System.Process ()
import Test.Cabal.Run ()

#ifdef mingw32_HOST_OS
import System.Win32 ()
#else
import System.Posix ()
#endif

main :: IO ()
main = return ()
