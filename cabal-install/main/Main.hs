module Main (main) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import System.Environment (getArgs)

import qualified Distribution.Client.Main as Client

import GHC.Debug.Stub

main :: IO ()
main = getArgs >>= withGhcDebug . Client.main
