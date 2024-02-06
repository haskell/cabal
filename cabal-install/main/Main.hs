module Main (main) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import System.Environment (getArgs)

import qualified Distribution.Client.Main as Client

main :: IO ()
main = getArgs >>= Client.main
