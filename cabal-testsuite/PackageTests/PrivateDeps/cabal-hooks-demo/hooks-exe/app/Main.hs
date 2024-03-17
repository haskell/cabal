{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Main where

import System.Environment

import Prelude hiding (getContents, putStr, hPutStr)
import Data.ByteString.Lazy (getContents, putStr, hPutStr)
import System.Environment (getArgs)
import System.IO (stderr, hPrint)
import Data.String
import Data.Binary
import L01.Lib

-- | Create an executable which accepts the name of a hook as the argument,
-- then reads arguments to the hook over stdin and writes the results of the hook
-- to stdout.
main :: IO ()
main = do
  [hookName] <- getArgs
  case hookName of
   "version" -> do
    putStr $ encode (VERSION_lib01 :: String)
   "show" -> do
      s <- getContents
      let a1 :: A = decode s
          res = show a1
      putStr (encode res)
   "inc" -> do
      s <- getContents
      let a1 :: A = decode s
          res = inc a1
      putStr (encode res)
   _ -> error "Hook not yet implemented"
