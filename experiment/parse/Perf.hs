module Main where

import IndexUtils

import qualified OldParse  (readFields, ParseResult(..))
import Distribution.Simple.Utils (fromUTF8)

import qualified Parser

import System.Environment
import Control.Exception (evaluate)
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Char8 as BS

main = do
  [which, n, indexfile] <- getArgs
  cabalFiles <- IndexUtils.readPackageIndexFile indexfile

  case which of
    "perf-baseline" -> print (length cabalFiles)

    "perf-old" -> let parse  = OldParse.readFields . fromUTF8 . LBS.unpack
                      parsed = [ pkg | OldParse.ParseOk _ pkg <- map parse cabalFiles ]
                  in  print (length parsed)

    "perf-new" -> let parse  = Parser.readFields . toStrict
                      parsed = [ pkg | Right pkg <- map parse cabalFiles ]
                   in  print (length parsed)

    "check-old" -> let parse  = OldParse.readFields . fromUTF8 . LBS.unpack
                       parsed = [ (msg, f)
                                | (OldParse.ParseFailed msg, f) <-
                                  map (\f -> (parse f, f)) cabalFiles ]
                   in  case parsed of
                         []           -> print "all ok!"
                         ((msg, f):_) -> do print msg
                                            LBS.putStr f

    "check-new" -> let parse  = Parser.readFields . toStrict
                       parsed = [ (msg, f)
                                | (Left msg, f) <-
                                  map (\f -> (parse f, f)) cabalFiles ]
                   in  case drop (read n) parsed of
                         []           -> print "all ok!"
                         ((msg, f):_) -> do print msg
                                            LBS.putStr f

    "extract-fail" -> let parse  = Parser.readFields . toStrict
                          parsed = [ f
                                   | (Left msg, f) <-
                                     map (\f -> (parse f, f)) cabalFiles ]
                      in  sequence_
                            [ LBS.writeFile ("fail/" ++ show n ++ ".cabal") f
                            | (f,n) <- zip parsed [0..] ]

  where
    toStrict = BS.concat . LBS.toChunks
