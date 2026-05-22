module Main where

import System.Environment (getArgs)

-- Fake alex 3.5: writes valid Haskell so that GHC accepts the output.
main :: IO ()
main = do
  args <- getArgs
  case args of
    "--version" : _ ->
      putStrLn "Alex version 3.5.0, (c) 2003 Chris Dornan and Simon Marlow"
    _ ->
      writeFile (outputFile args) "module Hello where\nhello :: String\nhello = \"hello from alex 3.5\"\n"

outputFile :: [String] -> FilePath
outputFile ("-o" : f : _) = f
outputFile (_ : rest)     = outputFile rest
outputFile []             = error "alex: no -o flag"
