module Main where

import System.Environment (getArgs)

-- Fake alex 3.4: writes invalid Haskell so that GHC rejects the output.
main :: IO ()
main = do
  args <- getArgs
  case args of
    "--version" : _ ->
      putStrLn "Alex version 3.4.0, (c) 2003 Chris Dornan and Simon Marlow"
    _ ->
      writeFile (outputFile args) "module Hello where\nTHIS IS NOT VALID HASKELL\n"

outputFile :: [String] -> FilePath
outputFile ("-o" : f : _) = f
outputFile (_ : rest) = outputFile rest
outputFile [] = error "alex: no -o flag"
