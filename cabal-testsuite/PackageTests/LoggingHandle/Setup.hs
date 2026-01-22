
module Main (main) where

import Distribution.Simple
import Distribution.Verbosity

import System.IO

-- Custom setup script which simply changes the logging handles of the default
-- user hooks.
--
-- This has the effect of redirecting MOST of the Cabal output to file.
-- MOST, not ALL, because 'simpleUserHooksWithHandles' doesn't modify the
-- logging handles for a few operations (e.g. 'confPkgDescr' in 'configureAction').

main :: IO ()
main = do
  let outPath = "stdout_log.txt"
      errPath = "stderr_log.txt"
  withFile outPath AppendMode $ \ outHandle ->
    withFile errPath AppendMode $ \ errHandle -> do
      hSetBuffering outHandle LineBuffering
      hSetBuffering errHandle LineBuffering
      let
        verbHandles = VerbosityHandles outHandle errHandle
      defaultMainWithHooks $ simpleUserHooksWithHandles verbHandles
