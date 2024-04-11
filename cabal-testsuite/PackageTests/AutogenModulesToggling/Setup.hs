{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.PreProcess
import Distribution.Simple.Program
import Distribution.Types.BuildInfo
import Distribution.Verbosity

import System.Directory

ppHGen :: BuildInfo -> LocalBuildInfo -> ComponentLocalBuildInfo -> PreProcessor
ppHGen _bi lbi _clbi = PreProcessor
  { platformIndependent = True
  , ppOrdering          = unsorted
  , runPreProcessor     = mkSimplePreProcessor $ \inFile outFile verbosity ->
      copyFile inFile outFile
  }

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { hookedPreProcessors = ("hgen", ppHGen) : hookedPreProcessors simpleUserHooks
  }
