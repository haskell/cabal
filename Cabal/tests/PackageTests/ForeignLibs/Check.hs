{-# LANGUAGE RecordWildCards #-}
module PackageTests.ForeignLibs.Check where

import Control.Exception
import Data.List
import System.Environment
import System.FilePath
import System.IO.Error

import Distribution.Simple.LocalBuildInfo
import Distribution.System

import Test.Tasty.HUnit
import PackageTests.PackageTester

dir :: FilePath
dir = "PackageTests" </> "ForeignLibs"

suite :: IO TestsConfig -> Assertion
suite cfg = do
    TestsConfig{..} <- cfg

    -- Compile and install the library
    let spec = PackageSpec
            { directory  = dir
            , configOpts = []
            , distPref   = Nothing
            }
    assertInstallSucceeded =<< cabal_install cfg spec

    -- Get the local build info from the myforeignlib test
    -- (as opposed to the local build info for the cabal package tests exe)
    lbi <- getPersistBuildConfig_ (dir </> "dist" </> "setup-config")
    let installDirs = absoluteInstallDirs
          (localPkgDescr lbi)
          lbi
          NoCopyDest

    -- Link a C program against the library
    requireSuccess =<< run (Just dir) testsConfigGccPath [] [
        "-o", "uselib"
      , "UseLib.c"
      , "-l", "myforeignlib"
      , "-L", flibdir installDirs
      ]

    -- Run the C program
    let ldPath = case hostPlatform lbi of
                   Platform _ OSX     -> "DYLD_LIBRARY_PATH"
                   Platform _ Windows -> "PATH"
                   Platform _ _other  -> "LD_LIBRARY_PATH"
    oldLdPath <- getEnv' ldPath
    let newLdPath = flibdir installDirs ++ [searchPathSeparator] ++ oldLdPath
    result@(_, _, output) <- run (Just dir)
                                 (dir </> "uselib")
                                 [(ldPath, Just newLdPath)]
                                 []
    requireSuccess result
    assertBool "Unexpected test output" $ "5678" `isInfixOf` output

getEnv' :: String -> IO String
getEnv' = handle handler . getEnv
  where
    handler e = if isDoesNotExistError e
                  then return ""
                  else throw e
