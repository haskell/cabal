{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- The logic here is tricky.
-- If this is compiled by cabal-install, then the MIN_VERSION_Cabal is set
-- otherwise, we are compiling against Cabal library under test,
-- which is new!
#ifndef MIN_VERSION_Cabal
#define MIN_VERSION_Cabal(x,y,z) 1
#endif

import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.PreProcess
import Distribution.Simple.Utils
import System.Exit
import System.FilePath
import System.Process (rawSystem)

main :: IO ()
main = defaultMainWithHooks
       simpleUserHooks { hookedPreProcessors = [("pre", myCustomPreprocessor)] }
  where
#if MIN_VERSION_Cabal(2,0,0)
    myCustomPreprocessor :: BuildInfo -> LocalBuildInfo -> ComponentLocalBuildInfo -> PreProcessor
    myCustomPreprocessor _bi lbi _clbi =
#else
    myCustomPreprocessor :: BuildInfo -> LocalBuildInfo -> PreProcessor
    myCustomPreprocessor _bi lbi =
#endif
      PreProcessor {
        platformIndependent = True,
        runPreProcessor = mkSimplePreProcessor $ \inFile outFile verbosity ->
          do info verbosity ("Preprocessing " ++ inFile ++ " to " ++ outFile)
#if MIN_VERSION_Cabal(3,7,0)
             callProcess progPath [inFile, outFile],
        ppOrdering = unsorted
#else
             callProcess progPath [inFile, outFile]
#endif
        }
      where
        builddir = buildDir lbi
        progName = "my-custom-preprocessor"
        progPath = builddir </> progName </> progName

    -- Backwards compat with process < 1.2.
    callProcess :: FilePath -> [String] -> IO ()
    callProcess path args =
      do exitCode <- rawSystem path args
         case exitCode of ExitSuccess       -> return ()
                          f@(ExitFailure _) -> fail $ "callProcess " ++ show path
                                               ++ " " ++ show args ++ " failed: "
                                               ++ show f
