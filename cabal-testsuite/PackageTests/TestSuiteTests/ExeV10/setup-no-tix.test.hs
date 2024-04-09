{-# LANGUAGE CPP #-}

-- The logic here is tricky.
-- If this is compiled by cabal-install, then the MIN_VERSION_Cabal is set
-- otherwise, we are compiling against Cabal library under test,
-- which is new!
#ifndef MIN_VERSION_Cabal
#define MIN_VERSION_Cabal(x,y,z) 1
#endif

import Test.Cabal.Prelude
import Distribution.Simple.Hpc
#if MIN_VERSION_Cabal(3,11,0)
import Distribution.Utils.Path
  ( unsafeMakeSymbolicPath, getSymbolicPath )
mkPath = unsafeMakeSymbolicPath
getPath = getSymbolicPath
#else
mkPath = id
getPath = id
#endif

-- When -fhpc is manually provided, but --enable-coverage is not,
-- the desired behavior is that we pass on -fhpc to GHC, but do NOT
-- attempt to do anything with the tix file (i.e., do not change
-- where it gets output, do not attempt to run hpc on it.)
--
-- This was requested in #1945, by a user who wanted to handle the
-- coverage manually.  Unfortunately, this behavior is (not yet)
-- documented in the manual. (In fact, coverage is not documented
-- at all.)
--
main = setupAndCabalTest $ do
        dist_dir <- fmap testDistDir getTestEnv
        setup_build
          [ "--enable-tests"
          , "--ghc-option=-fhpc"
          , "--ghc-option=-hpcdir"
          , "--ghc-option=" ++ dist_dir ++ "/hpc/vanilla" ]
        setup "test" ["test-Short", "--show-details=direct"]
        lbi <- getLocalBuildInfoM
        let way = guessWay lbi
        shouldNotExist $ getPath $ tixFilePath (mkPath dist_dir) way "test-Short"
