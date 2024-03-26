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


-- Ensures that even if a .tix file happens to be left around
-- markup isn't generated.
main = setupAndCabalTest $ do
    dist_dir <- fmap testDistDir getTestEnv
    let tixFile = getPath $ tixFilePath (mkPath dist_dir) Vanilla "test-Short"
    withEnv [("HPCTIXFILE", Just tixFile)] $ do
        setup_build
          [ "--enable-tests"
          , "--ghc-option=-fhpc"
          , "--ghc-option=-hpcdir"
          , "--ghc-option=" ++ dist_dir ++ "/hpc/vanilla" ]
        setup "test" ["test-Short", "--show-details=direct"]
    shouldNotExist $ getPath (htmlDir (mkPath dist_dir) Vanilla) </> "hpc_index.html"
