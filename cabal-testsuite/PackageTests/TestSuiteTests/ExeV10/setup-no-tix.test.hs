import Test.Cabal.Prelude
import Distribution.Simple.Hpc

-- Ensures that even if -fhpc is manually provided no .tix file is output.
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
    shouldNotExist $ tixFilePath dist_dir way "test-Short"
