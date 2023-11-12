import Test.Cabal.Prelude
import Distribution.Simple.Hpc

-- Ensures that even if a .tix file happens to be left around
-- markup isn't generated.
main = setupAndCabalTest $ do
  isWin <- isWindows
  ghc94 <- isGhcVersion "== 9.4.*"
  expectBrokenIf (isWin && ghc94) 9414 $ do
    dist_dir <- fmap testDistDir getTestEnv
    let tixFile = tixFilePath dist_dir Vanilla "test-Short"
    withEnv [("HPCTIXFILE", Just tixFile)] $ do
        setup_build
          [ "--enable-tests"
          , "--ghc-option=-fhpc"
          , "--ghc-option=-hpcdir"
          , "--ghc-option=" ++ dist_dir ++ "/hpc/vanilla" ]
        setup "test" ["test-Short", "--show-details=direct"]
    shouldNotExist $ htmlDir dist_dir Vanilla "test-Short" </> "hpc_index.html"
