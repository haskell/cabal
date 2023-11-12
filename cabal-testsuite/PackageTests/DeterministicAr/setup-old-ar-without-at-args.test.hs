
import Test.Cabal.Prelude

import Control.Monad.IO.Class

import Test.Cabal.CheckArMetadata

-- Test that setup deterministically generates object archives
main = setupAndCabalTest $ do
  isWin <- isWindows
  ghc94 <- isGhcVersion "== 9.4.*"
  expectBrokenIf (isWin && ghc94) 9414 $ do
    setup_build ["--disable-response-files"]
    dist_dir <- fmap testDistDir getTestEnv
    lbi <- getLocalBuildInfoM
    liftIO $ checkMetadata lbi (dist_dir </> "build")
