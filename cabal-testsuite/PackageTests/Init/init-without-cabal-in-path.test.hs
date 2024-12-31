import Distribution.Simple.Utils
import Distribution.Verbosity
import System.Directory
import System.FilePath
import Test.Cabal.Prelude

-- Test $cabal init -n when `cabal` executable is not in PATH
main = do
  tmp <- getTemporaryDirectory
  withTempDirectory normal tmp "bin" $ \bin -> cabalTest $ do
    ghc_path <- programPathM ghcProgram
    cabal_path <- programPathM cabalProgram
    withSymlink ghc_path (bin </> "ghc")
      . withSymlink cabal_path (bin </> "custom-cabal")
      . withEnv [("PATH", Just bin)]
      $ do
        cwd <- fmap testSourceCopyDir getTestEnv
        void . withDirectory cwd $ cabal "init" ["-n"]
