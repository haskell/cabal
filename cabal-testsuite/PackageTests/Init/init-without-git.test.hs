import Test.Cabal.Prelude
import System.Directory
import System.FilePath
import Distribution.Simple.Utils
import Distribution.Verbosity

-- Test cabal init when git is not installed
main = do
  skipIfWindows "Might fail on windows."
  tmp <- getTemporaryDirectory
  withTempDirectory normal tmp "bin" $
    \bin -> cabalTest $
      do
        ghc_path <- programPathM ghcProgram
        cabal_path <- programPathM cabalProgram
        withSymlink ghc_path (bin </> "ghc") . withSymlink cabal_path (bin </> "cabal") .
          withEnv [("PATH", Just bin)] $ do
            cwd <- fmap testSourceCopyDir getTestEnv

            void . withDirectory cwd $ do
              cabalWithStdin "init" ["-i"]
                "2\n\n5\n\n\n2\n\n\n\n\n\n\n\n\n\n"
