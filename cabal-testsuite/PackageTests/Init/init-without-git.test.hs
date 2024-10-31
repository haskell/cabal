import Test.Cabal.Prelude
import System.Directory
import System.FilePath
import Distribution.Simple.Utils
import Distribution.Verbosity

-- Test cabal init when git is not installed
main = do
  tmp <- getTemporaryDirectory
  withTempDirectory normal tmp "bin.XXXX" $
    \bin -> cabalTest . withSourceCopyDir "app" $
      do
        ghc_path <- programPathM ghcProgram
        cabal_path <- programPathM cabalProgram
        withSymlink ghc_path (bin </> "ghc") . withSymlink cabal_path (bin </> "cabal") .
          withEnv [("PATH", Just bin)] .
            withSourceCopyDir "app" $ do
              cwd <- fmap testSourceCopyDir getTestEnv

              buildOut <- withDirectory cwd $ do
                cabalWithStdin "init" ["-i"]
                  "2\n\n5\n\n\n2\n\n\n\n\n\n\n\n\n\n"
                setup "configure" []
                setup' "build" ["app"]

              assertFileDoesContain (cwd </> "app.cabal")   "3.0"
              assertOutputContains "Linking" buildOut
