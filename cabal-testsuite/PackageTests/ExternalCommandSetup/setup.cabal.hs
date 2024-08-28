import Test.Cabal.Prelude
import System.Environment

main = setupTest $ do
        withPackageDb $ do
          withDirectory "aaaa" $ setup_install []
          r <- runInstalledExe' "cabal-aaaa" []
          env <- getTestEnv
          let exe_path = testPrefixDir env </> "bin"
          addToPath exe_path $ do
            res <- fails $ withDirectory "custom" $ setup' "aaaa" []
            assertOutputContains "unrecognised command" res

