import Test.Cabal.Prelude
import System.Environment

main = setupTest $ expectBroken 9403 $ do
        withPackageDb $ do
          withDirectory "aaaa" $ setup_install []
          r <- runInstalledExe' "cabal-aaaa" []
          env <- getTestEnv
          path <- liftIO $ getEnv "PATH"
          let exe_path = testPrefixDir env </> "bin"
          let newpath = exe_path ++ ":" ++ path
          let new_env = (("PATH", Just newpath) : (testEnvironment env))
          withEnv new_env $ do
            res <- withDirectory "custom" $ setup' "aaaa" []
            assertOutputContains "did you mean" res


