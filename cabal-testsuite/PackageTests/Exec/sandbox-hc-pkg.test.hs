import Test.Cabal.Prelude
import Data.Maybe
import Distribution.Compat.Directory
import Control.Monad.IO.Class

main = cabalTest $ do
    withPackageDb $ do
        withSandbox $ do
            fails $ cabal "v1-exec" ["my-executable"]
            cabal "v1-install" []
            -- The library should not be available outside the sandbox
            ghcPkg' "list" [] >>= assertOutputDoesNotContain "my-0.1"
            -- When run inside 'cabal-exec' the 'sandbox hc-pkg list' sub-command
            -- should find the library.
            env <- getTestEnv
            -- NB: cabal_path might be relative, so we have to
            -- turn it absolute
            rel_cabal_path <- programPathM cabalProgram
            cabal_path <- liftIO $ makeAbsolute rel_cabal_path
            cabal' "v1-exec" ["sh", "--", "-c"
                          , "cd subdir && " ++ show cabal_path ++
                            -- TODO: Ugh. Test abstractions leaking
                            -- through
                            " --sandbox-config-file " ++ show (testSandboxConfigFile env) ++
                            " v1-sandbox hc-pkg list"]
                >>= assertOutputContains "my-0.1"
