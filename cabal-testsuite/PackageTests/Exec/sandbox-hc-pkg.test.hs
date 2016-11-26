import Test.Cabal.Prelude
import Data.Maybe
main = cabalTest $ do
    withPackageDb $ do
        withSandbox $ do
            fails $ cabal "exec" ["my-executable"]
            cabal "install" []
            -- The library should not be available outside the sandbox
            ghcPkg' "list" [] >>= assertOutputDoesNotContain "my-0.1"
            -- When run inside 'cabal-exec' the 'sandbox hc-pkg list' sub-command
            -- should find the library.
            env <- getTestEnv
            -- TODO: libify me
            let cabal_path = fromMaybe (error "No cabal-install path configured")
                                       (testCabalInstallPath env)
            cabal' "exec" ["sh", "--", "-c"
                          , "cd subdir && " ++ show cabal_path ++
                            -- TODO: Ugh. Test abstractions leaking
                            -- through
                            " --sandbox-config-file " ++ show (testSandboxConfigFile env) ++
                            " sandbox hc-pkg list"]
                >>= assertOutputContains "my-0.1"
