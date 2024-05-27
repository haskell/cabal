import Test.Cabal.Prelude
import System.Exit
import System.FilePath
import System.Process

main = cabalTest $ do
  skipIfJavaScript
  env <- getTestEnv
  recordMode DoNotRecord $ do
    let cwd = testCurrentDir env
    ghcPath <- programPathM ghcProgram
    -- Create a dummy GHC proxy with `ghc --info` table augmented with “ghc” entry
    mkProxy cwd ghcPath "ghc"
    -- Create a dummy ghc-pkg with an erroneous version.
    mkProxy cwd ghcPath "ghc-pkg"
    -- Use the proxy to compile a package.
    -- The test will pass only if cabal deduces the real GHC path from the “ghc”
    -- entry above, because it will then search for ghc-pkg in the real GHC path
    -- and not select the dummy ghc-pkg proxy.
    cabal "build" ["--with-compiler", cwd </> "proxies" </> "ghc", "p"]
  where
    -- Create a dummy program proxy using GHC make
    mkProxy cwd ghcPath prog = do
      (exitCode, _, err) <- liftIO $ readProcessWithExitCode
        ghcPath
        [ "--make"
        , "-DGHC_PATH=\"" ++ ghcPath ++ "\""
        , cwd </> "proxies" </> prog <.> "hs" ]
        ""
      case exitCode of
        ExitSuccess -> pure ()
        _ -> fail ("Cannot compile " ++ prog ++ " proxy: " ++ err)
