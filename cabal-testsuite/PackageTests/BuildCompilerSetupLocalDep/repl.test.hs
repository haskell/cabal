import Test.Cabal.Prelude

-- A multi-repl session over client's library and helper, under a distinct
-- build compiler. helper is a setup dependency of client, so the plan has
-- two copies of it that share the in-place unit id helper-0.1.0.0-inplace:
-- the host copy, which is the repl target, and the build-machine copy,
-- which client's Setup.hs links against.
--
-- Only the host copy may be loaded in memory. The user target used to
-- attach to both copies, so neither was built to disk and compiling
-- client's Setup.hs failed with "cannot satisfy -package-id".
main :: IO ()
main = cabalTest . recordMode DoNotRecord $ do
  skipUnlessGhcVersion ">= 9.4"
  withBuildCompiler $ \bc -> do
    env <- getTestEnv
    hostVersion <- numericVersionOf (testCompilerPath env)
    buildVersion <- numericVersionOf bc
    -- The test is vacuous if the harness handed us the same compiler twice.
    skipIf "build compiler equals host compiler" (hostVersion == buildVersion)
    res <-
      cabalWithStdin
        "v2-repl"
        ["--with-build-compiler=" ++ bc, "--enable-multi-repl", "lib:client", "helper"]
        "print Client.clientValue"
    -- The build-machine copy is built to disk for the Setup.hs, once ...
    assertOutputContains "Building library for helper-0.1.0.0" res
    -- ... and the host copy is loaded in the session with client's library.
    assertOutputContains "Ok, two modules loaded." res
    -- The session is functional (clientValue = 3735928559).
    assertOutputContains "3735928559" res
  where
    -- The numeric version (e.g. "9.10.3") of the given ghc.
    numericVersionOf ghc = do
      vr <- runM ghc ["--numeric-version"] Nothing
      return (takeWhile (/= '\n') (resultOutput vr))
