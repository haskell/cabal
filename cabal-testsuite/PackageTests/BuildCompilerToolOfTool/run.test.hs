import Test.Cabal.Prelude

-- 'cabal run' and 'cabal list-bin' on a local executable that is also a
-- build tool, under a distinct build compiler.
--
-- 'tool' is a build tool of 'app', so the plan has a build-machine copy of it
-- besides the host copy, with the same in-place unit id. A user target names
-- the host copy only. Before, the target 'tool' named both, and both commands
-- refused it as referring to two executables.
main :: IO ()
main = cabalTest . recordMode DoNotRecord $ do
  withBuildCompiler $ \bc -> do
    env <- getTestEnv
    hostVersion <- numericVersionOf (testCompilerPath env)
    buildVersion <- numericVersionOf bc
    -- The test is vacuous if the harness handed us the same compiler twice.
    skipIf "build compiler equals host compiler" (hostVersion == buildVersion)
    -- 'all' puts the build-machine copy of 'tool' in the plan, as app's tool.
    cabal "v2-build" ["--with-build-compiler=" ++ bc, "all"]
    res <- cabal' "v2-run" ["--with-build-compiler=" ++ bc, "tool"]
    assertOutputContains ("tool-ghc: " ++ show (ghcVersionInt hostVersion)) res
    bin <- cabal' "list-bin" ["--with-build-compiler=" ++ bc, "tool"]
    assertOutputContains ("ghc-" ++ hostVersion) bin
  where
    -- The numeric version (e.g. "9.10.3") of the given ghc.
    numericVersionOf ghc = do
      vr <- runM ghc ["--numeric-version"] Nothing
      return (takeWhile (/= '\n') (resultOutput vr))

-- Convert a GHC numeric version string "M.N.P" to the __GLASGOW_HASKELL__
-- integer M*100 + N (e.g. "9.10.2" -> 910).
ghcVersionInt :: String -> Int
ghcVersionInt s = major * 100 + minor
  where
    (majorStr, rest) = span (/= '.') s
    (minorStr, _) = span (/= '.') (drop 1 rest)
    major = read majorStr
    minor = read minorStr
