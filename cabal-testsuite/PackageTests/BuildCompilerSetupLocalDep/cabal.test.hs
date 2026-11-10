import Test.Cabal.Prelude

-- A custom Setup.hs whose setup-depends include a local package of the
-- project, under a distinct build compiler.
--
-- 'helper' is a project package, so it is a solver goal on both machines and
-- its two copies share the in-place unit id helper-0.1.0.0-inplace. client's
-- Setup.hs runs on the build machine and needs the build-machine copy built
-- and registered before it is compiled. Planning used to give the library
-- target to the host copy instead, which was then pruned because nothing on
-- the host machine needs it. The build-machine copy was only configured and
-- compiling the Setup.hs failed with "cannot satisfy -package-id".
--
-- The target is 'client' only, as in the original report: with 'all' the
-- host copy would be built as a target in its own right.
main :: IO ()
main = cabalTest . recordMode DoNotRecord $ do
  withBuildCompiler $ \bc -> do
    env <- getTestEnv
    hostVersion <- numericVersionOf (testCompilerPath env)
    buildVersion <- numericVersionOf bc
    -- The test is vacuous if the harness handed us the same compiler twice.
    skipIf "build compiler equals host compiler" (hostVersion == buildVersion)
    cabal "v2-build" ["--with-build-compiler=" ++ bc, "client"]
    withPlan $ do
      r <- runPlanExe' "client" "check-compiler" []
      assertOutputContains ("helper-ghc: " ++ show (ghcVersionInt buildVersion)) r
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
