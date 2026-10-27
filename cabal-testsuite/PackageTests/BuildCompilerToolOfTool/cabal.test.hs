import Test.Cabal.Prelude

import Distribution.Package (mkPackageName)
import System.FilePath ((</>))
import Test.Cabal.Plan (ConfiguredInplace (..), InstallItem (..), Plan (..), Machine (..))

-- A build tool that itself has a build tool and a custom Setup.hs, under a
-- distinct build compiler.
--
-- 'app' (host machine) has the build tool 'tool'. 'tool' is therefore at the
-- build machine, and its own tool dependency 'gen' and its setup dependencies
-- must stay on the build machine: tools of a build-machine package run where
-- the build runs, which is the build machine again.
-- BuildCompilerToolFromSource only crosses one tool boundary
-- (host -> build); this test crosses a second one (build -> build) for both
-- kinds of tool dependency.
--
-- Each executable prints the __GLASGOW_HASKELL__ it was compiled with, and
-- 'tool' also the one its Setup.hs was compiled with. If the second boundary
-- were resolved at the wrong machine, 'gen' would have no build-machine copy in
-- the plan, or it (or tool's Setup) would report the host compiler.
main :: IO ()
main = cabalTest . recordMode DoNotRecord $ do
  withBuildCompiler $ \bc -> do
    env <- getTestEnv
    hostVersion <- numericVersionOf (testCompilerPath env)
    buildVersion <- numericVersionOf bc
    -- The test is vacuous if the harness handed us the same compiler twice.
    skipIf "build compiler equals host compiler" (hostVersion == buildVersion)
    cabal "v2-build" ["--with-build-compiler=" ++ bc, "app"]
    withPlan $ do
      Just plan <- testPlan <$> getTestEnv
      -- Local packages are also host-machine solver goals, so 'gen' and 'tool'
      -- may each appear on both machines. We want the build-machine copies.
      genDist <- buildMachineDistDir plan "gen"
      toolDist <- buildMachineDistDir plan "tool"
      gen <- runM (genDist </> "build" </> "gen" </> "gen") [] Nothing
      assertOutputContains ("gen-ghc: " ++ show (ghcVersionInt buildVersion)) gen
      tool <- runM (toolDist </> "build" </> "tool" </> "tool") [] Nothing
      assertOutputContains ("tool-ghc: " ++ show (ghcVersionInt buildVersion)) tool
      assertOutputContains ("tool-setup-ghc: " ++ show (ghcVersionInt buildVersion)) tool
      app <- runPlanExe' "app" "app" []
      assertOutputContains ("app-ghc: " ++ show (ghcVersionInt hostVersion)) app
  where
    -- The numeric version (e.g. "9.10.3") of the given ghc.
    numericVersionOf ghc = do
      vr <- runM ghc ["--numeric-version"] Nothing
      return (takeWhile (/= '\n') (resultOutput vr))

    -- The dist dir of the one build-machine copy of the named package.
    buildMachineDistDir plan name =
      case
        [ configuredInplaceDistDir c
        | AConfiguredInplace c <- planInstallPlan plan
        , configuredInplacePackageName c == mkPackageName name
        , configuredInplaceMachine c == Build
        ] of
        [d] -> return d
        ds -> fail ("expected exactly one build-machine copy of " ++ name ++ " in the plan, found " ++ show ds)

-- Convert a GHC numeric version string "M.N.P" to the __GLASGOW_HASKELL__
-- integer M*100 + N (e.g. "9.10.2" -> 910).
ghcVersionInt :: String -> Int
ghcVersionInt s = major * 100 + minor
  where
    (majorStr, rest) = span (/= '.') s
    (minorStr, _) = span (/= '.') (drop 1 rest)
    major = read majorStr
    minor = read minorStr
