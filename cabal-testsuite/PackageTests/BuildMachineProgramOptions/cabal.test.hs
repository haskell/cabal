import Test.Cabal.Prelude

import Distribution.Package (mkPackageName)
import System.FilePath ((</>))
import Test.Cabal.Plan (ConfiguredInplace (..), InstallItem (..), Plan (..), Machine (..))

-- Program options are per machine: @--PROG-options@ applies to the host machine
-- and @--build-PROG-options@ to the build machine, and neither machine inherits
-- the other's.
--
-- 'app' (host machine) uses 'tool' as a build tool, so under cross-compilation
-- 'tool' and its copy of 'mylib' are built for the build machine. The host
-- options define HOST_MACHINE and the build options BUILD_MACHINE; each module
-- reports which of the two it was compiled with.
main :: IO ()
main = cabalTest . recordMode DoNotRecord $ do
  withBuildCompiler $ \bc -> do
    cabal
      "v2-build"
      [ "--with-build-compiler=" ++ bc
      , "--ghc-options=-DHOST_MACHINE"
      , "--build-ghc-options=-DBUILD_MACHINE"
      , "app"
      ]
    withPlan $ do
      Just plan <- testPlan <$> getTestEnv
      let toolDists =
            [ configuredInplaceDistDir c
            | AConfiguredInplace c <- planInstallPlan plan
            , configuredInplacePackageName c == mkPackageName "tool"
            , configuredInplaceMachine c == Build
            ]
      toolDist <- case toolDists of
        [d] -> return d
        ds -> fail ("expected exactly one build-machine copy of tool in the plan, found " ++ show ds)
      tool <- runM (toolDist </> "build" </> "tool" </> "tool") [] Nothing
      assertOutputContains "tool-machine: build" tool
      assertOutputContains "tool-lib-machine: build" tool
      app <- runPlanExe' "app" "app" []
      assertOutputContains "app-machine: host" app
      assertOutputContains "app-lib-machine: host" app
