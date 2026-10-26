import Test.Cabal.Prelude

import Distribution.Package (mkPackageName)
import System.FilePath ((</>))
import Test.Cabal.Plan (ConfiguredInplace (..), InstallItem (..), Plan (..), Stage (..))

-- Program options are per stage: @--PROG-options@ applies to the host stage
-- and @--build-PROG-options@ to the build stage, and neither stage inherits
-- the other's.
--
-- 'app' (host stage) uses 'tool' as a build tool, so under cross-compilation
-- 'tool' and its copy of 'mylib' are built for the build stage. The host
-- options define HOST_STAGE and the build options BUILD_STAGE; each module
-- reports which of the two it was compiled with.
main :: IO ()
main = cabalTest . recordMode DoNotRecord $ do
  withBuildCompiler $ \bc -> do
    cabal
      "v2-build"
      [ "--with-build-compiler=" ++ bc
      , "--ghc-options=-DHOST_STAGE"
      , "--build-ghc-options=-DBUILD_STAGE"
      , "app"
      ]
    withPlan $ do
      Just plan <- testPlan <$> getTestEnv
      let toolDists =
            [ configuredInplaceDistDir c
            | AConfiguredInplace c <- planInstallPlan plan
            , configuredInplacePackageName c == mkPackageName "tool"
            , configuredInplaceStage c == Build
            ]
      toolDist <- case toolDists of
        [d] -> return d
        ds -> fail ("expected exactly one build-stage copy of tool in the plan, found " ++ show ds)
      tool <- runM (toolDist </> "build" </> "tool" </> "tool") [] Nothing
      assertOutputContains "tool-stage: build" tool
      assertOutputContains "tool-lib-stage: build" tool
      app <- runPlanExe' "app" "app" []
      assertOutputContains "app-stage: host" app
      assertOutputContains "app-lib-stage: host" app
