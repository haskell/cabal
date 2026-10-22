import Test.Cabal.Prelude

import Distribution.Package (mkPackageName)
import Test.Cabal.Plan (ConfiguredInplace (..), InstallItem (..), Plan (..))

-- Pass @--with-build-compiler@ explicitly equal to the host compiler.
--
-- GHC's own staged bootstrap does exactly this for its non-cross stage1 (both
-- @--with-compiler@ and @--with-build-compiler@ point at the same bootstrap
-- GHC). Naively treating "@--with-build-compiler@ was passed" as "we are
-- cross-compiling" solves, elaborates and builds every shared dependency
-- twice, under two indistinguishable 'UnitId's -- here that dependency is
-- 'mylib', which is both a library dependency of @app@ and a dependency of
-- @tool@, which @app@ uses as a build tool. 'configureToolchains' compares the
-- configured build compiler against the host's, so when they coincide (as
-- here) there is no separate build stage at all: 'mylib' and 'tool' are
-- ordinary, unstaged dependencies and appear in the plan exactly once.
main :: IO ()
main = cabalTest . recordMode DoNotRecord $ do
  env <- getTestEnv
  let hostGhc = testCompilerPath env
  cabal "v2-build" ["--with-build-compiler=" ++ hostGhc, "all"]
  withPlan $ do
    Just plan <- testPlan <$> getTestEnv
    let copiesOf name =
          length
            [ ()
            | AConfiguredInplace ConfiguredInplace{configuredInplacePackageName = pkg} <- planInstallPlan plan
            , pkg == mkPackageName name
            ]
    assertEqual "mylib is planned exactly once (no separate build stage)" 1 (copiesOf "mylib")
    assertEqual "tool is planned exactly once (no separate build stage)" 1 (copiesOf "tool")
    app <- runPlanExe' "app" "app" []
    assertOutputContains "app: 42" app
