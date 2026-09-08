import Test.Cabal.Prelude

import Data.List (isInfixOf)
import Distribution.Package (mkPackageName)
import System.FilePath ((</>))
import Test.Cabal.Plan (ConfiguredInplace (..), InstallItem (..), Plan (..))

-- Build a build-tool dependency FROM SOURCE with a distinct build compiler.
--
-- 'app' (host stage) depends on the library 'mylib' and on the build tool
-- 'tool', which itself depends on 'mylib'. Neither 'tool' nor 'mylib' is
-- pre-installed anywhere, so the build stage has to configure, build and
-- register them with the build compiler, into that compiler's package
-- database. That is exactly what BuildCompilerSetup does not cover (its
-- setup dependencies are pre-existing in the build compiler's global db).
--
-- Each executable prints the __GLASGOW_HASKELL__ it was compiled with and the
-- one its copy of 'mylib' was compiled with; a package configured with the
-- wrong compiler, or linked against the other stage's copy of 'mylib',
-- shows up as a mismatch.
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
      -- 'tool' is in the plan twice: as a host-stage local package (every
      -- local package is a solver goal) and as app's build-stage build tool.
      -- plan.json does not record the stage yet, so pick the copy built by the
      -- build compiler through its dist dir, which carries the compiler id.
      Just plan <- testPlan <$> getTestEnv
      let toolDists =
            [ configuredInplaceDistDir c
            | AConfiguredInplace c <- planInstallPlan plan
            , configuredInplacePackageName c == mkPackageName "tool"
            , ("ghc-" ++ buildVersion) `isInfixOf` configuredInplaceDistDir c
            ]
      toolDist <- case toolDists of
        [d] -> return d
        ds -> fail ("expected exactly one build-stage copy of tool in the plan, found " ++ show ds)
      tool <- runM (toolDist </> "build" </> "tool" </> "tool") [] Nothing
      assertOutputContains ("tool-ghc: " ++ show (ghcVersionInt buildVersion)) tool
      assertOutputContains ("tool-lib-ghc: " ++ show (ghcVersionInt buildVersion)) tool
      app <- runPlanExe' "app" "app" []
      assertOutputContains "app: 42" app
      assertOutputContains ("app-ghc: " ++ show (ghcVersionInt hostVersion)) app
      assertOutputContains ("app-lib-ghc: " ++ show (ghcVersionInt hostVersion)) app
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
