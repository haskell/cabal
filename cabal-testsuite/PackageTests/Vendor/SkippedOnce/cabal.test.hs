import Data.List (isInfixOf)
import Test.Cabal.Prelude

-- A dependency that cannot be vendored is reported once, however many of its
-- components are in the plan. The dependency here is a tarball listed in the
-- project's packages, with a library and a public sublibrary that the project
-- depends on, so it is in the plan twice.
main = cabalTest $ do
  env <- getTestEnv
  withProjectFile "cabal.sdist.project" $
    cabal "v2-sdist" ["--output-directory", testCurrentDir env, "dep"]

  r <- cabal' "v2-vendor" ["--dry-run"]
  let warnings = filter ("Not vendoring dep-1.0" `isInfixOf`) (lines (resultOutput r))
  assertEqual "warnings about dep-1.0" 1 (length warnings)
