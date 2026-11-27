import Test.Cabal.Prelude

import Data.List (isInfixOf)

-- #9052: warn when -fprof-late is passed as a GHC option while the
-- profiling detail level is left at its default.
main = cabalTest $ do
  skipUnlessGhcVersion ">= 9.4"
  let warning = "The GHC option -fprof-late is set"

  res <- cabal' "build" ["profLateWarning", "--enable-profiling", "--ghc-options=-fprof-late"]
  assertOutputContains warning res

  cabal' "clean" []
  res' <- cabal' "build" ["profLateWarning", "--enable-profiling", "--profiling-detail=late", "--ghc-options=-fprof-late"]
  assertOutputDoesNotContain warning res'

  -- The check only looks at the component being configured, and it also
  -- looks at ghc-prof-options. Only the executable sets -fprof-late here.
  cabal' "clean" []
  res'' <- cabal' "build" ["multi", "--enable-profiling", "--enable-tests"]
  assertEqual
    "number of -fprof-late warnings"
    1
    (length (filter (warning `isInfixOf`) (lines (resultOutput res''))))
