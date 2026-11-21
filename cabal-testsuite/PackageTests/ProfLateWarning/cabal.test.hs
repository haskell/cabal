import Test.Cabal.Prelude

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
