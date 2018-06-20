import Test.Cabal.Prelude

-- This test is similar to the simplified example in issue #4288. The package's
-- setup script only depends on base and setup-helper. setup-helper exposes a
-- function that is a wrapper for Cabal's defaultMain (similar to
-- cabal-doctest). This test builds the package to check that the flags passed
-- to the setup script are compatible with the version of Cabal that it depends
-- on, even though Cabal is only a transitive dependency.
main = cabalTest $ do
  skipUnless =<< hasNewBuildCompatBootCabal
  r <- recordMode DoNotRecord $ cabal' "new-build" ["T4288"]
  assertOutputContains "This is setup-helper-1.0." r
  assertOutputContains
      ("In order, the following will be built: "
       ++ " - setup-helper-1.0 (lib:setup-helper) (first run) "
       ++ " - T4288-1.0 (lib:T4288) (first run)")
      r
