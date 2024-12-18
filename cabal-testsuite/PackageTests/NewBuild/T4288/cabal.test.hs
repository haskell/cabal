import Test.Cabal.Prelude
import Data.Function ((&))

-- This test is similar to the simplified example in issue #4288. The package's
-- setup script only depends on base and setup-helper. setup-helper exposes a
-- function that is a wrapper for Cabal's defaultMain (similar to
-- cabal-doctest). This test builds the package to check that the flags passed
-- to the setup script are compatible with the version of Cabal that it depends
-- on, even though Cabal is only a transitive dependency.
main = cabalTest $ do
  skipUnless "no v2-build compatible boot-Cabal" =<< hasNewBuildCompatBootCabal
  r <- recordMode DoNotRecord $ cabal' "v2-build" ["T4288"]
  assertOutputContains "This is setup-helper-1.0." r
  "In order, the following will be built:\n\
  \ - setup-helper-1.0 (lib:setup-helper) (first run)\n\
  \ - T4288-1.0 (lib:T4288) (first run)"
    & flip assertOutputContainsMultiline r
