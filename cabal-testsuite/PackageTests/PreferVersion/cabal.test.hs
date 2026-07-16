-- 2026-06-07
--
-- Purpose of this test:
-- Show that --prefer-version behaves as expected

import Test.Cabal.Prelude
main = cabalTest $ withRepo "repo" $ do
  cabal "update" []
  res <- cabal' "build" [ "-v3", "--prefer-version=latest" ]
  assertOutputContains "using containers-999.0" res
  res <- cabal' "build" [ "-v3", "--prefer-version=oldest" ]
  assertOutputContains "using containers-0.0.1" res
  res <- cabal' "build" [ "-v3", "--prefer-version=installed" ]
  assertOutputDoesNotContain "using containers-0.0.1" res
  assertOutputDoesNotContain "using containers-999.0" res

