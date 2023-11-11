-- 2022-07-03, issue #1954
--
-- Purpose of this test:
-- Make sure that `cabal get --only-package-description` works

import Test.Cabal.Prelude
main = cabalTest $ withRepo "repo" $ do
  cabal "update" []
  cabal
    "get"
    [ "criterion", "--only-package-description" ]
  void (shell "rm" ["criterion-1.1.4.0.cabal"])
