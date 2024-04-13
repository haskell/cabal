import Test.Cabal.Prelude
-- See #4332, dep solving output is not deterministic
main = cabalTest . recordMode DoNotRecord $
  -- Note: we bundle the configure script so no need to autoreconf while building
  cabal "v2-build" ["all"]
