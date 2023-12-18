import Test.Cabal.Prelude

-- #9331: PackageInfo functionality should be guarded by cabal-version,
-- does not error when cabal-version is 3.12 or higher.
main = cabalTest $
  cabal "check" []

