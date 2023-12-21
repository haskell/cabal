import Test.Cabal.Prelude

-- #9331: PackageInfo functionality should be guarded by cabal-version.
main = cabalTest $
  fails $ cabal "check" []

