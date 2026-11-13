import Test.Cabal.Prelude

-- #12539: as a courtesy, a custom Setup script runs in the package directory,
-- and is not passed a working directory.
main = cabalTest $ recordMode DoNotRecord $ do
  r <- cabal' "v2-build" ["pkg"]
  assertOutputContains "pkg Setup.hs: working directory OK" r
