import Test.Cabal.Prelude

main = cabalTest $ withProjectFile "cabal.project" $ do
  cabal "build" ["foo", "--dry-run"]
  fails $ cabal "build" ["foo", "-fbuild-depends-conflict"]
  fails $ cabal "build" ["foo", "-fpkg-config-conflict"]
  fails $ cabal "build" ["foo", "-fbuild-tool-conflict"]
