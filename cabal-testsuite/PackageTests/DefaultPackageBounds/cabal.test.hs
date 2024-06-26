import Test.Cabal.Prelude

main = cabalTest $ withProjectFile "cabal.project" $ do
  cabal "build" ["foo", "--dry-run"]
  fails $ cabal "build" ["foo", "--dry-run", "-fbuild-depends-conflict"]
  fails $ cabal "build" ["foo", "--dry-run", "-fbuild-tool-conflict"]
  cabal "build" ["foo", "--dry-run", "-fbuild-depends-conflict", "--allow-newer=time"]
