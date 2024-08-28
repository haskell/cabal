import Test.Cabal.Prelude

-- The given cabal.project has has "packages: p/ q/" and "cabal v2-sdist" writes
-- sdist/p-0.1.tar.gz and sdist/q-0.1.tar.gz.  This is correct.
main = cabalTest . withProjectFile "cabal.project" $ do
    cabal "v2-sdist" ["all"]
