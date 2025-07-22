import Test.Cabal.Prelude

-- The given cabal.project has has "packages: p/ q/" and "cabal sdist" writes
-- sdist/p-0.1.tar.gz and sdist/q-0.1.tar.gz.  This is correct but likely
-- accidental as the default cabal.project has the same packages.
main = cabalTest . withProjectFile "cabal.project" $ do
    cabal "sdist" ["all"]
