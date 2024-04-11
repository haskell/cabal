import Test.Cabal.Prelude

-- cabal.sub-rs.project has "packages: r/ s/" and "cabal v2-sdist" writes
-- sdist/r-0.1.tar.gz and sdist/s-0.1.tar.gz. This is correct.
main = cabalTest . withProjectFile "cabal.sub-rs.project" $ do
    cabal "v2-sdist" ["all"]
