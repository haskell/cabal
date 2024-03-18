import Test.Cabal.Prelude

-- cabal.dot-uv.project has "packages: .". That package is uv.cabal and "cabal
-- v2-sdist" writes sdist/p-0.1.tar.gz and sdist/q-0.1.tar.gz. That is correct.
main = cabalTest . withProjectFile "cabal.dot-uv.project" $ do
    cabal "v2-sdist" ["all"]
