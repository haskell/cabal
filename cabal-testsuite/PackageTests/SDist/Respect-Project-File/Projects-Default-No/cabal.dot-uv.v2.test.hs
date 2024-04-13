import Test.Cabal.Prelude

-- cabal.dot-uv.project has "packages: .". That package is uv.cabal and "cabal
-- v2-sdist" writes sdist/uv-0.1.tar.gz. This is correct.
main = cabalTest . withProjectFile "cabal.dot-uv.project" $ do
    cabal "v2-sdist" ["all"]
