import Test.Cabal.Prelude

-- This test doesn't ignore the project. It should have written
-- sdist/uv-0.1.tar.gz for the uv.cabal package but instead it wrote
-- sdist/p-0.1.tar.gz and sdist/q-0.1.tar.gz.
main = cabalTest $ do
    cabal "v2-sdist" ["all", "--ignore-project"]
