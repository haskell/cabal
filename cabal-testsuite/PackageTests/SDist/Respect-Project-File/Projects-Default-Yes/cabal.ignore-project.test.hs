import Test.Cabal.Prelude

-- This test correctly writes sdist/uv-0.1.tar.gz for the uv.cabal package.
main = cabalTest $ do
    cabal "sdist" ["all", "--ignore-project"]
