import Test.Cabal.Prelude

-- This test correctly writes sdist/z-0.1.tar.gz for the z.cabal package.
main = cabalTest $ do
    cabal "sdist" ["all", "--ignore-project"]
