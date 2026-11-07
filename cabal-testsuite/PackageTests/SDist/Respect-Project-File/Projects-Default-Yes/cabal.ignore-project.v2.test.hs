import Test.Cabal.Prelude

-- With "--ignore-project", "cabal v2-sdist" ignores the default cabal.project
-- that has "packages: p/ q/" and writes sdist/uv-0.1.tar.gz for the uv.cabal
-- package in the current directory. This is correct.
main = cabalTest $ do
    cabal "v2-sdist" ["all", "--ignore-project"]
