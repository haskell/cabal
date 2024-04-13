import Test.Cabal.Prelude

-- When no project is given, "cabal v2-sdist" finds a default cabal.project that
-- has "packages: Projects-Default-No/p" and writes sdist/p-0.1.tar.gz.  This is
-- correct.
main = cabalTest $ do
    cabal "v2-sdist" ["all"]
