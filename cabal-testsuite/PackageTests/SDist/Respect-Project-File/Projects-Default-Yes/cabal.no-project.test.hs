import Test.Cabal.Prelude

-- When no project is given, "cabal sdist" finds a default cabal.project that
-- has "packages: p/ q/" and writes sdist/p-0.1.tar.gz and sdist/q-0.1.tar.gz.
-- This is correct.
--
-- TODO: Check that the code is behaving the same as it would have if
-- "--project-file=cabal.project" was given or if it is using project probing.
main = cabalTest $ do
    cabal "sdist" ["all"]
