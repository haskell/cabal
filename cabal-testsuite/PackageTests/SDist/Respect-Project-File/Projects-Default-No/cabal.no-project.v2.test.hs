import Test.Cabal.Prelude

-- When no project is given, "cabal v2-sdist" probes up the directory tree,
-- finds a default cabal.project and writes sdist/p-0.1.tar.gz. That is
-- acceptable. An alternative and reasonable expectation (but not the behaviour
-- seen) is that project probing would not occur and "cabal v2-sdist" would work
-- on the local uv.cabal package.
main = cabalTest $ do
    cabal "v2-sdist" ["all"]
