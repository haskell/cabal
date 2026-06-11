import Test.Cabal.Prelude

-- Regression test for https://github.com/haskell/cabal/issues/10386
--
-- The library's Lib.hs uses CPP guards that turn into #error unless the
-- builder(...) conditionals in the .cabal file selected the expected branches.
-- So a successful build is the assertion: it proves cabal evaluated
-- builder(cabal >= 1.0) to true, builder(cabal >= 999999) to false, and
-- builder(mcabal) to false.
main = cabalTest $ do
  -- builder(...) requires cabal-version: 3.18, and a build-type: Simple
  -- package with cabal-version 3.18 needs a Cabal library new enough to
  -- satisfy its implicit setup dependency. So this test can only run once the
  -- Cabal library is at least 3.18; until then there is no Cabal version that
  -- both understands builder(...) and satisfies the cabal-version: 3.18 setup
  -- requirement.
  skipUnlessAnyCabalVersion ">= 3.18"
  cabal "v2-build" ["all"]
