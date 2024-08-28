import Test.Cabal.Prelude
main = do
  cabalTest $ do
    -- GHC 8.2.2 had a regression ("unknown package: hole"), see also #4908
    skipUnlessGhcVersion ">= 8.2 && <8.2.2 || >8.2.2"
    cabal "v2-build" ["all"]
