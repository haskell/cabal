import Test.Cabal.Prelude

main = setupAndCabalTest $ do
  skipUnlessGhcVersion ">= 8.1"
  r <- fails $ setup' "configure" []
  assertOutputContains "non-existent" r
  return ()
