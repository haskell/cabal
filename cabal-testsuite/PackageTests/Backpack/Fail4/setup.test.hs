import Test.Cabal.Prelude

main = setupAndCabalTest $ do
  skipUnlessGhcVersion ">= 8.1"
  r <- fails $ setup' "configure" []
  assertOutputContains "UnfilledSig" r
  assertOutputContains "brought into scope by build-depends: Fail4:sig-lib-a" r
  assertOutputContains "brought into scope by build-depends: Fail4:sig-lib-b" r
  return ()
