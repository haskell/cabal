import Test.Cabal.Prelude
-- Test that Paths module is generated and usable when relocatable is turned on.

main = do
  skipIfWindows "no relocatable builds"
  setupAndCabalTest $ do
    skipUnlessGhcVersion ">= 8.0"
    withPackageDb $ setup_build ["--enable-relocatable"]
