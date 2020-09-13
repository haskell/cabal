import Test.Cabal.Prelude
-- NB: cabal-install doesn't understand --dependency
main = setupTest $ do
    skipUnlessGhcVersion ">= 8.1"
    withPackageDb $ do
      withDirectory "repo/sigs-0.1.0.0" $ setup_install_with_docs ["--cid", "sigs-0.1.0.0", "lib:sigs"]
      withDirectory "repo/indef-0.1.0.0" $ setup_install_with_docs ["--cid", "indef-0.1.0.0", "--dependency=sigs=sigs-0.1.0.0", "lib:indef"]
