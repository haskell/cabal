import Test.Cabal.Prelude
main = setupAndCabalTest $ do
    skipUnlessGhcVersion ">= 8.1"
    fails (setup' "configure" [])
      >>= assertRegex "Expect problem with Asdf" "Asdf"
