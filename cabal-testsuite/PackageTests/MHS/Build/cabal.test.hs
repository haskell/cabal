import Test.Cabal.Prelude

-- Build and run a package with MicroHs, if it is installed.
main = cabalTest $ do
    skipUnless "no mhs" =<< isAvailableProgram mhsProgram
    recordMode DoNotRecord $ do
        cabal "v2-build" ["all"]
        cabal' "v2-run" ["mhs-test"] >>= assertOutputContains "Hello from MicroHs"
