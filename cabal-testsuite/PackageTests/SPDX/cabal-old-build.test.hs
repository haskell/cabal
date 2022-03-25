import Test.Cabal.Prelude
main = setupAndCabalTest $ withPackageDb $ do
    setup_install []
    recordMode DoNotRecord $ do
        ghc84 <- isGhcVersion ">= 8.4"
        let lic = if ghc84 then "BSD-3-Clause" else "BSD3"
        ghcPkg' "field" ["my", "license"] >>= assertOutputContains lic
