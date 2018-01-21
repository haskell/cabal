import Test.Cabal.Prelude
main = setupAndCabalTest $ withPackageDb $ do
    -- skip for GHC-8.4 and GHC-head until their Cabal modules are updated.
    skipUnless =<< ghcVersionIs (< mkVersion [8,3])

    setup_install []
    recordMode DoNotRecord $ do
        ghc84 <- ghcVersionIs (>= mkVersion [8,4])
        let lic = if ghc84 then "BSD-3-Clause" else "BSD3"
        ghcPkg' "field" ["my", "license"] >>= assertOutputContains lic
