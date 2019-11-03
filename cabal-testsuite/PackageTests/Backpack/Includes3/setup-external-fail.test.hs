import Test.Cabal.Prelude
main = setupAndCabalTest $ do
    skipUnless =<< ghcVersionIs (>= mkVersion [8,1])
    withPackageDb $ do
      withDirectory "repo/sigs-0.1.0.0" $ setup_install []
      withDirectory "repo/indef-0.1.0.0" $ setup_install []
      -- Forgot to build the instantiated versions!
      withDirectory "repo/exe-0.1.0.0" $ do
        -- Missing package message includes a unit identifier,
        -- which wobbles when version numbers change
        r <- recordMode DoNotRecord . fails $ setup' "configure" []
        assertOutputContains "indef-0.1.0.0" r
        return ()
