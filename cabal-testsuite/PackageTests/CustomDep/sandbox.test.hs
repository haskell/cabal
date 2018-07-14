import Test.Cabal.Prelude
main = cabalTest $ do
    osx <- isOSX
    win <- isWindows
    -- On Travis OSX, Cabal shipped with GHC 7.8 does not work
    -- with error "setup: /usr/bin/ar: permission denied"; see
    -- also https://github.com/haskell/cabal/issues/3938
    -- This is a hack to make the test not run in this case.
    when osx $ skipUnless =<< ghcVersionIs (>= mkVersion [7,10])
    -- On Appveyor, for some reason this test fails sometimes
    -- due to missing symbols in Cabal 1.24. The solution is to
    -- use a newer version of GHC that bundles a newer version
    -- of Cabal, but for now, we skip.
    when win $ skipUnless =<< ghcVersionIs (>= mkVersion [8,2])
    withSandbox $ do
        cabal_sandbox "add-source" ["custom"]
        cabal_sandbox "add-source" ["client"]
        -- NB: This test relies critically on the Setup script being
        -- built against GHC's bundled Cabal.  This means that the
        -- output we see may vary between tests, so we don't record this.
        recordMode DoNotRecord $
            cabal "v1-install" ["client"]
