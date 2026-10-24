import Test.Cabal.Prelude

-- A library that re-exports a module under another name does not build with
-- MicroHs, if it is installed: mhs sees the modules of the packages it loads
-- under their own names only, so Cabal rejects the renaming.
main = cabalTest $ do
    skipUnless "no mhs" =<< isAvailableProgram mhsProgram
    recordMode DoNotRecord $ do
        r <- fails $ cabal' "v2-build" ["all"]
        assertOutputContains "Data.Char as Characters" r
