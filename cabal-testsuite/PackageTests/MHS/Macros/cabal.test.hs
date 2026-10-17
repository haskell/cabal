import Test.Cabal.Prelude

-- The version macros of cabal_macros.h with MicroHs, if it is installed with
-- cpphs, which it runs for CPP.
main = cabalTest $ do
    skipUnless "no mhs" =<< isAvailableProgram mhsProgram
    skipUnless "no cpphs" =<< isAvailableProgram cpphsProgram
    recordMode DoNotRecord $ do
        cabal "v2-build" ["all"]
        out <- cabal' "v2-run" ["mhs-macros"]
        assertOutputContains "base 4.19.1.0" out
        assertOutputContains "bytestring 0.12.1.0" out
        assertOutputContains "package 1.0" out
        assertOutputContains "tool mhs" out
        assertOutputContains "library 1.0" out
