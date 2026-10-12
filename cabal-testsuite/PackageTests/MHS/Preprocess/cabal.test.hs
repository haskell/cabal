import Test.Cabal.Prelude

-- The modules that a preprocessor generates, with MicroHs, if it is installed
-- along with hsc2hs: a module of the library and the main module of the
-- executable come from .hsc files, which hsc2hs turns into Haskell in the
-- build directories, where mhs has to find them.
main = cabalTest $ do
    skipUnless "no mhs" =<< isAvailableProgram mhsProgram
    skipUnless "no hsc2hs" =<< isAvailableProgram hsc2hsProgram
    recordMode DoNotRecord $ do
        cabal "v2-build" ["all"]
        out <- cabal' "v2-run" ["mhs-preprocess"]
        assertOutputContains "sizeof int = 4" out
        assertOutputContains "char bits = 8" out
