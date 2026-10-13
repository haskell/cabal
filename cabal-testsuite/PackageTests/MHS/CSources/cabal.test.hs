import Test.Cabal.Prelude

-- The C sources of libraries with MicroHs, if it is installed: mhs compiles
-- them when it links a program, from beside the package file of the library,
-- with the install-includes of the library on the include path. mhs-cdep
-- comes from a repository and is installed in the store; mhs-csources is
-- built in place.
main = cabalTest $ do
    skipUnless "no mhs" =<< isAvailableProgram mhsProgram
    recordMode DoNotRecord $ withRepo "repo" $ do
        cabal "v2-build" ["all"]
        out <- cabal' "v2-run" ["mhs-csources"]
        assertOutputContains "add 2 3 = 6" out
        assertOutputContains "twice 21 = 42" out
        assertOutputContains "offset = 7" out
