import Test.Cabal.Prelude
-- Test building a dynamic library/executable which uses QuasiQuotes
main = setupAndCabalTest $ do
    skipIfNoSharedLibraries
    setup_build ["--enable-shared", "--enable-executable-dynamic"]
