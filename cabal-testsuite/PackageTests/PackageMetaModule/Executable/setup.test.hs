import Test.Cabal.Prelude
-- Test that PackageMeta module is generated and available for executables.
main = setupAndCabalTest $ setup_build []
