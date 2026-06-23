import Test.Cabal.Prelude
-- Test that PackageMeta module is generated and available for libraries.
main = setupAndCabalTest $ setup_build []
