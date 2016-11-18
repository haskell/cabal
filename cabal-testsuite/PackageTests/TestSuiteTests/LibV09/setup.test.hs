import Test.Cabal.Prelude
-- Test if detailed-0.9 builds correctly
main = setupAndCabalTest $ setup_build ["--enable-tests"]
