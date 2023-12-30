import Test.Cabal.Prelude
-- Test that we pick up include dirs from sublibrary
main = setupAndCabalTest $ setup_build []
