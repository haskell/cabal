import Test.Cabal.Prelude
-- Test executable depends on sublibrary.
main = setupAndCabalTest $ setup_build []

