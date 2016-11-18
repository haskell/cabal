import Test.Cabal.Prelude
-- On a previous buggy version of my patch, setup-external.test.hs
-- passed only because the installed library caused setup to think that
-- the dependency was fulfilled.  Make sure we ignore the dependency
-- entirely.
main = setupAndCabalTest $ setup_build []
