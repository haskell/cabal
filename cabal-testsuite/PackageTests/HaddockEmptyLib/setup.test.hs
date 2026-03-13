import Test.Cabal.Prelude
-- Test install when the library is empty, for #9997
main = setupAndCabalTest $
       withPackageDb $
       withDirectory "empty" $
       setup_install_with_docs []
