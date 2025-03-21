import Test.Cabal.Prelude

-- Test that configure scripts are passed CXX variable.
main = setupTest $ do
          -- 9.4 was the first version which required a C++ compiler.
          skipUnlessGhcVersion ">= 9.4"
          setup "configure" []

