import Test.Cabal.Prelude
-- https://github.com/haskell/cabal/issues/1919
main = setupAndCabalTest $
    withPackageDb $ do
        setup_install []
        setup "haddock" []
