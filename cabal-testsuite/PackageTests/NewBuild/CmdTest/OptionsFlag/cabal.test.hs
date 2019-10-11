import Test.Cabal.Prelude

main = cabalTest $ do
    cabal "v2-test"
      [ "--test-option=1"
      , "--test-options=\"2 3\""
      , "--test-option=4"
      , "--test-options=\"5 6\""
      ]
