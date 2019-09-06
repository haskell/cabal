import Test.Cabal.Prelude

main = cabalTest $ do
    cabal "v2-bench"
      [ "--benchmark-option=1"
      , "--benchmark-options=\"2 3\""
      , "--benchmark-option=4"
      , "--benchmark-options=\"5 6\""
      ]
