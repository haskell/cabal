import Test.Cabal.Prelude

main = cabalTest $ do
  -- Basic output
  void $ cabal "path" ["-z", "--output-format=key-value", "--compiler-info"]
  void $ cabal "path" ["-z", "--output-format=json", "--compiler-info"]
  void $ cabal "path" ["-z", "--compiler-info"]
