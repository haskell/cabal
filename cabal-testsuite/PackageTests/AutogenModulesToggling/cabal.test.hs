import Test.Cabal.Prelude

main :: IO ()
main = cabalTest . recordMode RecordMarked $ do
  cabal "v2-run" ["-fgenerate", "autogen-toggle-test"]
  cabal "v2-run" ["-f-generate", "autogen-toggle-test"]
