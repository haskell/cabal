import Test.Cabal.Prelude

main = do
  cabalTest $ do
    cabal' "clean" []
    res <- cabalWithStdin "v2-repl" ["-b", "containers"] ":m +Data.Map\n:t fromList"
    assertOutputContains "fromList :: Ord k => [(k, a)] -> Map k a" res

