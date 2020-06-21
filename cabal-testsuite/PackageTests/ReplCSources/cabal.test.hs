import Test.Cabal.Prelude

main = cabalTest $ do
    cabal' "clean" []
    res <- cabalWithStdin "repl" ["-v2"] "foo"
    -- Make sure we don't get this ghci error
    -- *Lib> ghc: ^^ Could not load '_foo', dependency unresolved. See top entry above.
    assertOutputDoesNotContain "Could not load" res
    assertOutputContains "Building C Sources..." res
