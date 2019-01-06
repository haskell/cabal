import Test.Cabal.Prelude

main = cabalTest $ do
    -- some ways of explicitly specifying an exe
    cabal' "v2-run" ["foo"] >>= assertOutputContains "Hello Foo"
    cabal' "v2-run" ["exe:bar"] >>= assertOutputContains "Hello Bar"
    cabal' "v2-run" ["MultipleExes:foo"] >>= assertOutputContains "Hello Foo"
    -- there are multiple exes in ...
    fails (cabal' "v2-run" []) >>= assertOutputDoesNotContain "Hello" -- in the same project
    fails (cabal' "v2-run" ["MultipleExes"]) >>= assertOutputDoesNotContain "Hello" -- in the same package

