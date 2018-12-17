import Test.Cabal.Prelude

main = cabalTest $ do
    -- some ways of specifying an exe without ambiguities
    cabal' "v2-run" ["bar-exe"]     >>= assertOutputContains "Hello bar:bar-exe"
    cabal' "v2-run" ["bar:bar-exe"] >>= assertOutputContains "Hello bar:bar-exe"
    cabal' "v2-run" ["foo:foo-exe"] >>= assertOutputContains "Hello foo:foo-exe"
    cabal' "v2-run" ["bar:foo-exe"] >>= assertOutputContains "Hello bar:foo-exe"
    -- there are multiple exes ...
    fails (cabal' "v2-run" [])              >>= assertOutputDoesNotContain "Hello" -- in the same project
    fails (cabal' "v2-run" ["bar"])         >>= assertOutputDoesNotContain "Hello" -- in the same package
    fails (cabal' "v2-run" ["foo-exe"])     >>= assertOutputDoesNotContain "Hello" -- with the same name
    -- invalid exes
    fails (cabal' "v2-run" ["foo:bar-exe"]) >>= assertOutputDoesNotContain "Hello" -- does not exist

