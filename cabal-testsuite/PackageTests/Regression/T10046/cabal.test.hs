import Test.Cabal.Prelude

-- T10046
main = cabalTest $ recordMode DoNotRecord $ do
  out <- cabal' "test" ["all"]
  assertOutputDoesNotContain "Failed to find the installed unit 'aa-0.1.0.0-inplace'" out
