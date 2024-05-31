import Test.Cabal.Prelude

main = cabalTest $ do
  result <- fails $ cabal' "build" []
  assertOutputContains "Error encountered when parsing project file" result
  assertOutputContains "'source-repository-package' is a stanza, not a field." result
