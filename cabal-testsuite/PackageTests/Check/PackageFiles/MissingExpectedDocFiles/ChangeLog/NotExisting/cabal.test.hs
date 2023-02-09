import Test.Cabal.Prelude

-- Omitting ChangeLog.md but not README in extra-doc-files
main = cabalTest $ do
  cabal "check" []
