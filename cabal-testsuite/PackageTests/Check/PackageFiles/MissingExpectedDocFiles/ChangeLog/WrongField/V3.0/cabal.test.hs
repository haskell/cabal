import Test.Cabal.Prelude

-- Included ChangeLog.md but not in extra-doc-files
main = cabalTest $ do
  cabal "check" []
