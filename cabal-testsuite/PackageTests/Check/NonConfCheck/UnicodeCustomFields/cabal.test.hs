import Test.Cabal.Prelude

-- No unicode in custom fields.
main = cabalTest . recordMode DoNotRecord $
  fails $ cabal "check" []

  -- You cannot check this against the output,
  -- as the way to display “Wnknown unicode
  -- char” wobbles between OSes.
