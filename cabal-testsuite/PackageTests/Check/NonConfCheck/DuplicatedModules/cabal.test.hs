import Test.Cabal.Prelude

-- Duplicated module.
main = cabalTest $
  fails $ cabal "check" []

  -- TODO: note how conditional give a “potential duplicate”,
  --       which is not true at all.
