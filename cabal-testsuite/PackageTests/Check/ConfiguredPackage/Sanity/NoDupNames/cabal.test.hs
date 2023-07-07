import Test.Cabal.Prelude

-- Duplicate section names.
-- This will be caught by an `error` before `check` has the opportunity
-- to report it.
main = cabalTest $
  fails $ cabal "check" []
