import Test.Cabal.Prelude

-- All `autogen-modules` have to appear in `other-modules` or
-- `exposed-modules` (executables).
main =
  cabalTest $
    fails $
      cabal "check" []
