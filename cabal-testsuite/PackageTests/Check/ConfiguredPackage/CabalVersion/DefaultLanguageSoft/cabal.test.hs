import Test.Cabal.Prelude

-- Please specify `default-language`.
main = cabalTest $
  fails $ cabal "check" []
