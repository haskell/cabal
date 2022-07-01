import Test.Cabal.Prelude

-- You need to specify `default-language`.
main = cabalTest $
  fails $ cabal "check" []
