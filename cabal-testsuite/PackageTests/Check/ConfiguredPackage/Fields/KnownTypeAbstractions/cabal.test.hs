import Test.Cabal.Prelude

-- Uknown extension, exception for TypeAbstractions, see #9496
main = cabalTest $
  cabal "check" []
