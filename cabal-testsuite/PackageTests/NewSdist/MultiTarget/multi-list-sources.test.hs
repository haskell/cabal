import Data.List
import Test.Cabal.Prelude

main =
  cabalTest $
    withSourceCopy $
      cabal "v2-sdist" ["a", "b", "--list-only"]
