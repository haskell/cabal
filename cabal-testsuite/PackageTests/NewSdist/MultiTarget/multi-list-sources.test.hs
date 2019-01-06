import Test.Cabal.Prelude
import Data.List
main = cabalTest $ withSourceCopy $
  cabal "v2-sdist" ["a", "b", "--list-only"]
