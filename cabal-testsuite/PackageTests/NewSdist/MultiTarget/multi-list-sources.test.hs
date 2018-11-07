import Test.Cabal.Prelude
import Data.List
main = cabalTest $ withSourceCopy $
  cabal "new-sdist" ["a", "b", "--list-only"]
