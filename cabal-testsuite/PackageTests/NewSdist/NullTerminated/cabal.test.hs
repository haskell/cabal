import Test.Cabal.Prelude
import Data.List
main = cabalTest $
  cabal "v2-sdist" ["--list-only", "--null"]
