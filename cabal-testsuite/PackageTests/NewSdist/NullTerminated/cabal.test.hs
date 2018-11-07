import Test.Cabal.Prelude
import Data.List
main = cabalTest $
  cabal "new-sdist" ["--list-only", "--null"]
