import Data.List
import Test.Cabal.Prelude

main =
  cabalTest $
    cabal "v2-sdist" ["--list-only", "--null"]
