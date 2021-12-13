import Test.Cabal.Prelude

main :: IO ()
main = cabalTest $ cabal "build" ["all"]
