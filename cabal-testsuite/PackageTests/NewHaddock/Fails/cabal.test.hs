import System.Exit (ExitCode (..))
import Test.Cabal.Prelude

main = cabalTest $ do
  fails $ cabal "v2-build" ["example"]
  fails $ cabal "v2-haddock" ["example"]
