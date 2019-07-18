import Test.Cabal.Prelude
import System.Exit (ExitCode (..))

main = cabalTest $ do
    fails $ cabal "v2-build" ["example"]
    fails $ cabal "v2-haddock" ["example"]
