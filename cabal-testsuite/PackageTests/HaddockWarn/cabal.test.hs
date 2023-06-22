import System.Exit (ExitCode (..))
import Test.Cabal.Prelude

main = cabalTest $ cabal "v2-haddock" []
