import Test.Cabal.Prelude
import System.Exit (ExitCode (..))

main = cabalTest $ cabal "v2-haddock" []
