import Control.Monad ((>=>))
import System.Exit (ExitCode (ExitFailure))
import Test.Cabal.Prelude

main =
  cabalTest $
    fails (cabal' "v2-run" ["foo"]) >>= assertExitCode (ExitFailure 42)
