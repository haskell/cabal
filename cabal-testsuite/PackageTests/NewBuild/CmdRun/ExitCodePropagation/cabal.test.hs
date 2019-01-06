import Test.Cabal.Prelude
import Control.Monad ( (>=>) )
import System.Exit (ExitCode(ExitFailure))
main = cabalTest $
    fails (cabal' "v2-run" ["foo"]) >>= assertExitCode (ExitFailure 42)

