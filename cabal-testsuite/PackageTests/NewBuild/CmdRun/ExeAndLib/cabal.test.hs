import Test.Cabal.Prelude
import Control.Monad ( (>=>) )
main = cabalTest $ do
    -- the exe
    cabal' "v2-run" ["foo"] >>= assertOutputContains "Hello World"
    -- the lib
    fails (cabal' "v2-run" ["ExeAndLib"]) >>= assertOutputDoesNotContain "Hello World"

