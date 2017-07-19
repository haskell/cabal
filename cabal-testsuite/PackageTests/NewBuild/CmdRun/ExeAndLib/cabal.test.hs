import Test.Cabal.Prelude
import Control.Monad ( (>=>) )
main = cabalTest $ expectBroken 4607 $ do
    -- the exe
    cabal' "new-run" ["foo"] >>= assertOutputContains "Hello World"
    -- the lib
    fails (cabal' "new-run" ["ExeAndLib"]) >>= assertOutputDoesNotContain "Hello World"

