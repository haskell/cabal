import Test.Cabal.Prelude

import Control.Applicative ((<$>))
import Control.Monad.IO.Class
import System.Environment

-- Test PATH-munging
main = setupAndCabalTest $ do
    path <- liftIO $ getEnv "PATH"
    cwd <- testCurrentDir <$> getTestEnv
    r <- withEnv [("PATH", Just $ cwd ++ ":" ++ path)] $ setup_build []
    runExe' "hello-world" []
        >>= assertOutputContains "1111"
