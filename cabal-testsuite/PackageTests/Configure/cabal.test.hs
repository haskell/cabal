import Test.Cabal.Prelude
import Control.Monad.IO.Class
import Data.Maybe
import System.Directory
import System.Environment
import Data.List (isSuffixOf)

-- Test for 'build-type: Configure' example from the setup manual.
main = cabalTest $ do
    if isWindows
    then do
        (mCI, mSh) <- liftIO $ (,) <$> lookupEnv "CI" <*> lookupEnv "SHELL"
        case (mCI, mSh) of
            (Nothing, Nothing) -> skip "Missing $SHELL"
            (Nothing, Just sh) -> do
                env <- getTestEnv
                void $ shell sh [ "-l", "-c", "cd $(cygpath -m '" <> testTmpDir env <> "') && autoreconf -i"]
                cabal "v2-build" []
            (Just{}, _) -> do
                env <- getTestEnv
                void $ shell "C:\\msys64\\usr\\bin\\bash.exe" [ "-l", "-c", "cd $(cygpath -m '" <> testTmpDir env <> "') && autoreconf -i"]
                cabal "v2-build" []
    else do
        hasAutoreconf <- liftIO $ fmap isJust $ findExecutable "autoreconf"
        skipUnless "no autoreconf" hasAutoreconf
        _ <- shell "autoreconf" ["-i"]
        cabal "v2-build" []
