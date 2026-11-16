import Test.Cabal.Prelude

import Data.Maybe (isJust)
import System.Directory (doesFileExist, findExecutable)
import System.Environment (lookupEnv)

-- The configure script needs config.guess and config.sub. Autoconf looks for
-- them in a list of directories that starts with $srcdir and is split on
-- $PATH_SEPARATOR. That is ':' under MSYS, so the search fails if Cabal runs
-- the script by a path with a drive letter (C:/...).
main = setupTest . recordMode DoNotRecord $ do
  autoreconf
  env <- getTestEnv
  hasAuxFiles <- liftIO $ doesFileExist (testCurrentDir env </> "config.guess")
  skipUnless "autoreconf did not install config.guess" hasAuxFiles
  setup "configure" []

-- Runs autoreconf the way the Configure test does.
autoreconf :: TestM ()
autoreconf
  | isWindows = do
      env <- getTestEnv
      (mCI, mSh) <- liftIO $ (,) <$> lookupEnv "CI" <*> lookupEnv "SHELL"
      let bash = if isJust mCI then Just "C:\\msys64\\usr\\bin\\bash.exe" else mSh
      case bash of
        Nothing -> skip "Missing $SHELL"
        Just sh -> void $ shell sh ["-l", "-c", "cd $(cygpath -m '" <> testTmpDir env <> "') && autoreconf -i"]
  | otherwise = do
      hasAutoreconf <- liftIO (isJust <$> findExecutable "autoreconf")
      skipUnless "no autoreconf" hasAutoreconf
      void $ shell "autoreconf" ["-i"]
