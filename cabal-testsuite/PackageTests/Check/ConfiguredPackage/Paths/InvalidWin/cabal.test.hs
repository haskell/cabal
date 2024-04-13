import Test.Cabal.Prelude

import System.Directory (createDirectoryIfMissing)

-- Invalid Windows filepath.
main = cabalTest $ do
  skipIfWindows
  cwd <- testCurrentDir <$> getTestEnv
  liftIO $ createDirectoryIfMissing False $ cwd </> "n?ul"
  liftIO $ writeFile (cwd </> "n?ul" </> "test.a") ""
    -- A directory named like `n?ul` on Windows will make external
    -- tools like git — and hence the whole testsuite — error.
  fails $ cabal "check" []
