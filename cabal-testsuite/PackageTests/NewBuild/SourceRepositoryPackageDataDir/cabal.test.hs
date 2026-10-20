import Test.Cabal.Prelude

import Control.Monad (forM_)
import Data.Maybe (isJust)
import System.Directory (copyFile, createDirectoryIfMissing, findExecutable)

-- A source-repository-package that depends on a local package is built in
-- place (it cannot go to the store), so its data files are reached through the
-- <pkg>_datadir variable cabal-install exports. That directory has to be the
-- unpacked source, not the tarball the checkout is packed into.
main = cabalTest . recordMode DoNotRecord $ do
    hasGit <- liftIO (isJust <$> findExecutable "git")
    skipUnless "no git" hasGit
    env <- getTestEnv
    let cwd = testCurrentDir env
        repo = testTmpDir env </> "datatool-repo"
    liftIO $ createDirectoryIfMissing True repo
    forM_ ["datatool.cabal", "Main.hs", "greeting.txt"] $ \f ->
        liftIO $ copyFile (cwd </> "datatool" </> f) (repo </> f)
    rev <- withDirectory repo $ do
        git "init" ["-q"]
        git "add" ["."]
        git "-c" ["user.name=cabal-testsuite", "-c", "user.email=cabal@example.invalid",
                  "commit", "-q", "-m", "datatool"]
        takeWhile (/= '\n') . resultOutput <$> git' "rev-parse" ["HEAD"]
    liftIO $ writeFile (cwd </> "cabal.project") $ unlines
        [ "packages: ."
        , "source-repository-package"
        , "  type: git"
        , "  location: " ++ repo
        , "  tag: " ++ rev
        ]
    r <- cabal' "v2-run" ["datatool"]
    assertOutputContains "hello from the data file of datatool" r
