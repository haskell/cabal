import Distribution.System (OS (Windows), buildOS)
import System.Directory (copyFile, createDirectoryIfMissing)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>), takeDirectory)
import qualified System.FilePath.Posix as Posix
import qualified System.FilePath.Windows as Windows
import Test.Cabal.Prelude

-- | Test that source-repository-package works correctly with shallow clones
-- when multiple packages from the same repository are referenced at different commits.
--
-- Regression test for: https://github.com/haskell/cabal/pull/10254#issuecomment-4586318112
-- Before the fix, Cabal would try to use --reference between shallow clones at
-- different commits, causing "fatal: reference repository '...' is shallow".
main :: IO ()
main = cabalTest $ withShorterPathForNewBuildStore $ recordMode DoNotRecord $ do
  env <- getTestEnv
  let testDir = testCurrentDir env
      upstreamPath = testDir </> "upstream"

  -- Copy template files from repo-template/ to upstream/
  -- We need to copy because:
  --   1. The test framework copies all files to a temp directory
  --   2. We need to initialize a git repo and make separate commits for pkg-a and pkg-b
  --   3. Can't initialize git repo in repo-template/ since it's tracked by the outer repo
  liftIO $ do
    copyFileToDir (testDir </> "repo-template/pkg-a/pkg-a.cabal") (upstreamPath </> "pkg-a/pkg-a.cabal")
    copyFileToDir (testDir </> "repo-template/pkg-a/src/PkgA.hs") (upstreamPath </> "pkg-a/src/PkgA.hs")
    copyFileToDir (testDir </> "repo-template/pkg-b/pkg-b.cabal") (upstreamPath </> "pkg-b/pkg-b.cabal")
    copyFileToDir (testDir </> "repo-template/pkg-b/src/PkgB.hs") (upstreamPath </> "pkg-b/src/PkgB.hs")

  -- Initialize git repo with two packages at different commits
  (pkgACommit, pkgBCommit) <- withDirectory "upstream" $ do
    git "init" []
    git "config" ["user.email", "testsuite@example.invalid"]
    git "config" ["user.name", "Cabal Testsuite"]
    
    -- Commit pkg-a first
    git "add" ["pkg-a"]
    git "commit" ["-m", "Add pkg-a"]
    pkgACommit <- gitCommitHash
    
    -- Then commit pkg-b
    git "add" ["pkg-b"]
    git "commit" ["-m", "Add pkg-b"]
    pkgBCommit <- gitCommitHash
    
    pure (pkgACommit, pkgBCommit)

  -- Generate cabal.project dynamically with commit hashes
  -- (Must use writeSourceFile because commit hashes are determined at runtime)
  writeSourceFile "cabal.project" $
    unlines
      [ "packages: dummy-app"
      , ""
      , "source-repository-package"
      , "  type: git"
      , "  location: " ++ fileUriFromPath upstreamPath
      , "  tag: " ++ pkgACommit
      , "  subdir: pkg-a"
      , ""
      , "source-repository-package"
      , "  type: git"
      , "  location: " ++ fileUriFromPath upstreamPath
      , "  tag: " ++ pkgBCommit
      , "  subdir: pkg-b"
      ]

  -- Build should succeed without shallow reference errors
  result <- cabal' "v2-build" ["all", "-v"]
  assertExitCode ExitSuccess result
  assertOutputDoesNotContain "reference repository" result

  where
    gitCommitHash = do
      result <- git' "rev-parse" ["HEAD"]
      case lines (resultOutput result) of
        (commit:_) -> pure commit
        [] -> error "git rev-parse returned no output"

    fileUriFromPath path =
      case buildOS of
        Windows -> "file:///" ++ toPosixPath path
        _ -> "file://" ++ toPosixPath path

    toPosixPath = map toPosixSeparator

    toPosixSeparator pathSeparator
      | pathSeparator == Windows.pathSeparator = Posix.pathSeparator
      | otherwise = pathSeparator
      
    copyFileToDir src dest = do
      createDirectoryIfMissing True (takeDirectory dest)
      copyFile src dest
