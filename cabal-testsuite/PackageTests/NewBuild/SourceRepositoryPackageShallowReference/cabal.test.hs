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
-- This is a regression test for: https://github.com/haskell/cabal/issues/10249
-- Before the fix, Cabal would try to use --reference between shallow clones at
-- different commits, causing "fatal: reference repository '...' is shallow".
main :: IO ()
main = cabalTest $ withShorterPathForNewBuildStore $ recordMode DoNotRecord $ do
  env <- getTestEnv
  let testDir = testCurrentDir env
      upstreamPath = testDir </> "upstream"

  -- Copy template files from upstream-repo/ to upstream/
  -- (the test framework copies upstream-repo/ to the temp dir automatically)
  liftIO $ do
    copyFileToDir (testDir </> "upstream-repo/pkg-a/pkg-a.cabal") (upstreamPath </> "pkg-a/pkg-a.cabal")
    copyFileToDir (testDir </> "upstream-repo/pkg-a/src/PkgA.hs") (upstreamPath </> "pkg-a/src/PkgA.hs")
    copyFileToDir (testDir </> "upstream-repo/pkg-b/pkg-b.cabal") (upstreamPath </> "pkg-b/pkg-b.cabal")
    copyFileToDir (testDir </> "upstream-repo/pkg-b/src/PkgB.hs") (upstreamPath </> "pkg-b/src/PkgB.hs")

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

  -- Generate cabal.project that references both packages at different commits
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
