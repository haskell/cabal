import Distribution.System (OS (Windows), buildOS)
import System.FilePath ((</>))
import qualified System.FilePath.Posix as Posix
import qualified System.FilePath.Windows as Windows
import Test.Cabal.Prelude

-- | A project that takes two packages from the same git repository, each at a
-- different commit.
--
-- Regression test for #12296. Checkouts for @source-repository-package@ are
-- shallow, and git rejects a shallow repository as the argument of
-- @git clone --reference@, so one checkout must never be used as the reference
-- for another.
main :: IO ()
main = cabalTest $ recordMode DoNotRecord $ do
  env <- getTestEnv

  (pkgACommit, pkgBCommit) <- withDirectory "upstream" $ do
    git "init" []
    git "config" ["user.email", "testsuite@example.invalid"]
    git "config" ["user.name", "Cabal Testsuite"]
    -- These commits are throwaway, and a developer's global config may well
    -- turn signing on, which would need a key that is not available here.
    git "config" ["commit.gpgsign", "false"]

    git "add" ["pkg-a"]
    git "commit" ["-m", "Add pkg-a"]
    pkgACommit <- headCommit

    git "add" ["pkg-b"]
    git "commit" ["-m", "Add pkg-b"]
    pkgBCommit <- headCommit

    pure (pkgACommit, pkgBCommit)

  let upstreamUri = fileUri (testCurrentDir env </> "upstream")
  writeSourceFile "cabal.project" $
    unlines
      [ "packages: dummy-app"
      , ""
      , "source-repository-package"
      , "  type: git"
      , "  location: " ++ upstreamUri
      , "  tag: " ++ pkgACommit
      , "  subdir: pkg-a"
      , ""
      , "source-repository-package"
      , "  type: git"
      , "  location: " ++ upstreamUri
      , "  tag: " ++ pkgBCommit
      , "  subdir: pkg-b"
      ]

  -- Planning is enough: the checkouts happen while the project is read, and
  -- the plan only comes out if both packages were found.
  cabal "v2-build" ["all", "--dry-run"]
  where
    headCommit = do
      result <- git' "rev-parse" ["HEAD"]
      case lines (resultOutput result) of
        (commit : _) -> pure commit
        [] -> error "git rev-parse HEAD produced no output"

    -- Git ignores --depth when cloning from a plain local path, so the location
    -- has to be a URI for the checkouts to be shallow in the first place.
    fileUri path = "file://" ++ root ++ map toPosixSeparator path
      where
        root = case buildOS of
          Windows -> "/"
          _ -> ""

    toPosixSeparator c
      | c == Windows.pathSeparator = Posix.pathSeparator
      | otherwise = c
