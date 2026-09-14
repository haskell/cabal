import Distribution.System (OS (Windows), buildOS)
import System.FilePath ((</>))
import qualified System.FilePath.Posix as Posix
import qualified System.FilePath.Windows as Windows
import System.Directory (removeDirectoryRecursive, removeFile)
import Test.Cabal.Prelude

-- A dependency taken from a git repository with source-repository-package is
-- vendored as the source distribution of its checkout, after which the
-- stanza can be dropped and the project built offline.
main :: IO ()
main = cabalTest $ withShorterPathForNewBuildStore $ recordMode DoNotRecord $ do
  env <- getTestEnv
  let cwd = testCurrentDir env
      vendorDir = cwd </> "vendor"

  withDirectory "upstream" $ do
    git "init" []
    git "config" ["user.email", "testsuite@example.invalid"]
    git "config" ["user.name", "Cabal Testsuite"]
    git "config" ["commit.gpgsign", "false"]
    git "add" ["pkg-a"]
    git "commit" ["-m", "Add pkg-a"]

  let upstreamUri = fileUri (cwd </> "upstream")
  writeSourceFile "cabal.project" $
    unlines
      [ "packages: dummy-app"
      , ""
      , "source-repository-package"
      , "  type: git"
      , "  location: " ++ upstreamUri
      , "  subdir: pkg-a"
      ]

  r <- cabal' "v2-vendor" []
  shouldExist (vendorDir </> "pkg-a-1.0.tar.gz")
  shouldNotExist (vendorDir </> "pkg-a-1.0.cabal")
  assertOutputContains "stanzas can be removed" r
  assertOutputContains "pkg-a-1.0 (git " r

  -- Drop the stanza and the upstream repository: the vendored source
  -- distribution is all that is left.
  writeSourceFile "cabal.vendored.project" $
    unlines
      [ "packages: dummy-app"
      , ""
      , "repository vendored"
      , "  url: file+noindex:vendor"
      , ""
      , "active-repositories: vendored"
      ]
  liftIO $ removeDirectoryRecursive (cwd </> "upstream")
  withProjectFile "cabal.vendored.project" $
    cabal "v2-build" ["--offline", "all"]
  where
    fileUri path = "file://" ++ root ++ map toPosixSeparator path
      where
        root = case buildOS of
          Windows -> "/"
          _ -> ""

    toPosixSeparator c
      | c == Windows.pathSeparator = Posix.pathSeparator
      | otherwise = c
