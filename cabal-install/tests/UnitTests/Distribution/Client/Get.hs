{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UnitTests.Distribution.Client.Get (tests) where

import Distribution.Client.Get

import Distribution.Client.Types.SourceRepo (SourceRepositoryPackage (..))
import Distribution.Types.PackageId
import Distribution.Types.PackageName
import Distribution.Types.SourceRepo (KnownRepoType (..), RepoKind (..), RepoType (..), SourceRepo (..), emptySourceRepo)
import Distribution.Verbosity as Verbosity
import Distribution.Version

import Control.Exception
import Control.Monad
import Data.Typeable
import System.Directory
import System.Exit
import System.FilePath
import System.IO.Error

import Test.Tasty
import Test.Tasty.HUnit
import UnitTests.Options (RunNetworkTests (..))
import Distribution.Utils.TempTestDir (withTestDir)

tests :: [TestTree]
tests =
  [ testGroup
      "forkPackages"
      [ testCase "no repos" testNoRepos
      , testCase "no repos of requested kind" testNoReposOfKind
      , testCase "no repo type specified" testNoRepoType
      , testCase "unsupported repo type" testUnsupportedRepoType
      , testCase "no repo location specified" testNoRepoLocation
      , testCase "correct repo kind selection" testSelectRepoKind
      , testCase "repo destination exists" testRepoDestinationExists
      , testCase "git fetch failure" testGitFetchFailed
      ]
  , askOption $ \(RunNetworkTests doRunNetTests) ->
      testGroup "forkPackages, network tests" $
        includeTestsIf doRunNetTests $
          [ testCase "git clone" testNetworkGitClone
          ]
  ]
  where
    includeTestsIf True xs = xs
    includeTestsIf False _ = []

verbosity :: Verbosity
verbosity = Verbosity.silent -- for debugging try verbose

pkgidfoo :: PackageId
pkgidfoo = PackageIdentifier (mkPackageName "foo") (mkVersion [1, 0])

-- ------------------------------------------------------------

-- * Unit tests

-- ------------------------------------------------------------

testNoRepos :: Assertion
testNoRepos = do
  e <-
    assertException $
      clonePackagesFromSourceRepo verbosity "." Nothing pkgrepos
  e @?= ClonePackageNoSourceRepos pkgidfoo
  where
    pkgrepos = [(pkgidfoo, [])]

testNoReposOfKind :: Assertion
testNoReposOfKind = do
  e <-
    assertException $
      clonePackagesFromSourceRepo verbosity "." repokind pkgrepos
  e @?= ClonePackageNoSourceReposOfKind pkgidfoo repokind
  where
    pkgrepos = [(pkgidfoo, [repo])]
    repo = emptySourceRepo RepoHead
    repokind = Just RepoThis

testNoRepoType :: Assertion
testNoRepoType = do
  e <-
    assertException $
      clonePackagesFromSourceRepo verbosity "." Nothing pkgrepos
  e @?= ClonePackageNoRepoType pkgidfoo repo
  where
    pkgrepos = [(pkgidfoo, [repo])]
    repo = emptySourceRepo RepoHead

testUnsupportedRepoType :: Assertion
testUnsupportedRepoType = do
  e <-
    assertException $
      clonePackagesFromSourceRepo verbosity "." Nothing pkgrepos
  e @?= ClonePackageUnsupportedRepoType pkgidfoo repo' repotype
  where
    pkgrepos = [(pkgidfoo, [repo])]
    repo =
      (emptySourceRepo RepoHead)
        { repoType = Just repotype
        , repoLocation = Just "loc"
        }
    repo' =
      SourceRepositoryPackage
        { srpType = repotype
        , srpLocation = "loc"
        , srpTag = Nothing
        , srpBranch = Nothing
        , srpSubdir = Proxy
        , srpCommand = []
        }
    repotype = OtherRepoType "baz"

testNoRepoLocation :: Assertion
testNoRepoLocation = do
  e <-
    assertException $
      clonePackagesFromSourceRepo verbosity "." Nothing pkgrepos
  e @?= ClonePackageNoRepoLocation pkgidfoo repo
  where
    pkgrepos = [(pkgidfoo, [repo])]
    repo =
      (emptySourceRepo RepoHead)
        { repoType = Just repotype
        }
    repotype = KnownRepoType Darcs

testSelectRepoKind :: Assertion
testSelectRepoKind =
  sequence_
    [ do
      e <- test requestedRepoType pkgrepos
      e @?= ClonePackageNoRepoType pkgidfoo expectedRepo

      e' <- test requestedRepoType (reverse pkgrepos)
      e' @?= ClonePackageNoRepoType pkgidfoo expectedRepo
    | let test rt rs =
            assertException $
              clonePackagesFromSourceRepo verbosity "." rt rs
    , (requestedRepoType, expectedRepo) <- cases
    ]
  where
    pkgrepos = [(pkgidfoo, [repo1, repo2, repo3])]
    repo1 = emptySourceRepo RepoThis
    repo2 = emptySourceRepo RepoHead
    repo3 = emptySourceRepo (RepoKindUnknown "bar")
    cases =
      [ (Nothing, repo1)
      , (Just RepoThis, repo1)
      , (Just RepoHead, repo2)
      , (Just (RepoKindUnknown "bar"), repo3)
      ]

testRepoDestinationExists :: Assertion
testRepoDestinationExists =
  withTestDir verbosity "repos" $ \tmpdir -> do
    let pkgdir = tmpdir </> "foo"
    createDirectory pkgdir
    e1 <-
      assertException $
        clonePackagesFromSourceRepo verbosity tmpdir Nothing pkgrepos
    e1 @?= ClonePackageDestinationExists pkgidfoo pkgdir True {- isdir -}
    removeDirectory pkgdir

    writeFile pkgdir ""
    e2 <-
      assertException $
        clonePackagesFromSourceRepo verbosity tmpdir Nothing pkgrepos
    e2 @?= ClonePackageDestinationExists pkgidfoo pkgdir False {- isfile -}
  where
    pkgrepos = [(pkgidfoo, [repo])]
    repo =
      (emptySourceRepo RepoHead)
        { repoType = Just (KnownRepoType Darcs)
        , repoLocation = Just ""
        }

testGitFetchFailed :: Assertion
testGitFetchFailed =
  withTestDir verbosity "repos" $ \tmpdir -> do
    let srcdir = tmpdir </> "src"
        repo =
          (emptySourceRepo RepoHead)
            { repoType = Just (KnownRepoType Git)
            , repoLocation = Just srcdir
            }
        repo' =
          SourceRepositoryPackage
            { srpType = KnownRepoType Git
            , srpLocation = srcdir
            , srpTag = Nothing
            , srpBranch = Nothing
            , srpSubdir = Proxy
            , srpCommand = []
            }
        pkgrepos = [(pkgidfoo, [repo])]
    e1 <-
      assertException $
        clonePackagesFromSourceRepo verbosity tmpdir Nothing pkgrepos
    e1 @?= ClonePackageFailedWithExitCode pkgidfoo repo' "git" (ExitFailure 128)

testNetworkGitClone :: Assertion
testNetworkGitClone =
  withTestDir verbosity "repos" $ \tmpdir -> do
    let repo1 =
          (emptySourceRepo RepoHead)
            { repoType = Just (KnownRepoType Git)
            , repoLocation = Just "https://github.com/haskell/zlib.git"
            }
    clonePackagesFromSourceRepo
      verbosity
      tmpdir
      Nothing
      [(mkpkgid "zlib1", [repo1])]
    assertFileContains (tmpdir </> "zlib1/zlib.cabal") ["name:", "zlib"]

    let repo2 =
          (emptySourceRepo RepoHead)
            { repoType = Just (KnownRepoType Git)
            , repoLocation = Just (tmpdir </> "zlib1")
            }
    clonePackagesFromSourceRepo
      verbosity
      tmpdir
      Nothing
      [(mkpkgid "zlib2", [repo2])]
    assertFileContains (tmpdir </> "zlib2/zlib.cabal") ["name:", "zlib"]

    let repo3 =
          (emptySourceRepo RepoHead)
            { repoType = Just (KnownRepoType Git)
            , repoLocation = Just (tmpdir </> "zlib1")
            , repoTag = Just "0.5.0.0"
            }
    clonePackagesFromSourceRepo
      verbosity
      tmpdir
      Nothing
      [(mkpkgid "zlib3", [repo3])]
    assertFileContains (tmpdir </> "zlib3/zlib.cabal") ["version:", "0.5.0.0"]
  where
    mkpkgid nm = PackageIdentifier (mkPackageName nm) (mkVersion [])

-- ------------------------------------------------------------

-- * HUnit utils

-- ------------------------------------------------------------

assertException :: forall e a. (Exception e, HasCallStack) => IO a -> IO e
assertException action = do
  r <- try action
  case r of
    Left e -> return e
    Right _ ->
      assertFailure $
        "expected exception of type "
          ++ show (typeOf (undefined :: e))

-- | Expect that one line in a file matches exactly the given words (i.e. at
-- least insensitive to whitespace)
assertFileContains :: HasCallStack => FilePath -> [String] -> Assertion
assertFileContains file expected = do
  c <-
    readFile file `catch` \e ->
      if isDoesNotExistError e
        then assertFailure $ "expected a file to exist: " ++ file
        else throwIO e
  unless (expected `elem` map words (lines c)) $
    assertFailure $
      "expected the file "
        ++ file
        ++ " to contain "
        ++ show (take 100 expected)
