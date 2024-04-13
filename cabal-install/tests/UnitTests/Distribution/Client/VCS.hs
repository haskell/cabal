{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module UnitTests.Distribution.Client.VCS (tests) where

import Distribution.Client.Compat.Prelude
import Distribution.Client.RebuildMonad
  ( execRebuild
  )
import Distribution.Client.Types.SourceRepo (SourceRepoProxy, SourceRepositoryPackage (..))
import Distribution.Client.VCS
import Distribution.Simple.Program
import Distribution.System (OS (Windows), buildOS)
import Distribution.Verbosity as Verbosity
import Test.Utils.TempTestDir (removeDirectoryRecursiveHack, withTestDir)

import Data.List (mapAccumL)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Tuple

import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Monad.State (StateT, execStateT, liftIO)
import qualified Control.Monad.State as State

import System.Directory
import System.FilePath
import System.IO
import System.Random

import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.QuickCheck
import UnitTests.Distribution.Client.ArbitraryInstances

-- | These tests take the following approach: we generate a pure representation
-- of a repository plus a corresponding real repository, and then run various
-- test operations and compare the actual working state with the expected
-- working state.
--
-- The first test simply checks that the test infrastructure works. It
-- constructs a repository on disk and then checks out every tag or commit
-- and checks that the working state is the same as the pure representation.
--
-- The second test works in a similar way but tests 'syncSourceRepos'. It
-- uses an arbitrary source repo and a set of (initially empty) destination
-- directories. It picks a number of tags or commits from the source repo and
-- synchronises the destination directories to those target states, and then
-- checks that the working state is as expected (given the pure representation).
tests :: MTimeChange -> [TestTree]
tests mtimeChange =
  map
    (localOption $ QuickCheckTests 10)
    [ ignoreInWindows "See issue #8048 and #9519" $
        testGroup
          "git"
          [ testProperty "check VCS test framework" prop_framework_git
          , testProperty "cloneSourceRepo" prop_cloneRepo_git
          , testProperty "syncSourceRepos" prop_syncRepos_git
          ]
    , --
      ignoreTestBecause "for the moment they're not yet working" $
        testGroup
          "darcs"
          [ testProperty "check VCS test framework" $ prop_framework_darcs mtimeChange
          , testProperty "cloneSourceRepo" $ prop_cloneRepo_darcs mtimeChange
          , testProperty "syncSourceRepos" $ prop_syncRepos_darcs mtimeChange
          ]
    , ignoreTestBecause "for the moment they're not yet working" $
        testGroup
          "pijul"
          [ testProperty "check VCS test framework" prop_framework_pijul
          , testProperty "cloneSourceRepo" prop_cloneRepo_pijul
          , testProperty "syncSourceRepos" prop_syncRepos_pijul
          ]
    , ignoreTestBecause "for the moment they're not yet working" $
        testGroup
          "mercurial"
          [ testProperty "check VCS test framework" prop_framework_hg
          , testProperty "cloneSourceRepo" prop_cloneRepo_hg
          , testProperty "syncSourceRepos" prop_syncRepos_hg
          ]
    ]
  where
    ignoreInWindows msg = case buildOS of
      Windows -> ignoreTestBecause msg
      _ -> id

prop_framework_git :: BranchingRepoRecipe 'SubmodulesSupported -> Property
prop_framework_git =
  ioProperty
    . prop_framework vcsGit vcsTestDriverGit
    . WithBranchingSupport

prop_framework_darcs :: MTimeChange -> NonBranchingRepoRecipe 'SubmodulesNotSupported -> Property
prop_framework_darcs mtimeChange =
  ioProperty
    . prop_framework vcsDarcs (vcsTestDriverDarcs mtimeChange)
    . WithoutBranchingSupport

prop_framework_pijul :: BranchingRepoRecipe 'SubmodulesNotSupported -> Property
prop_framework_pijul =
  ioProperty
    . prop_framework vcsPijul vcsTestDriverPijul
    . WithBranchingSupport

prop_framework_hg :: BranchingRepoRecipe 'SubmodulesNotSupported -> Property
prop_framework_hg =
  ioProperty
    . prop_framework vcsHg vcsTestDriverHg
    . WithBranchingSupport

prop_cloneRepo_git :: BranchingRepoRecipe 'SubmodulesSupported -> Property
prop_cloneRepo_git =
  ioProperty
    . prop_cloneRepo vcsGit vcsTestDriverGit
    . WithBranchingSupport

prop_cloneRepo_darcs
  :: MTimeChange
  -> NonBranchingRepoRecipe 'SubmodulesNotSupported
  -> Property
prop_cloneRepo_darcs mtimeChange =
  ioProperty
    . prop_cloneRepo vcsDarcs (vcsTestDriverDarcs mtimeChange)
    . WithoutBranchingSupport

prop_cloneRepo_pijul :: BranchingRepoRecipe 'SubmodulesNotSupported -> Property
prop_cloneRepo_pijul =
  ioProperty
    . prop_cloneRepo vcsPijul vcsTestDriverPijul
    . WithBranchingSupport

prop_cloneRepo_hg :: BranchingRepoRecipe 'SubmodulesNotSupported -> Property
prop_cloneRepo_hg =
  ioProperty
    . prop_cloneRepo vcsHg vcsTestDriverHg
    . WithBranchingSupport

prop_syncRepos_git
  :: RepoDirSet
  -> SyncTargetIterations
  -> PrngSeed
  -> BranchingRepoRecipe 'SubmodulesSupported
  -> Property
prop_syncRepos_git destRepoDirs syncTargetSetIterations seed =
  ioProperty
    . prop_syncRepos
      vcsGit
      vcsTestDriverGit
      destRepoDirs
      syncTargetSetIterations
      seed
    . WithBranchingSupport

prop_syncRepos_darcs
  :: MTimeChange
  -> RepoDirSet
  -> SyncTargetIterations
  -> PrngSeed
  -> NonBranchingRepoRecipe 'SubmodulesNotSupported
  -> Property
prop_syncRepos_darcs mtimeChange destRepoDirs syncTargetSetIterations seed =
  ioProperty
    . prop_syncRepos
      vcsDarcs
      (vcsTestDriverDarcs mtimeChange)
      destRepoDirs
      syncTargetSetIterations
      seed
    . WithoutBranchingSupport

prop_syncRepos_pijul
  :: RepoDirSet
  -> SyncTargetIterations
  -> PrngSeed
  -> BranchingRepoRecipe 'SubmodulesNotSupported
  -> Property
prop_syncRepos_pijul destRepoDirs syncTargetSetIterations seed =
  ioProperty
    . prop_syncRepos
      vcsPijul
      vcsTestDriverPijul
      destRepoDirs
      syncTargetSetIterations
      seed
    . WithBranchingSupport

prop_syncRepos_hg
  :: RepoDirSet
  -> SyncTargetIterations
  -> PrngSeed
  -> BranchingRepoRecipe 'SubmodulesNotSupported
  -> Property
prop_syncRepos_hg destRepoDirs syncTargetSetIterations seed =
  ioProperty
    . prop_syncRepos
      vcsHg
      vcsTestDriverHg
      destRepoDirs
      syncTargetSetIterations
      seed
    . WithBranchingSupport

-- ------------------------------------------------------------

-- * General test setup

-- ------------------------------------------------------------

testSetup
  :: VCS Program
  -> ( Verbosity
       -> VCS ConfiguredProgram
       -> FilePath
       -> FilePath
       -> VCSTestDriver
     )
  -> RepoRecipe submodules
  -> (VCSTestDriver -> FilePath -> RepoState -> IO a)
  -> IO a
testSetup vcs mkVCSTestDriver repoRecipe theTest = do
  -- test setup
  vcs' <- configureVCS verbosity [] vcs
  withTestDir verbosity "vcstest" $ \tmpdir -> do
    let srcRepoPath = tmpdir </> "src"
        submodulesPath = tmpdir </> "submodules"
        vcsDriver = mkVCSTestDriver verbosity vcs' submodulesPath srcRepoPath
    repoState <- createRepo vcsDriver repoRecipe

    -- actual test
    result <- theTest vcsDriver tmpdir repoState

    return result
  where
    verbosity = silent

-- ------------------------------------------------------------

-- * Test 1: VCS infrastructure

-- ------------------------------------------------------------

-- | This test simply checks that the test infrastructure works. It constructs
-- a repository on disk and then checks out every tag or commit and checks that
-- the working state is the same as the pure representation.
prop_framework
  :: VCS Program
  -> ( Verbosity
       -> VCS ConfiguredProgram
       -> FilePath
       -> FilePath
       -> VCSTestDriver
     )
  -> RepoRecipe submodules
  -> IO ()
prop_framework vcs mkVCSTestDriver repoRecipe =
  testSetup vcs mkVCSTestDriver repoRecipe $ \vcsDriver tmpdir repoState ->
    mapM_ (checkAtTag vcsDriver tmpdir) (Map.toList (allTags repoState))
  where
    -- Check for any given tag/commit in the 'RepoState' that the working state
    -- matches the actual working state from the repository at that tag/commit.
    checkAtTag VCSTestDriver{..} tmpdir (tagname, expectedState) =
      case vcsCheckoutTag of
        -- We handle two cases: inplace checkouts for VCSs that support it
        -- (e.g. git) and separate dir otherwise (e.g. darcs)
        Left checkoutInplace -> do
          checkoutInplace tagname
          checkExpectedWorkingState vcsIgnoreFiles vcsRepoRoot expectedState
        Right checkoutCloneTo -> do
          checkoutCloneTo tagname destRepoPath
          checkExpectedWorkingState vcsIgnoreFiles destRepoPath expectedState
          removeDirectoryRecursiveHack silent destRepoPath
      where
        destRepoPath = tmpdir </> "dest"

-- ------------------------------------------------------------

-- * Test 2: 'cloneSourceRepo'

-- ------------------------------------------------------------

prop_cloneRepo
  :: VCS Program
  -> ( Verbosity
       -> VCS ConfiguredProgram
       -> FilePath
       -> FilePath
       -> VCSTestDriver
     )
  -> RepoRecipe submodules
  -> IO ()
prop_cloneRepo vcs mkVCSTestDriver repoRecipe =
  testSetup vcs mkVCSTestDriver repoRecipe $ \vcsDriver tmpdir repoState ->
    mapM_ (checkAtTag vcsDriver tmpdir) (Map.toList (allTags repoState))
  where
    checkAtTag VCSTestDriver{..} tmpdir (tagname, expectedState) = do
      cloneSourceRepo verbosity vcsVCS repo destRepoPath
      checkExpectedWorkingState vcsIgnoreFiles destRepoPath expectedState
      removeDirectoryRecursiveHack verbosity destRepoPath
      where
        destRepoPath = tmpdir </> "dest"
        repo =
          SourceRepositoryPackage
            { srpType = vcsRepoType vcsVCS
            , srpLocation = vcsRepoRoot
            , srpTag = Just tagname
            , srpBranch = Nothing
            , srpSubdir = []
            , srpCommand = []
            }
    verbosity = silent

-- ------------------------------------------------------------

-- * Test 3: 'syncSourceRepos'

-- ------------------------------------------------------------

newtype RepoDirSet = RepoDirSet Int deriving (Show)
newtype SyncTargetIterations = SyncTargetIterations Int deriving (Show)
newtype PrngSeed = PrngSeed Int deriving (Show)

prop_syncRepos
  :: VCS Program
  -> ( Verbosity
       -> VCS ConfiguredProgram
       -> FilePath
       -> FilePath
       -> VCSTestDriver
     )
  -> RepoDirSet
  -> SyncTargetIterations
  -> PrngSeed
  -> RepoRecipe submodules
  -> IO ()
prop_syncRepos
  vcs
  mkVCSTestDriver
  repoDirs
  syncTargetSetIterations
  seed
  repoRecipe =
    testSetup vcs mkVCSTestDriver repoRecipe $ \vcsDriver tmpdir repoState ->
      let srcRepoPath = vcsRepoRoot vcsDriver
          destRepoPaths = map (tmpdir </>) (getRepoDirs repoDirs)
       in checkSyncRepos
            verbosity
            vcsDriver
            repoState
            srcRepoPath
            destRepoPaths
            syncTargetSetIterations
            seed
    where
      verbosity = silent

      getRepoDirs :: RepoDirSet -> [FilePath]
      getRepoDirs (RepoDirSet n) =
        ["dest" ++ show i | i <- [1 .. n]]

-- | The purpose of this test is to check that irrespective of the local cached
-- repo dir we can sync it to an arbitrary target state. So we do that by
-- syncing each target dir to a sequence of target states without cleaning it
-- in between.
--
-- One slight complication is that 'syncSourceRepos' takes a whole list of
-- target dirs to sync in one go (to allow for sharing). So we must actually
-- generate and sync to a sequence of list of target repo states.
--
-- So, given a source repo dir, the corresponding 'RepoState' and a number of
-- target repo dirs, pick a sequence of (lists of) sync targets from the
-- 'RepoState' and synchronise the target dirs with those targets, checking for
-- each one that the actual working state matches the expected repo state.
checkSyncRepos
  :: Verbosity
  -> VCSTestDriver
  -> RepoState
  -> FilePath
  -> [FilePath]
  -> SyncTargetIterations
  -> PrngSeed
  -> IO ()
checkSyncRepos
  verbosity
  VCSTestDriver{vcsVCS = vcs, vcsIgnoreFiles}
  repoState
  srcRepoPath
  destRepoPath
  (SyncTargetIterations syncTargetSetIterations)
  (PrngSeed seed) =
    mapM_ checkSyncTargetSet syncTargetSets
    where
      checkSyncTargetSet :: [(SourceRepoProxy, FilePath, RepoWorkingState)] -> IO ()
      checkSyncTargetSet syncTargets = do
        _ <-
          execRebuild "root-unused" $
            syncSourceRepos
              verbosity
              vcs
              [ (repo, repoPath)
              | (repo, repoPath, _) <- syncTargets
              ]
        sequence_
          [ checkExpectedWorkingState vcsIgnoreFiles repoPath workingState
          | (_, repoPath, workingState) <- syncTargets
          ]

      syncTargetSets =
        take syncTargetSetIterations $
          pickSyncTargetSets
            (vcsRepoType vcs)
            repoState
            srcRepoPath
            destRepoPath
            (mkStdGen seed)

pickSyncTargetSets
  :: RepoType
  -> RepoState
  -> FilePath
  -> [FilePath]
  -> StdGen
  -> [[(SourceRepoProxy, FilePath, RepoWorkingState)]]
pickSyncTargetSets repoType repoState srcRepoPath dstReposPath =
  assert (Map.size (allTags repoState) > 0) $
    unfoldr (Just . swap . pickSyncTargetSet)
  where
    pickSyncTargetSet :: Rand [(SourceRepoProxy, FilePath, RepoWorkingState)]
    pickSyncTargetSet = flip (mapAccumL (flip pickSyncTarget)) dstReposPath

    pickSyncTarget :: FilePath -> Rand (SourceRepoProxy, FilePath, RepoWorkingState)
    pickSyncTarget destRepoPath prng =
      (prng', (repo, destRepoPath, workingState))
      where
        repo =
          SourceRepositoryPackage
            { srpType = repoType
            , srpLocation = srcRepoPath
            , srpTag = Just tag
            , srpBranch = Nothing
            , srpSubdir = Proxy
            , srpCommand = []
            }
        (tag, workingState) = Map.elemAt tagIdx (allTags repoState)
        (tagIdx, prng') = randomR (0, Map.size (allTags repoState) - 1) prng

type Rand a = StdGen -> (StdGen, a)

instance Arbitrary RepoDirSet where
  arbitrary =
    sized $ \n ->
      oneof $
        [RepoDirSet <$> pure 1]
          ++ [RepoDirSet <$> choose (2, 5) | n >= 3]
  shrink (RepoDirSet n) =
    [RepoDirSet i | i <- shrink n, i > 0]

instance Arbitrary SyncTargetIterations where
  arbitrary =
    sized $ \n -> SyncTargetIterations <$> elements [1 .. min 20 (n + 1)]
  shrink (SyncTargetIterations n) =
    [SyncTargetIterations i | i <- shrink n, i > 0]

instance Arbitrary PrngSeed where
  arbitrary = PrngSeed <$> arbitraryBoundedRandom

-- ------------------------------------------------------------

-- * Instructions for constructing repositories

-- ------------------------------------------------------------

-- These instructions for constructing a repository can be interpreted in two
-- ways: to make a pure representation of repository state, and to execute
-- VCS commands to make a repository on-disk.

data SubmodulesSupport = SubmodulesSupported | SubmodulesNotSupported

class KnownSubmodulesSupport (a :: SubmodulesSupport) where
  submoduleSupport :: SubmodulesSupport

instance KnownSubmodulesSupport 'SubmodulesSupported where
  submoduleSupport = SubmodulesSupported

instance KnownSubmodulesSupport 'SubmodulesNotSupported where
  submoduleSupport = SubmodulesNotSupported

data FileUpdate = FileUpdate FilePath String
  deriving (Show)
data SubmoduleAdd = SubmoduleAdd FilePath FilePath (Commit 'SubmodulesSupported)
  deriving (Show)

newtype Commit (submodules :: SubmodulesSupport)
  = Commit [Either FileUpdate SubmoduleAdd]
  deriving (Show)

data TaggedCommits (submodules :: SubmodulesSupport)
  = TaggedCommits TagName [Commit submodules]
  deriving (Show)

data BranchCommits (submodules :: SubmodulesSupport)
  = BranchCommits BranchName [Commit submodules]
  deriving (Show)

type BranchName = String
type TagName = String

-- | Instructions to make a repository without branches, for VCSs that do not
-- support branches (e.g. darcs).
newtype NonBranchingRepoRecipe submodules
  = NonBranchingRepoRecipe [TaggedCommits submodules]
  deriving (Show)

-- | Instructions to make a repository with branches, for VCSs that do
-- support branches (e.g. git).
newtype BranchingRepoRecipe submodules
  = BranchingRepoRecipe [Either (TaggedCommits submodules) (BranchCommits submodules)]
  deriving (Show)

data RepoRecipe submodules
  = WithBranchingSupport (BranchingRepoRecipe submodules)
  | WithoutBranchingSupport (NonBranchingRepoRecipe submodules)
  deriving (Show)

-- ---------------------------------------------------------------------------
-- Arbitrary instances for them

genFileName :: Gen FilePath
genFileName = (\c -> "file" </> [c]) <$> choose ('A', 'E')

instance Arbitrary FileUpdate where
  arbitrary = genOnlyFileUpdate
    where
      genOnlyFileUpdate = FileUpdate <$> genFileName <*> genFileContent
      genFileContent = vectorOf 10 (choose ('#', '~'))

instance Arbitrary SubmoduleAdd where
  arbitrary = genOnlySubmoduleAdd
    where
      genOnlySubmoduleAdd = SubmoduleAdd <$> genFileName <*> genSubmoduleSrc <*> arbitrary
      genSubmoduleSrc = vectorOf 20 (choose ('a', 'z'))

instance forall submodules. KnownSubmodulesSupport submodules => Arbitrary (Commit submodules) where
  arbitrary = Commit <$> shortListOf1 5 fileUpdateOrSubmoduleAdd
    where
      fileUpdateOrSubmoduleAdd =
        case submoduleSupport @submodules of
          SubmodulesSupported ->
            frequency
              [ (10, Left <$> arbitrary)
              , (1, Right <$> arbitrary)
              ]
          SubmodulesNotSupported -> Left <$> arbitrary
  shrink (Commit writes) = Commit <$> filter (not . null) (shrink writes)

instance KnownSubmodulesSupport submodules => Arbitrary (TaggedCommits submodules) where
  arbitrary = TaggedCommits <$> genTagName <*> shortListOf1 5 arbitrary
    where
      genTagName = ("tag_" ++) <$> shortListOf1 5 (choose ('A', 'Z'))
  shrink (TaggedCommits tag commits) =
    TaggedCommits tag <$> filter (not . null) (shrink commits)

instance KnownSubmodulesSupport submodules => Arbitrary (BranchCommits submodules) where
  arbitrary = BranchCommits <$> genBranchName <*> shortListOf1 5 arbitrary
    where
      genBranchName =
        sized $ \n ->
          (\c -> "branch_" ++ [c]) <$> elements (take (max 1 n) ['A' .. 'E'])

  shrink (BranchCommits branch commits) =
    BranchCommits branch <$> filter (not . null) (shrink commits)

instance KnownSubmodulesSupport submodules => Arbitrary (NonBranchingRepoRecipe submodules) where
  arbitrary = NonBranchingRepoRecipe <$> shortListOf1 15 arbitrary
  shrink (NonBranchingRepoRecipe xs) =
    NonBranchingRepoRecipe <$> filter (not . null) (shrink xs)

instance KnownSubmodulesSupport submodules => Arbitrary (BranchingRepoRecipe submodules) where
  arbitrary = BranchingRepoRecipe <$> shortListOf1 15 taggedOrBranch
    where
      taggedOrBranch =
        frequency
          [ (3, Left <$> arbitrary)
          , (1, Right <$> arbitrary)
          ]
  shrink (BranchingRepoRecipe xs) =
    BranchingRepoRecipe <$> filter (not . null) (shrink xs)

-- ------------------------------------------------------------

-- * A pure model of repository state

-- ------------------------------------------------------------

-- | The full state of a repository. In particular it records the full working
-- state for every tag.
--
-- This is also the interpreter state for executing a 'RepoRecipe'.
--
-- This allows us to compare expected working states with the actual files in
-- the working directory of a repository. See 'checkExpectedWorkingState'.
data RepoState = RepoState
  { currentBranch :: BranchName
  , currentWorking :: RepoWorkingState
  , allTags :: Map TagOrCommitId RepoWorkingState
  , allBranches :: Map BranchName RepoWorkingState
  }
  deriving (Show)

type RepoWorkingState = Map FilePath String
type CommitId = String
type TagOrCommitId = String

------------------------------------------------------------------------------
-- Functions used to interpret instructions for constructing repositories

initialRepoState :: RepoState
initialRepoState =
  RepoState
    { currentBranch = "branch_master"
    , currentWorking = Map.empty
    , allTags = Map.empty
    , allBranches = Map.empty
    }

updateFile :: FilePath -> String -> RepoState -> RepoState
updateFile filename content state@RepoState{currentWorking} =
  let removeSubmodule = Map.filterWithKey (\path _ -> not $ filename `isPrefixOf` path) currentWorking
   in state{currentWorking = Map.insert filename content removeSubmodule}

addSubmodule :: FilePath -> RepoState -> RepoState -> RepoState
addSubmodule submodulePath submoduleState mainState =
  let newFiles = Map.mapKeys (submodulePath </>) (currentWorking submoduleState)
      removeSubmodule = Map.filterWithKey (\path _ -> not $ submodulePath `isPrefixOf` path) (currentWorking mainState)
      newWorking = Map.union removeSubmodule newFiles
   in mainState{currentWorking = newWorking}

addTagOrCommit :: TagOrCommitId -> RepoState -> RepoState
addTagOrCommit commit state@RepoState{currentWorking, allTags} =
  state{allTags = Map.insert commit currentWorking allTags}

switchBranch :: BranchName -> RepoState -> RepoState
switchBranch branch state@RepoState{currentWorking, currentBranch, allBranches} =
  -- Use updated allBranches to cover case of switching to the same branch
  let allBranches' = Map.insert currentBranch currentWorking allBranches
   in state
        { currentBranch = branch
        , currentWorking = case Map.lookup branch allBranches' of
            Just working -> working
            -- otherwise we're creating a new branch, which starts
            -- from our current branch state
            Nothing -> currentWorking
        , allBranches = allBranches'
        }

-- ------------------------------------------------------------

-- * Comparing on-disk with expected 'RepoWorkingState'

-- ------------------------------------------------------------

-- | Compare expected working states with the actual files in
-- the working directory of a repository.
checkExpectedWorkingState
  :: Set FilePath
  -> FilePath
  -> RepoWorkingState
  -> IO ()
checkExpectedWorkingState ignore repoPath expectedState = do
  currentState <- getCurrentWorkingState ignore repoPath
  unless (currentState == expectedState) $
    throwIO (WorkingStateMismatch expectedState currentState)

data WorkingStateMismatch
  = WorkingStateMismatch
      RepoWorkingState -- expected
      RepoWorkingState -- actual
  deriving (Show)

instance Exception WorkingStateMismatch

getCurrentWorkingState :: Set FilePath -> FilePath -> IO RepoWorkingState
getCurrentWorkingState ignore repoRoot = do
  entries <- getDirectoryContentsRecursive ignore repoRoot ""
  Map.fromList
    <$> mapM
      getFileEntry
      [file | (file, isDir) <- entries, not isDir]
  where
    getFileEntry name =
      withBinaryFile (repoRoot </> name) ReadMode $ \h -> do
        str <- hGetContents h
        _ <- evaluate (length str)
        return (name, str)

getDirectoryContentsRecursive
  :: Set FilePath
  -> FilePath
  -> FilePath
  -> IO [(FilePath, Bool)]
getDirectoryContentsRecursive ignore dir0 dir = do
  entries <- getDirectoryContents (dir0 </> dir)
  entries' <-
    sequence
      [ do
        isdir <- doesDirectoryExist (dir0 </> dir </> entry)
        return (dir </> entry, isdir)
      | entry <- entries
      , not (isPrefixOf "." entry)
      , (dir </> entry) `Set.notMember` ignore
      ]
  let subdirs = [d | (d, True) <- entries']
  subdirEntries <- mapM (getDirectoryContentsRecursive ignore dir0) subdirs
  return (concat (entries' : subdirEntries))

-- ------------------------------------------------------------

-- * Executing instructions to make on-disk VCS repos

-- ------------------------------------------------------------

-- | Execute the instructions in a 'RepoRecipe' using the given 'VCSTestDriver'
-- to make an on-disk repository.
--
-- This also returns a 'RepoState'. This is done as part of construction to
-- support VCSs like git that have commit ids, so that those commit ids can be
-- included in the 'RepoState's 'allTags' set.
createRepo :: VCSTestDriver -> RepoRecipe submodules -> IO RepoState
createRepo vcsDriver@VCSTestDriver{vcsRepoRoot, vcsInit} recipe = do
  createDirectoryIfMissing True vcsRepoRoot
  createDirectoryIfMissing True (vcsRepoRoot </> "file")
  vcsInit
  execStateT createRepoAction initialRepoState
  where
    createRepoAction :: StateT RepoState IO ()
    createRepoAction = case recipe of
      WithoutBranchingSupport r -> execNonBranchingRepoRecipe vcsDriver r
      WithBranchingSupport r -> execBranchingRepoRecipe vcsDriver r

type CreateRepoAction a = VCSTestDriver -> a -> StateT RepoState IO ()

execNonBranchingRepoRecipe :: CreateRepoAction (NonBranchingRepoRecipe submodules)
execNonBranchingRepoRecipe vcsDriver (NonBranchingRepoRecipe taggedCommits) =
  mapM_ (execTaggdCommits vcsDriver) taggedCommits

execBranchingRepoRecipe :: CreateRepoAction (BranchingRepoRecipe submodules)
execBranchingRepoRecipe vcsDriver (BranchingRepoRecipe taggedCommits) =
  mapM_
    ( either
        (execTaggdCommits vcsDriver)
        (execBranchCommits vcsDriver)
    )
    taggedCommits

execBranchCommits :: CreateRepoAction (BranchCommits submodules)
execBranchCommits
  vcsDriver@VCSTestDriver{vcsSwitchBranch}
  (BranchCommits branch commits) = do
    mapM_ (execCommit vcsDriver) commits
    -- add commits and then switch branch
    State.modify (switchBranch branch)
    state <- State.get -- repo state after the commits and branch switch
    liftIO $ vcsSwitchBranch state branch

-- It may seem odd that we add commits on the existing branch and then
-- switch branch. In part this is because git cannot branch from an empty
-- repo state, it complains that the master branch doesn't exist yet.

execTaggdCommits :: CreateRepoAction (TaggedCommits submodules)
execTaggdCommits
  vcsDriver@VCSTestDriver{vcsTagState}
  (TaggedCommits tagname commits) = do
    mapM_ (execCommit vcsDriver) commits
    -- add commits then tag
    state <- State.get -- repo state after the commits
    liftIO $ vcsTagState state tagname
    State.modify (addTagOrCommit tagname)

execCommit :: CreateRepoAction (Commit submodules)
execCommit vcsDriver@VCSTestDriver{..} (Commit fileUpdates) = do
  mapM_ (either (execFileUpdate vcsDriver) (execSubmoduleAdd vcsDriver)) fileUpdates
  state <- State.get -- existing state, not updated
  mcommit <- liftIO $ vcsCommitChanges state
  State.modify (maybe id addTagOrCommit mcommit)

execFileUpdate :: CreateRepoAction FileUpdate
execFileUpdate VCSTestDriver{..} (FileUpdate filename content) = do
  isDir <- liftIO $ doesDirectoryExist (vcsRepoRoot </> filename)
  liftIO . when isDir $ removeDirectoryRecursive (vcsRepoRoot </> filename)
  liftIO $ writeFile (vcsRepoRoot </> filename) content
  state <- State.get -- existing state, not updated
  liftIO $ vcsAddFile state filename
  State.modify (updateFile filename content)

execSubmoduleAdd :: CreateRepoAction SubmoduleAdd
execSubmoduleAdd vcsDriver (SubmoduleAdd submodulePath source submoduleCommit) = do
  submoduleVcsDriver <- liftIO $ vcsSubmoduleDriver vcsDriver source
  let submoduleRecipe = WithoutBranchingSupport $ NonBranchingRepoRecipe [TaggedCommits "submodule-tag" [submoduleCommit]]
  submoduleState <- liftIO $ createRepo submoduleVcsDriver submoduleRecipe
  mainState <- State.get -- existing state, not updated
  liftIO $ vcsAddSubmodule vcsDriver mainState (vcsRepoRoot submoduleVcsDriver) submodulePath
  State.modify $ addSubmodule submodulePath submoduleState

-- ------------------------------------------------------------

-- * VCSTestDriver for various VCSs

-- ------------------------------------------------------------

-- | Extends 'VCS' with extra methods to construct a repository. Used by
-- 'createRepo'.
--
-- Several of the methods are allowed to rely on the current 'RepoState'
-- because some VCSs need different commands for initial vs later actions
-- (like adding a file to the tracked set, or creating a new branch).
--
-- The driver instance knows the particular repo directory.
data VCSTestDriver = VCSTestDriver
  { vcsVCS :: VCS ConfiguredProgram
  , vcsRepoRoot :: FilePath
  , vcsIgnoreFiles :: Set FilePath
  , vcsInit :: IO ()
  , vcsAddFile :: RepoState -> FilePath -> IO ()
  , vcsSubmoduleDriver :: FilePath -> IO VCSTestDriver
  , vcsAddSubmodule :: RepoState -> FilePath -> FilePath -> IO ()
  , vcsCommitChanges :: RepoState -> IO (Maybe CommitId)
  , vcsTagState :: RepoState -> TagName -> IO ()
  , vcsSwitchBranch :: RepoState -> BranchName -> IO ()
  , vcsCheckoutTag
      :: Either
          (TagName -> IO ())
          (TagName -> FilePath -> IO ())
  }

vcsTestDriverGit
  :: Verbosity
  -> VCS ConfiguredProgram
  -> FilePath
  -> FilePath
  -> VCSTestDriver
vcsTestDriverGit verbosity vcs submoduleDir repoRoot =
  VCSTestDriver
    { vcsVCS = vcs
    , vcsRepoRoot = repoRoot
    , vcsIgnoreFiles = Set.empty
    , vcsInit =
        git $ ["init"] ++ verboseArg
    , vcsAddFile = \_ filename ->
        git ["add", filename]
    , vcsCommitChanges = \_state -> do
        git $
          [ "-c"
          , "user.name=A"
          , "-c"
          , "user.email=a@example.com"
          , "commit"
          , "--all"
          , "--message=a patch"
          , "--author=A <a@example.com>"
          ]
            ++ verboseArg
        commit <- git' ["log", "--format=%H", "-1"]
        let commit' = takeWhile (not . isSpace) commit
        return (Just commit')
    , vcsTagState = \_ tagname ->
        git ["tag", "--force", "--no-sign", tagname]
    , vcsSubmoduleDriver =
        pure . vcsTestDriverGit verbosity vcs submoduleDir . (submoduleDir </>)
    , vcsAddSubmodule = \_ source dest -> do
        destExists <-
          (||)
            <$> doesFileExist (repoRoot </> dest)
            <*> doesDirectoryExist (repoRoot </> dest)
        when destExists $ git ["rm", "-f", dest]
        -- If there is an old submodule git dir with the same name, remove it.
        -- It most likely has a different URL and `git submodule add` will fai.
        submoduleGitDirExists <- doesDirectoryExist $ submoduleGitDir dest
        when submoduleGitDirExists $ removeDirectoryRecursive (submoduleGitDir dest)
        git ["submodule", "add", source, dest]
        git ["submodule", "update", "--init", "--recursive", "--force"]
    , vcsSwitchBranch = \RepoState{allBranches} branchname -> do
        deinitAndRemoveCachedSubmodules
        unless (branchname `Map.member` allBranches) $
          git ["branch", branchname]
        git $ ["checkout", branchname] ++ verboseArg
        updateSubmodulesAndCleanup
    , vcsCheckoutTag = Left $ \tagname -> do
        deinitAndRemoveCachedSubmodules
        git $ ["checkout", "--detach", "--force", tagname] ++ verboseArg
        updateSubmodulesAndCleanup
    }
  where
    gitInvocation args =
      (programInvocation (vcsProgram vcs) args)
        { progInvokeCwd = Just repoRoot
        }
    git = runProgramInvocation verbosity . gitInvocation
    git' = getProgramInvocationOutput verbosity . gitInvocation
    verboseArg = ["--quiet" | verbosity < Verbosity.normal]
    submoduleGitDir path = repoRoot </> ".git" </> "modules" </> path
    deinitAndRemoveCachedSubmodules = do
      git $ ["submodule", "deinit", "--force", "--all"] ++ verboseArg
      let gitModulesDir = repoRoot </> ".git" </> "modules"
      gitModulesExists <- doesDirectoryExist gitModulesDir
      when gitModulesExists $ removeDirectoryRecursive gitModulesDir
    updateSubmodulesAndCleanup = do
      git $ ["submodule", "sync", "--recursive"] ++ verboseArg
      git $ ["submodule", "update", "--init", "--recursive", "--force"] ++ verboseArg
      git $ ["submodule", "foreach", "--recursive"] ++ verboseArg ++ ["git clean -ffxdq"]
      git $ ["clean", "-ffxdq"] ++ verboseArg

type MTimeChange = Int

vcsTestDriverDarcs
  :: MTimeChange
  -> Verbosity
  -> VCS ConfiguredProgram
  -> FilePath
  -> FilePath
  -> VCSTestDriver
vcsTestDriverDarcs mtimeChange verbosity vcs _ repoRoot =
  VCSTestDriver
    { vcsVCS = vcs
    , vcsRepoRoot = repoRoot
    , vcsIgnoreFiles = Set.singleton "_darcs"
    , vcsInit =
        darcs ["initialize"]
    , vcsAddFile = \state filename -> do
        threadDelay mtimeChange
        unless (filename `Map.member` currentWorking state) $
          darcs ["add", filename]
    , -- Darcs's file change tracking relies on mtime changes,
      -- so we have to be careful with doing stuff too quickly:

      vcsSubmoduleDriver = \_ ->
        fail "vcsSubmoduleDriver: darcs does not support submodules"
    , vcsAddSubmodule = \_ _ _ ->
        fail "vcsAddSubmodule: darcs does not support submodules"
    , vcsCommitChanges = \_state -> do
        threadDelay mtimeChange
        darcs ["record", "--all", "--author=author", "--name=a patch"]
        return Nothing
    , vcsTagState = \_ tagname ->
        darcs ["tag", "--author=author", tagname]
    , vcsSwitchBranch = \_ _ ->
        fail "vcsSwitchBranch: darcs does not support branches within a repo"
    , vcsCheckoutTag = Right $ \tagname dest ->
        darcs ["clone", "--lazy", "--tag=^" ++ tagname ++ "$", ".", dest]
    }
  where
    darcsInvocation args =
      (programInvocation (vcsProgram vcs) args)
        { progInvokeCwd = Just repoRoot
        }
    darcs = runProgramInvocation verbosity . darcsInvocation

vcsTestDriverPijul
  :: Verbosity
  -> VCS ConfiguredProgram
  -> FilePath
  -> FilePath
  -> VCSTestDriver
vcsTestDriverPijul verbosity vcs _ repoRoot =
  VCSTestDriver
    { vcsVCS = vcs
    , vcsRepoRoot = repoRoot
    , vcsIgnoreFiles = Set.empty
    , vcsInit =
        pijul $ ["init"]
    , vcsAddFile = \_ filename ->
        pijul ["add", filename]
    , vcsSubmoduleDriver = \_ ->
        fail "vcsSubmoduleDriver: pijul does not support submodules"
    , vcsAddSubmodule = \_ _ _ ->
        fail "vcsAddSubmodule: pijul does not support submodules"
    , vcsCommitChanges = \_state -> do
        pijul $
          [ "record"
          , "-a"
          , "-m 'a patch'"
          , "-A 'A <a@example.com>'"
          ]
        commit <- pijul' ["log"]
        let commit' = takeWhile (not . isSpace) commit
        return (Just commit')
    , -- tags work differently in pijul...
      -- so this is wrong
      vcsTagState = \_ tagname ->
        pijul ["tag", tagname]
    , vcsSwitchBranch = \_ branchname -> do
        --        unless (branchname `Map.member` allBranches) $
        --          pijul ["from-branch", branchname]
        pijul $ ["checkout", branchname]
    , vcsCheckoutTag = Left $ \tagname ->
        pijul $ ["checkout", tagname]
    }
  where
    gitInvocation args =
      (programInvocation (vcsProgram vcs) args)
        { progInvokeCwd = Just repoRoot
        }
    pijul = runProgramInvocation verbosity . gitInvocation
    pijul' = getProgramInvocationOutput verbosity . gitInvocation

vcsTestDriverHg
  :: Verbosity
  -> VCS ConfiguredProgram
  -> FilePath
  -> FilePath
  -> VCSTestDriver
vcsTestDriverHg verbosity vcs _ repoRoot =
  VCSTestDriver
    { vcsVCS = vcs
    , vcsRepoRoot = repoRoot
    , vcsIgnoreFiles = Set.empty
    , vcsInit =
        hg $ ["init"] ++ verboseArg
    , vcsAddFile = \_ filename ->
        hg ["add", filename]
    , vcsSubmoduleDriver = \_ ->
        fail "vcsSubmoduleDriver: hg submodules not supported"
    , vcsAddSubmodule = \_ _ _ ->
        fail "vcsAddSubmodule: hg submodules not supported"
    , vcsCommitChanges = \_state -> do
        hg $
          [ "--user='A <a@example.com>'"
          , "commit"
          , "--message=a patch"
          ]
            ++ verboseArg
        commit <- hg' ["log", "--template='{node}\\n' -l1"]
        let commit' = takeWhile (not . isSpace) commit
        return (Just commit')
    , vcsTagState = \_ tagname ->
        hg ["tag", "--force", tagname]
    , vcsSwitchBranch = \RepoState{allBranches} branchname -> do
        unless (branchname `Map.member` allBranches) $
          hg ["branch", branchname]
        hg $ ["checkout", branchname] ++ verboseArg
    , vcsCheckoutTag = Left $ \tagname ->
        hg $ ["checkout", "--rev", tagname] ++ verboseArg
    }
  where
    hgInvocation args =
      (programInvocation (vcsProgram vcs) args)
        { progInvokeCwd = Just repoRoot
        }
    hg = runProgramInvocation verbosity . hgInvocation
    hg' = getProgramInvocationOutput verbosity . hgInvocation
    verboseArg = ["--quiet" | verbosity < Verbosity.normal]
