module Distribution.Client.Get.Brancher where

import Prelude ()
import Distribution.Client.Compat.Prelude

import Distribution.Verbosity
         ( Verbosity )
import Distribution.Compat.Exception
        ( catchIO )
import qualified Distribution.PackageDescription as PD
import Distribution.Simple.Utils
         ( notice, rawSystemExitCode )

import Distribution.Client.Compat.Process
        ( readProcessWithExitCode )

import Control.Exception
         ( finally )
import qualified Data.Map
import Data.Ord
         ( comparing )
import System.Directory
         ( getCurrentDirectory, setCurrentDirectory )
import System.Exit
         ( ExitCode(..) )


data BranchCmd = BranchCmd (Verbosity -> FilePath -> IO ExitCode)

data Brancher = Brancher
    { brancherBinary :: String
    , brancherBuildCmd :: PD.SourceRepo -> Maybe BranchCmd
    }

-- | The set of all supported branch drivers.
allBranchers :: [(PD.RepoType, Brancher)]
allBranchers =
    [ (PD.Bazaar, branchBzr)
    , (PD.Darcs, branchDarcs)
    , (PD.Git, branchGit)
    , (PD.Mercurial, branchHg)
    , (PD.SVN, branchSvn)
    ]

-- | Find which usable branch drivers (selected from 'allBranchers') are
-- available and usable on the local machine.
--
-- Each driver's main command is run with @--help@, and if the child process
-- exits successfully, that brancher is considered usable.
findUsableBranchers :: IO (Data.Map.Map PD.RepoType Brancher)
findUsableBranchers = do
    let usable (_, brancher) = flip catchIO (const (return False)) $ do
         let cmd = brancherBinary brancher
         (exitCode, _, _) <- readProcessWithExitCode cmd ["--help"] ""
         return (exitCode == ExitSuccess)
    pairs <- filterM usable allBranchers
    return (Data.Map.fromList pairs)

-- | Given a set of possible branchers, and a set of possible source
-- repositories, find a repository that is both 1) likely to be specific to
-- this source version and 2) is supported by the local machine.
findBranchCmd :: Data.Map.Map PD.RepoType Brancher -> [PD.SourceRepo]
                 -> (Maybe PD.RepoKind) -> Maybe BranchCmd
findBranchCmd branchers allRepos maybeKind = cmd where
    -- Sort repositories by kind, from This to Head to Unknown. Repositories
    -- with equivalent kinds are selected based on the order they appear in
    -- the Cabal description file.
    repos' = sortBy (comparing thisFirst) allRepos
    thisFirst r = case PD.repoKind r of
        PD.RepoThis -> 0 :: Int
        PD.RepoHead -> case PD.repoTag r of
            -- If the type is 'head' but the author specified a tag, they
            -- probably meant to create a 'this' repository but screwed up.
            Just _ -> 0
            Nothing -> 1
        PD.RepoKindUnknown _ -> 2

    -- If the user has specified the repo kind, filter out the repositories
    -- she's not interested in.
    repos = maybe repos' (\k -> filter ((==) k . PD.repoKind) repos') maybeKind

    repoBranchCmd repo = do
        t <- PD.repoType repo
        brancher <- Data.Map.lookup t branchers
        brancherBuildCmd brancher repo

    cmd = listToMaybe (mapMaybe repoBranchCmd repos)

-- | Branch driver for Bazaar.
branchBzr :: Brancher
branchBzr = Brancher "bzr" $ \repo -> do
    src <- PD.repoLocation repo
    let args dst = case PD.repoTag repo of
         Just tag -> ["branch", src, dst, "-r", "tag:" ++ tag]
         Nothing -> ["branch", src, dst]
    return $ BranchCmd $ \verbosity dst -> do
        notice verbosity ("bzr: branch " ++ show src)
        rawSystemExitCode verbosity "bzr" (args dst)

-- | Branch driver for Darcs.
branchDarcs :: Brancher
branchDarcs = Brancher "darcs" $ \repo -> do
    src <- PD.repoLocation repo
    let args dst = case PD.repoTag repo of
         Just tag -> ["get", src, dst, "-t", tag]
         Nothing -> ["get", src, dst]
    return $ BranchCmd $ \verbosity dst -> do
        notice verbosity ("darcs: get " ++ show src)
        rawSystemExitCode verbosity "darcs" (args dst)

-- | Branch driver for Git.
branchGit :: Brancher
branchGit = Brancher "git" $ \repo -> do
    src <- PD.repoLocation repo
    let postClone verbosity dst = case PD.repoTag repo of
         Just t -> do
             cwd <- getCurrentDirectory
             setCurrentDirectory dst
             finally
                 (rawSystemExitCode verbosity "git" ["checkout", t])
                 (setCurrentDirectory cwd)
         Nothing -> return ExitSuccess
    return $ BranchCmd $ \verbosity dst -> do
        notice verbosity ("git: clone " ++ show src)
        code <- rawSystemExitCode verbosity "git" (["clone", src, dst] ++
                    case PD.repoBranch repo of
                        Nothing -> []
                        Just b -> ["--branch", b])
        case code of
            ExitFailure _ -> return code
            ExitSuccess -> postClone verbosity  dst

-- | Branch driver for Mercurial.
branchHg :: Brancher
branchHg = Brancher "hg" $ \repo -> do
    src <- PD.repoLocation repo
    let branchArgs = case PD.repoBranch repo of
         Just b -> ["--branch", b]
         Nothing -> []
    let tagArgs = case PD.repoTag repo of
         Just t -> ["--rev", t]
         Nothing -> []
    let args dst = ["clone", src, dst] ++ branchArgs ++ tagArgs
    return $ BranchCmd $ \verbosity dst -> do
        notice verbosity ("hg: clone " ++ show src)
        rawSystemExitCode verbosity "hg" (args dst)

-- | Branch driver for Subversion.
branchSvn :: Brancher
branchSvn = Brancher "svn" $ \repo -> do
    src <- PD.repoLocation repo
    let args dst = ["checkout", src, dst]
    return $ BranchCmd $ \verbosity dst -> do
        notice verbosity ("svn: checkout " ++ show src)
        rawSystemExitCode verbosity "svn" (args dst)
