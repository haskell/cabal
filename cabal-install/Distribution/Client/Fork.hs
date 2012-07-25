-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Fork
-- Copyright   :  (c) John Millikin 2012
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- The cabal fork command
-----------------------------------------------------------------------------
module Distribution.Client.Fork (
    fork,
  ) where

import Distribution.Package
         ( packageId )
import Distribution.Simple.Setup
         ( fromFlag, fromFlagOrDefault )
import Distribution.Simple.Utils
         ( notice, die )
import Distribution.Verbosity
         ( Verbosity )
import Distribution.Text(display)
import qualified Distribution.PackageDescription as PD

import Distribution.Client.Setup
         ( GlobalFlags(..), ForkFlags(..) )
import Distribution.Client.Types
import Distribution.Client.Targets
import Distribution.Client.Dependency
import Distribution.Client.IndexUtils as IndexUtils
         ( getSourcePackages )

import Control.Exception
         ( finally )
import Control.Monad
         ( filterM, unless, when )
import Data.List
         ( sortBy )
import qualified Data.Map
import Data.Maybe
         ( listToMaybe, mapMaybe )
import Data.Monoid
         ( mempty )
import Data.Ord
         ( comparing )
import System.Cmd
         ( rawSystem )
import System.Directory
         ( createDirectoryIfMissing, doesDirectoryExist, doesFileExist
         , getCurrentDirectory, setCurrentDirectory
         )
import System.Exit
         ( ExitCode(..) )
import System.FilePath
         ( (</>) )
import System.Process
         ( readProcessWithExitCode )


fork :: Verbosity
     -> [Repo]
     -> GlobalFlags
     -> ForkFlags
     -> [UserTarget]
     -> IO ()
fork verbosity _ _ _ [] =
    notice verbosity "No packages requested. Nothing to do."

fork verbosity repos globalFlags forkFlags userTargets = do
    let prefix = fromFlagOrDefault "" (forkDestDir forkFlags)

    --TODO: add commandline constraint and preference args?
    let resolverParams sourcePkgDb pkgSpecifiers =
         standardInstallPolicy mempty sourcePkgDb pkgSpecifiers

    sourcePkgDb <- getSourcePackages verbosity repos

    pkgSpecifiers <- resolveUserTargets
        verbosity
        (fromFlag $ globalWorldFile globalFlags)
        (packageIndex sourcePkgDb)
        userTargets

    pkgs <- case resolveWithoutDependencies (resolverParams sourcePkgDb pkgSpecifiers) of
        Right pkgs -> return pkgs
        Left errors -> die (unlines (map show errors))

    unless (null prefix) $
       createDirectoryIfMissing True prefix

    branchers <- findUsableBranchers
    mapM_ (forkPackage verbosity branchers prefix) pkgs

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
    let usable (_, brancher) = do
         let cmd = brancherBinary brancher
         (exitCode, _, _) <- readProcessWithExitCode cmd ["--help"] ""
         return (exitCode == ExitSuccess)
    pairs <- filterM usable allBranchers
    return (Data.Map.fromList pairs)

-- | Fork a single package from a remote source repository to the local
-- filesystem.
forkPackage :: Verbosity
            -> Data.Map.Map PD.RepoType Brancher
               -- ^ Branchers supported by the local machine.
            -> FilePath
               -- ^ The directory in which new branches or repositories will
               -- be created.
            -> SourcePackage
               -- ^ The package to fork.
            -> IO ()
forkPackage verbosity branchers prefix src = do
    let desc = PD.packageDescription (packageDescription src)
    let pkgname = display (packageId src)
    let destdir = prefix </> pkgname

    destDirExists <- doesDirectoryExist destdir
    when destDirExists $ do
        die ("The directory " ++ show destdir ++ " already exists, not forking.")

    destFileExists  <- doesFileExist destdir
    when destFileExists $ do
        die ("A file " ++ show destdir ++ " is in the way, not forking.")

    let repos = PD.sourceRepos desc
    case findBranchCmd branchers repos of
        Just (BranchCmd io) -> do
            exitCode <- io verbosity destdir
            case exitCode of
                ExitSuccess -> return ()
                ExitFailure _ -> die ("Couldn't fork package " ++ pkgname)
        Nothing -> case repos of
            [] -> die ("Package " ++ pkgname ++ " does not have any source repositories.")
            _ -> die ("Package " ++ pkgname ++ " does not have any usable source repositories.")

-- | Given a set of possible branchers, and a set of possible source
-- repositories, find a repository that is both 1) likely to be specific to
-- this source version and 2) is supported by the local machine.
findBranchCmd :: Data.Map.Map PD.RepoType Brancher -> [PD.SourceRepo] -> Maybe BranchCmd
findBranchCmd branchers allRepos = cmd where
    -- Sort repositories by kind, from This to Head to Unknown. Repositories
    -- with equivalent kinds are selected based on the order they appear in
    -- the Cabal description file.
    repos = sortBy (comparing thisFirst) allRepos
    thisFirst r = case PD.repoKind r of
        PD.RepoThis -> 0 :: Int
        PD.RepoHead -> case PD.repoTag r of
            -- If the type is 'head' but the author specified a tag, they
            -- probably meant to create a 'this' repository but screwed up.
            Just _ -> 0
            Nothing -> 1
        PD.RepoKindUnknown _ -> 2

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
        rawSystem "bzr" (args dst)

-- | Branch driver for Darcs.
branchDarcs :: Brancher
branchDarcs = Brancher "darcs" $ \repo -> do
    src <- PD.repoLocation repo
    let args dst = case PD.repoTag repo of
         Just tag -> ["get", src, dst, "-t", tag]
         Nothing -> ["get", src, dst]
    return $ BranchCmd $ \verbosity dst -> do
        notice verbosity ("darcs: get " ++ show src)
        rawSystem "darcs" (args dst)

-- | Branch driver for Git.
branchGit :: Brancher
branchGit = Brancher "git" $ \repo -> do
    src <- PD.repoLocation repo
    let branchArgs = case PD.repoBranch repo of
         Just b -> ["--branch", b]
         Nothing -> []
    let postClone dst = case PD.repoTag repo of
         Just t -> do
             cwd <- getCurrentDirectory
             setCurrentDirectory dst
             finally
                 (rawSystem "git" (["checkout", t] ++ branchArgs))
                 (setCurrentDirectory cwd)
         Nothing -> return ExitSuccess
    return $ BranchCmd $ \verbosity dst -> do
        notice verbosity ("git: clone " ++ show src)
        code <- rawSystem "git" (["clone", src, dst] ++ branchArgs)
        case code of
            ExitFailure _ -> return code
            ExitSuccess -> postClone dst

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
        rawSystem "hg" (args dst)

-- | Branch driver for Subversion.
branchSvn :: Brancher
branchSvn = Brancher "svn" $ \repo -> do
    src <- PD.repoLocation repo
    let args dst = ["checkout", src, dst]
    return $ BranchCmd $ \verbosity dst -> do
        notice verbosity ("svn: checkout " ++ show src)
        rawSystem "svn" (args dst)
