-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Get
-- Copyright   :  (c) Andrea Vezzosi 2008
--                    Duncan Coutts 2011
--                    John Millikin 2012
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- The 'cabal get' command.
-----------------------------------------------------------------------------

module Distribution.Client.Get (
    get
  ) where

import Distribution.Package
         ( PackageId, packageId, packageName )
import Distribution.Simple.Setup
         ( Flag(..), fromFlag, fromFlagOrDefault )
import Distribution.Simple.Utils
         ( notice, die, info, writeFileAtomic )
import Distribution.Verbosity
         ( Verbosity )
import Distribution.Text(display)
import qualified Distribution.PackageDescription as PD

import Distribution.Client.Setup
         ( GlobalFlags(..), GetFlags(..) )
import Distribution.Client.Types
import Distribution.Client.Targets
import Distribution.Client.Dependency
import Distribution.Client.FetchUtils
import qualified Distribution.Client.Tar as Tar (extractTarGzFile)
import Distribution.Client.IndexUtils as IndexUtils
        ( getSourcePackages )
import Distribution.Client.Compat.Process
        ( readProcessWithExitCode )
import Distribution.Compat.Exception
        ( catchIO )

import Control.Exception
         ( finally )
import Control.Monad
         ( filterM, forM_, unless, when )
import Data.List
         ( sortBy )
import qualified Data.Map
import Data.Maybe
         ( listToMaybe, mapMaybe )
import Data.Monoid
         ( mempty )
import Data.Ord
         ( comparing )
import System.Directory
         ( createDirectoryIfMissing, doesDirectoryExist, doesFileExist
         , getCurrentDirectory, setCurrentDirectory
         )
import System.Exit
         ( ExitCode(..) )
import System.FilePath
         ( (</>), (<.>), addTrailingPathSeparator )
import System.Process
         ( rawSystem )


-- | Entry point for the 'cabal get' command.
get :: Verbosity
    -> [Repo]
    -> GlobalFlags
    -> GetFlags
    -> [UserTarget]
    -> IO ()
get verbosity _ _ _ [] =
    notice verbosity "No packages requested. Nothing to do."

get verbosity repos globalFlags getFlags userTargets = do
  let useFork = case (getSourceRepository getFlags) of
        NoFlag -> False
        _      -> True

  unless useFork $
    mapM_ checkTarget userTargets

  sourcePkgDb <- getSourcePackages verbosity repos

  pkgSpecifiers <- resolveUserTargets verbosity
                   (fromFlag $ globalWorldFile globalFlags)
                   (packageIndex sourcePkgDb)
                   userTargets

  pkgs <- either (die . unlines . map show) return $
            resolveWithoutDependencies
              (resolverParams sourcePkgDb pkgSpecifiers)

  unless (null prefix) $
    createDirectoryIfMissing True prefix

  if useFork
    then fork pkgs
    else unpack pkgs

  where
    resolverParams sourcePkgDb pkgSpecifiers =
        --TODO: add commandline constraint and preference args for unpack
        standardInstallPolicy mempty sourcePkgDb pkgSpecifiers

    prefix = fromFlagOrDefault "" (getDestDir getFlags)

    fork :: [SourcePackage] -> IO ()
    fork pkgs = do
      let kind = fromFlag . getSourceRepository $ getFlags
      branchers <- findUsableBranchers
      mapM_ (forkPackage verbosity branchers prefix kind) pkgs

    unpack :: [SourcePackage] -> IO ()
    unpack pkgs = do
      forM_ pkgs $ \pkg -> do
        location <- fetchPackage verbosity (packageSource pkg)
        let pkgid = packageId pkg
            descOverride | usePristine = Nothing
                         | otherwise   = packageDescrOverride pkg
        case location of
          LocalTarballPackage tarballPath ->
            unpackPackage verbosity prefix pkgid descOverride tarballPath

          RemoteTarballPackage _tarballURL tarballPath ->
            unpackPackage verbosity prefix pkgid descOverride tarballPath

          RepoTarballPackage _repo _pkgid tarballPath ->
            unpackPackage verbosity prefix pkgid descOverride tarballPath

          LocalUnpackedPackage _ ->
            error "Distribution.Client.Get.unpack: the impossible happened."
      where
        usePristine = fromFlagOrDefault False (getPristine getFlags)

checkTarget :: UserTarget -> IO ()
checkTarget target = case target of
    UserTargetLocalDir       dir  -> die (notTarball dir)
    UserTargetLocalCabalFile file -> die (notTarball file)
    _                             -> return ()
  where
    notTarball t =
        "The 'get' command is for tarball packages. "
     ++ "The target '" ++ t ++ "' is not a tarball."

-- ------------------------------------------------------------
-- * Unpacking the source tarball
-- ------------------------------------------------------------

unpackPackage :: Verbosity -> FilePath -> PackageId
              -> PackageDescriptionOverride
              -> FilePath  -> IO ()
unpackPackage verbosity prefix pkgid descOverride pkgPath = do
    let pkgdirname = display pkgid
        pkgdir     = prefix </> pkgdirname
        pkgdir'    = addTrailingPathSeparator pkgdir
    existsDir  <- doesDirectoryExist pkgdir
    when existsDir $ die $
     "The directory \"" ++ pkgdir' ++ "\" already exists, not unpacking."
    existsFile  <- doesFileExist pkgdir
    when existsFile $ die $
     "A file \"" ++ pkgdir ++ "\" is in the way, not unpacking."
    notice verbosity $ "Unpacking to " ++ pkgdir'
    Tar.extractTarGzFile prefix pkgdirname pkgPath

    case descOverride of
      Nothing     -> return ()
      Just pkgtxt -> do
        let descFilePath = pkgdir </> display (packageName pkgid) <.> "cabal"
        info verbosity $
          "Updating " ++ descFilePath
                      ++ " with the latest revision from the index."
        writeFileAtomic descFilePath pkgtxt


-- ------------------------------------------------------------
-- * Forking the source repository
-- ------------------------------------------------------------

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

-- | Fork a single package from a remote source repository to the local
-- filesystem.
forkPackage :: Verbosity
            -> Data.Map.Map PD.RepoType Brancher
               -- ^ Branchers supported by the local machine.
            -> FilePath
               -- ^ The directory in which new branches or repositories will
               -- be created.
            -> (Maybe PD.RepoKind)
               -- ^ Which repo to choose.
            -> SourcePackage
               -- ^ The package to fork.
            -> IO ()
forkPackage verbosity branchers prefix kind src = do
    let desc    = PD.packageDescription (packageDescription src)
        pkgid   = display (packageId src)
        pkgname = display (packageName src)
        destdir = prefix </> pkgname

    destDirExists <- doesDirectoryExist destdir
    when destDirExists $ do
        die ("The directory " ++ show destdir ++ " already exists, not forking.")

    destFileExists  <- doesFileExist destdir
    when destFileExists $ do
        die ("A file " ++ show destdir ++ " is in the way, not forking.")

    let repos = PD.sourceRepos desc
    case findBranchCmd branchers repos kind of
        Just (BranchCmd io) -> do
            exitCode <- io verbosity destdir
            case exitCode of
                ExitSuccess -> return ()
                ExitFailure _ -> die ("Couldn't fork package " ++ pkgid)
        Nothing -> case repos of
            [] -> die ("Package " ++ pkgid
                       ++ " does not have any source repositories.")
            _ -> die ("Package " ++ pkgid
                      ++ " does not have any usable source repositories.")

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
