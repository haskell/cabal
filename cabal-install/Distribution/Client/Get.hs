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
    get,
    forkPackages,
    ForkException(..),
    forkPackagesRepo,
  ) where

import Prelude ()
import Distribution.Client.Compat.Prelude hiding (get)

import Distribution.Package
         ( PackageId, packageId, packageName )
import Distribution.Simple.Setup
         ( Flag(..), fromFlag, fromFlagOrDefault, flagToMaybe )
import Distribution.Simple.Utils
         ( notice, die', info, writeFileAtomic )
import Distribution.Verbosity
         ( Verbosity )
import Distribution.Text (display)
import qualified Distribution.PackageDescription as PD
import Distribution.Simple.Program
         ( programName )

import Distribution.Client.Setup
         ( GlobalFlags(..), GetFlags(..), RepoContext(..) )
import Distribution.Client.Types
import Distribution.Client.Targets
import Distribution.Client.Dependency
import Distribution.Client.VCS
import Distribution.Client.FetchUtils
import qualified Distribution.Client.Tar as Tar (extractTarGzFile)
import Distribution.Client.IndexUtils as IndexUtils
        ( getSourcePackagesAtIndexState )
import Distribution.Solver.Types.SourcePackage

import Control.Exception
         ( Exception(..), catch, throwIO )
import Control.Monad
         ( mapM, forM_, mapM_ )
import qualified Data.Map as Map
import System.Directory
         ( createDirectoryIfMissing, doesDirectoryExist, doesFileExist )
import System.Exit
         ( ExitCode(..) )
import System.FilePath
         ( (</>), (<.>), addTrailingPathSeparator )


-- | Entry point for the 'cabal get' command.
get :: Verbosity
    -> RepoContext
    -> GlobalFlags
    -> GetFlags
    -> [UserTarget]
    -> IO ()
get verbosity _ _ _ [] =
    notice verbosity "No packages requested. Nothing to do."

get verbosity repoCtxt globalFlags getFlags userTargets = do
  let useFork = case (getSourceRepository getFlags) of
        NoFlag -> False
        _      -> True

  unless useFork $
    mapM_ (checkTarget verbosity) userTargets

  let idxState = flagToMaybe $ getIndexState getFlags

  sourcePkgDb <- getSourcePackagesAtIndexState verbosity repoCtxt idxState

  pkgSpecifiers <- resolveUserTargets verbosity repoCtxt
                   (fromFlag $ globalWorldFile globalFlags)
                   (packageIndex sourcePkgDb)
                   userTargets

  pkgs <- either (die' verbosity . unlines . map show) return $
            resolveWithoutDependencies
              (resolverParams sourcePkgDb pkgSpecifiers)

  unless (null prefix) $
    createDirectoryIfMissing True prefix

  if useFork
    then fork pkgs
    else unpack pkgs

  where
    resolverParams sourcePkgDb pkgSpecifiers =
        --TODO: add command-line constraint and preference args for unpack
        standardInstallPolicy mempty sourcePkgDb pkgSpecifiers

    prefix = fromFlagOrDefault "" (getDestDir getFlags)
    kind   = fromFlag . getSourceRepository $ getFlags

    fork :: [UnresolvedSourcePackage] -> IO ()
    fork = forkPackages verbosity prefix kind

    unpack :: [UnresolvedSourcePackage] -> IO ()
    unpack pkgs = do
      forM_ pkgs $ \pkg -> do
        location <- fetchPackage verbosity repoCtxt (packageSource pkg)
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

checkTarget :: Verbosity -> UserTarget -> IO ()
checkTarget verbosity target = case target of
    UserTargetLocalDir       dir  -> die' verbosity (notTarball dir)
    UserTargetLocalCabalFile file -> die' verbosity (notTarball file)
    _                             -> return ()
  where
    notTarball :: String -> String
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
    when existsDir $ die' verbosity $
     "The directory \"" ++ pkgdir' ++ "\" already exists, not unpacking."
    existsFile  <- doesFileExist pkgdir
    when existsFile $ die' verbosity $
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

forkPackages :: Verbosity
             -> FilePath            -- ^ destination dir prefix
             -> Maybe RepoKind      -- ^ 
             -> [SourcePackage loc] -- ^ the packages
             -> IO ()
forkPackages verbosity destDirPrefix preferredRepoKind =
    forkPackagesRepo verbosity destDirPrefix preferredRepoKind
  . map (\pkg -> (packageId pkg, packageSourceRepos pkg))
  where
    packageSourceRepos :: SourcePackage loc -> [SourceRepo]
    packageSourceRepos = PD.sourceRepos
                       . PD.packageDescription
                       . packageDescription

data ForkException =
       ForkExceptionNoSourceRepos       PackageId
     | ForkExceptionNoSourceReposOfKind PackageId (Maybe RepoKind)
     | ForkExceptionNoRepoType          PackageId SourceRepo
     | ForkExceptionUnsupportedRepoType PackageId SourceRepo RepoType
     | ForkExceptionNoRepoLocation      PackageId SourceRepo
     | ForkExceptionDestinationExists   PackageId FilePath Bool
     | ForkExceptionFailedWithExitCode  PackageId SourceRepo
                                        String ExitCode
  deriving (Show, Eq)

instance Exception ForkException where
  displayException (ForkExceptionNoSourceRepos pkgid) =
       "Cannot fetch a source repository for package " ++ display pkgid
    ++ ". The package does not specify any source repositories."

  displayException (ForkExceptionNoSourceReposOfKind pkgid repoKind) =
       "Cannot fetch a source repository for package " ++ display pkgid
    ++ ". The package does not specify a source repository of the requested "
    ++ "kind" ++ maybe "." (\k -> " (kind " ++ display k ++ ").") repoKind

  displayException (ForkExceptionNoRepoType pkgid _repo) =
       "Cannot fetch the source repository for package " ++ display pkgid
    ++ ". The package's description specifies a source repository but does "
    ++ "not specify the repository 'type' field (e.g. git, darcs or hg)."

  displayException (ForkExceptionUnsupportedRepoType pkgid _repo repoType) =
       "Cannot fetch the source repository for package " ++ display pkgid
    ++ ". The repository type '" ++ display repoType
    ++ "' is not yet supported."

  displayException (ForkExceptionNoRepoLocation pkgid _repo) =
       "Cannot fetch the source repository for package " ++ display pkgid
    ++ ". The package's description specifies a source repository but does "
    ++ "not specify the repository 'location' field (i.e. the URL)."

  displayException (ForkExceptionDestinationExists pkgid dest isdir) =
       "Not fetching the source repository for package " ++ display pkgid ++ ". "
    ++ if isdir then "The destination directory " ++ dest ++ " already exists."
                else "A file " ++ dest ++ " is in the way."

  displayException (ForkExceptionFailedWithExitCode pkgid repo vcsprogname
                                                    exitcode) =
       "Failed to fetch the source repository for package " ++ display pkgid
    ++ maybe "" (", repository location " ++) (PD.repoLocation repo) ++ " ("
    ++ vcsprogname ++ " failed with " ++ show exitcode ++ ")."


forkPackagesRepo :: Verbosity
                 -> FilePath
                 -> Maybe RepoKind
                 -> [(PackageId, [SourceRepo])]
                 -> IO ()
forkPackagesRepo verbosity destDirPrefix preferredRepoKind pkgrepos = do

    -- Do a bunch of checks and collect the required info
    pkgrepos' <- mapM (prepareClonePackageRepo
                         preferredRepoKind destDirPrefix) pkgrepos

    -- Configure the VCS drivers for all the repository types we may need
    vcss <- configureVCSs verbosity $
              Map.fromList [ (vcsRepoType vcs, vcs)
                           | (_, _, vcs, _, _) <- pkgrepos' ]

    -- Now execute all the required commands for each repo
    sequence_
      [ cloneSourceRepo verbosity vcs' repo srcURL destDir
          `catch` \exitcode ->
           throwIO (ForkExceptionFailedWithExitCode
                      pkgid repo (programName (vcsProgram vcs)) exitcode)
      | (pkgid, repo, vcs, srcURL, destDir) <- pkgrepos'
      , let Just vcs' = Map.lookup (vcsRepoType vcs) vcss
      ]


prepareClonePackageRepo :: Maybe RepoKind
                        -> FilePath
                        -> (PackageId, [SourceRepo])
                        -> IO (PackageId, SourceRepo,
                               VCS Program, String, FilePath)
prepareClonePackageRepo preferredRepoKind destDirPrefix
                        (pkgid, repos) = do
    repo <- case selectPackageSourceRepo preferredRepoKind repos of
      Nothing | null repos -> throwIO (ForkExceptionNoSourceRepos pkgid)
      Nothing              -> throwIO (ForkExceptionNoSourceReposOfKind pkgid
                                         preferredRepoKind)
      Just repo -> return repo

    (vcs, srcURL) <- case selectSourceRepoVCS repo of
      Right x -> return x
      Left SourceRepoRepoTypeUnspecified ->
        throwIO (ForkExceptionNoRepoType pkgid repo)

      Left (SourceRepoRepoTypeUnsupported repoType) ->
        throwIO (ForkExceptionUnsupportedRepoType pkgid repo repoType)

      Left SourceRepoLocationUnspecified ->
        throwIO (ForkExceptionNoRepoLocation pkgid repo)

    destDirExists  <- doesDirectoryExist destDir
    destFileExists <- doesFileExist      destDir
    when (destDirExists || destFileExists) $
      throwIO (ForkExceptionDestinationExists pkgid destDir destDirExists)

    return (pkgid, repo, vcs, srcURL, destDir)
  where
    destDir = destDirPrefix </> display (packageName pkgid)

