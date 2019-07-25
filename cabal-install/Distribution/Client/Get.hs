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

    -- * Cloning 'SourceRepo's
    -- | Mainly exported for testing purposes
    clonePackagesFromSourceRepo,
    ClonePackageException(..),
  ) where

import Prelude ()
import Distribution.Client.Compat.Prelude hiding (get)
import Data.Ord (comparing)
import Distribution.Compat.Directory
         ( listDirectory )
import Distribution.Package
         ( PackageId, packageId, packageName )
import Distribution.Simple.Setup
         ( Flag(..), fromFlag, fromFlagOrDefault, flagToMaybe )
import Distribution.Simple.Utils
         ( notice, die', info, writeFileAtomic )
import Distribution.Verbosity
         ( Verbosity )
import Distribution.Deprecated.Text (display)
import qualified Distribution.PackageDescription as PD
import Distribution.Simple.Program
         ( programName )
import Distribution.Types.SourceRepo (RepoKind (..))
import Distribution.Client.SourceRepo (SourceRepositoryPackage (..), SourceRepoProxy, srpToProxy)

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
  let useSourceRepo = case getSourceRepository getFlags of
                        NoFlag -> False
                        _      -> True

  unless useSourceRepo $
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

  if useSourceRepo
    then clone  pkgs
    else unpack pkgs

  where
    resolverParams sourcePkgDb pkgSpecifiers =
        --TODO: add command-line constraint and preference args for unpack
        standardInstallPolicy mempty sourcePkgDb pkgSpecifiers

    prefix = fromFlagOrDefault "" (getDestDir getFlags)

    clone :: [UnresolvedSourcePackage] -> IO ()
    clone = clonePackagesFromSourceRepo verbosity prefix kind
          . map (\pkg -> (packageId pkg, packageSourceRepos pkg))
      where
        kind = fromFlag . getSourceRepository $ getFlags
        packageSourceRepos :: SourcePackage loc -> [PD.SourceRepo]
        packageSourceRepos = PD.sourceRepos
                           . PD.packageDescription
                           . packageDescription

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

          RemoteSourceRepoPackage _repo _ ->
            die' verbosity $ "The 'get' command does no yet support targets "
                          ++ "that are remote source repositories."

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
    let pkgdirname               = display pkgid
        pkgdir                   = prefix </> pkgdirname
        pkgdir'                  = addTrailingPathSeparator pkgdir
        emptyDirectory directory = null <$> listDirectory directory
    existsDir  <- doesDirectoryExist pkgdir
    when existsDir $ do
      isEmpty <- emptyDirectory pkgdir
      unless isEmpty $
        die' verbosity $
        "The directory \"" ++ pkgdir' ++ "\" already exists and is not empty, not unpacking."
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
-- * Cloning packages from their declared source repositories
-- ------------------------------------------------------------


data ClonePackageException =
       ClonePackageNoSourceRepos       PackageId
     | ClonePackageNoSourceReposOfKind PackageId (Maybe RepoKind)
     | ClonePackageNoRepoType          PackageId PD.SourceRepo
     | ClonePackageUnsupportedRepoType PackageId SourceRepoProxy RepoType
     | ClonePackageNoRepoLocation      PackageId PD.SourceRepo
     | ClonePackageDestinationExists   PackageId FilePath Bool
     | ClonePackageFailedWithExitCode  PackageId SourceRepoProxy String ExitCode
  deriving (Show, Eq)

instance Exception ClonePackageException where
  displayException (ClonePackageNoSourceRepos pkgid) =
       "Cannot fetch a source repository for package " ++ display pkgid
    ++ ". The package does not specify any source repositories."

  displayException (ClonePackageNoSourceReposOfKind pkgid repoKind) =
       "Cannot fetch a source repository for package " ++ display pkgid
    ++ ". The package does not specify a source repository of the requested "
    ++ "kind" ++ maybe "." (\k -> " (kind " ++ display k ++ ").") repoKind

  displayException (ClonePackageNoRepoType pkgid _repo) =
       "Cannot fetch the source repository for package " ++ display pkgid
    ++ ". The package's description specifies a source repository but does "
    ++ "not specify the repository 'type' field (e.g. git, darcs or hg)."

  displayException (ClonePackageUnsupportedRepoType pkgid _ repoType) =
       "Cannot fetch the source repository for package " ++ display pkgid
    ++ ". The repository type '" ++ display repoType
    ++ "' is not yet supported."

  displayException (ClonePackageNoRepoLocation pkgid _repo) =
       "Cannot fetch the source repository for package " ++ display pkgid
    ++ ". The package's description specifies a source repository but does "
    ++ "not specify the repository 'location' field (i.e. the URL)."

  displayException (ClonePackageDestinationExists pkgid dest isdir) =
       "Not fetching the source repository for package " ++ display pkgid ++ ". "
    ++ if isdir then "The destination directory " ++ dest ++ " already exists."
                else "A file " ++ dest ++ " is in the way."

  displayException (ClonePackageFailedWithExitCode
                      pkgid repo vcsprogname exitcode) =
       "Failed to fetch the source repository for package " ++ display pkgid
    ++ ", repository location " ++ srpLocation repo ++ " ("
    ++ vcsprogname ++ " failed with " ++ show exitcode ++ ")."


-- | Given a bunch of package ids and their corresponding available
-- 'SourceRepo's, pick a single 'SourceRepo' for each one and clone into
-- new subdirs of the given directory.
--
clonePackagesFromSourceRepo :: Verbosity
                            -> FilePath            -- ^ destination dir prefix
                            -> Maybe RepoKind      -- ^ preferred 'RepoKind'
                            -> [(PackageId, [PD.SourceRepo])]
                                                   -- ^ the packages and their
                                                   -- available 'SourceRepo's
                            -> IO ()
clonePackagesFromSourceRepo verbosity destDirPrefix
                            preferredRepoKind pkgrepos = do

    -- Do a bunch of checks and collect the required info
    pkgrepos' <- mapM preCloneChecks pkgrepos

    -- Configure the VCS drivers for all the repository types we may need
    vcss <- configureVCSs verbosity $
              Map.fromList [ (vcsRepoType vcs, vcs)
                           | (_, _, vcs, _) <- pkgrepos' ]

    -- Now execute all the required commands for each repo
    sequence_
      [ cloneSourceRepo verbosity vcs' repo destDir
          `catch` \exitcode ->
           throwIO (ClonePackageFailedWithExitCode
                      pkgid (srpToProxy repo) (programName (vcsProgram vcs)) exitcode)
      | (pkgid, repo, vcs, destDir) <- pkgrepos'
      , let Just vcs' = Map.lookup (vcsRepoType vcs) vcss
      ]

  where
    preCloneChecks :: (PackageId, [PD.SourceRepo])
                   -> IO (PackageId, SourceRepositoryPackage Maybe, VCS Program, FilePath)
    preCloneChecks (pkgid, repos) = do
      repo <- case selectPackageSourceRepo preferredRepoKind repos of
        Just repo            -> return repo
        Nothing | null repos -> throwIO (ClonePackageNoSourceRepos pkgid)
        Nothing              -> throwIO (ClonePackageNoSourceReposOfKind
                                           pkgid preferredRepoKind)

      (repo', vcs) <- case validatePDSourceRepo repo of
        Right (repo', _, _, vcs) -> return (repo', vcs)
        Left SourceRepoRepoTypeUnspecified ->
          throwIO (ClonePackageNoRepoType pkgid repo)

        Left (SourceRepoRepoTypeUnsupported repo' repoType) ->
          throwIO (ClonePackageUnsupportedRepoType pkgid repo' repoType)

        Left SourceRepoLocationUnspecified ->
          throwIO (ClonePackageNoRepoLocation pkgid repo)

      let destDir = destDirPrefix </> display (packageName pkgid)
      destDirExists  <- doesDirectoryExist destDir
      destFileExists <- doesFileExist      destDir
      when (destDirExists || destFileExists) $
        throwIO (ClonePackageDestinationExists pkgid destDir destDirExists)

      return (pkgid, repo', vcs, destDir)

-------------------------------------------------------------------------------
-- Selecting
-------------------------------------------------------------------------------

-- | Pick the 'SourceRepo' to use to get the package sources from.
--
-- Note that this does /not/ depend on what 'VCS' drivers we are able to
-- successfully configure. It is based only on the 'SourceRepo's declared
-- in the package, and optionally on a preferred 'RepoKind'.
--
selectPackageSourceRepo :: Maybe RepoKind
                        -> [PD.SourceRepo]
                        -> Maybe PD.SourceRepo
selectPackageSourceRepo preferredRepoKind =
    listToMaybe
    -- Sort repositories by kind, from This to Head to Unknown. Repositories
    -- with equivalent kinds are selected based on the order they appear in
    -- the Cabal description file.
  . sortBy (comparing thisFirst)
    -- If the user has specified the repo kind, filter out the repositories
    -- they're not interested in.
  . filter (\repo -> maybe True (PD.repoKind repo ==) preferredRepoKind)
  where
    thisFirst :: PD.SourceRepo -> Int
    thisFirst r = case PD.repoKind r of
        RepoThis -> 0
        RepoHead -> case PD.repoTag r of
            -- If the type is 'head' but the author specified a tag, they
            -- probably meant to create a 'this' repository but screwed up.
            Just _  -> 0
            Nothing -> 1
        RepoKindUnknown _ -> 2
