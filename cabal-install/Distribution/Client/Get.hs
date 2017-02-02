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

import Prelude ()
import Distribution.Client.Compat.Prelude hiding (get)

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

import Distribution.Client.Get.Brancher
         ( Brancher, BranchCmd(..), findBranchCmd, findUsableBranchers )
import Distribution.Client.Setup
         ( GlobalFlags(..), GetFlags(..), RepoContext(..) )
import Distribution.Client.Types
import Distribution.Client.Targets
import Distribution.Client.Dependency
import Distribution.Client.FetchUtils
import qualified Distribution.Client.Tar as Tar (extractTarGzFile)
import Distribution.Client.IndexUtils as IndexUtils
        ( getSourcePackagesAtIndexState, IndexState(..) )
import Distribution.Solver.Types.SourcePackage

import Control.Monad
         ( forM_, mapM_ )
import qualified Data.Map
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
    mapM_ checkTarget userTargets

  let idxState = fromFlagOrDefault IndexStateHead $
                       getIndexState getFlags

  sourcePkgDb <- getSourcePackagesAtIndexState verbosity repoCtxt idxState

  pkgSpecifiers <- resolveUserTargets verbosity repoCtxt
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
        --TODO: add command-line constraint and preference args for unpack
        standardInstallPolicy mempty sourcePkgDb pkgSpecifiers

    prefix = fromFlagOrDefault "" (getDestDir getFlags)

    fork :: [UnresolvedSourcePackage] -> IO ()
    fork pkgs = do
      let kind = fromFlag . getSourceRepository $ getFlags
      branchers <- findUsableBranchers
      mapM_ (forkPackage verbosity branchers prefix kind) pkgs

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

-- | Fork a single package from a remote source repository to the local
-- file system.
forkPackage :: Verbosity
            -> Data.Map.Map PD.RepoType Brancher
               -- ^ Branchers supported by the local machine.
            -> FilePath
               -- ^ The directory in which new branches or repositories will
               -- be created.
            -> (Maybe PD.RepoKind)
               -- ^ Which repo to choose.
            -> SourcePackage loc
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
