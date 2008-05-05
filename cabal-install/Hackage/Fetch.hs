-----------------------------------------------------------------------------
-- |
-- Module      :  Hackage.Fetch
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
--
-----------------------------------------------------------------------------
module Hackage.Fetch
    (
     -- * Commands
     fetch
    , -- * Utilities
      fetchPackage
    , isFetched
    , downloadIndex
    ) where

import Network.URI (URI,parseURI,uriScheme,uriPath)
import Network.HTTP (ConnError(..), Response(..))

import Hackage.Types
         ( UnresolvedDependency (..), AvailablePackage(..)
         , AvailablePackageSource(..), Repo(..), repoURL )
import Hackage.Dependency (resolveDependencies)
import qualified Hackage.IndexUtils as IndexUtils
import qualified Hackage.InstallPlan as InstallPlan
import Hackage.Utils (showDependencies)
import Hackage.HttpUtils (getHTTP)

import Distribution.Package
         ( PackageIdentifier(..), Package(..) )
import Distribution.Simple.Compiler
         ( Compiler(compilerId), PackageDB )
import Distribution.Simple.Program (ProgramConfiguration)
import Distribution.Simple.Configure (getInstalledPackages)
import Distribution.Simple.Utils
         ( die, notice, debug, setupMessage, intercalate )
import Distribution.System
         ( buildOS, buildArch )
import Distribution.Text
         ( display )
import Distribution.Verbosity (Verbosity)

import Data.Monoid (Monoid(mconcat))
import Control.Exception (bracket)
import Control.Monad (filterM)
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.FilePath ((</>), (<.>))
import System.Directory (copyFile)
import System.IO (IOMode(..), hPutStr, Handle, hClose, openBinaryFile)


downloadURI :: Verbosity
            -> FilePath -- ^ Where to put it
            -> URI      -- ^ What to download
            -> IO (Maybe ConnError)
downloadURI verbosity path uri
    | uriScheme uri == "file:" = do
        copyFile (uriPath uri) path
        return Nothing
    | otherwise = do
        eitherResult <- getHTTP verbosity uri
        case eitherResult of
           Left err -> return (Just err)
           Right rsp
               | rspCode rsp == (2,0,0) -> withBinaryFile path WriteMode (`hPutStr` rspBody rsp) 
                                                          >> return Nothing
               | otherwise -> return (Just (ErrorMisc ("Invalid HTTP code: " ++ show (rspCode rsp))))

downloadFile :: Verbosity
             -> FilePath
             -> String
             -> IO (Maybe ConnError)
downloadFile verbosity path url
    = case parseURI url of
        Just parsed -> downloadURI verbosity path parsed
        Nothing -> return (Just (ErrorMisc ("Failed to parse url: " ++ show url)))


-- Downloads a package to [config-dir/packages/package-id] and returns the path to the package.
downloadPackage :: Verbosity -> AvailablePackage -> IO String
downloadPackage verbosity pkg
    = do let url = packageURL pkg
             dir = packageDir pkg
             path = packageFile pkg
         debug verbosity $ "GET " ++ show url
         createDirectoryIfMissing True dir
         mbError <- downloadFile verbosity path url
         case mbError of
           Just err -> die $ "Failed to download '" ++ display (packageId pkg) ++ "': " ++ show err
           Nothing -> return path

-- Downloads an index file to [config-dir/packages/serv-id].
downloadIndex :: Verbosity -> Repo -> IO FilePath
downloadIndex verbosity repo
    = do let url = repoURL repo ++ "/" ++ "00-index.tar.gz"
             dir = repoCacheDir repo
             path = dir </> "00-index" <.> "tar.gz"
         createDirectoryIfMissing True dir
         mbError <- downloadFile verbosity path url
         case mbError of
           Just err -> die $ "Failed to download index '" ++ show err ++ "'"
           Nothing  -> return path

-- |Returns @True@ if the package has already been fetched.
isFetched :: AvailablePackage -> IO Bool
isFetched pkg = doesFileExist (packageFile pkg)

-- |Fetch a package if we don't have it already.
fetchPackage :: Verbosity -> AvailablePackage -> IO String
fetchPackage verbosity pkg
    = do fetched <- isFetched pkg
         if fetched
            then do notice verbosity $ "'" ++ display (packageId pkg) ++ "' is cached."
                    return (packageFile pkg)
            else do setupMessage verbosity "Downloading" (packageId pkg)
                    downloadPackage verbosity pkg

-- |Fetch a list of packages and their dependencies.
fetch :: Verbosity
      -> PackageDB
      -> [Repo]
      -> Compiler
      -> ProgramConfiguration
      -> [UnresolvedDependency]
      -> IO ()
fetch verbosity packageDB repos comp conf deps
    = do installed <- getInstalledPackages verbosity comp packageDB conf
         available <- fmap mconcat (mapM (IndexUtils.readRepoIndex verbosity) repos)
         deps' <- IndexUtils.disambiguateDependencies available deps
         case resolveDependencies buildOS buildArch
                (compilerId comp) installed available deps' of
           Left missing -> die $ "Unresolved dependencies: " ++ showDependencies missing
           Right pkgs   -> do
             ps <- filterM (fmap not . isFetched)
                     [ pkg | (InstallPlan.Configured
                               (InstallPlan.ConfiguredPackage pkg _ _))
                                 <- InstallPlan.toList pkgs ]
             mapM_ (fetchPackage verbosity) ps

withBinaryFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
withBinaryFile name mode = bracket (openBinaryFile name mode) hClose

-- |Generate the full path to the locally cached copy of
-- the tarball for a given @PackageIdentifer@.
packageFile :: AvailablePackage -> FilePath
packageFile pkg = packageDir pkg
              </> display (packageId pkg)
              <.> "tar.gz"

-- |Generate the full path to the directory where the local cached copy of
-- the tarball for a given @PackageIdentifer@ is stored.
packageDir :: AvailablePackage -> FilePath
packageDir AvailablePackage { packageInfoId = p
                            , packageSource = RepoTarballPackage repo } = 
                         repoCacheDir repo
                     </> pkgName p
                     </> display (pkgVersion p)

-- | Generate the URL of the tarball for a given package.
packageURL :: AvailablePackage -> String
packageURL AvailablePackage { packageInfoId = p
                            , packageSource = RepoTarballPackage repo } =
  intercalate "/"
    [repoURL repo,
     pkgName p, display (pkgVersion p),
     display p ++ ".tar.gz"]
