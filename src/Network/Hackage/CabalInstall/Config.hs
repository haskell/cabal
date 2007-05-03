-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Hackage.CabalInstall.Config
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Utilities for handling saved state such as known packages, known servers and downloaded packages.
-----------------------------------------------------------------------------
module Network.Hackage.CabalInstall.Config
    ( packagesDirectory
    , getDefaultConfigDir
    , getLocalConfigDir
    , getLocalCacheDir
    , getLocalPkgListDir
    , getKnownServers
    , getKnownPackages
    , writeKnownPackages
    , selectValidConfigDir
    ) where

import Prelude hiding (catch)
import Control.Exception (catch, Exception(IOException))
import Control.Monad.Error (mplus, filterM) -- Using Control.Monad.Error to get the Error instance for IO.
import System.Directory (Permissions (..), getPermissions, createDirectoryIfMissing
	                    ,getTemporaryDirectory)
import System.IO.Error (isDoesNotExistError)
import System.IO (hPutStrLn, stderr)
import System.IO.Unsafe

import Distribution.Package (PackageIdentifier)
import Distribution.Version (Dependency)
import System.FilePath ((</>))
import System.Directory

import Network.Hackage.CabalInstall.Types (ConfigFlags (..), OutputGen(..), PkgInfo (..))

import Paths_cabal_install (getDataDir)

-- |Compute the global config directory
-- (eg '/usr/local/share/cabal-install-0.3.0/' on Linux).
getDefaultConfigDir :: IO FilePath
getDefaultConfigDir = getDataDir

-- |Compute the local config directory ('~/.cabal-install' on Linux).
getLocalConfigDir :: IO FilePath
getLocalConfigDir
    = getAppUserDataDirectory "cabal-install"

getLocalCacheDir :: IO FilePath
getLocalCacheDir = getLocalConfigDir

getLocalPkgListDir :: IO FilePath
getLocalPkgListDir = getLocalConfigDir

pkgListFile :: FilePath
pkgListFile = "pkg.list"

servListFile :: FilePath
servListFile = "serv.list"

-- |Name of the packages directory.
packagesDirectoryName :: FilePath
packagesDirectoryName = "packages"

-- | Full path to the server list file
servList :: ConfigFlags -> FilePath
servList cfg = configConfDir cfg </> servListFile

-- | Full path to the packages directory.
packagesDirectory :: ConfigFlags -> FilePath
packagesDirectory cfg = configCacheDir cfg </> packagesDirectoryName

-- | Full path to the package list file
pkgList :: ConfigFlags -> FilePath
pkgList cfg = configPkgListDir cfg </> pkgListFile


-- |Read the list of known packages from the pkg.list file.
getKnownPackages :: ConfigFlags -> IO [PkgInfo]
getKnownPackages cfg
    = fmap read (readFile (pkgList cfg))
        `catch` (\e
                 -> do hPutStrLn stderr ("Warning: Problem opening package list '"
                                          ++ pkgList cfg
                                          ++ "'."
                                        )
                       case e of
                         IOException ioe | isDoesNotExistError ioe ->
                           hPutStrLn stderr "File doesn't exist. Run 'cabal-install update' to create the package list."
                         _ -> hPutStrLn stderr ("Error: " ++ (show e))
                       return [])

-- |Write the list of known packages to the pkg.list file.
writeKnownPackages :: ConfigFlags -> [PkgInfo] -> IO ()
writeKnownPackages cfg pkgs
    = do message (configOutputGen cfg) 2 $
           "creating package file " ++ pkgList cfg
         createDirectoryIfMissing True (configPkgListDir cfg)
         writeFile (pkgList cfg) (show pkgs)

getKnownServers :: ConfigFlags -> IO [String]
getKnownServers cfg
    = fmap read (readFile (servList cfg))
      `mplus` return []

-- |Confirms validity of a config directory by checking the permissions for the package-list file,
--  server-list file and downloaded packages directory.
isValidConfigDir :: FilePath -> IO Bool
isValidConfigDir path
    = do checks <- sequence
                   [ checkFiles readable [ path
                                         , path </> servListFile ]]
         return (and checks)

-- |Picks the first valid config directory or throws an exception if none were found.
selectValidConfigDir :: [FilePath] -> IO FilePath
selectValidConfigDir paths
    = do valids <- filterM isValidConfigDir paths
         case valids of
           []    -> error "No valid config dir found!"
           (x:_) -> return x

checkFiles :: (Permissions -> Bool) -> [FilePath] -> IO Bool
checkFiles check
    = worker True
    where worker r [] = return r
          worker r (x:xs)
              = do permissions <- getPermissions x
                   if check permissions
                      then worker r xs
                      else return False
              `mplus` worker False xs
