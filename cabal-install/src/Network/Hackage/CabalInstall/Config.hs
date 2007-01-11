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
    , defaultConfDir
    , defaultCacheDir
    , defaultPkgListDir
    , getKnownServers
    , getKnownPackages
    , writeKnownPackages
    , selectValidConfigDir
    ) where

import Prelude hiding (catch)
import Control.Exception (catch, Exception(IOException))
import Control.Monad.Error (mplus, filterM) -- Using Control.Monad.Error to get the Error instance for IO.
import System.Directory (Permissions (..), getPermissions, createDirectoryIfMissing)
import System.IO.Error (isDoesNotExistError)
import System.IO (hPutStrLn, stderr)

import Distribution.Package (PackageIdentifier)
import Distribution.Version (Dependency)
import Distribution.Compat.FilePath (joinFileName)

import Network.Hackage.CabalInstall.Types (ConfigFlags (..), PkgInfo (..))

-- FIXME: should be different on Windows
defaultConfDir :: FilePath
defaultConfDir = "/etc/cabal-install"

-- FIXME: should be different on Windows
defaultCacheDir :: FilePath
defaultCacheDir = "/var/cache/cabal-install"

-- FIXME: should be different on Windows
defaultPkgListDir :: FilePath
defaultPkgListDir = "/var/lib/cabal-install"

pkgListFile :: FilePath
pkgListFile = "pkg.list"

servListFile :: FilePath
servListFile = "serv.list"

-- |Name of the packages directory.
packagesDirectoryName :: FilePath
packagesDirectoryName = "packages"

-- | Full path to the server list file
servList :: ConfigFlags -> FilePath
servList cfg = configConfDir cfg `joinFileName` servListFile

-- | Full path to the packages directory.
packagesDirectory :: ConfigFlags -> FilePath
packagesDirectory cfg = configCacheDir cfg `joinFileName` packagesDirectoryName

-- | Full path to the package list file
pkgList :: ConfigFlags -> FilePath
pkgList cfg = configPkgListDir cfg `joinFileName` pkgListFile


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
    = do createDirectoryIfMissing True (configPkgListDir cfg)
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
                                         , path `joinFileName` servListFile ]]
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
