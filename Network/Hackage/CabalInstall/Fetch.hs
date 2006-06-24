-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Hackage.CabalInstall.Fetch
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
--
-----------------------------------------------------------------------------
module Network.Hackage.CabalInstall.Fetch
    (
     -- * Commands
     fetch
    , -- * Utilities
     fetchPackage
    , packageFile
    , packagesDirectory
    , isFetched
    , readURI
    ) where

import Network.URI (URI,parseURI,uriScheme,uriPath)
import Network.HTTP (ConnError(..), Request (..), simpleHTTP
                           , Response(..), RequestMethod (..))

import Control.Monad (filterM)
import Text.Printf (printf)
import System.Directory (doesFileExist, createDirectoryIfMissing)

import Network.Hackage.CabalInstall.Types (ConfigFlags (..), OutputGen (..), UnresolvedDependency (..))
import Network.Hackage.CabalInstall.Config (packagesDirectoryName)
import Network.Hackage.CabalInstall.Dependency (filterFetchables, resolveDependencies)

import Distribution.Package (PackageIdentifier, showPackageId)
import Distribution.Compat.FilePath (joinFileName)
import System.Directory (copyFile)
import Text.ParserCombinators.ReadP (readP_to_S)
import Distribution.ParseUtils (parseDependency)


readURI :: URI -> IO String
readURI uri
    | uriScheme uri == "file:" = (readFile $ uriPath uri)
    | otherwise = do
        eitherResult <- simpleHTTP (Request uri GET [] "")
        case eitherResult of
           Left err -> fail $ printf "Failed to download '%s': %s" (show uri) (show err)
           Right rsp
               | rspCode rsp == (2,0,0) -> return (rspBody rsp)
               | otherwise -> fail $ "Failed to download '" ++ show uri ++ "': Invalid HTTP code: " ++ show (rspCode rsp)

downloadURI :: FilePath -- ^ Where to put it
            -> URI      -- ^ What to download
            -> IO (Maybe ConnError)
downloadURI path uri
    | uriScheme uri == "file:" = do
        copyFile (uriPath uri) path
        return Nothing
    | otherwise = do
        eitherResult <- simpleHTTP request
        case eitherResult of
           Left err -> return (Just err)
           Right rsp
               | rspCode rsp == (2,0,0) -> writeFile path (rspBody rsp) >> return Nothing
               | otherwise -> return (Just (ErrorMisc ("Invalid HTTP code: " ++ show (rspCode rsp))))
    where request = Request uri GET [] ""


downloadFile :: FilePath
             -> String
             -> IO (Maybe ConnError)
downloadFile path url
    = case parseURI url of
        Just parsed -> downloadURI path parsed
        Nothing -> return (Just (ErrorMisc ("Failed to parse url: " ++ show url)))


-- Downloads a package to [config-dir/packages/package-id] and returns the path to the package.
downloadPackage :: ConfigFlags -> PackageIdentifier -> String -> IO String
downloadPackage cfg pkg url
    = do mbError <- downloadFile path url
         case mbError of
           Just err -> fail $ printf "Failed to download '%s': %s" (showPackageId pkg) (show err)
           Nothing -> return path
    where path = configConfPath cfg `joinFileName` packagesDirectoryName `joinFileName` showPackageId pkg

-- |Full path to the packages directory.
packagesDirectory :: ConfigFlags -> FilePath
packagesDirectory cfg = configConfPath cfg `joinFileName` packagesDirectoryName

-- |Generate the full path to a given @PackageIdentifer@.
packageFile :: ConfigFlags -> PackageIdentifier -> FilePath
packageFile cfg pkg = packagesDirectory cfg `joinFileName` (showPackageId pkg)

-- |Returns @True@ if the package has already been fetched.
isFetched :: ConfigFlags -> PackageIdentifier -> IO Bool
isFetched cfg pkg
    = doesFileExist (packageFile cfg pkg)

-- |Fetch a package if we don't have it already.
fetchPackage :: ConfigFlags -> PackageIdentifier -> String -> IO String
fetchPackage cfg pkg location
    = do createDirectoryIfMissing True (packagesDirectory cfg)
         fetched <- isFetched cfg pkg
         if fetched
            then return (packageFile cfg pkg)
            else downloadPackage cfg pkg location

-- |Fetch a list of packages and their dependencies.
fetch :: ConfigFlags -> [String] -> IO ()
fetch cfg pkgs
    = do apkgs <- fmap filterFetchables (resolveDependencies cfg [] (map parseDep pkgs))
         mapM_ (\(pkg,location)
                    -> do downloadingPkg output pkg
                          fetchPackage cfg pkg location
               ) =<< filterM isNotFetched apkgs
    where parseDep dep
              = case readP_to_S parseDependency dep of
                 [] -> error ("Failed to parse package dependency: " ++ show dep)
                 x  -> UnresolvedDependency
                       { dependency = (fst (last x))
                       , depOptions = [] }
          isNotFetched (pkg,_location)
              = do fetched <- isFetched cfg pkg
                   pkgIsPresent output pkg
                   return (not fetched)
          output = configOutputGen cfg
