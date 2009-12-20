-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Unpack
-- Copyright   :  (c) Andrea Vezzosi 2008
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
--
-----------------------------------------------------------------------------
module Distribution.Client.Unpack (

    -- * Commands
    unpack,

  ) where

import Distribution.Package
         ( PackageId, packageId, Dependency(..) )
import Distribution.Client.PackageIndex as PackageIndex (lookupDependency)
import Distribution.Simple.Setup(fromFlag, fromFlagOrDefault)
import Distribution.Simple.Utils
         ( notice, die )
import Distribution.Verbosity
         ( Verbosity )
import Distribution.Text(display)
import Distribution.Version
         ( anyVersion, intersectVersionRanges )

import Distribution.Client.Setup(UnpackFlags(unpackVerbosity,
                                             unpackDestDir))
import Distribution.Client.Types(UnresolvedDependency(..),
                                 Repo, AvailablePackageSource(..),
                                 AvailablePackage(AvailablePackage),
                                 AvailablePackageDb(AvailablePackageDb))
import Distribution.Client.Fetch(fetchPackage)
import qualified Distribution.Client.Tar as Tar (extractTarGzFile)
import Distribution.Client.IndexUtils as IndexUtils
    (getAvailablePackages, disambiguateDependencies)

import System.Directory
         ( createDirectoryIfMissing, doesDirectoryExist, doesFileExist )
import Control.Monad
         ( unless, when )
import Data.Ord (comparing)
import Data.List(maximumBy)
import System.FilePath
         ( (</>), addTrailingPathSeparator )
import qualified Data.Map as Map

unpack :: UnpackFlags -> [Repo] -> [Dependency] -> IO ()
unpack flags repos deps 
    | null deps = notice verbosity
                  "No packages requested. Nothing to do."
    | otherwise = do
  db@(AvailablePackageDb available _)
            <- getAvailablePackages verbosity repos
  deps' <- fmap (map dependency) 
           . IndexUtils.disambiguateDependencies available 
           . map toUnresolved $ deps

  let pkgs = resolvePackages db deps'

  unless (null prefix) $
         createDirectoryIfMissing True prefix

  flip mapM_ pkgs $ \pkg ->
      case pkg of

        Left (Dependency name ver) -> 
            die $ "There is no available version of " ++ display name 
                  ++ " that satisfies " ++ display ver

        Right (AvailablePackage pkgid _ (RepoTarballPackage repo)) -> do
            pkgPath <- fetchPackage verbosity repo pkgid
            unpackPackage verbosity prefix pkgid pkgPath

        Right (AvailablePackage _ _ LocalUnpackedPackage) -> 
            error "Distribution.Client.Unpack.unpack: the impossible happened."
    where 
      verbosity = fromFlag (unpackVerbosity flags)
      prefix = fromFlagOrDefault "" (unpackDestDir flags)
      toUnresolved d = UnresolvedDependency d []

unpackPackage :: Verbosity -> FilePath -> PackageId -> FilePath -> IO ()
unpackPackage verbosity prefix pkgid pkgPath = do
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

resolvePackages :: AvailablePackageDb
                   -> [Dependency]
                   -> [Either Dependency AvailablePackage]
resolvePackages (AvailablePackageDb available prefs) deps =
    map (\d -> best d (candidates d)) deps
    where
      candidates dep@(Dependency name ver) =
          let [x,y] = map (PackageIndex.lookupDependency available)
                      [ Dependency name
                        (maybe anyVersion id (Map.lookup name prefs)
                         `intersectVersionRanges` ver)
                      , dep ]
          in if null x then y else x
      best d [] = Left d
      best _ xs = Right $ maximumBy (comparing packageId) xs

