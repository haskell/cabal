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

import Distribution.Package ( packageId, Dependency(..) )
import Distribution.Simple.PackageIndex as PackageIndex (lookupDependency)
import Distribution.Simple.Setup(fromFlag, fromFlagOrDefault)
import Distribution.Simple.Utils(info, notice)
import Distribution.Text(display)
import Distribution.Version (VersionRange(..))

import Distribution.Client.Setup(UnpackFlags(unpackVerbosity,
                                             unpackDestDir))
import Distribution.Client.Types(UnresolvedDependency(..),
                                 Repo, AvailablePackageSource(RepoTarballPackage),
                                 AvailablePackage(AvailablePackage),
                                 AvailablePackageDb(AvailablePackageDb))
import Distribution.Client.Fetch(fetchPackage)
import Distribution.Client.Tar(extractTarGzFile)
import Distribution.Client.IndexUtils as IndexUtils
    (getAvailablePackages, disambiguateDependencies)

import System.Directory(createDirectoryIfMissing)
import Control.Monad(unless)
import Data.Ord (comparing)
import Data.List(null, maximumBy)
import System.FilePath((</>))
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
  sequence_
      [ do pkgPath <- fetchPackage verbosity repo pkgid
           let pkgdir = display pkgid
           notice verbosity $ "Unpacking " ++ display pkgid ++ "..."
           info verbosity $ "Extracting " ++ pkgPath
                    ++ " to " ++ prefix </> pkgdir ++ "..."
           extractTarGzFile prefix pkgPath
      | (AvailablePackage pkgid _ (RepoTarballPackage repo)) <- pkgs ]

    where 
      verbosity = fromFlag (unpackVerbosity flags)
      prefix = fromFlagOrDefault "" (unpackDestDir flags)
      toUnresolved d = UnresolvedDependency d []

resolvePackages :: AvailablePackageDb
                   -> [Dependency]
                   -> [AvailablePackage]
resolvePackages (AvailablePackageDb available prefs) deps =
    map (maximumBy (comparing packageId) . candidates) deps
    where
      candidates dep@(Dependency name ver) =
          let [x,y] = map (PackageIndex.lookupDependency available)
                      [ Dependency name
                        (maybe AnyVersion id (Map.lookup name prefs)
                         `IntersectVersionRanges` ver)
                      , dep ]
          in if null x then y else x
