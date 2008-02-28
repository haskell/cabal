-----------------------------------------------------------------------------
-- |
-- Module      :  Hackage.Upgrade
-- Copyright   :  (c) Isaac Potoczny-Jones 2007
-- License     :  BSD-like
--
-- Maintainer  :  ijones@syntaxpolice.org
-- Stability   :  provisional
-- Portability :  portable
--
-- High level interface to package upgrade.  Gets list of installed packages
-- and if there are any newer versions available, upgrades them.

-----------------------------------------------------------------------------
module Hackage.Upgrade
    ( upgrade
    ) where

import qualified Hackage.IndexUtils as IndexUtils
import Hackage.Dependency (getUpgradableDeps)
import Hackage.Install (install)
import Hackage.Types (UnresolvedDependency(..), Repo)
import Distribution.Simple.Program (ProgramConfiguration)
import Distribution.Simple.Compiler (Compiler, PackageDB)
import Distribution.Simple.Configure (getInstalledPackages)
import Distribution.Package (showPackageId, PackageIdentifier(..), Package(..))
import Distribution.Version (VersionRange(..), Dependency(..))
import Distribution.Verbosity (Verbosity)
import qualified Distribution.Simple.Setup as Cabal

import Data.Monoid (Monoid(..))

upgrade :: Verbosity
        -> PackageDB -> [Repo]
        -> Compiler
        -> ProgramConfiguration
        -> Cabal.ConfigFlags
        -> IO ()
upgrade verbosity packageDB repos comp conf configFlags = do 
  Just installed <- getInstalledPackages verbosity comp packageDB conf
  available <- fmap mconcat (mapM (IndexUtils.readRepoIndex verbosity) repos)      
  let upgradable = getUpgradableDeps installed available
  putStrLn "Upgrading the following packages: "
  --FIXME: check if upgradable is null
  mapM_ putStrLn [showPackageId (packageId x) | x <- upgradable]
  install verbosity packageDB repos comp conf configFlags mempty
              [UnresolvedDependency (identifierToDependency $ packageId x) []
                                  | x <- upgradable]

identifierToDependency :: PackageIdentifier -> Dependency
identifierToDependency (PackageIdentifier name version)
    = Dependency name (ThisVersion version)
