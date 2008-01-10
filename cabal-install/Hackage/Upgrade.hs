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
import Hackage.Dependency (getUpgradableDeps)
import Hackage.Install (install)
import Hackage.Types (PkgInfo (..), UnresolvedDependency(..), Repo)
import Distribution.Simple.Program (ProgramConfiguration)
import Distribution.Simple.Compiler (Compiler, PackageDB)
import Distribution.Package (showPackageId, PackageIdentifier(..))
import Distribution.Version (VersionRange(..), Dependency(..))
import Distribution.Verbosity (Verbosity)
import qualified Distribution.Simple.Setup as Cabal

upgrade :: Verbosity
        -> PackageDB -> [Repo]
        -> Compiler
        -> ProgramConfiguration
        -> Cabal.ConfigFlags
        -> IO ()
upgrade verbosity packagedb repos comp conf configFlags = do 
  upgradable <- getUpgradableDeps verbosity packagedb repos comp conf
  putStrLn "Upgrading the following packages: "
  --FIXME: check if upgradable is null
  mapM_ putStrLn [showPackageId (pkgInfoId x) | x <- upgradable]
  install verbosity packagedb repos comp conf configFlags
              [UnresolvedDependency (identifierToDependency $ pkgInfoId x) []
                                  | x <- upgradable]

identifierToDependency :: PackageIdentifier -> Dependency
identifierToDependency (PackageIdentifier name version)
    = Dependency name (ThisVersion version)
