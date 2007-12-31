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
import Hackage.Types (ConfigFlags(..), PkgInfo (..), UnresolvedDependency(..))
import Distribution.Simple.Program (ProgramConfiguration)
import Distribution.Simple.Compiler (Compiler)
import Distribution.Package (showPackageId, PackageIdentifier(..))
import Distribution.Version (VersionRange(..), Dependency(..))
import qualified Distribution.Simple.Setup as Cabal

upgrade :: ConfigFlags
        -> Compiler
        -> ProgramConfiguration
        -> Cabal.ConfigFlags
        -> IO ()
upgrade config comp conf configFlags = do 
  upgradable <- getUpgradableDeps config comp conf
  putStrLn "Upgrading the following packages: "
  mapM_ putStrLn [showPackageId (pkgInfoId x) | x <- upgradable]
  install config comp conf configFlags
              [UnresolvedDependency (identifierToDependency $ pkgInfoId x) []
                                  | x <- upgradable]

identifierToDependency :: PackageIdentifier -> Dependency
identifierToDependency (PackageIdentifier name version)
    = Dependency name (ThisVersion version)
