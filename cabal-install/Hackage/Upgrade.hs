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
import Hackage.Dependency (upgradableDependencies)
import Hackage.Install (install)
import Hackage.Types (UnresolvedDependency(..), Repo)
import Hackage.Setup (InstallFlags(..))

import Distribution.Simple.Program (ProgramConfiguration)
import Distribution.Simple.Compiler (Compiler, PackageDB)
import Distribution.Simple.Configure (getInstalledPackages)
import Distribution.Verbosity (Verbosity)
import qualified Distribution.Simple.Setup as Cabal

import Data.Monoid (Monoid(..))

upgrade :: Verbosity
        -> PackageDB -> [Repo]
        -> Compiler
        -> ProgramConfiguration
        -> Cabal.ConfigFlags
        -> InstallFlags
        -> IO ()
upgrade verbosity packageDB repos comp conf configFlags installFlags = do
  Just installed <- getInstalledPackages verbosity comp packageDB conf
  available <- fmap mconcat (mapM (IndexUtils.readRepoIndex verbosity) repos)      
  let upgradable = upgradableDependencies installed available
  install verbosity packageDB repos comp conf configFlags installFlags
    [ UnresolvedDependency dep [] | dep <- upgradable ]
