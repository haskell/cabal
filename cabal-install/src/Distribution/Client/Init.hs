-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.Client.Init
-- Copyright   :  (c) Brent Yorgey 2009
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Implementation of the 'cabal init' command, which creates an initial .cabal
-- file for a project.
module Distribution.Client.Init (initCmd) where

import Distribution.Client.IndexUtils
import Distribution.Client.Init.FileCreators
import qualified Distribution.Client.Init.Interactive.Command as Interactive
import qualified Distribution.Client.Init.NonInteractive.Command as NonInteractive
import qualified Distribution.Client.Init.Simple as Simple
import Distribution.Client.Init.Types
import Distribution.Client.Setup (RepoContext)
import Distribution.Simple.Compiler
import Distribution.Simple.Program (ProgramDb)
import Distribution.Simple.Setup
import Distribution.Verbosity
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)

-- | This is the main driver for the init script.
initCmd
  :: Verbosity
  -> PackageDBStack
  -> RepoContext
  -> Compiler
  -> ProgramDb
  -> InitFlags
  -> IO ()
initCmd v packageDBs repoCtxt comp progdb initFlags = do
  installedPkgIndex <- getInstalledPackages v comp packageDBs progdb
  sourcePkgDb <- getSourcePackages v repoCtxt
  hSetBuffering stdout NoBuffering
  settings <- createProject v installedPkgIndex sourcePkgDb initFlags
  writeProject settings
  where
    -- When no flag is set, default to interactive.
    --
    -- When `--interactive` is set, if we also set `--simple`,
    -- then we interactive generate a simple project with sensible defaults.
    --
    -- If `--simple` is not set, default to interactive. When the flag
    -- is explicitly set to `--non-interactive`, then we choose non-interactive.
    --
    createProject
      | fromFlagOrDefault False (simpleProject initFlags) =
          Simple.createProject
      | otherwise = case interactive initFlags of
          Flag False -> NonInteractive.createProject comp
          _ -> Interactive.createProject
