{-# LANGUAGE LambdaCase #-}

-- | Tier-0 stable API: loading a cabal project.
--
-- Thin wrapper over the internal @Distribution.Client.ProjectConfig@,
-- @Distribution.Client.DistDirLayout@ and
-- @Distribution.Client.ProjectOrchestration@.
module Distribution.Client.API.Project
  ( ProjectContext (..)
  , loadProjectContext
  , projectRootDir
  , projectDistDir
  , projectLocalCabalFiles
  ) where

import Control.Exception (throwIO)
import Control.Monad.IO.Class (liftIO)
import Distribution.Client.DistDirLayout
  ( DistDirLayout (..)
  , ProjectRoot (..)
  , defaultDistDirLayout
  )
import Distribution.Client.HttpUtils (configureTransport)
import Distribution.Client.ProjectConfig
  ( ProjectFileParser (..)
  , ProjectPackageLocation (..)
  , findProjectPackages
  , findProjectRoot
  , readProjectConfig
  )
import Distribution.Client.ProjectOrchestration
  ( CurrentCommand (..)
  , ProjectBaseContext (..)
  , establishProjectBaseContextWithRoot
  )
import Distribution.Client.RebuildMonad (runRebuild)
import Distribution.Types.CondTree (ignoreConditions)
import Distribution.Verbosity (Verbosity)
import System.Directory (canonicalizePath)
import System.FilePath (splitFileName, (</>))

-- | Everything known about a project before solving:
-- the @cabal.project@ contents, the local @.cabal@ files and the
-- @dist-newstyle@ layout.
--
-- TODO: expose selected parts (root, config, local packages) via accessors
-- instead of leaking 'ProjectBaseContext' internals.
newtype ProjectContext = ProjectContext ProjectBaseContext

-- | Load the project rooted at the given directory.
--
-- TODO: support an explicit @cabal.project@ file path override
-- ('ProjectRootExplicit'), mirroring @--project-file@.
loadProjectContext :: Verbosity -> FilePath -> IO ProjectContext
loadProjectContext verbosity rootDir = do
  root <- canonicalizePath rootDir
  projectCtx <-
    establishProjectBaseContextWithRoot
      verbosity
      mempty
      (ProjectRootImplicit root)
      OtherCommand
  pure (ProjectContext projectCtx)

-- | The absolute root directory of the project.
projectRootDir :: ProjectContext -> FilePath
projectRootDir (ProjectContext ctx) =
  distProjectRootDirectory (distDirLayout ctx)

-- | The project-wide build directory (usually @<root>/dist-newstyle@).
projectDistDir :: ProjectContext -> FilePath
projectDistDir (ProjectContext ctx) =
  distDirectory (distDirLayout ctx)

-- | Enumerate the local @.cabal@ files that make up the project given by a
-- @cabal.project@ file (or, more generally, any project file path).
--
-- Only project-local package locations are returned; tarballs, remote
-- repositories and other exotic locations are filtered out. The result paths
-- are absolute.
--
-- This is the stable replacement for third-party tools that used
-- @findProjectRoot@ + @readProjectConfig@ + @findProjectPackages@ directly
-- (see cabal-add).
projectLocalCabalFiles :: Verbosity -> FilePath -> IO [FilePath]
projectLocalCabalFiles verbosity projectFile = do
  let (projectDir, projFile) = splitFileName projectFile
  badOrRoot <- findProjectRoot verbosity (Just projectDir) (Just projFile)
  case badOrRoot of
    Left problem -> throwIO problem
    Right projectRoot -> do
      let dirLayout = defaultDistDirLayout projectRoot Nothing Nothing
          rootDir = distProjectRootDirectory dirLayout
      runRebuild rootDir $ do
        httpTransport <- liftIO $ configureTransport verbosity [] Nothing
        skeleton <-
          readProjectConfig
            verbosity
            ParsecParser
            httpTransport
            mempty
            mempty
            dirLayout
        let projectConfig = snd (ignoreConditions skeleton)
        locations <- findProjectPackages dirLayout projectConfig
        pure [rootDir </> fn | Just fn <- map toLocalCabalFile locations]
  where
    toLocalCabalFile = \case
      ProjectPackageLocalCabalFile fn -> Just fn
      ProjectPackageLocalDirectory _ fn -> Just fn
      _ -> Nothing
