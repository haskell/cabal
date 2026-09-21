-- | Tier-0 stable API: loading a cabal project.
--
-- Thin wrapper over the internal @Distribution.Client.ProjectConfig@,
-- @Distribution.Client.DistDirLayout@ and @Distribution.Client.RebuildMonad@.
module Distribution.Client.API.Project
  ( projectLocalCabalFiles -- Note: cabal-add
  ) where

import Control.Exception (throwIO)
import Control.Monad.IO.Class (liftIO)
import Distribution.Client.DistDirLayout (DistDirLayout (..), defaultDistDirLayout)
import Distribution.Client.HttpUtils (configureTransport)
import Distribution.Client.ProjectConfig
  ( ProjectFileParser (..)
  , ProjectPackageLocation (..)
  , findProjectPackages
  , findProjectRoot
  , readProjectConfig
  )
import Distribution.Client.RebuildMonad (runRebuild)
import Distribution.Types.CondTree (ignoreConditions)
import Distribution.Verbosity (Verbosity)
import System.FilePath (splitFileName, (</>))

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
      projectFiles verbosity dirLayout rootDir

projectFiles :: Verbosity -> DistDirLayout -> FilePath -> IO [FilePath]
projectFiles verbosity dirLayout rootDir = runRebuild rootDir do
  httpTransport <- liftIO $ configureTransport verbosity [] Nothing
  skeleton <- readProjectConfig verbosity ParsecParser httpTransport mempty mempty dirLayout
  let (_, projectConfig) = ignoreConditions skeleton
  locations <- findProjectPackages dirLayout projectConfig
  pure [rootDir </> path | Just path <- map toLocalCabalFile locations]

toLocalCabalFile :: ProjectPackageLocation -> Maybe FilePath
toLocalCabalFile = \case
  ProjectPackageLocalCabalFile path -> Just path
  ProjectPackageLocalDirectory _dir path -> Just path
  ProjectPackageLocalTarball _path -> Nothing
  ProjectPackageRemoteTarball _uri -> Nothing
  ProjectPackageRemoteRepo _sourceRepoList -> Nothing
  ProjectPackageNamed _packageVersionConstraint -> Nothing
