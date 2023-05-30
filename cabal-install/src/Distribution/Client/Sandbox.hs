{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.Client.Sandbox
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- UI for the sandboxing functionality.
module Distribution.Client.Sandbox
  ( loadConfigOrSandboxConfig
  , findSavedDistPref
  , updateInstallDirs
  , getPersistOrConfigCompiler
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Client.Config
  ( SavedConfig (..)
  , defaultUserInstall
  , loadConfig
  )
import Distribution.Client.Setup
  ( ConfigFlags (..)
  , GlobalFlags (..)
  , configCompilerAux'
  )

import Distribution.Client.Sandbox.PackageEnvironment
  ( PackageEnvironmentType (..)
  , classifyPackageEnvironment
  , loadUserConfig
  )
import Distribution.Client.SetupWrapper
  ( SetupScriptOptions (..)
  , defaultSetupScriptOptions
  )
import Distribution.Simple.Compiler (Compiler (..))
import Distribution.Simple.Configure
  ( findDistPref
  , findDistPrefOrDefault
  , maybeGetPersistBuildConfig
  )
import qualified Distribution.Simple.LocalBuildInfo as LocalBuildInfo
import Distribution.Simple.Program (ProgramDb)
import Distribution.Simple.Setup
  ( Flag (..)
  , flagToMaybe
  , fromFlagOrDefault
  )
import Distribution.System (Platform)

import System.Directory (getCurrentDirectory)

-- * Basic sandbox functions.

--

updateInstallDirs :: Flag Bool -> SavedConfig -> SavedConfig
updateInstallDirs userInstallFlag savedConfig =
  savedConfig
    { savedConfigureFlags =
        configureFlags
          { configInstallDirs = installDirs
          }
    }
  where
    configureFlags = savedConfigureFlags savedConfig
    userInstallDirs = savedUserInstallDirs savedConfig
    globalInstallDirs = savedGlobalInstallDirs savedConfig
    installDirs
      | userInstall = userInstallDirs
      | otherwise = globalInstallDirs
    userInstall =
      fromFlagOrDefault
        defaultUserInstall
        (configUserInstall configureFlags `mappend` userInstallFlag)

-- | Check which type of package environment we're in and return a
-- correctly-initialised @SavedConfig@ and a @UseSandbox@ value that indicates
-- whether we're working in a sandbox.
loadConfigOrSandboxConfig
  :: Verbosity
  -> GlobalFlags
  -- ^ For @--config-file@ and
  -- @--sandbox-config-file@.
  -> IO SavedConfig
loadConfigOrSandboxConfig verbosity globalFlags = do
  let configFileFlag = globalConfigFile globalFlags

  pkgEnvDir <- getCurrentDirectory
  pkgEnvType <- classifyPackageEnvironment pkgEnvDir
  case pkgEnvType of
    -- Only @cabal.config@ is present.
    UserPackageEnvironment -> do
      config <- loadConfig verbosity configFileFlag
      userConfig <- loadUserConfig verbosity pkgEnvDir Nothing
      let config' = config `mappend` userConfig
      return config'

    -- Neither @cabal.sandbox.config@ nor @cabal.config@ are present.
    AmbientPackageEnvironment -> do
      config <- loadConfig verbosity configFileFlag
      let globalConstraintsOpt =
            flagToMaybe . globalConstraintsFile . savedGlobalFlags $ config
      globalConstraintConfig <-
        loadUserConfig verbosity pkgEnvDir globalConstraintsOpt
      let config' = config `mappend` globalConstraintConfig
      return config'

-- | Return the saved \"dist/\" prefix, or the default prefix.
findSavedDistPref :: SavedConfig -> Flag FilePath -> IO FilePath
findSavedDistPref config flagDistPref = do
  let defDistPref = useDistPref defaultSetupScriptOptions
      flagDistPref' =
        configDistPref (savedConfigureFlags config)
          `mappend` flagDistPref
  findDistPref defDistPref flagDistPref'

-- Utils (transitionary)
--

-- | Try to read the most recently configured compiler from the
-- 'localBuildInfoFile', falling back on 'configCompilerAuxEx' if it
-- cannot be read.
getPersistOrConfigCompiler
  :: ConfigFlags
  -> IO (Compiler, Platform, ProgramDb)
getPersistOrConfigCompiler configFlags = do
  distPref <- findDistPrefOrDefault (configDistPref configFlags)
  mlbi <- maybeGetPersistBuildConfig distPref
  case mlbi of
    Nothing -> do configCompilerAux' configFlags
    Just lbi ->
      return
        ( LocalBuildInfo.compiler lbi
        , LocalBuildInfo.hostPlatform lbi
        , LocalBuildInfo.withPrograms lbi
        )
