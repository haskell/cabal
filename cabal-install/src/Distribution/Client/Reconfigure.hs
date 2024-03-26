{-# LANGUAGE DataKinds #-}

module Distribution.Client.Reconfigure (Check (..), reconfigure) where

import Distribution.Client.Compat.Prelude

import Data.Monoid (Any (..))
import System.Directory (doesFileExist)

import Distribution.Simple.Configure (localBuildInfoFile)
import Distribution.Simple.Setup (Flag, flagToMaybe, toFlag)
import Distribution.Simple.Utils
  ( defaultPackageDescCwd
  , existsAndIsMoreRecentThan
  , info
  )
import Distribution.Utils.Path

import Distribution.Client.Config (SavedConfig (..))
import Distribution.Client.Configure (readConfigFlags)
import Distribution.Client.Nix (findNixExpr, inNixShell, nixInstantiate)
import Distribution.Client.Sandbox (findSavedDistPref, updateInstallDirs)
import Distribution.Client.Sandbox.PackageEnvironment
  ( userPackageEnvironmentFile
  )
import Distribution.Client.Setup
  ( CommonSetupFlags (..)
  , ConfigExFlags
  , ConfigFlags (..)
  , GlobalFlags (..)
  )

-- | @Check@ represents a function to check some condition on type @a@. The
-- returned 'Any' is 'True' if any part of the condition failed.
newtype Check a = Check
  { runCheck
      :: Any -- Did any previous check fail?
      -> a -- value returned by previous checks
      -> IO (Any, a) -- Did this check fail? What value is returned?
  }

instance Semigroup (Check a) where
  (<>) c d = Check $ \any0 a0 -> do
    (any1, a1) <- runCheck c any0 a0
    (any2, a2) <- runCheck d (any0 <> any1) a1
    return (any0 <> any1 <> any2, a2)

instance Monoid (Check a) where
  mempty = Check $ \_ a -> return (mempty, a)
  mappend = (<>)

-- | Re-configure the package in the current directory if needed. Deciding
-- when to reconfigure and with which options is convoluted:
--
-- If we are reconfiguring, we must always run @configure@ with the
-- verbosity option we are given; however, that a previous configuration
-- uses a different verbosity setting is not reason enough to reconfigure.
--
-- The package should be configured to use the same \"dist\" prefix as
-- given to the @build@ command, otherwise the build will probably
-- fail. Not only does this determine the \"dist\" prefix setting if we
-- need to reconfigure anyway, but an existing configuration should be
-- invalidated if its \"dist\" prefix differs.
--
-- If the package has never been configured (i.e., there is no
-- LocalBuildInfo), we must configure first, using the default options.
--
-- If the package has been configured, there will be a 'LocalBuildInfo'.
-- If there no package description file, we assume that the
-- 'PackageDescription' is up to date, though the configuration may need
-- to be updated for other reasons (see above). If there is a package
-- description file, and it has been modified since the 'LocalBuildInfo'
-- was generated, then we need to reconfigure.
--
-- The caller of this function may also have specific requirements
-- regarding the flags the last configuration used. For example,
-- 'testAction' requires that the package be configured with test suites
-- enabled. The caller may pass the required settings to this function
-- along with a function to check the validity of the saved 'ConfigFlags';
-- these required settings will be checked first upon determining that
-- a previous configuration exists.
reconfigure
  :: ((ConfigFlags, ConfigExFlags) -> [String] -> GlobalFlags -> IO ())
  -- ^ configure action
  -> Verbosity
  -- ^ Verbosity setting
  -> SymbolicPath Pkg (Dir Dist)
  -- ^ \"dist\" prefix
  -> Flag (Maybe Int)
  -- ^ -j flag for reinstalling add-source deps.
  -> Check (ConfigFlags, ConfigExFlags)
  -- ^ Check that the required flags are set.
  -- If they are not set, provide a message explaining the
  -- reason for reconfiguration.
  -> [String]
  -- ^ Extra arguments
  -> GlobalFlags
  -- ^ Global flags
  -> SavedConfig
  -> IO SavedConfig
reconfigure
  configureAction
  verbosity
  dist
  _numJobsFlag
  check
  extraArgs
  globalFlags
  config =
    do
      savedFlags@(_, _) <- readConfigFlags $ getSymbolicPath dist

      useNix <- fmap isJust (findNixExpr globalFlags config)
      alreadyInNixShell <- inNixShell

      if useNix && not alreadyInNixShell
        then do
          -- If we are using Nix, we must reinstantiate the derivation outside
          -- the shell. Eventually, the caller will invoke 'nixShell' which will
          -- rerun cabal inside the shell. That will bring us back to 'reconfigure',
          -- but inside the shell we'll take the second branch, below.

          -- This seems to have a problem: won't 'configureAction' call 'nixShell'
          -- yet again, spawning an infinite tree of subprocesses?
          -- No, because 'nixShell' doesn't spawn a new process if it is already
          -- running in a Nix shell.

          nixInstantiate verbosity (getSymbolicPath dist) False globalFlags config
          return config
        else do
          let checks :: Check (ConfigFlags, ConfigExFlags)
              checks =
                checkVerb
                  <> checkDist
                  <> checkOutdated
                  <> check
          (Any frc, flags@(configFlags, _)) <- runCheck checks mempty savedFlags

          let config' :: SavedConfig
              config' = updateInstallDirs (configUserInstall configFlags) config

          when frc $ configureAction flags extraArgs globalFlags
          return config'
    where
      mbWorkDir = flagToMaybe $ configWorkingDir $ savedConfigureFlags config
      -- Changing the verbosity does not require reconfiguration, but the new
      -- verbosity should be used if reconfiguring.
      checkVerb :: Check (ConfigFlags, b)
      checkVerb = Check $ \_ (configFlags, configExFlags) -> do
        let common = configCommonFlags configFlags
            configFlags' :: ConfigFlags
            configFlags' =
              configFlags
                { configCommonFlags =
                    common{setupVerbosity = toFlag verbosity}
                }
        return (mempty, (configFlags', configExFlags))

      -- Reconfiguration is required if @--build-dir@ changes.
      checkDist :: Check (ConfigFlags, b)
      checkDist = Check $ \_ (configFlags, configExFlags) -> do
        -- Always set the chosen @--build-dir@ before saving the flags,
        -- or bad things could happen.
        let common = configCommonFlags configFlags
        savedDist <- findSavedDistPref config (setupDistPref common)
        let distChanged :: Bool
            distChanged = dist /= savedDist
        when distChanged $ info verbosity "build directory changed"
        let configFlags' :: ConfigFlags
            configFlags' =
              configFlags
                { configCommonFlags =
                    common{setupDistPref = toFlag dist}
                }
        return (Any distChanged, (configFlags', configExFlags))

      checkOutdated :: Check (ConfigFlags, b)
      checkOutdated = Check $ \_ flags@(configFlags, _) -> do
        let common = configCommonFlags configFlags
            buildConfig, userCabalConfig :: FilePath
            buildConfig = interpretSymbolicPath mbWorkDir $ localBuildInfoFile dist
            userCabalConfig = userPackageEnvironmentFile

        -- Has the package ever been configured? If not, reconfiguration is
        -- required.
        configured <- doesFileExist buildConfig
        unless configured $ info verbosity "package has never been configured"

        -- Is the @cabal.config@ file newer than @dist/setup.config@? Then we need
        -- to force reconfigure. Note that it's possible to use @cabal.config@
        -- even without sandboxes.
        userPackageEnvironmentFileModified <-
          existsAndIsMoreRecentThan userCabalConfig buildConfig
        when userPackageEnvironmentFileModified $
          info
            verbosity
            ( "user package environment file ('"
                ++ userPackageEnvironmentFile
                ++ "') was modified"
            )

        -- Is the configuration older than the package description?
        descrFile <-
          maybe
            (relativeSymbolicPath <$> defaultPackageDescCwd verbosity)
            return
            (flagToMaybe (setupCabalFilePath common))
        let descrPath = interpretSymbolicPath mbWorkDir descrFile
        outdated <- existsAndIsMoreRecentThan descrPath buildConfig
        when outdated $ info verbosity (getSymbolicPath descrFile ++ " was changed")

        let failed :: Any
            failed =
              Any outdated
                <> Any userPackageEnvironmentFileModified
                <> Any (not configured)
        return (failed, flags)
