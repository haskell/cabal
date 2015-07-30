{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Entry point to the default cabal-install front-end.
-----------------------------------------------------------------------------

module Main (main) where

import Distribution.Client.Setup
         ( GlobalFlags(..), globalCommand, globalRepos
         , ConfigFlags(..)
         , ConfigExFlags(..), defaultConfigExFlags, configureExCommand
         , BuildFlags(..), BuildExFlags(..), SkipAddSourceDepsCheck(..)
         , buildCommand, replCommand, testCommand, benchmarkCommand
         , InstallFlags(..), defaultInstallFlags
         , installCommand, upgradeCommand, uninstallCommand
         , FetchFlags(..), fetchCommand
         , FreezeFlags(..), freezeCommand
         , GetFlags(..), getCommand, unpackCommand
         , checkCommand
         , formatCommand
         , updateCommand
         , ListFlags(..), listCommand
         , InfoFlags(..), infoCommand
         , UploadFlags(..), uploadCommand
         , ReportFlags(..), reportCommand
         , runCommand
         , InitFlags(initVerbosity), initCommand
         , SDistFlags(..), SDistExFlags(..), sdistCommand
         , Win32SelfUpgradeFlags(..), win32SelfUpgradeCommand
         , ActAsSetupFlags(..), actAsSetupCommand
         , SandboxFlags(..), sandboxCommand
         , ExecFlags(..), execCommand
         , UserConfigFlags(..), userConfigCommand
         , reportCommand
         )
import Distribution.Simple.Setup
         ( HaddockFlags(..), haddockCommand, defaultHaddockFlags
         , HscolourFlags(..), hscolourCommand
         , ReplFlags(..)
         , CopyFlags(..), copyCommand
         , RegisterFlags(..), registerCommand
         , CleanFlags(..), cleanCommand
         , TestFlags(..), BenchmarkFlags(..)
         , Flag(..), fromFlag, fromFlagOrDefault, flagToMaybe, toFlag
         , configAbsolutePaths
         )

import Distribution.Client.SetupWrapper
         ( setupWrapper, SetupScriptOptions(..), defaultSetupScriptOptions )
import Distribution.Client.Config
         ( SavedConfig(..), loadConfig, defaultConfigFile, userConfigDiff
         , userConfigUpdate )
import Distribution.Client.Targets
         ( readUserTargets )
import qualified Distribution.Client.List as List
         ( list, info )

import Distribution.Client.Install            (install)
import Distribution.Client.Configure          (configure)
import Distribution.Client.Update             (update)
import Distribution.Client.Exec               (exec)
import Distribution.Client.Fetch              (fetch)
import Distribution.Client.Freeze             (freeze)
import Distribution.Client.Check as Check     (check)
--import Distribution.Client.Clean            (clean)
import Distribution.Client.Upload as Upload   (upload, check, report)
import Distribution.Client.Run                (run, splitRunArgs)
import Distribution.Client.HttpUtils          (configureTransport)
import Distribution.Client.SrcDist            (sdist)
import Distribution.Client.Get                (get)
import Distribution.Client.Sandbox            (sandboxInit
                                              ,sandboxAddSource
                                              ,sandboxDelete
                                              ,sandboxDeleteSource
                                              ,sandboxListSources
                                              ,sandboxHcPkg
                                              ,dumpPackageEnvironment

                                              ,getSandboxConfigFilePath
                                              ,loadConfigOrSandboxConfig
                                              ,findSavedDistPref
                                              ,initPackageDBIfNeeded
                                              ,maybeWithSandboxDirOnSearchPath
                                              ,maybeWithSandboxPackageInfo
                                              ,WereDepsReinstalled(..)
                                              ,maybeReinstallAddSourceDeps
                                              ,tryGetIndexFilePath
                                              ,sandboxBuildDir
                                              ,updateSandboxConfigFileFlag
                                              ,updateInstallDirs

                                              ,configCompilerAux'
                                              ,configPackageDB')
import Distribution.Client.Sandbox.PackageEnvironment
                                              (setPackageDB
                                              ,userPackageEnvironmentFile)
import Distribution.Client.Sandbox.Timestamp  (maybeAddCompilerTimestampRecord)
import Distribution.Client.Sandbox.Types      (UseSandbox(..), whenUsingSandbox)
import Distribution.Client.Types              (Password (..))
import Distribution.Client.Init               (initCabal)
import qualified Distribution.Client.Win32SelfUpgrade as Win32SelfUpgrade
import Distribution.Client.Utils              (determineNumJobs
#if defined(mingw32_HOST_OS)
                                              ,relaxEncodingErrors
#endif
                                              ,existsAndIsMoreRecentThan)

import Distribution.PackageDescription
         ( BuildType(..), Executable(..), benchmarkName, benchmarkBuildInfo
         , testName, testBuildInfo, buildable )
import Distribution.PackageDescription.Parse
         ( readPackageDescription )
import Distribution.PackageDescription.PrettyPrint
         ( writeGenericPackageDescription )
import qualified Distribution.Simple as Simple
import qualified Distribution.Make as Make
import Distribution.Simple.Build
         ( startInterpreter )
import Distribution.Simple.Command
         ( CommandParse(..), CommandUI(..), Command
         , commandsRun, commandAddAction, hiddenCommand )
import Distribution.Simple.Compiler
         ( Compiler(..) )
import Distribution.Simple.Configure
         ( checkPersistBuildConfigOutdated, configCompilerAuxEx
         , ConfigStateFileError(..), localBuildInfoFile
         , getPersistBuildConfig, tryGetPersistBuildConfig )
import qualified Distribution.Simple.LocalBuildInfo as LBI
import Distribution.Simple.Program (defaultProgramConfiguration
                                   ,configureAllKnownPrograms
                                   ,simpleProgramInvocation
                                   ,getProgramInvocationOutput)
import Distribution.Simple.Program.Db (reconfigurePrograms)
import qualified Distribution.Simple.Setup as Cabal
import Distribution.Simple.Utils
         ( cabalVersion, die, notice, info, topHandler
         , findPackageDesc, tryFindPackageDesc )
import Distribution.Text
         ( display )
import Distribution.Verbosity as Verbosity
         ( Verbosity, normal )
import Distribution.Version
         ( Version(..), orLaterVersion )
import qualified Paths_cabal_install (version)

import System.Environment       (getArgs, getProgName)
import System.Exit              (exitFailure)
import System.FilePath          (splitExtension, takeExtension)
import System.IO                ( BufferMode(LineBuffering), hSetBuffering
#ifdef mingw32_HOST_OS
                                , stderr
#endif
                                , stdout )
import System.Directory         (doesFileExist, getCurrentDirectory)
import Data.List                (intercalate)
import Data.Maybe               (mapMaybe)
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid              (Monoid(..))
import Control.Applicative      (pure, (<$>))
#endif
import Control.Monad            (when, unless)

-- | Entry point
--
main :: IO ()
main = do
  -- Enable line buffering so that we can get fast feedback even when piped.
  -- This is especially important for CI and build systems.
  hSetBuffering stdout LineBuffering
  -- The default locale encoding for Windows CLI is not UTF-8 and printing
  -- Unicode characters to it will fail unless we relax the handling of encoding
  -- errors when writing to stderr and stdout.
#ifdef mingw32_HOST_OS
  relaxEncodingErrors stdout
  relaxEncodingErrors stderr
#endif
  getArgs >>= mainWorker

mainWorker :: [String] -> IO ()
mainWorker args = topHandler $
  case commandsRun (globalCommand commands) commands args of
    CommandHelp   help                 -> printGlobalHelp help
    CommandList   opts                 -> printOptionsList opts
    CommandErrors errs                 -> printErrors errs
    CommandReadyToGo (globalFlags, commandParse)  ->
      case commandParse of
        _ | fromFlagOrDefault False (globalVersion globalFlags)
            -> printVersion
          | fromFlagOrDefault False (globalNumericVersion globalFlags)
            -> printNumericVersion
        CommandHelp     help           -> printCommandHelp help
        CommandList     opts           -> printOptionsList opts
        CommandErrors   errs           -> printErrors errs
        CommandReadyToGo action        -> do
          globalFlags' <- updateSandboxConfigFileFlag globalFlags
          action globalFlags'

  where
    printCommandHelp help = do
      pname <- getProgName
      putStr (help pname)
    printGlobalHelp help = do
      pname <- getProgName
      configFile <- defaultConfigFile
      putStr (help pname)
      putStr $ "\nYou can edit the cabal configuration file to set defaults:\n"
            ++ "  " ++ configFile ++ "\n"
      exists <- doesFileExist configFile
      when (not exists) $
          putStrLn $ "This file will be generated with sensible "
                  ++ "defaults if you run 'cabal update'."
    printOptionsList = putStr . unlines
    printErrors errs = die $ intercalate "\n" errs
    printNumericVersion = putStrLn $ display Paths_cabal_install.version
    printVersion        = putStrLn $ "cabal-install version "
                                  ++ display Paths_cabal_install.version
                                  ++ "\nusing version "
                                  ++ display cabalVersion
                                  ++ " of the Cabal library "

    commands =
      [installCommand         `commandAddAction` installAction
      ,updateCommand          `commandAddAction` updateAction
      ,listCommand            `commandAddAction` listAction
      ,infoCommand            `commandAddAction` infoAction
      ,fetchCommand           `commandAddAction` fetchAction
      ,freezeCommand          `commandAddAction` freezeAction
      ,getCommand             `commandAddAction` getAction
      ,hiddenCommand $
       unpackCommand          `commandAddAction` unpackAction
      ,checkCommand           `commandAddAction` checkAction
      ,sdistCommand           `commandAddAction` sdistAction
      ,uploadCommand          `commandAddAction` uploadAction
      ,reportCommand          `commandAddAction` reportAction
      ,runCommand             `commandAddAction` runAction
      ,initCommand            `commandAddAction` initAction
      ,configureExCommand     `commandAddAction` configureAction
      ,buildCommand           `commandAddAction` buildAction
      ,replCommand            `commandAddAction` replAction
      ,sandboxCommand         `commandAddAction` sandboxAction
      ,haddockCommand         `commandAddAction` haddockAction
      ,execCommand            `commandAddAction` execAction
      ,userConfigCommand      `commandAddAction` userConfigAction
      ,cleanCommand           `commandAddAction` cleanAction
      ,wrapperAction copyCommand
                     copyVerbosity     copyDistPref
      ,wrapperAction hscolourCommand
                     hscolourVerbosity hscolourDistPref
      ,wrapperAction registerCommand
                     regVerbosity      regDistPref
      ,testCommand            `commandAddAction` testAction
      ,benchmarkCommand       `commandAddAction` benchmarkAction
      ,hiddenCommand $
       uninstallCommand       `commandAddAction` uninstallAction
      ,hiddenCommand $
       formatCommand          `commandAddAction` formatAction
      ,hiddenCommand $
       upgradeCommand         `commandAddAction` upgradeAction
      ,hiddenCommand $
       win32SelfUpgradeCommand`commandAddAction` win32SelfUpgradeAction
      ,hiddenCommand $
       actAsSetupCommand`commandAddAction` actAsSetupAction
      ]

wrapperAction :: Monoid flags
              => CommandUI flags
              -> (flags -> Flag Verbosity)
              -> (flags -> Flag String)
              -> Command (GlobalFlags -> IO ())
wrapperAction command verbosityFlag distPrefFlag =
  commandAddAction command
    { commandDefaultFlags = mempty } $ \flags extraArgs globalFlags -> do
    let verbosity = fromFlagOrDefault normal (verbosityFlag flags)
    (_, config) <- loadConfigOrSandboxConfig verbosity globalFlags
    distPref <- findSavedDistPref config (distPrefFlag flags)
    let setupScriptOptions = defaultSetupScriptOptions { useDistPref = distPref }
    setupWrapper verbosity setupScriptOptions Nothing
                 command (const flags) extraArgs

configureAction :: (ConfigFlags, ConfigExFlags)
                -> [String] -> GlobalFlags -> IO ()
configureAction (configFlags, configExFlags) extraArgs globalFlags = do
  let verbosity = fromFlagOrDefault normal (configVerbosity configFlags)

  (useSandbox, config) <- fmap
                          (updateInstallDirs (configUserInstall configFlags))
                          (loadConfigOrSandboxConfig verbosity globalFlags)
  let configFlags'   = savedConfigureFlags   config `mappend` configFlags
      configExFlags' = savedConfigureExFlags config `mappend` configExFlags
      globalFlags'   = savedGlobalFlags      config `mappend` globalFlags
  (comp, platform, conf) <- configCompilerAuxEx configFlags'

  -- If we're working inside a sandbox and the user has set the -w option, we
  -- may need to create a sandbox-local package DB for this compiler and add a
  -- timestamp record for this compiler to the timestamp file.
  let configFlags''  = case useSandbox of
        NoSandbox               -> configFlags'
        (UseSandbox sandboxDir) -> setPackageDB sandboxDir
                                   comp platform configFlags' conf

  whenUsingSandbox useSandbox $ \sandboxDir -> do
    initPackageDBIfNeeded verbosity configFlags'' comp conf
    -- NOTE: We do not write the new sandbox package DB location to
    -- 'cabal.sandbox.config' here because 'configure -w' must not affect
    -- subsequent 'install' (for UI compatibility with non-sandboxed mode).

    indexFile     <- tryGetIndexFilePath config
    maybeAddCompilerTimestampRecord verbosity sandboxDir indexFile
      (compilerId comp) platform

  maybeWithSandboxDirOnSearchPath useSandbox $
    configure verbosity
              (configPackageDB' configFlags'')
              (globalRepos globalFlags')
              comp platform conf configFlags'' configExFlags' extraArgs

buildAction :: (BuildFlags, BuildExFlags) -> [String] -> GlobalFlags -> IO ()
buildAction (buildFlags, buildExFlags) extraArgs globalFlags = do
  let verbosity   = fromFlagOrDefault normal (buildVerbosity buildFlags)
      noAddSource = fromFlagOrDefault DontSkipAddSourceDepsCheck
                    (buildOnly buildExFlags)

  -- Calls 'configureAction' to do the real work, so nothing special has to be
  -- done to support sandboxes.
  (useSandbox, config, distPref) <- reconfigure verbosity
                                    (buildDistPref buildFlags)
                                    mempty [] globalFlags noAddSource
                                    (buildNumJobs buildFlags) (const Nothing)

  maybeWithSandboxDirOnSearchPath useSandbox $
    build verbosity config distPref buildFlags extraArgs


-- | Actually do the work of building the package. This is separate from
-- 'buildAction' so that 'testAction' and 'benchmarkAction' do not invoke
-- 'reconfigure' twice.
build :: Verbosity -> SavedConfig -> FilePath -> BuildFlags -> [String] -> IO ()
build verbosity config distPref buildFlags extraArgs =
  setupWrapper verbosity setupOptions Nothing
               (Cabal.buildCommand progConf) mkBuildFlags extraArgs
  where
    progConf     = defaultProgramConfiguration
    setupOptions = defaultSetupScriptOptions { useDistPref = distPref }

    mkBuildFlags version = filterBuildFlags version config buildFlags'
    buildFlags' = buildFlags
      { buildVerbosity = toFlag verbosity
      , buildDistPref  = toFlag distPref
      }

-- | Make sure that we don't pass new flags to setup scripts compiled against
-- old versions of Cabal.
filterBuildFlags :: Version -> SavedConfig -> BuildFlags -> BuildFlags
filterBuildFlags version config buildFlags
  | version >= Version [1,19,1] [] = buildFlags_latest
  -- Cabal < 1.19.1 doesn't support 'build -j'.
  | otherwise                      = buildFlags_pre_1_19_1
  where
    buildFlags_pre_1_19_1 = buildFlags {
      buildNumJobs = NoFlag
      }
    buildFlags_latest     = buildFlags {
      -- Take the 'jobs' setting '~/.cabal/config' into account.
      buildNumJobs = Flag . Just . determineNumJobs $
                     (numJobsConfigFlag `mappend` numJobsCmdLineFlag)
      }
    numJobsConfigFlag  = installNumJobs . savedInstallFlags $ config
    numJobsCmdLineFlag = buildNumJobs buildFlags


replAction :: (ReplFlags, BuildExFlags) -> [String] -> GlobalFlags -> IO ()
replAction (replFlags, buildExFlags) extraArgs globalFlags = do
  cwd     <- getCurrentDirectory
  pkgDesc <- findPackageDesc cwd
  either (const onNoPkgDesc) (const onPkgDesc) pkgDesc
  where
    verbosity = fromFlagOrDefault normal (replVerbosity replFlags)

    -- There is a .cabal file in the current directory: start a REPL and load
    -- the project's modules.
    onPkgDesc = do
      let noAddSource = case replReload replFlags of
            Flag True -> SkipAddSourceDepsCheck
            _         -> fromFlagOrDefault DontSkipAddSourceDepsCheck
                         (buildOnly buildExFlags)
      -- Calls 'configureAction' to do the real work, so nothing special has to
      -- be done to support sandboxes.
      (useSandbox, _config, distPref) <-
        reconfigure verbosity (replDistPref replFlags)
                    mempty [] globalFlags noAddSource NoFlag
                    (const Nothing)
      let progConf     = defaultProgramConfiguration
          setupOptions = defaultSetupScriptOptions
            { useCabalVersion = orLaterVersion $ Version [1,18,0] []
            , useDistPref     = distPref
            }
          replFlags'   = replFlags
            { replVerbosity = toFlag verbosity
            , replDistPref  = toFlag distPref
            }

      maybeWithSandboxDirOnSearchPath useSandbox $
        setupWrapper verbosity setupOptions Nothing
        (Cabal.replCommand progConf) (const replFlags') extraArgs

    -- No .cabal file in the current directory: just start the REPL (possibly
    -- using the sandbox package DB).
    onNoPkgDesc = do
      (_useSandbox, config) <- loadConfigOrSandboxConfig verbosity globalFlags
      let configFlags = savedConfigureFlags config
      (comp, _platform, programDb) <- configCompilerAux' configFlags
      programDb' <- reconfigurePrograms verbosity
                                        (replProgramPaths replFlags)
                                        (replProgramArgs replFlags)
                                        programDb
      startInterpreter verbosity programDb' comp (configPackageDB' configFlags)

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
reconfigure :: Verbosity    -- ^ Verbosity setting
            -> Flag FilePath  -- ^ \"dist\" prefix
            -> ConfigFlags  -- ^ Additional config flags to set. These flags
                            -- will be 'mappend'ed to the last used or
                            -- default 'ConfigFlags' as appropriate, so
                            -- this value should be 'mempty' with only the
                            -- required flags set. The required verbosity
                            -- and \"dist\" prefix flags will be set
                            -- automatically because they are always
                            -- required; therefore, it is not necessary to
                            -- set them here.
            -> [String]     -- ^ Extra arguments
            -> GlobalFlags  -- ^ Global flags
            -> SkipAddSourceDepsCheck
                            -- ^ Should we skip the timestamp check for modified
                            -- add-source dependencies?
            -> Flag (Maybe Int)
                            -- ^ -j flag for reinstalling add-source deps.
            -> (ConfigFlags -> Maybe String)
                            -- ^ Check that the required flags are set in
                            -- the last used 'ConfigFlags'. If the required
                            -- flags are not set, provide a message to the
                            -- user explaining the reason for
                            -- reconfiguration. Because the correct \"dist\"
                            -- prefix setting is always required, it is checked
                            -- automatically; this function need not check
                            -- for it.
            -> IO (UseSandbox, SavedConfig, FilePath)
reconfigure verbosity flagDistPref addConfigFlags extraArgs globalFlags
            skipAddSourceDepsCheck numJobsFlag    checkFlags = do
  (useSandbox, config) <- loadConfigOrSandboxConfig verbosity globalFlags
  distPref <- findSavedDistPref config flagDistPref
  eLbi <- tryGetPersistBuildConfig distPref
  config' <- case eLbi of
    Left err  -> onNoBuildConfig (useSandbox, config) distPref err
    Right lbi -> onBuildConfig (useSandbox, config) distPref lbi
  return (useSandbox, config', distPref)

  where

    -- We couldn't load the saved package config file.
    --
    -- If we're in a sandbox: add-source deps don't have to be reinstalled
    -- (since we don't know the compiler & platform).
    onNoBuildConfig :: (UseSandbox, SavedConfig) -> FilePath
                    -> ConfigStateFileError -> IO SavedConfig
    onNoBuildConfig (_, config) distPref err = do
      let msg = case err of
            ConfigStateFileMissing -> "Package has never been configured."
            ConfigStateFileNoParse -> "Saved package config file seems "
                                      ++ "to be corrupt."
            _ -> show err
      case err of
        ConfigStateFileBadVersion _ _ _ -> info verbosity msg
        _                               -> do
          let distVerbFlags = mempty
                { configVerbosity = toFlag verbosity
                , configDistPref  = toFlag distPref
                }
              defaultFlags = mappend addConfigFlags distVerbFlags
          notice verbosity
            $ msg ++ " Configuring with default flags." ++ configureManually
          configureAction (defaultFlags, defaultConfigExFlags)
            extraArgs globalFlags
      return config

    -- Package has been configured, but the configuration may be out of
    -- date or required flags may not be set.
    --
    -- If we're in a sandbox: reinstall the modified add-source deps and
    -- force reconfigure if we did.
    onBuildConfig :: (UseSandbox, SavedConfig) -> FilePath
                  -> LBI.LocalBuildInfo -> IO SavedConfig
    onBuildConfig (useSandbox, config) distPref lbi = do
      let configFlags = LBI.configFlags lbi
          distVerbFlags = mempty
            { configVerbosity = toFlag verbosity
            , configDistPref  = toFlag distPref
            }
          flags       = mconcat [configFlags, addConfigFlags, distVerbFlags]

      -- Was the sandbox created after the package was already configured? We
      -- may need to skip reinstallation of add-source deps and force
      -- reconfigure.
      let buildConfig       = localBuildInfoFile distPref
      sandboxConfig        <- getSandboxConfigFilePath globalFlags
      isSandboxConfigNewer <-
        sandboxConfig `existsAndIsMoreRecentThan` buildConfig

      let skipAddSourceDepsCheck'
            | isSandboxConfigNewer = SkipAddSourceDepsCheck
            | otherwise            = skipAddSourceDepsCheck

      when (skipAddSourceDepsCheck' == SkipAddSourceDepsCheck) $
        info verbosity "Skipping add-source deps check..."

      let (_, config') = updateInstallDirs
                         (configUserInstall flags)
                         (useSandbox, config)

      depsReinstalled <-
        case skipAddSourceDepsCheck' of
          DontSkipAddSourceDepsCheck ->
            maybeReinstallAddSourceDeps
              verbosity numJobsFlag flags globalFlags
              (useSandbox, config')
          SkipAddSourceDepsCheck -> do
            return NoDepsReinstalled

      -- Is the @cabal.config@ file newer than @dist/setup.config@? Then we need
      -- to force reconfigure. Note that it's possible to use @cabal.config@
      -- even without sandboxes.
      isUserPackageEnvironmentFileNewer <-
        userPackageEnvironmentFile `existsAndIsMoreRecentThan` buildConfig

      -- Determine whether we need to reconfigure and which message to show to
      -- the user if that is the case.
      mMsg <- determineMessageToShow distPref lbi configFlags
                                     depsReinstalled isSandboxConfigNewer
                                     isUserPackageEnvironmentFileNewer
      case mMsg of

        -- No message for the user indicates that reconfiguration
        -- is not required.
        Nothing -> return config'

        -- Show the message and reconfigure.
        Just msg -> do
          notice verbosity msg
          configureAction (flags, defaultConfigExFlags)
            extraArgs globalFlags
          return config'

    -- Determine what message, if any, to display to the user if reconfiguration
    -- is required.
    determineMessageToShow :: FilePath -> LBI.LocalBuildInfo -> ConfigFlags
                            -> WereDepsReinstalled -> Bool -> Bool
                            -> IO (Maybe String)
    determineMessageToShow _ _   _           _               True  _     =
      -- The sandbox was created after the package was already configured.
      return $! Just $! sandboxConfigNewerMessage

    determineMessageToShow _ _   _           _               False True  =
      -- The user package environment file was modified.
      return $! Just $! userPackageEnvironmentFileModifiedMessage

    determineMessageToShow distPref lbi configFlags depsReinstalled
                           False False = do
      let savedDistPref = fromFlagOrDefault
                          (useDistPref defaultSetupScriptOptions)
                          (configDistPref configFlags)
      case depsReinstalled of
        ReinstalledSomeDeps ->
          -- Some add-source deps were reinstalled.
          return $! Just $! reinstalledDepsMessage
        NoDepsReinstalled ->
          case checkFlags configFlags of
            -- Flag required by the caller is not set.
            Just msg -> return $! Just $! msg ++ configureManually

            Nothing
              -- Required "dist" prefix is not set.
              | savedDistPref /= distPref ->
                return $! Just distPrefMessage

              -- All required flags are set, but the configuration
              -- may be outdated.
              | otherwise -> case LBI.pkgDescrFile lbi of
                Nothing -> return Nothing
                Just pdFile -> do
                  outdated <- checkPersistBuildConfigOutdated
                              distPref pdFile
                  return $! if outdated
                            then Just $! outdatedMessage pdFile
                            else Nothing

    reconfiguringMostRecent = " Re-configuring with most recently used options."
    configureManually       = " If this fails, please run configure manually."
    sandboxConfigNewerMessage =
        "The sandbox was created after the package was already configured."
        ++ reconfiguringMostRecent
        ++ configureManually
    userPackageEnvironmentFileModifiedMessage =
        "The user package environment file ('"
        ++ userPackageEnvironmentFile ++ "') was modified."
        ++ reconfiguringMostRecent
        ++ configureManually
    distPrefMessage =
        "Package previously configured with different \"dist\" prefix."
        ++ reconfiguringMostRecent
        ++ configureManually
    outdatedMessage pdFile =
        pdFile ++ " has been changed."
        ++ reconfiguringMostRecent
        ++ configureManually
    reinstalledDepsMessage =
        "Some add-source dependencies have been reinstalled."
        ++ reconfiguringMostRecent
        ++ configureManually

installAction :: (ConfigFlags, ConfigExFlags, InstallFlags, HaddockFlags)
              -> [String] -> GlobalFlags -> IO ()
installAction (configFlags, _, installFlags, _) _ globalFlags
  | fromFlagOrDefault False (installOnly installFlags) = do
      let verbosity = fromFlagOrDefault normal (configVerbosity configFlags)
      (_, config) <- loadConfigOrSandboxConfig verbosity globalFlags
      distPref <- findSavedDistPref config (configDistPref configFlags)
      let setupOpts = defaultSetupScriptOptions { useDistPref = distPref }
      setupWrapper verbosity setupOpts Nothing installCommand (const mempty) []

installAction (configFlags, configExFlags, installFlags, haddockFlags)
              extraArgs globalFlags = do
  let verbosity = fromFlagOrDefault normal (configVerbosity configFlags)
  (useSandbox, config) <- fmap
                          (updateInstallDirs (configUserInstall configFlags))
                          (loadConfigOrSandboxConfig verbosity globalFlags)
  targets <- readUserTargets verbosity extraArgs

  -- TODO: It'd be nice if 'cabal install' picked up the '-w' flag passed to
  -- 'configure' when run inside a sandbox.  Right now, running
  --
  -- $ cabal sandbox init && cabal configure -w /path/to/ghc
  --   && cabal build && cabal install
  --
  -- performs the compilation twice unless you also pass -w to 'install'.
  -- However, this is the same behaviour that 'cabal install' has in the normal
  -- mode of operation, so we stick to it for consistency.

  let sandboxDistPref = case useSandbox of
        NoSandbox             -> NoFlag
        UseSandbox sandboxDir -> Flag $ sandboxBuildDir sandboxDir
  distPref <- findSavedDistPref config
              (configDistPref configFlags `mappend` sandboxDistPref)

  let configFlags'    = maybeForceTests installFlags' $
                        savedConfigureFlags   config `mappend`
                        configFlags { configDistPref = toFlag distPref }
      configExFlags'  = defaultConfigExFlags         `mappend`
                        savedConfigureExFlags config `mappend` configExFlags
      installFlags'   = defaultInstallFlags          `mappend`
                        savedInstallFlags     config `mappend` installFlags
      haddockFlags'   = defaultHaddockFlags          `mappend`
                        savedHaddockFlags     config `mappend`
                        haddockFlags { haddockDistPref = toFlag distPref }
      globalFlags'    = savedGlobalFlags      config `mappend` globalFlags
  (comp, platform, conf) <- configCompilerAux' configFlags'
  -- TODO: Redesign ProgramDB API to prevent such problems as #2241 in the future.
  conf' <- configureAllKnownPrograms verbosity conf

  -- If we're working inside a sandbox and the user has set the -w option, we
  -- may need to create a sandbox-local package DB for this compiler and add a
  -- timestamp record for this compiler to the timestamp file.
  configFlags'' <- case useSandbox of
    NoSandbox               -> configAbsolutePaths $ configFlags'
    (UseSandbox sandboxDir) ->
      return $ setPackageDB sandboxDir comp platform configFlags' conf
  whenUsingSandbox useSandbox $ \sandboxDir -> do
    initPackageDBIfNeeded verbosity configFlags'' comp conf'

    indexFile     <- tryGetIndexFilePath config
    maybeAddCompilerTimestampRecord verbosity sandboxDir indexFile
      (compilerId comp) platform

  -- FIXME: Passing 'SandboxPackageInfo' to install unconditionally here means
  -- that 'cabal install some-package' inside a sandbox will sometimes reinstall
  -- modified add-source deps, even if they are not among the dependencies of
  -- 'some-package'. This can also prevent packages that depend on older
  -- versions of add-source'd packages from building (see #1362).
  maybeWithSandboxPackageInfo verbosity configFlags'' globalFlags'
                              comp platform conf useSandbox $ \mSandboxPkgInfo ->
                              maybeWithSandboxDirOnSearchPath useSandbox $
      install verbosity
              (configPackageDB' configFlags'')
              (globalRepos globalFlags')
              comp platform conf'
              useSandbox mSandboxPkgInfo
              globalFlags' configFlags'' configExFlags'
              installFlags' haddockFlags'
              targets

    where
      -- '--run-tests' implies '--enable-tests'.
      maybeForceTests installFlags' configFlags' =
        if fromFlagOrDefault False (installRunTests installFlags')
        then configFlags' { configTests = toFlag True }
        else configFlags'

testAction :: (TestFlags, BuildFlags, BuildExFlags) -> [String] -> GlobalFlags
           -> IO ()
testAction (testFlags, buildFlags, buildExFlags) extraArgs globalFlags = do
  let verbosity      = fromFlagOrDefault normal (testVerbosity testFlags)
      addConfigFlags = mempty { configTests = toFlag True }
      noAddSource    = fromFlagOrDefault DontSkipAddSourceDepsCheck
                       (buildOnly buildExFlags)
      buildFlags'    = buildFlags
                       { buildVerbosity = testVerbosity testFlags }
      checkFlags flags
        | fromFlagOrDefault False (configTests flags) = Nothing
        | otherwise  = Just "Re-configuring with test suites enabled."

  -- reconfigure also checks if we're in a sandbox and reinstalls add-source
  -- deps if needed.
  (useSandbox, config, distPref) <-
    reconfigure verbosity (testDistPref testFlags)
                addConfigFlags [] globalFlags noAddSource
                (buildNumJobs buildFlags') checkFlags
  let setupOptions   = defaultSetupScriptOptions { useDistPref = distPref }
      testFlags'     = testFlags { testDistPref = toFlag distPref }

  -- the package was just configured, so the LBI must be available
  lbi <- getPersistBuildConfig distPref
  let pkgDescr = LBI.localPkgDescr lbi
      nameTestsOnly =
        LBI.foldComponent
          (const Nothing)
          (const Nothing)
          (\t ->
            if buildable (testBuildInfo t)
              then Just (testName t)
            else Nothing)
          (const Nothing)
      tests = mapMaybe nameTestsOnly $ LBI.pkgComponents pkgDescr
      extraArgs'
        | null extraArgs = tests
        | otherwise = extraArgs

  if null tests
    then notice verbosity "Package has no buildable test suites."
    else do
      maybeWithSandboxDirOnSearchPath useSandbox $
        build verbosity config distPref buildFlags' extraArgs'

      maybeWithSandboxDirOnSearchPath useSandbox $
        setupWrapper verbosity setupOptions Nothing
          Cabal.testCommand (const testFlags') extraArgs'

benchmarkAction :: (BenchmarkFlags, BuildFlags, BuildExFlags)
                   -> [String] -> GlobalFlags
                   -> IO ()
benchmarkAction (benchmarkFlags, buildFlags, buildExFlags)
                extraArgs globalFlags = do
  let verbosity      = fromFlagOrDefault normal
                       (benchmarkVerbosity benchmarkFlags)
      addConfigFlags = mempty { configBenchmarks = toFlag True }
      buildFlags'    = buildFlags
                       { buildVerbosity = benchmarkVerbosity benchmarkFlags }
      checkFlags flags
        | fromFlagOrDefault False (configBenchmarks flags) = Nothing
        | otherwise = Just "Re-configuring with benchmarks enabled."
      noAddSource   = fromFlagOrDefault DontSkipAddSourceDepsCheck
                      (buildOnly buildExFlags)

  -- reconfigure also checks if we're in a sandbox and reinstalls add-source
  -- deps if needed.
  (useSandbox, config, distPref) <-
    reconfigure verbosity (benchmarkDistPref benchmarkFlags)
                addConfigFlags [] globalFlags noAddSource
                (buildNumJobs buildFlags') checkFlags
  let setupOptions   = defaultSetupScriptOptions { useDistPref = distPref }
      benchmarkFlags'= benchmarkFlags { benchmarkDistPref = toFlag distPref }

  -- the package was just configured, so the LBI must be available
  lbi <- getPersistBuildConfig distPref
  let pkgDescr = LBI.localPkgDescr lbi
      nameBenchsOnly =
        LBI.foldComponent
          (const Nothing)
          (const Nothing)
          (const Nothing)
          (\b ->
            if buildable (benchmarkBuildInfo b)
              then Just (benchmarkName b)
            else Nothing)
      benchs = mapMaybe nameBenchsOnly $ LBI.pkgComponents pkgDescr
      extraArgs'
        | null extraArgs = benchs
        | otherwise = extraArgs

  if null benchs
    then notice verbosity "Package has no buildable benchmarks."
    else do
      maybeWithSandboxDirOnSearchPath useSandbox $
        build verbosity config distPref buildFlags' extraArgs'

      maybeWithSandboxDirOnSearchPath useSandbox $
        setupWrapper verbosity setupOptions Nothing
          Cabal.benchmarkCommand (const benchmarkFlags') extraArgs'

haddockAction :: HaddockFlags -> [String] -> GlobalFlags -> IO ()
haddockAction haddockFlags extraArgs globalFlags = do
  let verbosity = fromFlag (haddockVerbosity haddockFlags)
  (_useSandbox, config, distPref) <-
    reconfigure verbosity (haddockDistPref haddockFlags)
                mempty [] globalFlags DontSkipAddSourceDepsCheck
                NoFlag (const Nothing)
  let haddockFlags' = defaultHaddockFlags      `mappend`
                      savedHaddockFlags config `mappend`
                      haddockFlags { haddockDistPref = toFlag distPref }
      setupScriptOptions = defaultSetupScriptOptions { useDistPref = distPref }
  setupWrapper verbosity setupScriptOptions Nothing
    haddockCommand (const haddockFlags') extraArgs

cleanAction :: CleanFlags -> [String] -> GlobalFlags -> IO ()
cleanAction cleanFlags extraArgs globalFlags = do
  (_, config) <- loadConfigOrSandboxConfig verbosity globalFlags
  distPref <- findSavedDistPref config (cleanDistPref cleanFlags)
  let setupScriptOptions = defaultSetupScriptOptions
                           { useDistPref = distPref
                           , useWin32CleanHack = True
                           }
      cleanFlags' = cleanFlags { cleanDistPref = toFlag distPref }
  setupWrapper verbosity setupScriptOptions Nothing
               cleanCommand (const cleanFlags') extraArgs
  where
    verbosity = fromFlagOrDefault normal (cleanVerbosity cleanFlags)

listAction :: ListFlags -> [String] -> GlobalFlags -> IO ()
listAction listFlags extraArgs globalFlags = do
  let verbosity = fromFlag (listVerbosity listFlags)
  (_useSandbox, config) <- loadConfigOrSandboxConfig verbosity
                           (globalFlags { globalRequireSandbox = Flag False })
  let configFlags' = savedConfigureFlags config
      configFlags  = configFlags' {
        configPackageDBs = configPackageDBs configFlags'
                           `mappend` listPackageDBs listFlags
        }
      globalFlags' = savedGlobalFlags    config `mappend` globalFlags
  (comp, _, conf) <- configCompilerAux' configFlags
  List.list verbosity
       (configPackageDB' configFlags)
       (globalRepos globalFlags')
       comp
       conf
       listFlags
       extraArgs

infoAction :: InfoFlags -> [String] -> GlobalFlags -> IO ()
infoAction infoFlags extraArgs globalFlags = do
  let verbosity = fromFlag (infoVerbosity infoFlags)
  targets <- readUserTargets verbosity extraArgs
  (_useSandbox, config) <- loadConfigOrSandboxConfig verbosity
                           (globalFlags { globalRequireSandbox = Flag False })
  let configFlags' = savedConfigureFlags config
      configFlags  = configFlags' {
        configPackageDBs = configPackageDBs configFlags'
                           `mappend` infoPackageDBs infoFlags
        }
      globalFlags' = savedGlobalFlags    config `mappend` globalFlags
  (comp, _, conf) <- configCompilerAuxEx configFlags
  List.info verbosity
       (configPackageDB' configFlags)
       (globalRepos globalFlags')
       comp
       conf
       globalFlags'
       infoFlags
       targets

updateAction :: Flag Verbosity -> [String] -> GlobalFlags -> IO ()
updateAction verbosityFlag extraArgs globalFlags = do
  unless (null extraArgs) $
    die $ "'update' doesn't take any extra arguments: " ++ unwords extraArgs
  let verbosity = fromFlag verbosityFlag
  (_useSandbox, config) <- loadConfigOrSandboxConfig verbosity
                           (globalFlags { globalRequireSandbox = Flag False })
  let globalFlags' = savedGlobalFlags config `mappend` globalFlags
  transport <- configureTransport verbosity (flagToMaybe (globalHttpTransport globalFlags'))
  update transport verbosity (globalRepos globalFlags')

upgradeAction :: (ConfigFlags, ConfigExFlags, InstallFlags, HaddockFlags)
              -> [String] -> GlobalFlags -> IO ()
upgradeAction _ _ _ = die $
    "Use the 'cabal install' command instead of 'cabal upgrade'.\n"
 ++ "You can install the latest version of a package using 'cabal install'. "
 ++ "The 'cabal upgrade' command has been removed because people found it "
 ++ "confusing and it often led to broken packages.\n"
 ++ "If you want the old upgrade behaviour then use the install command "
 ++ "with the --upgrade-dependencies flag (but check first with --dry-run "
 ++ "to see what would happen). This will try to pick the latest versions "
 ++ "of all dependencies, rather than the usual behaviour of trying to pick "
 ++ "installed versions of all dependencies. If you do use "
 ++ "--upgrade-dependencies, it is recommended that you do not upgrade core "
 ++ "packages (e.g. by using appropriate --constraint= flags)."

fetchAction :: FetchFlags -> [String] -> GlobalFlags -> IO ()
fetchAction fetchFlags extraArgs globalFlags = do
  let verbosity = fromFlag (fetchVerbosity fetchFlags)
  targets <- readUserTargets verbosity extraArgs
  config <- loadConfig verbosity (globalConfigFile globalFlags)
  let configFlags  = savedConfigureFlags config
      globalFlags' = savedGlobalFlags config `mappend` globalFlags
  (comp, platform, conf) <- configCompilerAux' configFlags
  fetch verbosity
        (configPackageDB' configFlags)
        (globalRepos globalFlags')
        comp platform conf globalFlags' fetchFlags
        targets

freezeAction :: FreezeFlags -> [String] -> GlobalFlags -> IO ()
freezeAction freezeFlags _extraArgs globalFlags = do
  let verbosity = fromFlag (freezeVerbosity freezeFlags)
  (useSandbox, config) <- loadConfigOrSandboxConfig verbosity globalFlags
  let configFlags  = savedConfigureFlags config
      globalFlags' = savedGlobalFlags config `mappend` globalFlags
  (comp, platform, conf) <- configCompilerAux' configFlags

  maybeWithSandboxPackageInfo verbosity configFlags globalFlags'
                              comp platform conf useSandbox $ \mSandboxPkgInfo ->
                              maybeWithSandboxDirOnSearchPath useSandbox $
      freeze verbosity
            (configPackageDB' configFlags)
            (globalRepos globalFlags')
            comp platform conf
            mSandboxPkgInfo
            globalFlags' freezeFlags

uploadAction :: UploadFlags -> [String] -> GlobalFlags -> IO ()
uploadAction uploadFlags extraArgs globalFlags = do
  let verbosity = fromFlag (uploadVerbosity uploadFlags)
  config <- loadConfig verbosity (globalConfigFile globalFlags)
  let uploadFlags' = savedUploadFlags config `mappend` uploadFlags
      globalFlags' = savedGlobalFlags config `mappend` globalFlags
      tarfiles     = extraArgs
  checkTarFiles extraArgs
  maybe_password <-
    case uploadPasswordCmd uploadFlags'
    of Flag (xs:xss) -> Just . Password <$>
                        getProgramInvocationOutput verbosity
                        (simpleProgramInvocation xs xss)
       _             -> pure $ flagToMaybe $ uploadPassword uploadFlags'
  transport <- configureTransport verbosity (flagToMaybe (globalHttpTransport globalFlags'))
  if fromFlag (uploadCheck uploadFlags')
    then Upload.check transport verbosity tarfiles
    else upload transport
                verbosity
                (globalRepos globalFlags')
                (flagToMaybe $ uploadUsername uploadFlags')
                maybe_password
                tarfiles
  where
    checkTarFiles tarfiles
      | null tarfiles
      = die "the 'upload' command expects one or more .tar.gz packages."
      | not (null otherFiles)
      = die $ "the 'upload' command expects only .tar.gz packages: "
           ++ intercalate ", " otherFiles
      | otherwise = sequence_
                      [ do exists <- doesFileExist tarfile
                           unless exists $ die $ "file not found: " ++ tarfile
                      | tarfile <- tarfiles ]

      where otherFiles = filter (not . isTarGzFile) tarfiles
            isTarGzFile file = case splitExtension file of
              (file', ".gz") -> takeExtension file' == ".tar"
              _              -> False

checkAction :: Flag Verbosity -> [String] -> GlobalFlags -> IO ()
checkAction verbosityFlag extraArgs _globalFlags = do
  unless (null extraArgs) $
    die $ "'check' doesn't take any extra arguments: " ++ unwords extraArgs
  allOk <- Check.check (fromFlag verbosityFlag)
  unless allOk exitFailure

formatAction :: Flag Verbosity -> [String] -> GlobalFlags -> IO ()
formatAction verbosityFlag extraArgs _globalFlags = do
  let verbosity = fromFlag verbosityFlag
  path <- case extraArgs of
    [] -> do cwd <- getCurrentDirectory
             tryFindPackageDesc cwd
    (p:_) -> return p
  pkgDesc <- readPackageDescription verbosity path
  -- Uses 'writeFileAtomic' under the hood.
  writeGenericPackageDescription path pkgDesc

uninstallAction :: Flag Verbosity -> [String] -> GlobalFlags -> IO ()
uninstallAction _verbosityFlag extraArgs _globalFlags = do
  let package = case extraArgs of
        p:_ -> p
        _   -> "PACKAGE_NAME"
  die $ "This version of 'cabal-install' does not support the 'uninstall' operation. "
        ++ "It will likely be implemented at some point in the future; in the meantime "
        ++ "you're advised to use either 'ghc-pkg unregister " ++ package ++ "' or "
        ++ "'cabal sandbox hc-pkg -- unregister " ++ package ++ "'."


sdistAction :: (SDistFlags, SDistExFlags) -> [String] -> GlobalFlags -> IO ()
sdistAction (sdistFlags, sdistExFlags) extraArgs globalFlags = do
  unless (null extraArgs) $
    die $ "'sdist' doesn't take any extra arguments: " ++ unwords extraArgs
  let verbosity = fromFlag (sDistVerbosity sdistFlags)
  (_, config) <- loadConfigOrSandboxConfig verbosity globalFlags
  distPref <- findSavedDistPref config (sDistDistPref sdistFlags)
  let sdistFlags' = sdistFlags { sDistDistPref = toFlag distPref }
  sdist sdistFlags' sdistExFlags

reportAction :: ReportFlags -> [String] -> GlobalFlags -> IO ()
reportAction reportFlags extraArgs globalFlags = do
  unless (null extraArgs) $
    die $ "'report' doesn't take any extra arguments: " ++ unwords extraArgs

  let verbosity = fromFlag (reportVerbosity reportFlags)
  config <- loadConfig verbosity (globalConfigFile globalFlags)
  let globalFlags' = savedGlobalFlags config `mappend` globalFlags
      reportFlags' = savedReportFlags config `mappend` reportFlags

  Upload.report verbosity (globalRepos globalFlags')
    (flagToMaybe $ reportUsername reportFlags')
    (flagToMaybe $ reportPassword reportFlags')

runAction :: (BuildFlags, BuildExFlags) -> [String] -> GlobalFlags -> IO ()
runAction (buildFlags, buildExFlags) extraArgs globalFlags = do
  let verbosity   = fromFlagOrDefault normal (buildVerbosity buildFlags)
  let noAddSource = fromFlagOrDefault DontSkipAddSourceDepsCheck
                    (buildOnly buildExFlags)

  -- reconfigure also checks if we're in a sandbox and reinstalls add-source
  -- deps if needed.
  (useSandbox, config, distPref) <-
    reconfigure verbosity (buildDistPref buildFlags) mempty []
                globalFlags noAddSource (buildNumJobs buildFlags)
                (const Nothing)

  lbi <- getPersistBuildConfig distPref
  (exe, exeArgs) <- splitRunArgs verbosity lbi extraArgs

  maybeWithSandboxDirOnSearchPath useSandbox $
    build verbosity config distPref buildFlags ["exe:" ++ exeName exe]

  maybeWithSandboxDirOnSearchPath useSandbox $
    run verbosity lbi exe exeArgs

getAction :: GetFlags -> [String] -> GlobalFlags -> IO ()
getAction getFlags extraArgs globalFlags = do
  let verbosity = fromFlag (getVerbosity getFlags)
  targets <- readUserTargets verbosity extraArgs
  config <- loadConfig verbosity (globalConfigFile globalFlags)
  let globalFlags' = savedGlobalFlags config `mappend` globalFlags
  get verbosity
    (globalRepos (savedGlobalFlags config))
    globalFlags'
    getFlags
    targets

unpackAction :: GetFlags -> [String] -> GlobalFlags -> IO ()
unpackAction getFlags extraArgs globalFlags = do
  getAction getFlags extraArgs globalFlags

initAction :: InitFlags -> [String] -> GlobalFlags -> IO ()
initAction initFlags _extraArgs globalFlags = do
  let verbosity = fromFlag (initVerbosity initFlags)
  (_useSandbox, config) <- loadConfigOrSandboxConfig verbosity
                           (globalFlags { globalRequireSandbox = Flag False })
  let configFlags  = savedConfigureFlags config
  let globalFlags' = savedGlobalFlags    config `mappend` globalFlags
  (comp, _, conf) <- configCompilerAux' configFlags
  initCabal verbosity
            (configPackageDB' configFlags)
            (globalRepos globalFlags')
            comp
            conf
            initFlags

sandboxAction :: SandboxFlags -> [String] -> GlobalFlags -> IO ()
sandboxAction sandboxFlags extraArgs globalFlags = do
  let verbosity = fromFlag (sandboxVerbosity sandboxFlags)
  case extraArgs of
    -- Basic sandbox commands.
    ["init"] -> sandboxInit verbosity sandboxFlags globalFlags
    ["delete"] -> sandboxDelete verbosity sandboxFlags globalFlags
    ("add-source":extra) -> do
        when (noExtraArgs extra) $
          die "The 'sandbox add-source' command expects at least one argument"
        sandboxAddSource verbosity extra sandboxFlags globalFlags
    ("delete-source":extra) -> do
        when (noExtraArgs extra) $
          die ("The 'sandbox delete-source' command expects " ++
              "at least one argument")
        sandboxDeleteSource verbosity extra sandboxFlags globalFlags
    ["list-sources"] -> sandboxListSources verbosity sandboxFlags globalFlags

    -- More advanced commands.
    ("hc-pkg":extra) -> do
        when (noExtraArgs extra) $
            die $ "The 'sandbox hc-pkg' command expects at least one argument"
        sandboxHcPkg verbosity sandboxFlags globalFlags extra
    ["buildopts"] -> die "Not implemented!"

    -- Hidden commands.
    ["dump-pkgenv"]  -> dumpPackageEnvironment verbosity sandboxFlags globalFlags

    -- Error handling.
    [] -> die $ "Please specify a subcommand (see 'help sandbox')"
    _  -> die $ "Unknown 'sandbox' subcommand: " ++ unwords extraArgs

  where
    noExtraArgs = (<1) . length

execAction :: ExecFlags -> [String] -> GlobalFlags -> IO ()
execAction execFlags extraArgs globalFlags = do
  let verbosity = fromFlag (execVerbosity execFlags)
  (useSandbox, config) <- loadConfigOrSandboxConfig verbosity globalFlags
  let configFlags = savedConfigureFlags config
  (comp, platform, conf) <- configCompilerAux' configFlags
  exec verbosity useSandbox comp platform conf extraArgs

userConfigAction :: UserConfigFlags -> [String] -> GlobalFlags -> IO ()
userConfigAction ucflags extraArgs globalFlags = do
  let verbosity = fromFlag (userConfigVerbosity ucflags)
  case extraArgs of
    ("diff":_) -> mapM_ putStrLn =<< userConfigDiff globalFlags
    ("update":_) -> userConfigUpdate verbosity globalFlags
    -- Error handling.
    [] -> die $ "Please specify a subcommand (see 'help user-config')"
    _  -> die $ "Unknown 'user-config' subcommand: " ++ unwords extraArgs


-- | See 'Distribution.Client.Install.withWin32SelfUpgrade' for details.
--
win32SelfUpgradeAction :: Win32SelfUpgradeFlags -> [String] -> GlobalFlags
                          -> IO ()
win32SelfUpgradeAction selfUpgradeFlags (pid:path:_extraArgs) _globalFlags = do
  let verbosity = fromFlag (win32SelfUpgradeVerbosity selfUpgradeFlags)
  Win32SelfUpgrade.deleteOldExeFile verbosity (read pid) path
win32SelfUpgradeAction _ _ _ = return ()

-- | Used as an entry point when cabal-install needs to invoke itself
-- as a setup script. This can happen e.g. when doing parallel builds.
--
actAsSetupAction :: ActAsSetupFlags -> [String] -> GlobalFlags -> IO ()
actAsSetupAction actAsSetupFlags args _globalFlags =
  let bt = fromFlag (actAsSetupBuildType actAsSetupFlags)
  in case bt of
    Simple    -> Simple.defaultMainArgs args
    Configure -> Simple.defaultMainWithHooksArgs
                  Simple.autoconfUserHooks args
    Make      -> Make.defaultMainArgs args
    Custom               -> error "actAsSetupAction Custom"
    (UnknownBuildType _) -> error "actAsSetupAction UnknownBuildType"
