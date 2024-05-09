{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------

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
--
-- @since 3.10.0.0
module Distribution.Client.Main (main) where

import Distribution.Client.Setup
  ( ActAsSetupFlags (..)
  , BuildFlags (..)
  , CheckFlags (..)
  , CommonSetupFlags (..)
  , ConfigExFlags (..)
  , ConfigFlags (..)
  , FetchFlags (..)
  , FreezeFlags (..)
  , GetFlags (..)
  , GlobalFlags (..)
  , InfoFlags (..)
  , InitFlags (initHcPath, initVerbosity)
  , InstallFlags (..)
  , ListFlags (..)
  , ReportFlags (..)
  , UploadFlags (..)
  , UserConfigFlags (..)
  , actAsSetupCommand
  , benchmarkCommand
  , buildCommand
  , checkCommand
  , cleanCommand
  , configCompilerAux'
  , configPackageDB'
  , configureExCommand
  , copyCommand
  , defaultConfigExFlags
  , defaultInstallFlags
  , fetchCommand
  , filterCommonFlags
  , formatCommand
  , freezeCommand
  , genBoundsCommand
  , getCommand
  , globalCommand
  , haddockCommand
  , infoCommand
  , initCommand
  , installCommand
  , listCommand
  , listNeedsCompiler
  , manpageCommand
  , reconfigureCommand
  , registerCommand
  , replCommand
  , reportCommand
  , runCommand
  , testCommand
  , unpackCommand
  , uploadCommand
  , userConfigCommand
  , withRepoContext
  )
import Distribution.Simple.Setup
  ( BenchmarkFlags (..)
  , CleanFlags (..)
  , CopyFlags (..)
  , Flag (..)
  , HaddockFlags (..)
  , HaddockTarget (..)
  , HscolourFlags (..)
  , RegisterFlags (..)
  , ReplFlags (..)
  , TestFlags (..)
  , defaultHaddockFlags
  , flagToMaybe
  , fromFlag
  , fromFlagOrDefault
  , hscolourCommand
  , toFlag
  )

import Distribution.Client.Compat.Prelude hiding (get)
import Prelude ()

import Distribution.Client.Config
  ( SavedConfig (..)
  , createDefaultConfigFile
  , defaultConfigFile
  , defaultUserInstall
  , getConfigFilePath
  , loadConfig
  , userConfigDiff
  , userConfigUpdate
  )
import qualified Distribution.Client.List as List
  ( info
  , list
  )
import Distribution.Client.SetupWrapper
  ( SetupRunnerArgs (NotInLibrary)
  , SetupScriptOptions (..)
  , defaultSetupScriptOptions
  , setupWrapper
  )
import Distribution.Client.Targets
  ( readUserTargets
  )

import qualified Distribution.Client.CmdBench as CmdBench
import qualified Distribution.Client.CmdBuild as CmdBuild
import qualified Distribution.Client.CmdClean as CmdClean
import qualified Distribution.Client.CmdConfigure as CmdConfigure
import qualified Distribution.Client.CmdExec as CmdExec
import qualified Distribution.Client.CmdFreeze as CmdFreeze
import qualified Distribution.Client.CmdHaddock as CmdHaddock
import qualified Distribution.Client.CmdHaddockProject as CmdHaddockProject
import qualified Distribution.Client.CmdInstall as CmdInstall
import Distribution.Client.CmdLegacy
import qualified Distribution.Client.CmdListBin as CmdListBin
import qualified Distribution.Client.CmdOutdated as CmdOutdated
import qualified Distribution.Client.CmdPath as CmdPath
import qualified Distribution.Client.CmdRepl as CmdRepl
import qualified Distribution.Client.CmdRun as CmdRun
import qualified Distribution.Client.CmdSdist as CmdSdist
import qualified Distribution.Client.CmdTest as CmdTest
import qualified Distribution.Client.CmdUpdate as CmdUpdate

import Distribution.Client.Check as Check (check)
import Distribution.Client.Configure (configure, writeConfigFlags)
import Distribution.Client.Fetch (fetch)
import Distribution.Client.Freeze (freeze)
import Distribution.Client.GenBounds (genBounds)
import Distribution.Client.Install (install)

-- import Distribution.Client.Clean            (clean)

import Distribution.Client.Get (get)
import Distribution.Client.Init (initCmd)
import Distribution.Client.Manpage (manpageCmd)
import Distribution.Client.ManpageFlags (ManpageFlags (..))
import Distribution.Client.Nix
  ( nixInstantiate
  , nixShell
  )
import Distribution.Client.Reconfigure (Check (..), reconfigure)
import Distribution.Client.Run (run, splitRunArgs)
import Distribution.Client.Sandbox
  ( findSavedDistPref
  , loadConfigOrSandboxConfig
  , updateInstallDirs
  )
import Distribution.Client.Signal
  ( installTerminationHandler
  )
import Distribution.Client.Tar (createTarGzFile)
import Distribution.Client.Types.Credentials (Password (..))
import qualified Distribution.Client.Upload as Upload
import Distribution.Client.Utils
  ( determineNumJobs
  , relaxEncodingErrors
  )
import Distribution.Client.Version
  ( cabalInstallVersion
  )

import Distribution.Package (packageId)
import Distribution.PackageDescription
  ( BuildType (..)
  , Executable (..)
  , buildable
  )

import Distribution.Client.Errors
import Distribution.Compat.ResponseFile
import qualified Distribution.Make as Make
import Distribution.PackageDescription.PrettyPrint
  ( writeGenericPackageDescription
  )
import qualified Distribution.Simple as Simple
import Distribution.Simple.Build
  ( startInterpreter
  )
import Distribution.Simple.Command
  ( Command
  , CommandParse (..)
  , CommandSpec (..)
  , CommandType (..)
  , CommandUI (..)
  , commandAddAction
  , commandFromSpec
  , commandShowOptions
  , commandsRunWithFallback
  , defaultCommandFallback
  , hiddenCommand
  )
import Distribution.Simple.Compiler (PackageDBStack)
import Distribution.Simple.Configure
  ( ConfigStateFileError (..)
  , configCompilerAuxEx
  , getPersistBuildConfig
  , interpretPackageDbFlags
  , tryGetPersistBuildConfig
  )
import qualified Distribution.Simple.LocalBuildInfo as LBI
import Distribution.Simple.PackageDescription (readGenericPackageDescription)
import Distribution.Simple.Program
  ( configureAllKnownPrograms
  , defaultProgramDb
  , defaultProgramSearchPath
  , findProgramOnSearchPath
  , getProgramInvocationOutput
  , simpleProgramInvocation
  )
import Distribution.Simple.Program.Db (reconfigurePrograms)
import qualified Distribution.Simple.Setup as Cabal
import Distribution.Simple.Utils
  ( cabalVersion
  , createDirectoryIfMissingVerbose
  , dieNoVerbosity
  , dieWithException
  , findPackageDesc
  , info
  , notice
  , topHandler
  , tryFindPackageDesc
  )
import Distribution.Text
  ( display
  )
import qualified Distribution.Types.UnqualComponentName as Make
import Distribution.Utils.Path hiding
  ( (<.>)
  , (</>)
  )
import Distribution.Verbosity as Verbosity
  ( normal
  )
import Distribution.Version
  ( Version
  , mkVersion
  , orLaterVersion
  )

import Control.Exception (AssertionFailed, assert, try)
import Data.Monoid (Any (..))
import System.Directory
  ( doesFileExist
  , withCurrentDirectory
  )
import System.Environment (getEnvironment, getExecutablePath, getProgName)
import System.FilePath
  ( dropExtension
  , splitExtension
  , takeExtension
  , (<.>)
  , (</>)
  )
import System.IO
  ( BufferMode (LineBuffering)
  , hPutStrLn
  , hSetBuffering
  , stderr
  , stdout
  )
import System.Process (createProcess, env, proc)

-- | Entry point
--
-- This does three things.
--
-- One, it initializes the program, providing support for termination
-- signals, preparing console linebuffering, and relaxing encoding errors.
--
-- Two, it processes (via an IO action) response
-- files, calling 'expandResponse' in Cabal/Distribution.Compat.ResponseFile
--
-- Note that here, it splits the arguments on a strict match to
-- "--", and won't parse response files after the split.
--
-- Three, it calls the 'mainWorker', which calls the argument parser,
-- producing 'CommandParse' data, which mainWorker pattern-matches
-- into IO actions for execution.
main :: [String] -> IO ()
main args = do
  installTerminationHandler
  -- Enable line buffering so that we can get fast feedback even when piped.
  -- This is especially important for CI and build systems.
  hSetBuffering stdout LineBuffering

  -- If the locale encoding for CLI doesn't support all Unicode characters,
  -- printing to it may fail unless we relax the handling of encoding errors
  -- when writing to stderr and stdout.
  relaxEncodingErrors stdout
  relaxEncodingErrors stderr

  -- Response files support.
  -- See 'expandResponse' documentation in Cabal/Distribution.Compat.ResponseFile
  -- for more information.
  let (args0, args1) = break (== "--") args

  mainWorker =<< (++ args1) <$> expandResponse args0

-- | Check whether assertions are enabled and print a warning in that case.
warnIfAssertionsAreEnabled :: IO ()
warnIfAssertionsAreEnabled =
  assert False (return ())
    `catch` (\(_e :: AssertionFailed) -> hPutStrLn stderr assertionsEnabledMsg)
  where
    -- Andreas, 2022-12-30, issue #8654:
    -- The verbosity machinery is not in place at this point (option -v not parsed),
    -- so instead of using function @warn@, we print straight to stderr.

    assertionsEnabledMsg =
      "Warning: this is a debug build of cabal-install with assertions enabled."

-- | Core worker, similar to 'defaultMainHelper' in Cabal/Distribution.Simple
--
-- With an exception-handler @topHandler@, mainWorker calls commandsRun
-- to parse arguments, then pattern-matches the CommandParse data
-- into IO actions for execution.
mainWorker :: [String] -> IO ()
mainWorker args = do
  topHandler $ do
    command <- commandsRunWithFallback (globalCommand commands) commands delegateToExternal args
    case command of
      CommandHelp help -> printGlobalHelp help
      CommandList opts -> printOptionsList opts
      CommandErrors errs -> printErrors errs
      CommandReadyToGo (globalFlags, commandParse) ->
        case commandParse of
          _
            | fromFlagOrDefault False (globalVersion globalFlags) ->
                printVersion
            | fromFlagOrDefault False (globalNumericVersion globalFlags) ->
                printNumericVersion
          CommandHelp help -> printCommandHelp help
          CommandList opts -> printOptionsList opts
          CommandErrors errs -> do
            -- Check whether cabal is called from a script, like #!/path/to/cabal.
            case args of
              [] -> printErrors errs
              script : scriptArgs ->
                CmdRun.validScript script >>= \case
                  False -> printErrors errs
                  True -> do
                    -- In main operation (not help, version etc.) print warning if assertions are on.
                    warnIfAssertionsAreEnabled
                    CmdRun.handleShebang script scriptArgs
          CommandReadyToGo action -> do
            -- In main operation (not help, version etc.) print warning if assertions are on.
            warnIfAssertionsAreEnabled
            action globalFlags
  where
    delegateToExternal
      :: [Command Action]
      -> String
      -> [String]
      -> IO (CommandParse Action)
    delegateToExternal commands' name cmdArgs = do
      mCommand <- findProgramOnSearchPath normal defaultProgramSearchPath ("cabal-" <> name)
      case mCommand of
        Just (exec, _) -> return (CommandReadyToGo $ \_ -> callExternal exec name cmdArgs)
        Nothing -> defaultCommandFallback commands' name cmdArgs

    callExternal :: String -> String -> [String] -> IO ()
    callExternal exec name cmdArgs = do
      cur_env <- getEnvironment
      cabal_exe <- getExecutablePath
      let new_env = ("CABAL", cabal_exe) : cur_env
      result <- try $ createProcess ((proc exec (name : cmdArgs)){env = Just new_env})
      case result of
        Left ex -> printErrors ["Error executing external command: " ++ show (ex :: SomeException)]
        Right _ -> return ()

    printCommandHelp help = do
      pname <- getProgName
      putStr (help pname)
    printGlobalHelp help = do
      pname <- getProgName
      configFile <- defaultConfigFile
      putStr (help pname)
      -- Andreas Abel, 2024-01-28: https://github.com/haskell/cabal/pull/9614
      -- See cabal-testsuite/PackageTests/Help/HelpPrintsConfigFile/
      -- Third-party tools may rely on the specific wording
      -- to find the config file in the help text, so do not change!
      putStr $
        "\nYou can edit the cabal configuration file to set defaults:\n"
          ++ "  "
          ++ configFile
          ++ "\n"
      exists <- doesFileExist configFile
      unless exists $
        putStrLn $
          "This file will be generated with sensible "
            ++ "defaults if you run 'cabal update'."
    printOptionsList = putStr . unlines
    printErrors errs = dieNoVerbosity $ intercalate "\n" errs
    printNumericVersion = putStrLn $ display cabalInstallVersion
    printVersion =
      putStrLn $
        "cabal-install version "
          ++ display cabalInstallVersion
          ++ "\ncompiled using version "
          ++ display cabalVersion
          ++ " of the Cabal library "

    commands = map commandFromSpec commandSpecs
    commandSpecs =
      [ regularCmd listCommand listAction
      , regularCmd infoCommand infoAction
      , regularCmd fetchCommand fetchAction
      , regularCmd getCommand getAction
      , regularCmd unpackCommand unpackAction
      , regularCmd checkCommand checkAction
      , regularCmd uploadCommand uploadAction
      , regularCmd reportCommand reportAction
      , regularCmd initCommand initAction
      , regularCmd userConfigCommand userConfigAction
      , regularCmd CmdPath.pathCommand CmdPath.pathAction
      , regularCmd genBoundsCommand genBoundsAction
      , regularCmd CmdOutdated.outdatedCommand CmdOutdated.outdatedAction
      , wrapperCmd hscolourCommand hscolourCommonFlags
      , hiddenCmd formatCommand formatAction
      , hiddenCmd actAsSetupCommand actAsSetupAction
      , hiddenCmd manpageCommand (manpageAction commandSpecs)
      , regularCmd CmdListBin.listbinCommand CmdListBin.listbinAction
      ]
        ++ concat
          [ newCmd CmdConfigure.configureCommand CmdConfigure.configureAction
          , newCmd CmdUpdate.updateCommand CmdUpdate.updateAction
          , newCmd CmdBuild.buildCommand CmdBuild.buildAction
          , newCmd CmdRepl.replCommand CmdRepl.replAction
          , newCmd CmdFreeze.freezeCommand CmdFreeze.freezeAction
          , newCmd CmdHaddock.haddockCommand CmdHaddock.haddockAction
          , newCmd
              CmdHaddockProject.haddockProjectCommand
              CmdHaddockProject.haddockProjectAction
          , newCmd CmdInstall.installCommand CmdInstall.installAction
          , newCmd CmdRun.runCommand CmdRun.runAction
          , newCmd CmdTest.testCommand CmdTest.testAction
          , newCmd CmdBench.benchCommand CmdBench.benchAction
          , newCmd CmdExec.execCommand CmdExec.execAction
          , newCmd CmdClean.cleanCommand CmdClean.cleanAction
          , newCmd CmdSdist.sdistCommand CmdSdist.sdistAction
          , legacyCmd configureExCommand configureAction
          , legacyCmd buildCommand buildAction
          , legacyCmd replCommand replAction
          , legacyCmd freezeCommand freezeAction
          , legacyCmd haddockCommand haddockAction
          , legacyCmd installCommand installAction
          , legacyCmd runCommand runAction
          , legacyCmd testCommand testAction
          , legacyCmd benchmarkCommand benchmarkAction
          , legacyCmd cleanCommand cleanAction
          , legacyWrapperCmd copyCommand copyCommonFlags
          , legacyWrapperCmd registerCommand registerCommonFlags
          , legacyCmd reconfigureCommand reconfigureAction
          ]

type Action = GlobalFlags -> IO ()

-- Duplicated in Distribution.Client.CmdLegacy. Any changes must be
-- reflected there, as well.
regularCmd
  :: CommandUI flags
  -> (flags -> [String] -> action)
  -> CommandSpec action
regularCmd ui action =
  CommandSpec ui ((flip commandAddAction) action) NormalCommand

hiddenCmd
  :: CommandUI flags
  -> (flags -> [String] -> action)
  -> CommandSpec action
hiddenCmd ui action =
  CommandSpec
    ui
    (\ui' -> hiddenCommand (commandAddAction ui' action))
    HiddenCommand

wrapperCmd
  :: Monoid flags
  => CommandUI flags
  -> (flags -> CommonSetupFlags)
  -> CommandSpec Action
wrapperCmd ui getCommonFlags =
  CommandSpec ui (\ui' -> wrapperAction ui' getCommonFlags) NormalCommand

wrapperAction
  :: Monoid flags
  => CommandUI flags
  -> (flags -> CommonSetupFlags)
  -> Command Action
wrapperAction command getCommonFlags =
  commandAddAction
    command
      { commandDefaultFlags = mempty
      }
    $ \flags extraArgs globalFlags -> do
      let common = getCommonFlags flags
          verbosity = fromFlagOrDefault normal $ setupVerbosity common
          mbWorkDir = flagToMaybe $ setupWorkingDir common
      load <- try (loadConfigOrSandboxConfig verbosity globalFlags)
      let config = either (\(SomeException _) -> mempty) id load
      distPref <- findSavedDistPref config (setupDistPref common)
      let setupScriptOptions =
            defaultSetupScriptOptions
              { useDistPref = distPref
              , useWorkingDir = mbWorkDir
              }
      setupWrapper
        verbosity
        setupScriptOptions
        Nothing
        command
        getCommonFlags
        (const flags)
        (const extraArgs)
        NotInLibrary

configureAction
  :: (ConfigFlags, ConfigExFlags)
  -> [String]
  -> Action
configureAction (configFlags, configExFlags) extraArgs globalFlags = do
  let common = configCommonFlags configFlags
      verbosity = fromFlagOrDefault normal $ setupVerbosity common
  config <-
    updateInstallDirs (configUserInstall configFlags)
      <$> loadConfigOrSandboxConfig verbosity globalFlags
  distPref <- getSymbolicPath <$> findSavedDistPref config (setupDistPref common)
  nixInstantiate verbosity distPref True globalFlags config
  nixShell verbosity distPref globalFlags config $ do
    let configFlags' = savedConfigureFlags config `mappend` configFlags
        configExFlags' = savedConfigureExFlags config `mappend` configExFlags
        globalFlags' = savedGlobalFlags config `mappend` globalFlags
    (comp, platform, progdb) <- configCompilerAuxEx configFlags'

    writeConfigFlags verbosity distPref (configFlags', configExFlags')

    -- What package database(s) to use
    let packageDBs :: PackageDBStack
        packageDBs =
          interpretPackageDbFlags
            (fromFlagOrDefault defaultUserInstall (configUserInstall configFlags'))
            (configPackageDBs configFlags')

    withRepoContext verbosity globalFlags' $ \repoContext ->
      configure
        verbosity
        packageDBs
        repoContext
        comp
        platform
        progdb
        configFlags'
        configExFlags'
        extraArgs

reconfigureAction
  :: (ConfigFlags, ConfigExFlags)
  -> [String]
  -> Action
reconfigureAction flags@(configFlags, _) _ globalFlags = do
  let common = configCommonFlags configFlags
      verbosity = fromFlagOrDefault normal (setupVerbosity common)
  config <-
    updateInstallDirs (configUserInstall configFlags)
      <$> loadConfigOrSandboxConfig verbosity globalFlags
  distPref <- findSavedDistPref config (setupDistPref common)
  let checkFlags = Check $ \_ saved -> do
        let flags' = saved <> flags
        unless (saved == flags') $ info verbosity message
        pure (Any True, flags')
        where
          -- This message is correct, but not very specific: it will list all
          -- of the new flags, even if some have not actually changed. The
          -- \*minimal* set of changes is more difficult to determine.
          message =
            "flags changed: "
              ++ unwords (commandShowOptions configureExCommand flags)
  nixInstantiate verbosity (getSymbolicPath distPref) True globalFlags config
  _ <-
    reconfigure
      configureAction
      verbosity
      distPref
      NoFlag
      checkFlags
      []
      globalFlags
      config
  pure ()

buildAction :: BuildFlags -> [String] -> Action
buildAction buildFlags extraArgs globalFlags = do
  let common = buildCommonFlags buildFlags
      verbosity = fromFlagOrDefault normal $ setupVerbosity common
  config <- loadConfigOrSandboxConfig verbosity globalFlags
  distPref <- findSavedDistPref config (setupDistPref common)
  -- Calls 'configureAction' to do the real work, so nothing special has to be
  -- done to support sandboxes.
  config' <-
    reconfigure
      configureAction
      verbosity
      distPref
      (buildNumJobs buildFlags)
      mempty
      []
      globalFlags
      config
  nixShell verbosity (getSymbolicPath distPref) globalFlags config $ do
    build verbosity config' distPref buildFlags extraArgs

-- | Actually do the work of building the package. This is separate from
-- 'buildAction' so that 'testAction' and 'benchmarkAction' do not invoke
-- 'reconfigure' twice.
build :: Verbosity -> SavedConfig -> SymbolicPath Pkg (Dir Dist) -> BuildFlags -> [String] -> IO ()
build verbosity config distPref buildFlags extraArgs =
  setupWrapper
    verbosity
    setupOptions
    Nothing
    (Cabal.buildCommand progDb)
    buildCommonFlags
    mkBuildFlags
    (const extraArgs)
    NotInLibrary
  where
    progDb = defaultProgramDb
    setupOptions = defaultSetupScriptOptions{useDistPref = distPref}

    mkBuildFlags version = filterBuildFlags version config buildFlags'
    commonFlags = buildCommonFlags buildFlags
    buildFlags' =
      buildFlags
        { buildCommonFlags =
            commonFlags
              { setupVerbosity = toFlag verbosity
              , setupDistPref = toFlag distPref
              }
        }

-- | Make sure that we don't pass new flags to setup scripts compiled against
-- old versions of Cabal.
filterBuildFlags :: Version -> SavedConfig -> BuildFlags -> BuildFlags
filterBuildFlags version config buildFlags =
  let flags' = filterBuildFlags' version config buildFlags
   in flags'
        { buildCommonFlags =
            filterCommonFlags (buildCommonFlags flags') version
        }

filterBuildFlags' :: Version -> SavedConfig -> BuildFlags -> BuildFlags
filterBuildFlags' version config buildFlags
  | version >= mkVersion [1, 19, 1] = buildFlags_latest
  -- Cabal < 1.19.1 doesn't support 'build -j'.
  | otherwise = buildFlags_pre_1_19_1
  where
    buildFlags_pre_1_19_1 =
      buildFlags
        { buildNumJobs = NoFlag
        }
    buildFlags_latest =
      buildFlags
        { -- Take the 'jobs' setting config file into account.
          buildNumJobs =
            Flag . Just . determineNumJobs $
              (numJobsConfigFlag `mappend` numJobsCmdLineFlag)
        }
    numJobsConfigFlag = installNumJobs . savedInstallFlags $ config
    numJobsCmdLineFlag = buildNumJobs buildFlags

replAction :: ReplFlags -> [String] -> Action
replAction replFlags extraArgs globalFlags = do
  let common = replCommonFlags replFlags
      verbosity = fromFlagOrDefault normal $ setupVerbosity common
  config <- loadConfigOrSandboxConfig verbosity globalFlags
  distPref <- findSavedDistPref config (setupDistPref common)
  pkgDesc <- findPackageDesc Nothing
  let
    -- There is a .cabal file in the current directory: start a REPL and load
    -- the project's modules.
    onPkgDesc = do
      -- Calls 'configureAction' to do the real work, so nothing special has to
      -- be done to support sandboxes.
      _ <-
        reconfigure
          configureAction
          verbosity
          distPref
          NoFlag
          mempty
          []
          globalFlags
          config
      let progDb = defaultProgramDb
          setupOptions =
            defaultSetupScriptOptions
              { useCabalVersion = orLaterVersion $ mkVersion [1, 18, 0]
              , useDistPref = distPref
              }
          commonFlags = replCommonFlags replFlags
          replFlags' =
            replFlags
              { replCommonFlags =
                  commonFlags
                    { setupVerbosity = toFlag verbosity
                    , setupDistPref = toFlag distPref
                    }
              }

      nixShell verbosity (getSymbolicPath distPref) globalFlags config $
        setupWrapper
          verbosity
          setupOptions
          Nothing
          (Cabal.replCommand progDb)
          Cabal.replCommonFlags
          (const replFlags')
          (const extraArgs)
          NotInLibrary

    -- No .cabal file in the current directory: just start the REPL (possibly
    -- using the sandbox package DB).
    onNoPkgDesc = do
      let configFlags = savedConfigureFlags config
      (comp, platform, programDb) <- configCompilerAux' configFlags
      programDb' <-
        reconfigurePrograms
          verbosity
          (replProgramPaths replFlags)
          (replProgramArgs replFlags)
          programDb
      nixShell verbosity (getSymbolicPath distPref) globalFlags config $ do
        startInterpreter
          verbosity
          programDb'
          comp
          platform
          (configPackageDB' configFlags)

  either (const onNoPkgDesc) (const onPkgDesc) pkgDesc

installAction
  :: ( ConfigFlags
     , ConfigExFlags
     , InstallFlags
     , HaddockFlags
     , TestFlags
     , BenchmarkFlags
     )
  -> [String]
  -> Action
installAction (configFlags, _, installFlags, _, _, _) _ globalFlags
  | fromFlagOrDefault False (installOnly installFlags) = do
      let common = configCommonFlags configFlags
          verb = fromFlagOrDefault normal (setupVerbosity common)
      config <- loadConfigOrSandboxConfig verb globalFlags
      dist <- findSavedDistPref config (setupDistPref common)
      let setupOpts = defaultSetupScriptOptions{useDistPref = dist}
      setupWrapper
        verb
        setupOpts
        Nothing
        installCommand
        (const common)
        (const (mempty, mempty, mempty, mempty, mempty, mempty))
        (const [])
        NotInLibrary
installAction
  ( configFlags
    , configExFlags
    , installFlags
    , haddockFlags
    , testFlags
    , benchmarkFlags
    )
  extraArgs
  globalFlags = do
    let common = configCommonFlags configFlags
        verb = fromFlagOrDefault normal $ setupVerbosity common
    config <-
      updateInstallDirs (configUserInstall configFlags)
        <$> loadConfigOrSandboxConfig verb globalFlags

    dist <- findSavedDistPref config $ setupDistPref common

    do
      targets <- readUserTargets verb extraArgs

      let configFlags' =
            maybeForceTests installFlags' $
              savedConfigureFlags config
                `mappend` configFlags
                  { configCommonFlags =
                      (configCommonFlags configFlags)
                        { setupDistPref = toFlag dist
                        }
                  }
          configExFlags' =
            defaultConfigExFlags
              `mappend` savedConfigureExFlags config
              `mappend` configExFlags
          installFlags' =
            defaultInstallFlags
              `mappend` savedInstallFlags config
              `mappend` installFlags
          haddockFlags' =
            defaultHaddockFlags
              `mappend` savedHaddockFlags config
              `mappend` haddockFlags
                { haddockCommonFlags =
                    (haddockCommonFlags haddockFlags)
                      { setupDistPref = toFlag dist
                      }
                }
          testFlags' =
            Cabal.defaultTestFlags
              `mappend` savedTestFlags config
              `mappend` testFlags
                { testCommonFlags =
                    (testCommonFlags testFlags)
                      { setupDistPref = toFlag dist
                      }
                }
          benchmarkFlags' =
            Cabal.defaultBenchmarkFlags
              `mappend` savedBenchmarkFlags config
              `mappend` benchmarkFlags
                { benchmarkCommonFlags =
                    (benchmarkCommonFlags benchmarkFlags)
                      { setupDistPref = toFlag dist
                      }
                }
          globalFlags' = savedGlobalFlags config `mappend` globalFlags
      (comp, platform, progdb) <- configCompilerAux' configFlags'

      -- TODO: Redesign ProgramDB API to prevent such problems as #2241 in the
      -- future.
      progdb' <- configureAllKnownPrograms verb progdb

      withRepoContext verb globalFlags' $ \repoContext ->
        install
          verb
          (configPackageDB' configFlags')
          repoContext
          comp
          platform
          progdb'
          globalFlags'
          configFlags'
          configExFlags'
          installFlags'
          haddockFlags'
          testFlags'
          benchmarkFlags'
          targets
    where
      -- '--run-tests' implies '--enable-tests'.
      maybeForceTests installFlags' configFlags' =
        if fromFlagOrDefault False (installRunTests installFlags')
          then configFlags'{configTests = toFlag True}
          else configFlags'

testAction
  :: (BuildFlags, TestFlags)
  -> [String]
  -> GlobalFlags
  -> IO ()
testAction (buildFlags, testFlags) extraArgs globalFlags = do
  let verbosity = fromFlagOrDefault normal (setupVerbosity $ buildCommonFlags buildFlags)
  config <- loadConfigOrSandboxConfig verbosity globalFlags
  distPref <- findSavedDistPref config (setupDistPref $ testCommonFlags testFlags)
  let buildFlags' =
        buildFlags
          { buildCommonFlags =
              (buildCommonFlags buildFlags)
                { setupVerbosity = setupVerbosity $ testCommonFlags testFlags
                }
          }
      checkFlags = Check $ \_ flags@(configFlags, configExFlags) ->
        if fromFlagOrDefault False (configTests configFlags)
          then pure (mempty, flags)
          else do
            info verbosity "reconfiguring to enable tests"
            let flags' =
                  ( configFlags{configTests = toFlag True}
                  , configExFlags
                  )
            pure (Any True, flags')

  _ <-
    reconfigure
      configureAction
      verbosity
      distPref
      (buildNumJobs buildFlags')
      checkFlags
      []
      globalFlags
      config
  nixShell verbosity (getSymbolicPath distPref) globalFlags config $ do
    let setupOptions = defaultSetupScriptOptions{useDistPref = distPref}
        testFlags' =
          testFlags
            { testCommonFlags =
                (testCommonFlags testFlags){setupDistPref = toFlag distPref}
            }
        mbWorkDir = flagToMaybe $ testWorkingDir testFlags

    -- The package was just configured, so the LBI must be available.
    names <-
      componentNamesFromLBI
        verbosity
        mbWorkDir
        distPref
        "test suites"
        (\c -> case c of LBI.CTest{} -> True; _ -> False)
    let extraArgs'
          | null extraArgs = case names of
              ComponentNamesUnknown -> []
              ComponentNames names' ->
                [ Make.unUnqualComponentName name
                | LBI.CTestName name <- names'
                ]
          | otherwise = extraArgs

    build verbosity config distPref buildFlags' extraArgs'
    setupWrapper
      verbosity
      setupOptions
      Nothing
      Cabal.testCommand
      Cabal.testCommonFlags
      (const testFlags')
      (const extraArgs')
      NotInLibrary

data ComponentNames
  = ComponentNamesUnknown
  | ComponentNames [LBI.ComponentName]

-- | Return the names of all buildable components matching a given predicate.
componentNamesFromLBI
  :: Verbosity
  -> Maybe (SymbolicPath CWD (Dir Pkg))
  -- ^ working directory
  -> SymbolicPath Pkg (Dir Dist)
  -> String
  -> (LBI.Component -> Bool)
  -> IO ComponentNames
componentNamesFromLBI verbosity mbWorkDir distPref targetsDescr compPred = do
  eLBI <- tryGetPersistBuildConfig mbWorkDir distPref
  case eLBI of
    Left err -> case err of
      -- Note: the build config could have been generated by a custom setup
      -- script built against a different Cabal version, so it's crucial that
      -- we ignore the bad version error here.
      ConfigStateFileBadVersion _ _ _ -> return ComponentNamesUnknown
      _ -> dieWithException verbosity $ ConfigStateFileException (show err)
    Right lbi -> do
      let pkgDescr = LBI.localPkgDescr lbi
          names =
            map LBI.componentName
              . filter (buildable . LBI.componentBuildInfo)
              . filter compPred
              $ LBI.pkgComponents pkgDescr
      if null names
        then do
          notice verbosity $
            "Package has no buildable "
              ++ targetsDescr
              ++ "."
          exitSuccess -- See #3215.
        else return $! (ComponentNames names)

benchmarkAction
  :: (BuildFlags, BenchmarkFlags)
  -> [String]
  -> GlobalFlags
  -> IO ()
benchmarkAction
  (buildFlags, benchmarkFlags)
  extraArgs
  globalFlags = do
    let verbosity =
          fromFlagOrDefault
            normal
            (setupVerbosity $ buildCommonFlags buildFlags)

    config <- loadConfigOrSandboxConfig verbosity globalFlags
    distPref <- findSavedDistPref config (setupDistPref $ benchmarkCommonFlags benchmarkFlags)
    let buildFlags' =
          buildFlags
            { buildCommonFlags =
                (buildCommonFlags buildFlags)
                  { setupVerbosity = setupVerbosity $ benchmarkCommonFlags benchmarkFlags
                  }
            }

    let checkFlags = Check $ \_ flags@(configFlags, configExFlags) ->
          if fromFlagOrDefault False (configBenchmarks configFlags)
            then pure (mempty, flags)
            else do
              info verbosity "reconfiguring to enable benchmarks"
              let flags' =
                    ( configFlags{configBenchmarks = toFlag True}
                    , configExFlags
                    )
              pure (Any True, flags')

    config' <-
      reconfigure
        configureAction
        verbosity
        distPref
        (buildNumJobs buildFlags')
        checkFlags
        []
        globalFlags
        config
    nixShell verbosity (getSymbolicPath distPref) globalFlags config $ do
      let setupOptions = defaultSetupScriptOptions{useDistPref = distPref}
          benchmarkFlags' =
            benchmarkFlags
              { benchmarkCommonFlags =
                  (benchmarkCommonFlags benchmarkFlags)
                    { setupDistPref = toFlag distPref
                    }
              }
          mbWorkDir = flagToMaybe $ benchmarkWorkingDir benchmarkFlags

      -- The package was just configured, so the LBI must be available.
      names <-
        componentNamesFromLBI
          verbosity
          mbWorkDir
          distPref
          "benchmarks"
          (\c -> case c of LBI.CBench{} -> True; _ -> False)
      let extraArgs'
            | null extraArgs = case names of
                ComponentNamesUnknown -> []
                ComponentNames names' ->
                  [ Make.unUnqualComponentName name
                  | LBI.CBenchName name <- names'
                  ]
            | otherwise = extraArgs

      build verbosity config' distPref buildFlags' extraArgs'
      setupWrapper
        verbosity
        setupOptions
        Nothing
        Cabal.benchmarkCommand
        Cabal.benchmarkCommonFlags
        (const benchmarkFlags')
        (const extraArgs')
        NotInLibrary

haddockAction :: HaddockFlags -> [String] -> Action
haddockAction haddockFlags extraArgs globalFlags = do
  let common = haddockCommonFlags haddockFlags
      verbosity = fromFlag $ setupVerbosity common
  config <- loadConfigOrSandboxConfig verbosity globalFlags
  distPref <- findSavedDistPref config (setupDistPref common)
  config' <-
    reconfigure
      configureAction
      verbosity
      distPref
      NoFlag
      mempty
      []
      globalFlags
      config
  let mbWorkDir = flagToMaybe $ setupWorkingDir common
  nixShell verbosity (getSymbolicPath distPref) globalFlags config $ do
    let haddockFlags' =
          defaultHaddockFlags
            `mappend` savedHaddockFlags config'
            `mappend` haddockFlags
              { haddockCommonFlags =
                  (haddockCommonFlags haddockFlags)
                    { setupDistPref = toFlag distPref
                    }
              }
        setupScriptOptions =
          defaultSetupScriptOptions
            { useDistPref = distPref
            }
    setupWrapper
      verbosity
      setupScriptOptions
      Nothing
      haddockCommand
      haddockCommonFlags
      (const haddockFlags')
      (const extraArgs)
      NotInLibrary
    when (haddockForHackage haddockFlags == Flag ForHackage) $ do
      pkg <- fmap LBI.localPkgDescr (getPersistBuildConfig mbWorkDir distPref)
      let dest = getSymbolicPath distPref </> name <.> "tar.gz"
          name = display (packageId pkg) ++ "-docs"
          docDir = getSymbolicPath distPref </> "doc" </> "html"
      createTarGzFile dest docDir name
      notice verbosity $ "Documentation tarball created: " ++ dest

cleanAction :: CleanFlags -> [String] -> Action
cleanAction cleanFlags extraArgs globalFlags = do
  let common = cleanCommonFlags cleanFlags
      verbosity = fromFlagOrDefault normal $ setupVerbosity common
  load <- try (loadConfigOrSandboxConfig verbosity globalFlags)
  let config = either (\(SomeException _) -> mempty) id load
  distPref <- findSavedDistPref config $ setupDistPref common
  let setupScriptOptions =
        defaultSetupScriptOptions
          { useDistPref = distPref
          , useWin32CleanHack = True
          }
      cleanFlags' =
        cleanFlags
          { cleanCommonFlags =
              (cleanCommonFlags cleanFlags)
                { setupDistPref = toFlag distPref
                }
          }
  setupWrapper
    verbosity
    setupScriptOptions
    Nothing
    cleanCommand
    cleanCommonFlags
    (const cleanFlags')
    (const extraArgs)
    NotInLibrary

listAction :: ListFlags -> [String] -> Action
listAction listFlags extraArgs globalFlags = do
  let verbosity = fromFlag (listVerbosity listFlags)
  config <- loadConfigOrSandboxConfig verbosity globalFlags
  let configFlags' = savedConfigureFlags config
      configFlags =
        configFlags'
          { configPackageDBs =
              configPackageDBs configFlags'
                `mappend` listPackageDBs listFlags
          , configHcPath = listHcPath listFlags
          }
      globalFlags' = savedGlobalFlags config `mappend` globalFlags
  compProgdb <-
    if listNeedsCompiler listFlags
      then do
        (comp, _, progdb) <- configCompilerAux' configFlags
        return (Just (comp, progdb))
      else return Nothing
  withRepoContext verbosity globalFlags' $ \repoContext ->
    List.list
      verbosity
      (configPackageDB' configFlags)
      repoContext
      compProgdb
      listFlags
      extraArgs

infoAction :: InfoFlags -> [String] -> Action
infoAction infoFlags extraArgs globalFlags = do
  let verbosity = fromFlag (infoVerbosity infoFlags)
  targets <- readUserTargets verbosity extraArgs
  config <- loadConfigOrSandboxConfig verbosity globalFlags
  let configFlags' = savedConfigureFlags config
      configFlags =
        configFlags'
          { configPackageDBs =
              configPackageDBs configFlags'
                `mappend` infoPackageDBs infoFlags
          }
      globalFlags' = savedGlobalFlags config `mappend` globalFlags
  (comp, _, progdb) <- configCompilerAuxEx configFlags
  withRepoContext verbosity globalFlags' $ \repoContext ->
    List.info
      verbosity
      (configPackageDB' configFlags)
      repoContext
      comp
      progdb
      globalFlags'
      infoFlags
      targets

fetchAction :: FetchFlags -> [String] -> Action
fetchAction fetchFlags extraArgs globalFlags = do
  let verbosity = fromFlag (fetchVerbosity fetchFlags)
  targets <- readUserTargets verbosity extraArgs
  config <- loadConfig verbosity (globalConfigFile globalFlags)
  let configFlags = savedConfigureFlags config
      globalFlags' = savedGlobalFlags config `mappend` globalFlags
  (comp, platform, progdb) <- configCompilerAux' configFlags
  withRepoContext verbosity globalFlags' $ \repoContext ->
    fetch
      verbosity
      (configPackageDB' configFlags)
      repoContext
      comp
      platform
      progdb
      globalFlags'
      fetchFlags
      targets

freezeAction :: FreezeFlags -> [String] -> Action
freezeAction freezeFlags _extraArgs globalFlags = do
  let verbosity = fromFlag (freezeVerbosity freezeFlags)
  config <- loadConfigOrSandboxConfig verbosity globalFlags
  distPref <- findSavedDistPref config NoFlag
  nixShell verbosity (getSymbolicPath distPref) globalFlags config $ do
    let configFlags = savedConfigureFlags config
        globalFlags' = savedGlobalFlags config `mappend` globalFlags
    (comp, platform, progdb) <- configCompilerAux' configFlags

    withRepoContext verbosity globalFlags' $ \repoContext ->
      freeze
        verbosity
        (configPackageDB' configFlags)
        repoContext
        comp
        platform
        progdb
        globalFlags'
        freezeFlags

genBoundsAction :: FreezeFlags -> [String] -> GlobalFlags -> IO ()
genBoundsAction freezeFlags _extraArgs globalFlags = do
  let verbosity = fromFlag (freezeVerbosity freezeFlags)
  config <- loadConfigOrSandboxConfig verbosity globalFlags
  distPref <- findSavedDistPref config NoFlag
  nixShell verbosity (getSymbolicPath distPref) globalFlags config $ do
    let configFlags = savedConfigureFlags config
        globalFlags' = savedGlobalFlags config `mappend` globalFlags
    (comp, platform, progdb) <- configCompilerAux' configFlags

    withRepoContext verbosity globalFlags' $ \repoContext ->
      genBounds
        verbosity
        (configPackageDB' configFlags)
        repoContext
        comp
        platform
        progdb
        globalFlags'
        freezeFlags

uploadAction :: UploadFlags -> [String] -> Action
uploadAction uploadFlags extraArgs globalFlags = do
  config <- loadConfig verbosity (globalConfigFile globalFlags)
  let uploadFlags' = savedUploadFlags config `mappend` uploadFlags
      globalFlags' = savedGlobalFlags config `mappend` globalFlags
      tarfiles = extraArgs
  when (null tarfiles && not (fromFlag (uploadDoc uploadFlags'))) $
    dieWithException verbosity UploadAction
  checkTarFiles extraArgs
  maybe_password <-
    case uploadPasswordCmd uploadFlags' of
      Flag (xs : xss) ->
        Just . Password
          <$> getProgramInvocationOutput
            verbosity
            (simpleProgramInvocation xs xss)
      _ -> pure $ flagToMaybe $ uploadPassword uploadFlags'
  withRepoContext verbosity globalFlags' $ \repoContext -> do
    if fromFlag (uploadDoc uploadFlags')
      then do
        when (length tarfiles > 1) $
          dieWithException verbosity UploadActionDocumentation
        tarfile <- maybe (generateDocTarball config) return $ listToMaybe tarfiles
        Upload.uploadDoc
          verbosity
          repoContext
          (flagToMaybe $ uploadToken uploadFlags')
          (flagToMaybe $ uploadUsername uploadFlags')
          maybe_password
          (fromFlag (uploadCandidate uploadFlags'))
          tarfile
      else do
        Upload.upload
          verbosity
          repoContext
          (flagToMaybe $ uploadToken uploadFlags')
          (flagToMaybe $ uploadUsername uploadFlags')
          maybe_password
          (fromFlag (uploadCandidate uploadFlags'))
          tarfiles
  where
    verbosity = fromFlag (uploadVerbosity uploadFlags)
    checkTarFiles tarfiles
      | not (null otherFiles) =
          dieWithException verbosity $ UploadActionOnlyArchives otherFiles
      | otherwise =
          sequence_
            [ do
              exists <- doesFileExist tarfile
              unless exists $ dieWithException verbosity $ FileNotFound tarfile
            | tarfile <- tarfiles
            ]
      where
        otherFiles = filter (not . isTarGzFile) tarfiles
        isTarGzFile file = case splitExtension file of
          (file', ".gz") -> takeExtension file' == ".tar"
          _ -> False
    generateDocTarball config = do
      notice verbosity $
        "No documentation tarball specified. "
          ++ "Building a documentation tarball with default settings...\n"
          ++ "If you need to customise Haddock options, "
          ++ "run 'haddock --for-hackage' first "
          ++ "to generate a documentation tarball."
      haddockAction
        (defaultHaddockFlags{haddockForHackage = Flag ForHackage})
        []
        globalFlags
      distPref <- findSavedDistPref config NoFlag
      let mbWorkDir = flagToMaybe $ configWorkingDir $ savedConfigureFlags config
      pkg <- fmap LBI.localPkgDescr (getPersistBuildConfig mbWorkDir distPref)
      return $ getSymbolicPath distPref </> display (packageId pkg) ++ "-docs" <.> "tar.gz"

checkAction :: CheckFlags -> [String] -> Action
checkAction checkFlags extraArgs _globalFlags = do
  let verbosityFlag = checkVerbosity checkFlags
      verbosity = fromFlag verbosityFlag
  unless (null extraArgs) $
    dieWithException verbosity $
      CheckAction extraArgs
  allOk <- Check.check (fromFlag verbosityFlag) (checkIgnore checkFlags)
  unless allOk exitFailure

formatAction :: Flag Verbosity -> [String] -> Action
formatAction verbosityFlag extraArgs _globalFlags = do
  let verbosity = fromFlag verbosityFlag
  path <- case extraArgs of
    [] -> relativeSymbolicPath <$> tryFindPackageDesc verbosity Nothing
    (p : _) -> return $ makeSymbolicPath p
  pkgDesc <- readGenericPackageDescription verbosity Nothing path
  -- Uses 'writeFileAtomic' under the hood.
  writeGenericPackageDescription (getSymbolicPath path) pkgDesc

reportAction :: ReportFlags -> [String] -> Action
reportAction reportFlags extraArgs globalFlags = do
  let verbosity = fromFlag (reportVerbosity reportFlags)
  unless (null extraArgs) $
    dieWithException verbosity $
      ReportAction extraArgs
  config <- loadConfig verbosity (globalConfigFile globalFlags)
  let globalFlags' = savedGlobalFlags config `mappend` globalFlags
      reportFlags' = savedReportFlags config `mappend` reportFlags

  withRepoContext verbosity globalFlags' $ \repoContext ->
    Upload.report
      verbosity
      repoContext
      (flagToMaybe $ reportToken reportFlags')
      (flagToMaybe $ reportUsername reportFlags')
      (flagToMaybe $ reportPassword reportFlags')

runAction :: BuildFlags -> [String] -> Action
runAction buildFlags extraArgs globalFlags = do
  let common = buildCommonFlags buildFlags
      verbosity = fromFlagOrDefault normal $ setupVerbosity common
  config <- loadConfigOrSandboxConfig verbosity globalFlags
  distPref <- findSavedDistPref config $ setupDistPref common
  config' <-
    reconfigure
      configureAction
      verbosity
      distPref
      (buildNumJobs buildFlags)
      mempty
      []
      globalFlags
      config
  let mbWorkDir = flagToMaybe $ setupWorkingDir common
  nixShell verbosity (getSymbolicPath distPref) globalFlags config $ do
    lbi <- getPersistBuildConfig mbWorkDir distPref
    (exe, exeArgs) <- splitRunArgs verbosity lbi extraArgs

    build verbosity config' distPref buildFlags ["exe:" ++ display (exeName exe)]
    run verbosity lbi exe exeArgs

getAction :: GetFlags -> [String] -> Action
getAction getFlags extraArgs globalFlags = do
  let verbosity = fromFlag (getVerbosity getFlags)
  targets <- readUserTargets verbosity extraArgs
  config <- loadConfigOrSandboxConfig verbosity globalFlags
  let globalFlags' = savedGlobalFlags config `mappend` globalFlags
  withRepoContext verbosity (savedGlobalFlags config) $ \repoContext ->
    get
      verbosity
      repoContext
      globalFlags'
      getFlags
      targets

unpackAction :: GetFlags -> [String] -> Action
unpackAction getFlags extraArgs globalFlags = do
  getAction getFlags extraArgs globalFlags

initAction :: InitFlags -> [String] -> Action
initAction initFlags extraArgs globalFlags = do
  -- it takes the first value within extraArgs (if there's one)
  -- and uses it as the root directory for the new project
  case extraArgs of
    [] -> initAction'
    [projectDir] -> do
      createDirectoryIfMissingVerbose verbosity True projectDir
      withCurrentDirectory projectDir initAction'
    _ -> dieWithException verbosity InitAction
  where
    initAction' = do
      confFlags <- loadConfigOrSandboxConfig verbosity globalFlags
      -- override with `--with-compiler` from CLI if available
      let confFlags' = savedConfigureFlags confFlags `mappend` compFlags
          initFlags' = savedInitFlags confFlags `mappend` initFlags
          globalFlags' = savedGlobalFlags confFlags `mappend` globalFlags

      (comp, _, progdb) <- configCompilerAux' confFlags'

      withRepoContext verbosity globalFlags' $ \repoContext ->
        initCmd
          verbosity
          (configPackageDB' confFlags')
          repoContext
          comp
          progdb
          initFlags'

    verbosity = fromFlag (initVerbosity initFlags)
    compFlags = mempty{configHcPath = initHcPath initFlags}

userConfigAction :: UserConfigFlags -> [String] -> Action
userConfigAction ucflags extraArgs globalFlags = do
  let verbosity = fromFlag (userConfigVerbosity ucflags)
      frc = fromFlag (userConfigForce ucflags)
      extraLines = fromFlag (userConfigAppendLines ucflags)
  case extraArgs of
    ("init" : _) -> do
      path <- configFile
      fileExists <- doesFileExist path
      if (not fileExists || (fileExists && frc))
        then void $ createDefaultConfigFile verbosity extraLines path
        else dieWithException verbosity $ UserConfigAction path
    ("diff" : _) -> traverse_ putStrLn =<< userConfigDiff verbosity globalFlags extraLines
    ("update" : _) -> userConfigUpdate verbosity globalFlags extraLines
    -- Error handling.
    [] -> dieWithException verbosity SpecifySubcommand
    _ -> dieWithException verbosity $ UnknownUserConfigSubcommand extraArgs
  where
    configFile = getConfigFilePath (globalConfigFile globalFlags)

-- | Used as an entry point when cabal-install needs to invoke itself
-- as a setup script. This can happen e.g. when doing parallel builds.
actAsSetupAction :: ActAsSetupFlags -> [String] -> Action
actAsSetupAction actAsSetupFlags args _globalFlags =
  let bt = fromFlag (actAsSetupBuildType actAsSetupFlags)
   in case bt of
        Simple -> Simple.defaultMainArgs args
        Configure ->
          Simple.defaultMainWithSetupHooksArgs
            Simple.autoconfSetupHooks
            args
        Make -> Make.defaultMainArgs args
        Hooks -> error "actAsSetupAction Hooks"
        Custom -> error "actAsSetupAction Custom"

manpageAction :: [CommandSpec action] -> ManpageFlags -> [String] -> Action
manpageAction commands flags extraArgs _ = do
  let verbosity = fromFlag (manpageVerbosity flags)
  unless (null extraArgs) $
    dieWithException verbosity $
      ManpageAction extraArgs
  pname <- getProgName
  let cabalCmd =
        if takeExtension pname == ".exe"
          then dropExtension pname
          else pname
  manpageCmd cabalCmd commands flags
