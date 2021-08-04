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
         ( GlobalFlags(..), globalCommand, withRepoContext
         , ConfigFlags(..)
         , ConfigExFlags(..), defaultConfigExFlags, configureExCommand
         , reconfigureCommand
         , configCompilerAux', configPackageDB'
         , BuildFlags(..)
         , buildCommand, replCommand, testCommand, benchmarkCommand
         , InstallFlags(..), defaultInstallFlags
         , installCommand
         , FetchFlags(..), fetchCommand
         , FreezeFlags(..), freezeCommand
         , genBoundsCommand
         , GetFlags(..), getCommand, unpackCommand
         , checkCommand
         , formatCommand
         , ListFlags(..), listCommand, listNeedsCompiler
         , InfoFlags(..), infoCommand
         , UploadFlags(..), uploadCommand
         , ReportFlags(..), reportCommand
         , runCommand
         , InitFlags(initVerbosity, initHcPath), initCommand
         , ActAsSetupFlags(..), actAsSetupCommand
         , UserConfigFlags(..), userConfigCommand
         , reportCommand
         , manpageCommand
         , haddockCommand
         , cleanCommand
         , copyCommand
         , registerCommand
         )
import Distribution.Simple.Setup
         ( HaddockTarget(..)
         , HaddockFlags(..), defaultHaddockFlags
         , HscolourFlags(..), hscolourCommand
         , ReplFlags(..)
         , CopyFlags(..)
         , RegisterFlags(..)
         , CleanFlags(..)
         , TestFlags(..), BenchmarkFlags(..)
         , Flag(..), fromFlag, fromFlagOrDefault, flagToMaybe, toFlag
         , configAbsolutePaths
         )

import Prelude ()
import Distribution.Client.Compat.Prelude hiding (get)

import Distribution.Client.SetupWrapper
         ( setupWrapper, SetupScriptOptions(..), defaultSetupScriptOptions )
import Distribution.Client.Config
         ( SavedConfig(..), loadConfig, defaultConfigFile, userConfigDiff
         , userConfigUpdate, createDefaultConfigFile, getConfigFilePath )
import Distribution.Client.Targets
         ( readUserTargets )
import qualified Distribution.Client.List as List
         ( list, info )

import qualified Distribution.Client.CmdConfigure as CmdConfigure
import qualified Distribution.Client.CmdUpdate    as CmdUpdate
import qualified Distribution.Client.CmdBuild     as CmdBuild
import qualified Distribution.Client.CmdRepl      as CmdRepl
import qualified Distribution.Client.CmdFreeze    as CmdFreeze
import qualified Distribution.Client.CmdHaddock   as CmdHaddock
import qualified Distribution.Client.CmdInstall   as CmdInstall
import qualified Distribution.Client.CmdRun       as CmdRun
import qualified Distribution.Client.CmdTest      as CmdTest
import qualified Distribution.Client.CmdBench     as CmdBench
import qualified Distribution.Client.CmdExec      as CmdExec
import qualified Distribution.Client.CmdClean     as CmdClean
import qualified Distribution.Client.CmdSdist     as CmdSdist
import qualified Distribution.Client.CmdListBin   as CmdListBin
import qualified Distribution.Client.CmdOutdated  as CmdOutdated
import           Distribution.Client.CmdLegacy

import Distribution.Client.Install            (install)
import Distribution.Client.Configure          (configure, writeConfigFlags)
import Distribution.Client.Fetch              (fetch)
import Distribution.Client.Freeze             (freeze)
import Distribution.Client.GenBounds          (genBounds)
import Distribution.Client.Check as Check     (check)
--import Distribution.Client.Clean            (clean)
import qualified Distribution.Client.Upload as Upload
import Distribution.Client.Run                (run, splitRunArgs)
import Distribution.Client.Get                (get)
import Distribution.Client.Reconfigure        (Check(..), reconfigure)
import Distribution.Client.Nix                (nixInstantiate
                                              ,nixShell
                                              )
import Distribution.Client.Sandbox            (loadConfigOrSandboxConfig
                                              ,findSavedDistPref
                                              ,updateInstallDirs)
import Distribution.Client.Tar                (createTarGzFile)
import Distribution.Client.Types.Credentials  (Password (..))
import Distribution.Client.Init               (initCmd)
import Distribution.Client.Manpage            (manpageCmd)
import Distribution.Client.ManpageFlags       (ManpageFlags (..))
import Distribution.Client.Utils              (determineNumJobs
                                              ,relaxEncodingErrors
                                              ,cabalInstallVersion
                                              )

import Distribution.Package (packageId)
import Distribution.PackageDescription
         ( BuildType(..), Executable(..), buildable )
import Distribution.PackageDescription.Parsec ( readGenericPackageDescription )

import Distribution.PackageDescription.PrettyPrint
         ( writeGenericPackageDescription )
import qualified Distribution.Simple as Simple
import qualified Distribution.Make as Make
import qualified Distribution.Types.UnqualComponentName as Make
import Distribution.Simple.Build
         ( startInterpreter )
import Distribution.Simple.Command
         ( CommandParse(..), CommandUI(..), Command, CommandSpec(..)
         , CommandType(..), commandsRun, commandAddAction, hiddenCommand
         , commandFromSpec, commandShowOptions )
import Distribution.Simple.Compiler (PackageDBStack)
import Distribution.Simple.Configure
         ( configCompilerAuxEx, ConfigStateFileError(..)
         , getPersistBuildConfig, interpretPackageDbFlags
         , tryGetPersistBuildConfig )
import qualified Distribution.Simple.LocalBuildInfo as LBI
import Distribution.Simple.Program (defaultProgramDb
                                   ,configureAllKnownPrograms
                                   ,simpleProgramInvocation
                                   ,getProgramInvocationOutput)
import Distribution.Simple.Program.Db (reconfigurePrograms)
import qualified Distribution.Simple.Setup as Cabal
import Distribution.Simple.Utils
         ( cabalVersion, die', dieNoVerbosity, info, notice, topHandler
         , findPackageDesc, tryFindPackageDesc )
import Distribution.Text
         ( display )
import Distribution.Verbosity as Verbosity
         ( normal )
import Distribution.Version
         ( Version, mkVersion, orLaterVersion )

import Distribution.Compat.ResponseFile
import System.Environment       (getArgs, getProgName)
import System.FilePath          ( dropExtension, splitExtension
                                , takeExtension, (</>), (<.>) )
import System.IO                ( BufferMode(LineBuffering), hSetBuffering
                                , stderr, stdout )
import System.Directory         (doesFileExist, getCurrentDirectory)
import Data.Monoid              (Any(..))
import Control.Exception        (try)


-- | Entry point
--
main :: IO ()
main = do
  -- Enable line buffering so that we can get fast feedback even when piped.
  -- This is especially important for CI and build systems.
  hSetBuffering stdout LineBuffering
  -- If the locale encoding for CLI doesn't support all Unicode characters,
  -- printing to it may fail unless we relax the handling of encoding errors
  -- when writing to stderr and stdout.
  relaxEncodingErrors stdout
  relaxEncodingErrors stderr
  (args0, args1) <- break (== "--") <$> getArgs
  mainWorker =<< (++ args1) <$> expandResponse args0

mainWorker :: [String] -> IO ()
mainWorker args = do
  maybeScriptAndArgs <- case args of
    []     -> return Nothing
    (h:tl) -> (\b -> if b then Just (h:|tl) else Nothing) <$> CmdRun.validScript h

  topHandler $
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
          CommandErrors   errs           -> maybe (printErrors errs) go maybeScriptAndArgs where
            go (script:|scriptArgs) = CmdRun.handleShebang script scriptArgs
          CommandReadyToGo action        -> action globalFlags

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
      unless exists $
          putStrLn $ "This file will be generated with sensible "
                  ++ "defaults if you run 'cabal update'."
    printOptionsList = putStr . unlines
    printErrors errs = dieNoVerbosity $ intercalate "\n" errs
    printNumericVersion = putStrLn $ display cabalInstallVersion
    printVersion        = putStrLn $ "cabal-install version "
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
      , hiddenCmd  unpackCommand unpackAction
      , regularCmd checkCommand checkAction
      , regularCmd uploadCommand uploadAction
      , regularCmd reportCommand reportAction
      , regularCmd initCommand initAction
      , regularCmd userConfigCommand userConfigAction
      , regularCmd genBoundsCommand genBoundsAction
      , regularCmd CmdOutdated.outdatedCommand CmdOutdated.outdatedAction
      , wrapperCmd hscolourCommand hscolourVerbosity hscolourDistPref
      , hiddenCmd  formatCommand formatAction
      , hiddenCmd  actAsSetupCommand actAsSetupAction
      , hiddenCmd  manpageCommand (manpageAction commandSpecs)
      , regularCmd CmdListBin.listbinCommand     CmdListBin.listbinAction

      ] ++ concat
      [ newCmd  CmdConfigure.configureCommand CmdConfigure.configureAction
      , newCmd  CmdUpdate.updateCommand       CmdUpdate.updateAction
      , newCmd  CmdBuild.buildCommand         CmdBuild.buildAction
      , newCmd  CmdRepl.replCommand           CmdRepl.replAction
      , newCmd  CmdFreeze.freezeCommand       CmdFreeze.freezeAction
      , newCmd  CmdHaddock.haddockCommand     CmdHaddock.haddockAction
      , newCmd  CmdInstall.installCommand     CmdInstall.installAction
      , newCmd  CmdRun.runCommand             CmdRun.runAction
      , newCmd  CmdTest.testCommand           CmdTest.testAction
      , newCmd  CmdBench.benchCommand         CmdBench.benchAction
      , newCmd  CmdExec.execCommand           CmdExec.execAction
      , newCmd  CmdClean.cleanCommand         CmdClean.cleanAction
      , newCmd  CmdSdist.sdistCommand         CmdSdist.sdistAction

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
      , legacyWrapperCmd copyCommand copyVerbosity copyDistPref
      , legacyWrapperCmd registerCommand regVerbosity regDistPref
      , legacyCmd reconfigureCommand reconfigureAction
      ]

type Action = GlobalFlags -> IO ()

-- Duplicated in Distribution.Client.CmdLegacy. Any changes must be
-- reflected there, as well.
regularCmd :: CommandUI flags -> (flags -> [String] -> action)
           -> CommandSpec action
regularCmd ui action =
  CommandSpec ui ((flip commandAddAction) action) NormalCommand

hiddenCmd :: CommandUI flags -> (flags -> [String] -> action)
          -> CommandSpec action
hiddenCmd ui action =
  CommandSpec ui (\ui' -> hiddenCommand (commandAddAction ui' action))
  HiddenCommand

wrapperCmd :: Monoid flags => CommandUI flags -> (flags -> Flag Verbosity)
           -> (flags -> Flag String) -> CommandSpec Action
wrapperCmd ui verbosity distPref =
  CommandSpec ui (\ui' -> wrapperAction ui' verbosity distPref) NormalCommand

wrapperAction :: Monoid flags
              => CommandUI flags
              -> (flags -> Flag Verbosity)
              -> (flags -> Flag String)
              -> Command Action
wrapperAction command verbosityFlag distPrefFlag =
  commandAddAction command
    { commandDefaultFlags = mempty } $ \flags extraArgs globalFlags -> do
    let verbosity = fromFlagOrDefault normal (verbosityFlag flags)
    load <- try (loadConfigOrSandboxConfig verbosity globalFlags)
    let config = either (\(SomeException _) -> mempty) id load
    distPref <- findSavedDistPref config (distPrefFlag flags)
    let setupScriptOptions = defaultSetupScriptOptions { useDistPref = distPref }
    setupWrapper verbosity setupScriptOptions Nothing
                 command (const flags) (const extraArgs)

configureAction :: (ConfigFlags, ConfigExFlags)
                -> [String] -> Action
configureAction (configFlags, configExFlags) extraArgs globalFlags = do
  let verbosity = fromFlagOrDefault normal (configVerbosity configFlags)
  config <- updateInstallDirs (configUserInstall configFlags)
                          <$> loadConfigOrSandboxConfig verbosity globalFlags
  distPref <- findSavedDistPref config (configDistPref configFlags)
  nixInstantiate verbosity distPref True globalFlags config
  nixShell verbosity distPref globalFlags config $ do
    let configFlags'   = savedConfigureFlags   config `mappend` configFlags
        configExFlags' = savedConfigureExFlags config `mappend` configExFlags
        globalFlags'   = savedGlobalFlags      config `mappend` globalFlags
    (comp, platform, progdb) <- configCompilerAuxEx configFlags'

    writeConfigFlags verbosity distPref (configFlags', configExFlags')

    -- What package database(s) to use
    let packageDBs :: PackageDBStack
        packageDBs
          = interpretPackageDbFlags
            (fromFlag (configUserInstall configFlags'))
            (configPackageDBs configFlags')

    withRepoContext verbosity globalFlags' $ \repoContext ->
        configure verbosity packageDBs repoContext
                  comp platform progdb configFlags' configExFlags' extraArgs

reconfigureAction :: (ConfigFlags, ConfigExFlags)
                  -> [String] -> Action
reconfigureAction flags@(configFlags, _) _ globalFlags = do
  let verbosity = fromFlagOrDefault normal (configVerbosity configFlags)
  config <- updateInstallDirs (configUserInstall configFlags)
                          <$> loadConfigOrSandboxConfig verbosity globalFlags
  distPref <- findSavedDistPref config (configDistPref configFlags)
  let checkFlags = Check $ \_ saved -> do
        let flags' = saved <> flags
        unless (saved == flags') $ info verbosity message
        pure (Any True, flags')
        where
          -- This message is correct, but not very specific: it will list all
          -- of the new flags, even if some have not actually changed. The
          -- *minimal* set of changes is more difficult to determine.
          message =
            "flags changed: "
            ++ unwords (commandShowOptions configureExCommand flags)
  nixInstantiate verbosity distPref True globalFlags config
  _ <-
    reconfigure configureAction
    verbosity distPref NoFlag
    checkFlags [] globalFlags config
  pure ()

buildAction :: BuildFlags -> [String] -> Action
buildAction buildFlags extraArgs globalFlags = do
  let verbosity = fromFlagOrDefault normal (buildVerbosity buildFlags)
  config <- loadConfigOrSandboxConfig verbosity globalFlags
  distPref <- findSavedDistPref config (buildDistPref buildFlags)
  -- Calls 'configureAction' to do the real work, so nothing special has to be
  -- done to support sandboxes.
  config' <-
    reconfigure configureAction
    verbosity distPref (buildNumJobs buildFlags)
    mempty [] globalFlags config
  nixShell verbosity distPref globalFlags config $ do
    build verbosity config' distPref buildFlags extraArgs


-- | Actually do the work of building the package. This is separate from
-- 'buildAction' so that 'testAction' and 'benchmarkAction' do not invoke
-- 'reconfigure' twice.
build :: Verbosity -> SavedConfig -> FilePath -> BuildFlags -> [String] -> IO ()
build verbosity config distPref buildFlags extraArgs =
  setupWrapper verbosity setupOptions Nothing
               (Cabal.buildCommand progDb) mkBuildFlags (const extraArgs)
  where
    progDb       = defaultProgramDb
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
  | version >= mkVersion [1,19,1] = buildFlags_latest
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


replAction :: ReplFlags -> [String] -> Action
replAction replFlags extraArgs globalFlags = do
  let verbosity = fromFlagOrDefault normal (replVerbosity replFlags)
  config <- loadConfigOrSandboxConfig verbosity globalFlags
  distPref <- findSavedDistPref config (replDistPref replFlags)
  cwd     <- getCurrentDirectory
  pkgDesc <- findPackageDesc cwd
  let
    -- There is a .cabal file in the current directory: start a REPL and load
    -- the project's modules.
    onPkgDesc = do
      -- Calls 'configureAction' to do the real work, so nothing special has to
      -- be done to support sandboxes.
      _ <-
        reconfigure configureAction
        verbosity distPref NoFlag
        mempty [] globalFlags config
      let progDb = defaultProgramDb
          setupOptions = defaultSetupScriptOptions
            { useCabalVersion = orLaterVersion $ mkVersion [1,18,0]
            , useDistPref     = distPref
            }
          replFlags'   = replFlags
            { replVerbosity = toFlag verbosity
            , replDistPref  = toFlag distPref
            }

      nixShell verbosity distPref globalFlags config $
        setupWrapper verbosity setupOptions Nothing (Cabal.replCommand progDb) (const replFlags') (const extraArgs)

    -- No .cabal file in the current directory: just start the REPL (possibly
    -- using the sandbox package DB).
    onNoPkgDesc = do
      let configFlags = savedConfigureFlags config
      (comp, platform, programDb) <- configCompilerAux' configFlags
      programDb' <- reconfigurePrograms verbosity
                                        (replProgramPaths replFlags)
                                        (replProgramArgs replFlags)
                                        programDb
      nixShell verbosity distPref globalFlags config $ do
        startInterpreter verbosity programDb' comp platform
                        (configPackageDB' configFlags)

  either (const onNoPkgDesc) (const onPkgDesc) pkgDesc

installAction :: ( ConfigFlags, ConfigExFlags, InstallFlags
                 , HaddockFlags, TestFlags, BenchmarkFlags )
              -> [String] -> Action
installAction (configFlags, _, installFlags, _, _, _) _ globalFlags
  | fromFlagOrDefault False (installOnly installFlags) = do
      let verb = fromFlagOrDefault normal (configVerbosity configFlags)
      config <- loadConfigOrSandboxConfig verb globalFlags
      dist <- findSavedDistPref config (configDistPref configFlags)
      let setupOpts = defaultSetupScriptOptions { useDistPref = dist }
      setupWrapper
        verb setupOpts Nothing
        installCommand (const (mempty, mempty, mempty, mempty, mempty, mempty))
                       (const [])

installAction
  ( configFlags, configExFlags, installFlags
  , haddockFlags, testFlags, benchmarkFlags )
  extraArgs globalFlags = do
  let verb = fromFlagOrDefault normal (configVerbosity configFlags)
  config <- updateInstallDirs (configUserInstall configFlags)
                          <$> loadConfigOrSandboxConfig verb globalFlags

  dist <- findSavedDistPref config (configDistPref configFlags)

  do
    targets <- readUserTargets verb extraArgs

    let configFlags'    = maybeForceTests installFlags' $
                          savedConfigureFlags   config `mappend`
                          configFlags { configDistPref = toFlag dist }
        configExFlags'  = defaultConfigExFlags         `mappend`
                          savedConfigureExFlags config `mappend` configExFlags
        installFlags'   = defaultInstallFlags          `mappend`
                          savedInstallFlags     config `mappend` installFlags
        haddockFlags'   = defaultHaddockFlags          `mappend`
                          savedHaddockFlags     config `mappend`
                          haddockFlags { haddockDistPref = toFlag dist }
        testFlags'      = Cabal.defaultTestFlags       `mappend`
                          savedTestFlags        config `mappend`
                          testFlags { testDistPref = toFlag dist }
        benchmarkFlags' = Cabal.defaultBenchmarkFlags  `mappend`
                          savedBenchmarkFlags   config `mappend`
                          benchmarkFlags { benchmarkDistPref = toFlag dist }
        globalFlags'    = savedGlobalFlags      config `mappend` globalFlags
    (comp, platform, progdb) <- configCompilerAux' configFlags'

    -- TODO: Redesign ProgramDB API to prevent such problems as #2241 in the
    -- future.
    progdb' <- configureAllKnownPrograms verb progdb

    configFlags'' <- configAbsolutePaths configFlags'

    withRepoContext verb globalFlags' $ \repoContext ->
        install verb
                (configPackageDB' configFlags'')
                repoContext
                comp platform progdb'
                globalFlags' configFlags'' configExFlags'
                installFlags' haddockFlags' testFlags' benchmarkFlags'
                targets

      where
        -- '--run-tests' implies '--enable-tests'.
        maybeForceTests installFlags' configFlags' =
          if fromFlagOrDefault False (installRunTests installFlags')
          then configFlags' { configTests = toFlag True }
          else configFlags'

testAction :: (BuildFlags, TestFlags) -> [String] -> GlobalFlags
           -> IO ()
testAction (buildFlags, testFlags) extraArgs globalFlags = do
  let verbosity      = fromFlagOrDefault normal (buildVerbosity buildFlags)
  config <- loadConfigOrSandboxConfig verbosity globalFlags
  distPref <- findSavedDistPref config (testDistPref testFlags)
  let buildFlags'    = buildFlags
                      { buildVerbosity = testVerbosity testFlags }
      checkFlags = Check $ \_ flags@(configFlags, configExFlags) ->
        if fromFlagOrDefault False (configTests configFlags)
          then pure (mempty, flags)
          else do
            info verbosity "reconfiguring to enable tests"
            let flags' = ( configFlags { configTests = toFlag True }
                        , configExFlags
                        )
            pure (Any True, flags')

  _ <-
    reconfigure configureAction
    verbosity distPref (buildNumJobs buildFlags')
    checkFlags [] globalFlags config
  nixShell verbosity distPref globalFlags config $ do
    let setupOptions   = defaultSetupScriptOptions { useDistPref = distPref }
        testFlags'     = testFlags { testDistPref = toFlag distPref }

    -- The package was just configured, so the LBI must be available.
    names <- componentNamesFromLBI verbosity distPref "test suites"
              (\c -> case c of { LBI.CTest{} -> True; _ -> False })
    let extraArgs'
          | null extraArgs = case names of
            ComponentNamesUnknown -> []
            ComponentNames names' -> [ Make.unUnqualComponentName name
                                    | LBI.CTestName name <- names' ]
          | otherwise      = extraArgs

    build verbosity config distPref buildFlags' extraArgs'
    setupWrapper verbosity setupOptions Nothing Cabal.testCommand (const testFlags') (const extraArgs')

data ComponentNames = ComponentNamesUnknown
                    | ComponentNames [LBI.ComponentName]

-- | Return the names of all buildable components matching a given predicate.
componentNamesFromLBI :: Verbosity -> FilePath -> String
                         -> (LBI.Component -> Bool)
                         -> IO ComponentNames
componentNamesFromLBI verbosity distPref targetsDescr compPred = do
  eLBI <- tryGetPersistBuildConfig distPref
  case eLBI of
    Left err -> case err of
      -- Note: the build config could have been generated by a custom setup
      -- script built against a different Cabal version, so it's crucial that
      -- we ignore the bad version error here.
      ConfigStateFileBadVersion _ _ _ -> return ComponentNamesUnknown
      _                               -> die' verbosity (show err)
    Right lbi -> do
      let pkgDescr = LBI.localPkgDescr lbi
          names    = map LBI.componentName
                     . filter (buildable . LBI.componentBuildInfo)
                     . filter compPred $
                     LBI.pkgComponents pkgDescr
      if null names
        then do notice verbosity $ "Package has no buildable "
                  ++ targetsDescr ++ "."
                exitSuccess -- See #3215.

        else return $! (ComponentNames names)

benchmarkAction :: (BuildFlags, BenchmarkFlags)
                   -> [String] -> GlobalFlags
                   -> IO ()
benchmarkAction
  (buildFlags, benchmarkFlags)
  extraArgs globalFlags = do
  let verbosity      = fromFlagOrDefault normal
                       (buildVerbosity buildFlags)

  config <- loadConfigOrSandboxConfig verbosity globalFlags
  distPref <- findSavedDistPref config (benchmarkDistPref benchmarkFlags)
  let buildFlags'    = buildFlags
                      { buildVerbosity = benchmarkVerbosity benchmarkFlags }

  let checkFlags = Check $ \_ flags@(configFlags, configExFlags) ->
        if fromFlagOrDefault False (configBenchmarks configFlags)
          then pure (mempty, flags)
          else do
            info verbosity "reconfiguring to enable benchmarks"
            let flags' = ( configFlags { configBenchmarks = toFlag True }
                        , configExFlags
                        )
            pure (Any True, flags')

  config' <-
    reconfigure configureAction
    verbosity distPref (buildNumJobs buildFlags')
    checkFlags [] globalFlags config
  nixShell verbosity distPref globalFlags config $ do
    let setupOptions   = defaultSetupScriptOptions { useDistPref = distPref }
        benchmarkFlags'= benchmarkFlags { benchmarkDistPref = toFlag distPref }

    -- The package was just configured, so the LBI must be available.
    names <- componentNamesFromLBI verbosity distPref "benchmarks"
            (\c -> case c of { LBI.CBench{} -> True; _ -> False; })
    let extraArgs'
          | null extraArgs = case names of
            ComponentNamesUnknown -> []
            ComponentNames names' -> [ Make.unUnqualComponentName name
                                    | LBI.CBenchName name <- names']
          | otherwise      = extraArgs

    build verbosity config' distPref buildFlags' extraArgs'
    setupWrapper verbosity setupOptions Nothing Cabal.benchmarkCommand (const benchmarkFlags') (const extraArgs')

haddockAction :: HaddockFlags -> [String] -> Action
haddockAction haddockFlags extraArgs globalFlags = do
  let verbosity = fromFlag (haddockVerbosity haddockFlags)
  config <- loadConfigOrSandboxConfig verbosity globalFlags
  distPref <- findSavedDistPref config (haddockDistPref haddockFlags)
  config' <-
    reconfigure configureAction
    verbosity distPref NoFlag
    mempty [] globalFlags config
  nixShell verbosity distPref globalFlags config $ do
    let haddockFlags' = defaultHaddockFlags      `mappend`
                        savedHaddockFlags config' `mappend`
                        haddockFlags { haddockDistPref = toFlag distPref }
        setupScriptOptions = defaultSetupScriptOptions
                             { useDistPref = distPref }
    setupWrapper verbosity setupScriptOptions Nothing
      haddockCommand (const haddockFlags') (const extraArgs)
    when (haddockForHackage haddockFlags == Flag ForHackage) $ do
      pkg <- fmap LBI.localPkgDescr (getPersistBuildConfig distPref)
      let dest = distPref </> name <.> "tar.gz"
          name = display (packageId pkg) ++ "-docs"
          docDir = distPref </> "doc" </> "html"
      createTarGzFile dest docDir name
      notice verbosity $ "Documentation tarball created: " ++ dest

cleanAction :: CleanFlags -> [String] -> Action
cleanAction cleanFlags extraArgs globalFlags = do
  load <- try (loadConfigOrSandboxConfig verbosity globalFlags)
  let config = either (\(SomeException _) -> mempty) id load
  distPref <- findSavedDistPref config (cleanDistPref cleanFlags)
  let setupScriptOptions = defaultSetupScriptOptions
                           { useDistPref = distPref
                           , useWin32CleanHack = True
                           }
      cleanFlags' = cleanFlags { cleanDistPref = toFlag distPref }
  setupWrapper verbosity setupScriptOptions Nothing
               cleanCommand (const cleanFlags') (const extraArgs)
  where
    verbosity = fromFlagOrDefault normal (cleanVerbosity cleanFlags)

listAction :: ListFlags -> [String] -> Action
listAction listFlags extraArgs globalFlags = do
  let verbosity = fromFlag (listVerbosity listFlags)
  config <- loadConfigOrSandboxConfig verbosity globalFlags
  let configFlags' = savedConfigureFlags config
      configFlags  = configFlags'
        { configPackageDBs = configPackageDBs configFlags'
                           `mappend` listPackageDBs listFlags
        , configHcPath     = listHcPath listFlags
        }
      globalFlags' = savedGlobalFlags    config `mappend` globalFlags
  compProgdb <- if listNeedsCompiler listFlags
      then do
          (comp, _, progdb) <- configCompilerAux' configFlags
          return (Just (comp, progdb))
      else return Nothing
  withRepoContext verbosity globalFlags' $ \repoContext ->
    List.list verbosity
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
      configFlags  = configFlags' {
        configPackageDBs = configPackageDBs configFlags'
                           `mappend` infoPackageDBs infoFlags
        }
      globalFlags' = savedGlobalFlags    config `mappend` globalFlags
  (comp, _, progdb) <- configCompilerAuxEx configFlags
  withRepoContext verbosity globalFlags' $ \repoContext ->
    List.info verbosity
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
  let configFlags  = savedConfigureFlags config
      globalFlags' = savedGlobalFlags config `mappend` globalFlags
  (comp, platform, progdb) <- configCompilerAux' configFlags
  withRepoContext verbosity globalFlags' $ \repoContext ->
    fetch verbosity
        (configPackageDB' configFlags)
        repoContext
        comp platform progdb globalFlags' fetchFlags
        targets

freezeAction :: FreezeFlags -> [String] -> Action
freezeAction freezeFlags _extraArgs globalFlags = do
  let verbosity = fromFlag (freezeVerbosity freezeFlags)
  config <- loadConfigOrSandboxConfig verbosity globalFlags
  distPref <- findSavedDistPref config NoFlag
  nixShell verbosity distPref globalFlags config $ do
    let configFlags  = savedConfigureFlags config
        globalFlags' = savedGlobalFlags config `mappend` globalFlags
    (comp, platform, progdb) <- configCompilerAux' configFlags

    withRepoContext verbosity globalFlags' $ \repoContext ->
        freeze verbosity
            (configPackageDB' configFlags)
            repoContext
            comp platform progdb
            globalFlags' freezeFlags

genBoundsAction :: FreezeFlags -> [String] -> GlobalFlags -> IO ()
genBoundsAction freezeFlags _extraArgs globalFlags = do
  let verbosity = fromFlag (freezeVerbosity freezeFlags)
  config <- loadConfigOrSandboxConfig verbosity globalFlags
  distPref <- findSavedDistPref config NoFlag
  nixShell verbosity distPref globalFlags config $ do
    let configFlags  = savedConfigureFlags config
        globalFlags' = savedGlobalFlags config `mappend` globalFlags
    (comp, platform, progdb) <- configCompilerAux' configFlags

    withRepoContext verbosity globalFlags' $ \repoContext ->
        genBounds verbosity
                (configPackageDB' configFlags)
                repoContext
                comp platform progdb
                globalFlags' freezeFlags

uploadAction :: UploadFlags -> [String] -> Action
uploadAction uploadFlags extraArgs globalFlags = do
  config <- loadConfig verbosity (globalConfigFile globalFlags)
  let uploadFlags' = savedUploadFlags config `mappend` uploadFlags
      globalFlags' = savedGlobalFlags config `mappend` globalFlags
      tarfiles     = extraArgs
  when (null tarfiles && not (fromFlag (uploadDoc uploadFlags'))) $
    die' verbosity "the 'upload' command expects at least one .tar.gz archive."
  checkTarFiles extraArgs
  maybe_password <-
    case uploadPasswordCmd uploadFlags'
    of Flag (xs:xss) -> Just . Password <$>
                        getProgramInvocationOutput verbosity
                        (simpleProgramInvocation xs xss)
       _             -> pure $ flagToMaybe $ uploadPassword uploadFlags'
  withRepoContext verbosity globalFlags' $ \repoContext -> do
    if fromFlag (uploadDoc uploadFlags')
    then do
      when (length tarfiles > 1) $
       die' verbosity $ "the 'upload' command can only upload documentation "
             ++ "for one package at a time."
      tarfile <- maybe (generateDocTarball config) return $ listToMaybe tarfiles
      Upload.uploadDoc verbosity
                       repoContext
                       (flagToMaybe $ uploadUsername uploadFlags')
                       maybe_password
                       (fromFlag (uploadCandidate uploadFlags'))
                       tarfile
    else do
      Upload.upload verbosity
                    repoContext
                    (flagToMaybe $ uploadUsername uploadFlags')
                    maybe_password
                    (fromFlag (uploadCandidate uploadFlags'))
                    tarfiles
    where
    verbosity = fromFlag (uploadVerbosity uploadFlags)
    checkTarFiles tarfiles
      | not (null otherFiles)
      = die' verbosity $ "the 'upload' command expects only .tar.gz archives: "
           ++ intercalate ", " otherFiles
      | otherwise = sequence_
                      [ do exists <- doesFileExist tarfile
                           unless exists $ die' verbosity $ "file not found: " ++ tarfile
                      | tarfile <- tarfiles ]

      where otherFiles = filter (not . isTarGzFile) tarfiles
            isTarGzFile file = case splitExtension file of
              (file', ".gz") -> takeExtension file' == ".tar"
              _              -> False
    generateDocTarball config = do
      notice verbosity $
        "No documentation tarball specified. "
        ++ "Building a documentation tarball with default settings...\n"
        ++ "If you need to customise Haddock options, "
        ++ "run 'haddock --for-hackage' first "
        ++ "to generate a documentation tarball."
      haddockAction (defaultHaddockFlags { haddockForHackage = Flag ForHackage })
                    [] globalFlags
      distPref <- findSavedDistPref config NoFlag
      pkg <- fmap LBI.localPkgDescr (getPersistBuildConfig distPref)
      return $ distPref </> display (packageId pkg) ++ "-docs" <.> "tar.gz"

checkAction :: Flag Verbosity -> [String] -> Action
checkAction verbosityFlag extraArgs _globalFlags = do
  let verbosity = fromFlag verbosityFlag
  unless (null extraArgs) $
    die' verbosity $ "'check' doesn't take any extra arguments: " ++ unwords extraArgs
  allOk <- Check.check (fromFlag verbosityFlag)
  unless allOk exitFailure

formatAction :: Flag Verbosity -> [String] -> Action
formatAction verbosityFlag extraArgs _globalFlags = do
  let verbosity = fromFlag verbosityFlag
  path <- case extraArgs of
    [] -> do cwd <- getCurrentDirectory
             tryFindPackageDesc verbosity cwd
    (p:_) -> return p
  pkgDesc <- readGenericPackageDescription verbosity path
  -- Uses 'writeFileAtomic' under the hood.
  writeGenericPackageDescription path pkgDesc

reportAction :: ReportFlags -> [String] -> Action
reportAction reportFlags extraArgs globalFlags = do
  let verbosity = fromFlag (reportVerbosity reportFlags)
  unless (null extraArgs) $
    die' verbosity $ "'report' doesn't take any extra arguments: " ++ unwords extraArgs
  config <- loadConfig verbosity (globalConfigFile globalFlags)
  let globalFlags' = savedGlobalFlags config `mappend` globalFlags
      reportFlags' = savedReportFlags config `mappend` reportFlags

  withRepoContext verbosity globalFlags' $ \repoContext ->
   Upload.report verbosity repoContext
    (flagToMaybe $ reportUsername reportFlags')
    (flagToMaybe $ reportPassword reportFlags')

runAction :: BuildFlags -> [String] -> Action
runAction buildFlags extraArgs globalFlags = do
  let verbosity   = fromFlagOrDefault normal (buildVerbosity buildFlags)
  config <- loadConfigOrSandboxConfig verbosity globalFlags
  distPref <- findSavedDistPref config (buildDistPref buildFlags)
  config' <-
    reconfigure configureAction
    verbosity distPref (buildNumJobs buildFlags)
    mempty [] globalFlags config
  nixShell verbosity distPref globalFlags config $ do
    lbi <- getPersistBuildConfig distPref
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
   get verbosity
    repoContext
    globalFlags'
    getFlags
    targets

unpackAction :: GetFlags -> [String] -> Action
unpackAction getFlags extraArgs globalFlags = do
  getAction getFlags extraArgs globalFlags

initAction :: InitFlags -> [String] -> Action
initAction initFlags extraArgs globalFlags
    | not (null extraArgs) =
      die' verbosity $ "'init' doesn't take any extra arguments: " ++ unwords extraArgs
    | otherwise = do
      confFlags <- loadConfigOrSandboxConfig verbosity globalFlags
      -- override with `--with-compiler` from CLI if available
      let confFlags' = savedConfigureFlags confFlags `mappend` compFlags
          initFlags' = savedInitFlags confFlags `mappend` initFlags
          globalFlags' = savedGlobalFlags confFlags `mappend` globalFlags

      (comp, _, progdb) <- configCompilerAux' confFlags'

      withRepoContext verbosity globalFlags' $ \repoContext ->
        initCmd verbosity (configPackageDB' confFlags')
          repoContext comp progdb initFlags'
  where
    verbosity = fromFlag (initVerbosity initFlags)
    compFlags = mempty { configHcPath = initHcPath initFlags }

userConfigAction :: UserConfigFlags -> [String] -> Action
userConfigAction ucflags extraArgs globalFlags = do
  let verbosity  = fromFlag (userConfigVerbosity ucflags)
      frc        = fromFlag (userConfigForce ucflags)
      extraLines = fromFlag (userConfigAppendLines ucflags)
  case extraArgs of
    ("init":_) -> do
      path       <- configFile
      fileExists <- doesFileExist path
      if (not fileExists || (fileExists && frc))
      then void $ createDefaultConfigFile verbosity extraLines path
      else die' verbosity $ path ++ " already exists."
    ("diff":_) -> traverse_ putStrLn =<< userConfigDiff verbosity globalFlags extraLines
    ("update":_) -> userConfigUpdate verbosity globalFlags extraLines
    -- Error handling.
    [] -> die' verbosity $ "Please specify a subcommand (see 'help user-config')"
    _  -> die' verbosity $ "Unknown 'user-config' subcommand: " ++ unwords extraArgs
  where configFile = getConfigFilePath (globalConfigFile globalFlags)

-- | Used as an entry point when cabal-install needs to invoke itself
-- as a setup script. This can happen e.g. when doing parallel builds.
--
actAsSetupAction :: ActAsSetupFlags -> [String] -> Action
actAsSetupAction actAsSetupFlags args _globalFlags =
  let bt = fromFlag (actAsSetupBuildType actAsSetupFlags)
  in case bt of
    Simple    -> Simple.defaultMainArgs args
    Configure -> Simple.defaultMainWithHooksArgs
                  Simple.autoconfUserHooks args
    Make      -> Make.defaultMainArgs args
    Custom    -> error "actAsSetupAction Custom"

manpageAction :: [CommandSpec action] -> ManpageFlags -> [String] -> Action
manpageAction commands flags extraArgs _ = do
  let verbosity = fromFlag (manpageVerbosity flags)
  unless (null extraArgs) $
    die' verbosity $ "'manpage' doesn't take any extra arguments: " ++ unwords extraArgs
  pname <- getProgName
  let cabalCmd = if takeExtension pname == ".exe"
                 then dropExtension pname
                 else pname
  manpageCmd cabalCmd commands flags
