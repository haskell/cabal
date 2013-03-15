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
         , BuildFlags(..), buildCommand
         , InstallFlags(..), defaultInstallFlags
         , installCommand, upgradeCommand
         , FetchFlags(..), fetchCommand
         , GetFlags(..), getCommand, unpackCommand
         , checkCommand
         , updateCommand
         , ListFlags(..), listCommand
         , InfoFlags(..), infoCommand
         , UploadFlags(..), uploadCommand
         , ReportFlags(..), reportCommand
         , runCommand
         , InitFlags(initVerbosity), initCommand
         , SDistFlags(..), SDistExFlags(..), sdistCommand
         , Win32SelfUpgradeFlags(..), win32SelfUpgradeCommand
         , IndexFlags(..), indexCommand
         , SandboxFlags(..), sandboxInitCommand, sandboxDeleteCommand
         , sandboxAddSourceCommand, sandboxConfigureCommand
         , sandboxBuildCommand, sandboxInstallCommand
         , dumpPkgEnvCommand
         , reportCommand
         )
import Distribution.Simple.Setup
         ( HaddockFlags(..), haddockCommand
         , HscolourFlags(..), hscolourCommand
         , CopyFlags(..), copyCommand
         , RegisterFlags(..), registerCommand
         , CleanFlags(..), cleanCommand
         , TestFlags(..), testCommand
         , BenchmarkFlags(..), benchmarkCommand
         , Flag(..), fromFlag, fromFlagOrDefault, flagToMaybe, toFlag )

import Distribution.Client.SetupWrapper
         ( setupWrapper, SetupScriptOptions(..), defaultSetupScriptOptions )
import Distribution.Client.Config
         ( SavedConfig(..), loadConfig, defaultConfigFile )
import Distribution.Client.Targets
         ( readUserTargets )

import Distribution.Client.List               (list, info)
import Distribution.Client.Install            (install)
import Distribution.Client.Configure          (configure)
import Distribution.Client.Update             (update)
import Distribution.Client.Fetch              (fetch)
import Distribution.Client.Check as Check     (check)
--import Distribution.Client.Clean            (clean)
import Distribution.Client.Upload as Upload   (upload, check, report)
import Distribution.Client.Run                (run)
import Distribution.Client.SrcDist            (sdist)
import Distribution.Client.Get                (get)
import Distribution.Client.Index              (index)
import Distribution.Client.Sandbox            (sandboxInit
                                              , sandboxDelete
                                              , sandboxAddSource
                                              , sandboxBuild
                                              , sandboxConfigure
                                              , sandboxInstall
                                              , dumpPackageEnvironment)
import Distribution.Client.Init               (initCabal)
import qualified Distribution.Client.Win32SelfUpgrade as Win32SelfUpgrade

import Distribution.Simple.Compiler
         ( Compiler, PackageDBStack )
import Distribution.Simple.Program
         ( ProgramConfiguration )
import Distribution.Simple.Command
         ( CommandParse(..), CommandUI(..), Command
         , commandsRun, commandAddAction, hiddenCommand )
import Distribution.Simple.Configure
         ( checkPersistBuildConfigOutdated, configCompilerAux
         , interpretPackageDbFlags, maybeGetPersistBuildConfig )
import qualified Distribution.Simple.LocalBuildInfo as LBI
import Distribution.System ( Platform )
import Distribution.Simple.Utils
         ( cabalVersion, die, notice, topHandler )
import Distribution.Text
         ( display )
import Distribution.Verbosity as Verbosity
       ( Verbosity, normal, lessVerbose )
import qualified Paths_cabal_install (version)

import System.Environment       (getArgs, getProgName)
import System.Exit              (exitFailure)
import System.FilePath          (splitExtension, takeExtension)
import System.Directory         (doesFileExist)
import Data.List                (intercalate)
import Data.Monoid              (Monoid(..))
import Control.Monad            (when, unless)

-- | Entry point
--
main :: IO ()
main = getArgs >>= mainWorker

mainWorker :: [String] -> IO ()
mainWorker args = topHandler $
  case commandsRun globalCommand commands args of
    CommandHelp   help                 -> printGlobalHelp help
    CommandList   opts                 -> printOptionsList opts
    CommandErrors errs                 -> printErrors errs
    CommandReadyToGo (globalflags, commandParse)  ->
      case commandParse of
        _ | fromFlag (globalVersion globalflags)        -> printVersion
          | fromFlag (globalNumericVersion globalflags) -> printNumericVersion
        CommandHelp     help           -> printCommandHelp help
        CommandList     opts           -> printOptionsList opts
        CommandErrors   errs           -> printErrors errs
        CommandReadyToGo action        -> action globalflags

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
      ,wrapperAction copyCommand
                     copyVerbosity     copyDistPref
      ,wrapperAction haddockCommand
                     haddockVerbosity  haddockDistPref
      ,wrapperAction cleanCommand
                     cleanVerbosity    cleanDistPref
      ,wrapperAction hscolourCommand
                     hscolourVerbosity hscolourDistPref
      ,wrapperAction registerCommand
                     regVerbosity      regDistPref
      ,testCommand            `commandAddAction` testAction
      ,benchmarkCommand       `commandAddAction` benchmarkAction
      ,hiddenCommand $
       upgradeCommand         `commandAddAction` upgradeAction
      ,hiddenCommand $
       win32SelfUpgradeCommand`commandAddAction` win32SelfUpgradeAction
      ,hiddenCommand $
       indexCommand `commandAddAction` indexAction
      ,hiddenCommand $
       sandboxInitCommand `commandAddAction` sandboxInitAction
      ,hiddenCommand $
       sandboxDeleteCommand `commandAddAction` sandboxDeleteAction
      ,hiddenCommand $
       sandboxAddSourceCommand `commandAddAction` sandboxAddSourceAction
      ,hiddenCommand $
       sandboxConfigureCommand `commandAddAction` sandboxConfigureAction
      ,hiddenCommand $
       sandboxBuildCommand `commandAddAction` sandboxBuildAction
      ,hiddenCommand $
       sandboxInstallCommand `commandAddAction` sandboxInstallAction
      ,hiddenCommand $
       dumpPkgEnvCommand `commandAddAction` dumpPkgEnvAction
      ]

wrapperAction :: Monoid flags
              => CommandUI flags
              -> (flags -> Flag Verbosity)
              -> (flags -> Flag String)
              -> Command (GlobalFlags -> IO ())
wrapperAction command verbosityFlag distPrefFlag =
  commandAddAction command
    { commandDefaultFlags = mempty } $ \flags extraArgs _globalFlags -> do
    let verbosity = fromFlagOrDefault normal (verbosityFlag flags)
        setupScriptOptions = defaultSetupScriptOptions {
          useDistPref = fromFlagOrDefault
                          (useDistPref defaultSetupScriptOptions)
                          (distPrefFlag flags)
        }
    setupWrapper verbosity setupScriptOptions Nothing
                 command (const flags) extraArgs

configureAction :: (ConfigFlags, ConfigExFlags)
                -> [String] -> GlobalFlags -> IO ()
configureAction (configFlags, configExFlags) extraArgs globalFlags = do
  let verbosity = fromFlagOrDefault normal (configVerbosity configFlags)
  config <- loadConfig verbosity (globalConfigFile globalFlags)
                                 (configUserInstall configFlags)
  let configFlags'   = savedConfigureFlags   config `mappend` configFlags
      configExFlags' = savedConfigureExFlags config `mappend` configExFlags
      globalFlags'   = savedGlobalFlags      config `mappend` globalFlags
  (comp, platform, conf) <- configCompilerAux configFlags'
  configure verbosity
            (configPackageDB' configFlags') (globalRepos globalFlags')
            comp platform conf configFlags' configExFlags' extraArgs

buildAction :: BuildFlags -> [String] -> GlobalFlags -> IO ()
buildAction buildFlags extraArgs globalFlags = do
    let distPref = fromFlagOrDefault (useDistPref defaultSetupScriptOptions)
                                     (buildDistPref buildFlags)
        verbosity = fromFlagOrDefault normal (buildVerbosity buildFlags)

    reconfigure verbosity distPref mempty [] globalFlags (const Nothing)
    build verbosity distPref buildFlags extraArgs

-- | Actually do the work of building the package. This is separate from
-- 'buildAction' so that 'testAction' and 'benchmarkAction' do not invoke
-- 'reconfigure' twice.
build :: Verbosity -> FilePath -> BuildFlags -> [String] -> IO ()
build verbosity distPref buildFlags extraArgs =
    setupWrapper verbosity setupOptions Nothing
                 buildCommand (const buildFlags') extraArgs
  where
    setupOptions = defaultSetupScriptOptions { useDistPref = distPref }
    buildFlags' = buildFlags
        { buildVerbosity = toFlag verbosity
        , buildDistPref = toFlag distPref
        }

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
            -> FilePath     -- ^ \"dist\" prefix
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
            -> (ConfigFlags -> Maybe String)
                            -- ^ Check that the required flags are set in
                            -- the last used 'ConfigFlags'. If the required
                            -- flags are not set, provide a message to the
                            -- user explaining the reason for
                            -- reconfiguration. Because the correct \"dist\"
                            -- prefix setting is always required, it is checked
                            -- automatically; this function need not check
                            -- for it.
            -> IO ()
reconfigure verbosity distPref    addConfigFlags
            extraArgs globalFlags checkFlags = do
    mLbi <- maybeGetPersistBuildConfig distPref
    case mLbi of

        -- Package has never been configured.
        Nothing -> do
            notice verbosity
                $ "Configuring with default flags." ++ configureManually
            configureAction (defaultFlags, defaultConfigExFlags)
                            extraArgs globalFlags

        -- Package has been configured, but the configuration may be out of
        -- date or required flags may not be set.
        Just lbi -> do
            let configFlags = LBI.configFlags lbi
                flags = mconcat [configFlags, addConfigFlags, distVerbFlags]
                savedDistPref = fromFlagOrDefault
                    (useDistPref defaultSetupScriptOptions)
                    (configDistPref configFlags)

            -- Determine what message, if any, to display to the user if
            -- reconfiguration is required.
            message <- case checkFlags configFlags of

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

            case message of

                -- No message for the user indicates that reconfiguration
                -- is not required.
                Nothing -> return ()

                Just msg -> do
                    notice verbosity msg
                    configureAction (flags, defaultConfigExFlags)
                                    extraArgs globalFlags
  where
    defaultFlags = mappend addConfigFlags distVerbFlags
    distVerbFlags = mempty
        { configVerbosity = toFlag verbosity
        , configDistPref = toFlag distPref
        }
    configureManually = " If this fails, please run configure manually.\n"
    distPrefMessage =
        "Package previously configured with different \"dist\" prefix. "
        ++ "Re-configuring based on most recently used options."
        ++ configureManually
    outdatedMessage pdFile =
        pdFile ++ " has been changed. "
        ++ "Re-configuring with most recently used options."
        ++ configureManually

installAction :: (ConfigFlags, ConfigExFlags, InstallFlags, HaddockFlags)
              -> [String] -> GlobalFlags -> IO ()
installAction (configFlags, _, installFlags, _) _ _globalFlags
  | fromFlagOrDefault False (installOnly installFlags)
  = let verbosity = fromFlagOrDefault normal (configVerbosity configFlags)
    in setupWrapper verbosity defaultSetupScriptOptions Nothing
         installCommand (const mempty) []

installAction (configFlags, configExFlags, installFlags, haddockFlags)
              extraArgs globalFlags = do
  let verbosity = fromFlagOrDefault normal (configVerbosity configFlags)
  targets <- readUserTargets verbosity extraArgs
  config <- loadConfig verbosity (globalConfigFile globalFlags)
                                 (configUserInstall configFlags)
  let configFlags'   = savedConfigureFlags   config `mappend` configFlags
      configExFlags' = defaultConfigExFlags         `mappend`
                       savedConfigureExFlags config `mappend` configExFlags
      installFlags'  = defaultInstallFlags          `mappend`
                       savedInstallFlags     config `mappend` installFlags
      globalFlags'   = savedGlobalFlags      config `mappend` globalFlags
  (comp, platform, conf) <- configCompilerAux' configFlags'
  install verbosity
          (configPackageDB' configFlags') (globalRepos globalFlags')
          comp platform conf globalFlags' configFlags' configExFlags'
          installFlags' haddockFlags
          targets

testAction :: TestFlags -> [String] -> GlobalFlags -> IO ()
testAction testFlags extraArgs globalFlags = do
    let verbosity = fromFlagOrDefault normal (testVerbosity testFlags)
        distPref = fromFlagOrDefault (useDistPref defaultSetupScriptOptions)
                                     (testDistPref testFlags)
        setupOptions = defaultSetupScriptOptions { useDistPref = distPref }
        addConfigFlags = mempty { configTests = toFlag True }
        checkFlags flags
            | fromFlagOrDefault False (configTests flags) = Nothing
            | otherwise = Just "Re-configuring with test suites enabled."

    reconfigure verbosity distPref addConfigFlags [] globalFlags checkFlags
    build verbosity distPref mempty []

    setupWrapper verbosity setupOptions Nothing
                 testCommand (const testFlags) extraArgs

benchmarkAction :: BenchmarkFlags -> [String] -> GlobalFlags -> IO ()
benchmarkAction benchmarkFlags extraArgs globalFlags = do
    let verbosity = fromFlagOrDefault normal (benchmarkVerbosity benchmarkFlags)
        distPref = fromFlagOrDefault (useDistPref defaultSetupScriptOptions)
                                     (benchmarkDistPref benchmarkFlags)
        setupOptions = defaultSetupScriptOptions { useDistPref = distPref }
        addConfigFlags = mempty { configBenchmarks = toFlag True }
        checkFlags flags
            | fromFlagOrDefault False (configBenchmarks flags) = Nothing
            | otherwise = Just "Re-configuring with benchmarks enabled."

    reconfigure verbosity distPref addConfigFlags [] globalFlags checkFlags
    build verbosity distPref mempty []

    setupWrapper verbosity setupOptions Nothing
                 benchmarkCommand (const benchmarkFlags) extraArgs

listAction :: ListFlags -> [String] -> GlobalFlags -> IO ()
listAction listFlags extraArgs globalFlags = do
  let verbosity = fromFlag (listVerbosity listFlags)
  config <- loadConfig verbosity (globalConfigFile globalFlags) mempty
  let configFlags  = savedConfigureFlags config
      globalFlags' = savedGlobalFlags    config `mappend` globalFlags
  (comp, _, conf) <- configCompilerAux' configFlags
  list verbosity
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
  config <- loadConfig verbosity (globalConfigFile globalFlags) mempty
  let configFlags  = savedConfigureFlags config
      globalFlags' = savedGlobalFlags    config `mappend` globalFlags
  (comp, _, conf) <- configCompilerAux configFlags
  info verbosity
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
  config <- loadConfig verbosity (globalConfigFile globalFlags) mempty
  let globalFlags' = savedGlobalFlags config `mappend` globalFlags
  update verbosity (globalRepos globalFlags')

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
  config <- loadConfig verbosity (globalConfigFile globalFlags) mempty
  let configFlags  = savedConfigureFlags config
      globalFlags' = savedGlobalFlags config `mappend` globalFlags
  (comp, platform, conf) <- configCompilerAux' configFlags
  fetch verbosity
        (configPackageDB' configFlags) (globalRepos globalFlags')
        comp platform conf globalFlags' fetchFlags
        targets

uploadAction :: UploadFlags -> [String] -> GlobalFlags -> IO ()
uploadAction uploadFlags extraArgs globalFlags = do
  let verbosity = fromFlag (uploadVerbosity uploadFlags)
  config <- loadConfig verbosity (globalConfigFile globalFlags) mempty
  let uploadFlags' = savedUploadFlags config `mappend` uploadFlags
      globalFlags' = savedGlobalFlags config `mappend` globalFlags
      tarfiles     = extraArgs
  checkTarFiles extraArgs
  if fromFlag (uploadCheck uploadFlags')
    then Upload.check  verbosity tarfiles
    else upload verbosity
                (globalRepos globalFlags')
                (flagToMaybe $ uploadUsername uploadFlags')
                (flagToMaybe $ uploadPassword uploadFlags')
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


sdistAction :: (SDistFlags, SDistExFlags) -> [String] -> GlobalFlags -> IO ()
sdistAction (sdistFlags, sdistExFlags) extraArgs _globalFlags = do
  unless (null extraArgs) $
    die $ "'sdist' doesn't take any extra arguments: " ++ unwords extraArgs
  sdist sdistFlags sdistExFlags

reportAction :: ReportFlags -> [String] -> GlobalFlags -> IO ()
reportAction reportFlags extraArgs globalFlags = do
  unless (null extraArgs) $
    die $ "'report' doesn't take any extra arguments: " ++ unwords extraArgs

  let verbosity = fromFlag (reportVerbosity reportFlags)
  config <- loadConfig verbosity (globalConfigFile globalFlags) mempty
  let globalFlags' = savedGlobalFlags config `mappend` globalFlags
      reportFlags' = savedReportFlags config `mappend` reportFlags

  Upload.report verbosity (globalRepos globalFlags')
    (flagToMaybe $ reportUsername reportFlags')
    (flagToMaybe $ reportPassword reportFlags')

runAction :: BuildFlags -> [String] -> GlobalFlags -> IO ()
runAction buildFlags extraArgs globalFlags = do
  let verbosity    = fromFlagOrDefault normal (buildVerbosity buildFlags)
      distPref     = fromFlagOrDefault (useDistPref defaultSetupScriptOptions)
                     (buildDistPref buildFlags)

  reconfigure verbosity distPref mempty [] globalFlags (const Nothing)
  build verbosity distPref mempty []

  run verbosity buildFlags extraArgs

getAction :: GetFlags -> [String] -> GlobalFlags -> IO ()
getAction getFlags extraArgs globalFlags = do
  let verbosity = fromFlag (getVerbosity getFlags)
  targets <- readUserTargets verbosity extraArgs
  config <- loadConfig verbosity (globalConfigFile globalFlags) mempty
  let globalFlags' = savedGlobalFlags config `mappend` globalFlags
  get verbosity
    (globalRepos (savedGlobalFlags config))
    globalFlags'
    getFlags
    targets

unpackAction :: GetFlags -> [String] -> GlobalFlags -> IO ()
unpackAction getFlags extraArgs globalFlags = do
  let verbosity = fromFlag (getVerbosity getFlags)
  notice verbosity $ "The 'unpack' command is deprecated "
    ++ "and will be removed in a future release. "
    ++ "Please use 'cabal get' instead."
  getAction getFlags extraArgs globalFlags

initAction :: InitFlags -> [String] -> GlobalFlags -> IO ()
initAction initFlags _extraArgs globalFlags = do
  let verbosity = fromFlag (initVerbosity initFlags)
  config <- loadConfig verbosity (globalConfigFile globalFlags) mempty
  let configFlags  = savedConfigureFlags config
  (comp, _, conf) <- configCompilerAux' configFlags
  initCabal verbosity
            (configPackageDB' configFlags)
            comp
            conf
            initFlags

indexAction :: IndexFlags -> [String] -> GlobalFlags -> IO ()
indexAction indexFlags extraArgs _globalFlags = do
  when (null extraArgs) $
    die $ "the 'index' command expects a single argument."
  when ((>1). length $ extraArgs) $
    die $ "the 'index' command expects a single argument: " ++ unwords extraArgs
  let verbosity = fromFlag (indexVerbosity indexFlags)
  index verbosity indexFlags (head extraArgs)

sandboxInitAction :: SandboxFlags -> [String] -> GlobalFlags -> IO ()
sandboxInitAction sandboxFlags extraArgs globalFlags = do
  when ((>0). length $ extraArgs) $
    die $ "the 'sandbox-init' command doesn't expect any arguments: "
      ++ unwords extraArgs
  let verbosity = fromFlag (sandboxVerbosity sandboxFlags)
  sandboxInit verbosity sandboxFlags globalFlags

sandboxDeleteAction :: SandboxFlags -> [String] -> GlobalFlags -> IO ()
sandboxDeleteAction sandboxFlags extraArgs globalFlags = do
  when ((>0). length $ extraArgs) $
    die $ "the 'sandbox-init' command doesn't expect any arguments: "
      ++ unwords extraArgs
  let verbosity = fromFlag (sandboxVerbosity sandboxFlags)
  sandboxDelete verbosity sandboxFlags globalFlags

sandboxAddSourceAction :: SandboxFlags -> [String] -> GlobalFlags -> IO ()
sandboxAddSourceAction sandboxFlags extraArgs globalFlags = do
  let verbosity = fromFlag (sandboxVerbosity sandboxFlags)
  sandboxAddSource verbosity extraArgs sandboxFlags globalFlags

sandboxConfigureAction :: (SandboxFlags, ConfigFlags, ConfigExFlags)
                          -> [String] -> GlobalFlags -> IO ()
sandboxConfigureAction (sandboxFlags, configFlags, configExFlags)
  extraArgs globalFlags = do
  let verbosity = fromFlag (sandboxVerbosity sandboxFlags)
  sandboxConfigure verbosity sandboxFlags configFlags configExFlags
    extraArgs globalFlags

sandboxBuildAction :: (SandboxFlags, BuildFlags) -> [String] -> GlobalFlags
                      -> IO ()
sandboxBuildAction (sandboxFlags, buildFlags) extraArgs globalFlags = do
  let verbosity = fromFlag (sandboxVerbosity sandboxFlags)
  sandboxBuild verbosity sandboxFlags buildFlags globalFlags extraArgs

sandboxInstallAction :: (SandboxFlags, ConfigFlags, ConfigExFlags,
                         InstallFlags, HaddockFlags)
                        -> [String] -> GlobalFlags -> IO ()
sandboxInstallAction
  (sandboxFlags, configFlags, configExFlags, installFlags, haddockFlags)
  extraArgs globalFlags = do
  let verbosity = fromFlag (sandboxVerbosity sandboxFlags)
  sandboxInstall verbosity sandboxFlags configFlags configExFlags
    installFlags haddockFlags extraArgs globalFlags mempty

dumpPkgEnvAction :: SandboxFlags -> [String] -> GlobalFlags -> IO ()
dumpPkgEnvAction sandboxFlags extraArgs globalFlags = do
  when ((>0). length $ extraArgs) $
    die $ "the 'dump-pkgenv' command doesn't expect any arguments: "
      ++ unwords extraArgs
  let verbosity = fromFlag (sandboxVerbosity sandboxFlags)
  dumpPackageEnvironment verbosity sandboxFlags globalFlags

-- | See 'Distribution.Client.Install.withWin32SelfUpgrade' for details.
--
win32SelfUpgradeAction :: Win32SelfUpgradeFlags -> [String] -> GlobalFlags
                          -> IO ()
win32SelfUpgradeAction selfUpgradeFlags (pid:path:_extraArgs) _globalFlags = do
  let verbosity = fromFlag (win32SelfUpgradeVerbosity selfUpgradeFlags)
  Win32SelfUpgrade.deleteOldExeFile verbosity (read pid) path
win32SelfUpgradeAction _ _ _ = return ()

--
-- Utils (transitionary)
--

configPackageDB' :: ConfigFlags -> PackageDBStack
configPackageDB' cfg =
    interpretPackageDbFlags userInstall (configPackageDBs cfg)
  where
    userInstall = fromFlagOrDefault True (configUserInstall cfg)

configCompilerAux' :: ConfigFlags
                   -> IO (Compiler, Platform, ProgramConfiguration)
configCompilerAux' configFlags =
  configCompilerAux configFlags
    --FIXME: make configCompilerAux use a sensible verbosity
    { configVerbosity = fmap lessVerbose (configVerbosity configFlags) }
