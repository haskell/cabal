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

module Main where

import Distribution.Client.Setup
         ( GlobalFlags(..), globalCommand, globalRepos
         , ConfigFlags(..), configureCommand
         , InstallFlags(..), installCommand, upgradeCommand
         , fetchCommand, checkCommand
         , updateCommand
         , ListFlags(..), listCommand
         , UploadFlags(..), uploadCommand
         , reportCommand
         , parsePackageArgs, configPackageDB' )
import Distribution.Simple.Setup
         ( BuildFlags(..), buildCommand
         , HaddockFlags(..), haddockCommand
         , HscolourFlags(..), hscolourCommand
         , CopyFlags(..), copyCommand
         , RegisterFlags(..), registerCommand
         , CleanFlags(..), cleanCommand
         , SDistFlags(..), sdistCommand
         , TestFlags(..), testCommand
         , Flag(..), fromFlag, fromFlagOrDefault, flagToMaybe )

import Distribution.Client.Types
         ( UnresolvedDependency(UnresolvedDependency) )
import Distribution.Client.SetupWrapper
         ( setupWrapper, SetupScriptOptions(..), defaultSetupScriptOptions )
import Distribution.Client.Config
         ( SavedConfig(..), loadConfig )
import Distribution.Client.List             (list)
import Distribution.Client.Install          (install, upgrade)
import Distribution.Client.Update           (update)
import Distribution.Client.Fetch            (fetch)
import Distribution.Client.Check as Check   (check)
--import Distribution.Client.Clean            (clean)
import Distribution.Client.Upload as Upload (upload, check, report)
import Distribution.Client.SrcDist          (sdist)
import qualified Distribution.Client.Win32SelfUpgrade as Win32SelfUpgrade

import Distribution.Simple.Program (defaultProgramConfiguration)
import Distribution.Simple.Command
import Distribution.Simple.Configure (configCompilerAux)
import Distribution.Simple.Utils (cabalVersion, die, intercalate)
import Distribution.Text
         ( display )
import Distribution.Verbosity as Verbosity
       ( Verbosity, normal, intToVerbosity )
import qualified Paths_cabal_install (version)

import System.Environment       (getArgs, getProgName)
import System.Exit              (exitFailure)
import System.FilePath          (splitExtension, takeExtension)
import System.Directory         (doesFileExist)
import Data.List                (intersperse)
import Data.Maybe               (fromMaybe)
import Data.Monoid              (Monoid(..))
import Control.Monad            (unless)

-- | Entry point
--
main :: IO ()
main = getArgs >>= mainWorker

mainWorker :: [String] -> IO ()
mainWorker ("win32selfupgrade":args) = win32SelfUpgradeAction args
mainWorker args =
  case commandsRun globalCommand commands args of
    CommandHelp   help                 -> printHelp help
    CommandList   opts                 -> printOptionsList opts
    CommandErrors errs                 -> printErrors errs
    CommandReadyToGo (globalflags, commandParse)  ->
      case commandParse of
        _ | fromFlag (globalVersion globalflags)        -> printVersion
          | fromFlag (globalNumericVersion globalflags) -> printNumericVersion
        CommandHelp     help           -> printHelp help
        CommandList     opts           -> printOptionsList opts
        CommandErrors   errs           -> printErrors errs
        CommandReadyToGo action        -> action globalflags

  where
    printHelp help = getProgName >>= putStr . help
    printOptionsList = putStr . unlines
    printErrors errs = do
      putStr (concat (intersperse "\n" errs))
      exitFailure
    printNumericVersion = putStrLn $ display Paths_cabal_install.version
    printVersion        = putStrLn $ "cabal-install version "
                                  ++ display Paths_cabal_install.version
                                  ++ "\nusing version "
                                  ++ display cabalVersion
                                  ++ " of the Cabal library "

    commands =
      [configureCommand       `commandAddAction` configureAction
      ,installCommand         `commandAddAction` installAction
      ,listCommand            `commandAddAction` listAction
      ,updateCommand          `commandAddAction` updateAction
      ,upgradeCommand         `commandAddAction` upgradeAction
      ,fetchCommand           `commandAddAction` fetchAction
      ,uploadCommand          `commandAddAction` uploadAction
      ,checkCommand           `commandAddAction` checkAction
      ,sdistCommand           `commandAddAction` sdistAction
      ,reportCommand          `commandAddAction` reportAction
      ,wrapperAction (buildCommand defaultProgramConfiguration)
                     buildVerbosity    buildDistPref
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
      ,wrapperAction testCommand
                     testVerbosity     testDistPref
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

configureAction :: ConfigFlags -> [String] -> GlobalFlags -> IO ()
configureAction flags extraArgs globalFlags = do
  let verbosity = fromFlagOrDefault normal (configVerbosity flags)
  config <- loadConfig verbosity (globalConfigFile globalFlags)
                                 (configUserInstall flags)
  let flags' = savedConfigureFlags config `mappend` flags
  (comp, conf) <- configCompilerAux flags'
  let setupScriptOptions = defaultSetupScriptOptions {
        useCompiler      = Just comp,
        useProgramConfig = conf,
        useDistPref      = fromFlagOrDefault
                             (useDistPref defaultSetupScriptOptions)
                             (configDistPref flags)
      }
  setupWrapper verbosity setupScriptOptions Nothing
    configureCommand (const flags') extraArgs

installAction :: (ConfigFlags, InstallFlags) -> [String] -> GlobalFlags -> IO ()
installAction (cflags,iflags) _ _globalFlags
  | fromFlag (installOnly iflags)
  = let verbosity = fromFlagOrDefault normal (configVerbosity cflags)
    in setupWrapper verbosity defaultSetupScriptOptions Nothing
         installCommand mempty []

installAction (cflags,iflags) extraArgs globalFlags = do
  pkgs <- either die return (parsePackageArgs extraArgs)
  let verbosity = fromFlagOrDefault normal (configVerbosity cflags)
  config <- loadConfig verbosity (globalConfigFile globalFlags)
                                 (configUserInstall cflags)
  let cflags' = savedConfigureFlags config `mappend` cflags
      iflags' = savedInstallFlags   config `mappend` iflags
  (comp, conf) <- configCompilerAux cflags'
  install verbosity
          (configPackageDB' cflags') (globalRepos (savedGlobalFlags config))
          comp conf cflags' iflags'
          [ UnresolvedDependency pkg (configConfigurationsFlags cflags')
          | pkg <- pkgs ]

listAction :: ListFlags -> [String] -> GlobalFlags -> IO ()
listAction listFlags extraArgs globalFlags = do
  let verbosity = fromFlag (listVerbosity listFlags)
  config <- loadConfig verbosity (globalConfigFile globalFlags) mempty
  let flags = savedConfigureFlags config
  (comp, conf) <- configCompilerAux flags
  list verbosity
       (configPackageDB' flags)
       (globalRepos (savedGlobalFlags config))
       comp
       conf
       listFlags
       extraArgs

updateAction :: Flag Verbosity -> [String] -> GlobalFlags -> IO ()
updateAction verbosityFlag extraArgs globalFlags = do
  unless (null extraArgs) $ do
    die $ "'update' doesn't take any extra arguments: " ++ unwords extraArgs
  let verbosity = fromFlag verbosityFlag
  config <- loadConfig verbosity (globalConfigFile globalFlags) mempty
  update verbosity (globalRepos (savedGlobalFlags config))

upgradeAction :: (ConfigFlags, InstallFlags) -> [String] -> GlobalFlags -> IO ()
upgradeAction (cflags,iflags) extraArgs globalFlags = do
  pkgs <- either die return (parsePackageArgs extraArgs)
  let verbosity = fromFlagOrDefault normal (configVerbosity cflags)
  config <- loadConfig verbosity (globalConfigFile globalFlags)
                                 (configUserInstall cflags)
  let cflags' = savedConfigureFlags config `mappend` cflags
      iflags' = savedInstallFlags   config `mappend` iflags
  (comp, conf) <- configCompilerAux cflags'
  upgrade verbosity
          (configPackageDB' cflags') (globalRepos (savedGlobalFlags config))
          comp conf cflags' iflags'
          [ UnresolvedDependency pkg (configConfigurationsFlags cflags')
          | pkg <- pkgs ]

fetchAction :: Flag Verbosity -> [String] -> GlobalFlags -> IO ()
fetchAction verbosityFlag extraArgs globalFlags = do
  pkgs <- either die return (parsePackageArgs extraArgs)
  let verbosity = fromFlag verbosityFlag
  config <- loadConfig verbosity (globalConfigFile globalFlags) mempty
  let flags = savedConfigureFlags config
  (comp, conf) <- configCompilerAux flags
  fetch verbosity
        (configPackageDB' flags) (globalRepos (savedGlobalFlags config))
        comp conf
        [ UnresolvedDependency pkg [] --TODO: flags?
        | pkg <- pkgs ]

uploadAction :: UploadFlags -> [String] -> GlobalFlags -> IO ()
uploadAction uploadFlags extraArgs globalFlags = do
  let verbosity = fromFlag (uploadVerbosity uploadFlags)
  config <- loadConfig verbosity (globalConfigFile globalFlags) mempty
  let uploadFlags' = savedUploadFlags config `mappend` uploadFlags
  -- FIXME: check that the .tar.gz files exist and report friendly error message if not
  let tarfiles = extraArgs
  checkTarFiles tarfiles
  if fromFlag (uploadCheck uploadFlags')
    then Upload.check  verbosity tarfiles
    else upload verbosity
                (globalRepos (savedGlobalFlags config))
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
  unless (null extraArgs) $ do
    die $ "'check' doesn't take any extra arguments: " ++ unwords extraArgs
  allOk <- Check.check (fromFlag verbosityFlag)
  unless allOk exitFailure


sdistAction :: SDistFlags -> [String] -> GlobalFlags -> IO ()
sdistAction sflags extraArgs _globalFlags = do
  unless (null extraArgs) $ do
    die $ "'sdist' doesn't take any extra arguments: " ++ unwords extraArgs
  sdist sflags

reportAction :: Flag Verbosity -> [String] -> GlobalFlags -> IO ()
reportAction verbosityFlag extraArgs globalFlags = do
  unless (null extraArgs) $ do
    die $ "'report' doesn't take any extra arguments: " ++ unwords extraArgs

  let verbosity = fromFlag verbosityFlag
  config <- loadConfig verbosity (globalConfigFile globalFlags) mempty

  Upload.report verbosity (globalRepos (savedGlobalFlags config))

win32SelfUpgradeAction :: [String] -> IO ()
win32SelfUpgradeAction (pid:path:rest) =
  Win32SelfUpgrade.deleteOldExeFile verbosity (read pid) path
  where
    verbosity = case rest of
      (['-','-','v','e','r','b','o','s','e','=',n]:_) | n `elem` ['0'..'9']
         -> fromMaybe Verbosity.normal (Verbosity.intToVerbosity (read [n]))
      _  ->           Verbosity.normal
win32SelfUpgradeAction _ = return ()
