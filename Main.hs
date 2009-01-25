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
         , InfoFlags(..), infoCommand
         , UploadFlags(..), uploadCommand
         , reportCommand
         , unpackCommand, UnpackFlags(..)
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
         ( SavedConfig(..), loadConfig, defaultConfigFile )
import Distribution.Client.List             (list, info)
import Distribution.Client.Install          (install, upgrade)
import Distribution.Client.Configure        (configure)
import Distribution.Client.Update           (update)
import Distribution.Client.Fetch            (fetch)
import Distribution.Client.Check as Check   (check)
--import Distribution.Client.Clean            (clean)
import Distribution.Client.Upload as Upload (upload, check, report)
import Distribution.Client.SrcDist          (sdist)
import Distribution.Client.Unpack           (unpack)
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
      ,infoCommand            `commandAddAction` infoAction
      ,updateCommand          `commandAddAction` updateAction
      ,upgradeCommand         `commandAddAction` upgradeAction
      ,fetchCommand           `commandAddAction` fetchAction
      ,uploadCommand          `commandAddAction` uploadAction
      ,checkCommand           `commandAddAction` checkAction
      ,sdistCommand           `commandAddAction` sdistAction
      ,reportCommand          `commandAddAction` reportAction
      ,unpackCommand          `commandAddAction` unpackAction
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
configureAction configFlags extraArgs globalFlags = do
  let verbosity = fromFlagOrDefault normal (configVerbosity configFlags)
  config <- loadConfig verbosity (globalConfigFile globalFlags)
                                 (configUserInstall configFlags)
  let configFlags'  = savedConfigureFlags config `mappend` configFlags
      installFlags' = savedInstallFlags   config --TODO: `mappend` installFlags
      globalFlags'  = savedGlobalFlags    config `mappend` globalFlags
  (comp, conf) <- configCompilerAux configFlags'
  configure verbosity
            (configPackageDB' configFlags') (globalRepos globalFlags')
            comp conf configFlags' installFlags' extraArgs

installAction :: (ConfigFlags, InstallFlags) -> [String] -> GlobalFlags -> IO ()
installAction (configFlags, installFlags) _ _globalFlags
  | fromFlagOrDefault False (installOnly installFlags)
  = let verbosity = fromFlagOrDefault normal (configVerbosity configFlags)
    in setupWrapper verbosity defaultSetupScriptOptions Nothing
         installCommand (const mempty) []

installAction (configFlags, installFlags) extraArgs globalFlags = do
  pkgs <- either die return (parsePackageArgs extraArgs)
  let verbosity = fromFlagOrDefault normal (configVerbosity configFlags)
  config <- loadConfig verbosity (globalConfigFile globalFlags)
                                 (configUserInstall configFlags)
  let configFlags'  = savedConfigureFlags config `mappend` configFlags
      installFlags' = savedInstallFlags   config `mappend` installFlags
      globalFlags'  = savedGlobalFlags    config `mappend` globalFlags
  (comp, conf) <- configCompilerAux configFlags'
  install verbosity
          (configPackageDB' configFlags') (globalRepos globalFlags')
          comp conf configFlags' installFlags'
          [ UnresolvedDependency pkg (configConfigurationsFlags configFlags')
          | pkg <- pkgs ]

listAction :: ListFlags -> [String] -> GlobalFlags -> IO ()
listAction listFlags extraArgs globalFlags = do
  let verbosity = fromFlag (listVerbosity listFlags)
  config <- loadConfig verbosity (globalConfigFile globalFlags) mempty
  let configFlags  = savedConfigureFlags config
      globalFlags' = savedGlobalFlags    config `mappend` globalFlags
  (comp, conf) <- configCompilerAux configFlags
  list verbosity
       (configPackageDB' configFlags)
       (globalRepos globalFlags')
       comp
       conf
       listFlags
       extraArgs

infoAction :: InfoFlags -> [String] -> GlobalFlags -> IO ()
infoAction infoFlags extraArgs globalFlags = do
  pkgs <- either die return (parsePackageArgs extraArgs)
  let verbosity = fromFlag (infoVerbosity infoFlags)
  config <- loadConfig verbosity (globalConfigFile globalFlags) mempty
  let configFlags  = savedConfigureFlags config
      globalFlags' = savedGlobalFlags    config `mappend` globalFlags
  (comp, conf) <- configCompilerAux configFlags
  info verbosity
       (configPackageDB' configFlags)
       (globalRepos globalFlags')
       comp
       conf
       infoFlags
       [ UnresolvedDependency pkg []  | pkg <- pkgs ]

updateAction :: Flag Verbosity -> [String] -> GlobalFlags -> IO ()
updateAction verbosityFlag extraArgs globalFlags = do
  unless (null extraArgs) $ do
    die $ "'update' doesn't take any extra arguments: " ++ unwords extraArgs
  let verbosity = fromFlag verbosityFlag
  config <- loadConfig verbosity (globalConfigFile globalFlags) mempty
  let globalFlags' = savedGlobalFlags config `mappend` globalFlags
  update verbosity (globalRepos globalFlags')

upgradeAction :: (ConfigFlags, InstallFlags) -> [String] -> GlobalFlags -> IO ()
upgradeAction (configFlags, installFlags) extraArgs globalFlags = do
  pkgs <- either die return (parsePackageArgs extraArgs)
  let verbosity = fromFlagOrDefault normal (configVerbosity configFlags)
  config <- loadConfig verbosity (globalConfigFile globalFlags)
                                 (configUserInstall configFlags)
  let configFlags'  = savedConfigureFlags config `mappend` configFlags
      installFlags' = savedInstallFlags   config `mappend` installFlags
      globalFlags'  = savedGlobalFlags    config `mappend` globalFlags
  (comp, conf) <- configCompilerAux configFlags'
  upgrade verbosity
          (configPackageDB' configFlags') (globalRepos globalFlags')
          comp conf configFlags' installFlags'
          [ UnresolvedDependency pkg (configConfigurationsFlags configFlags')
          | pkg <- pkgs ]

fetchAction :: Flag Verbosity -> [String] -> GlobalFlags -> IO ()
fetchAction verbosityFlag extraArgs globalFlags = do
  pkgs <- either die return (parsePackageArgs extraArgs)
  let verbosity = fromFlag verbosityFlag
  config <- loadConfig verbosity (globalConfigFile globalFlags) mempty
  let configFlags  = savedConfigureFlags config
      globalFlags' = savedGlobalFlags config `mappend` globalFlags
  (comp, conf) <- configCompilerAux configFlags
  fetch verbosity
        (configPackageDB' configFlags) (globalRepos globalFlags')
        comp conf
        [ UnresolvedDependency pkg [] --TODO: flags?
        | pkg <- pkgs ]

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
  let globalFlags' = savedGlobalFlags config `mappend` globalFlags

  Upload.report verbosity (globalRepos globalFlags')

unpackAction :: UnpackFlags -> [String] -> GlobalFlags -> IO ()
unpackAction flags extraArgs globalFlags = do
  pkgs <- either die return (parsePackageArgs extraArgs)
  let verbosity = fromFlag (unpackVerbosity flags)
  config <- loadConfig verbosity (globalConfigFile globalFlags) mempty
  unpack flags (globalRepos (savedGlobalFlags config)) pkgs

win32SelfUpgradeAction :: [String] -> IO ()
win32SelfUpgradeAction (pid:path:rest) =
  Win32SelfUpgrade.deleteOldExeFile verbosity (read pid) path
  where
    verbosity = case rest of
      (['-','-','v','e','r','b','o','s','e','=',n]:_) | n `elem` ['0'..'9']
         -> fromMaybe Verbosity.normal (Verbosity.intToVerbosity (read [n]))
      _  ->           Verbosity.normal
win32SelfUpgradeAction _ = return ()
