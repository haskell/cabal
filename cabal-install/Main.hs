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
import Distribution.Client.Types
         ( UnresolvedDependency(UnresolvedDependency) )

import Distribution.Simple.Setup
         ( Flag(..), fromFlag, fromFlagOrDefault, flagToMaybe
         , SDistFlags, sdistCommand )
import qualified Distribution.Simple.Setup as Cabal
import Distribution.Simple.Program (defaultProgramConfiguration)
import Distribution.Simple.Command
import Distribution.Simple.Configure (configCompilerAux)
import Distribution.Simple.Utils (cabalVersion, die, intercalate)
import Distribution.Text
         ( display )

import Distribution.Client.SetupWrapper
         ( setupWrapper, SetupScriptOptions(..), defaultSetupScriptOptions )
import Distribution.Client.Config
         ( SavedConfig(..), savedConfigToConfigFlags, defaultConfigFile
         , loadConfig, configRepos, configPackageDB )
import Distribution.Client.List             (list)
import Distribution.Client.Install          (install, upgrade)
import Distribution.Client.Update           (update)
import Distribution.Client.Fetch            (fetch)
import Distribution.Client.Check as Check   (check)
--import Distribution.Client.Clean            (clean)
import Distribution.Client.Upload as Upload (upload, check, report)
import Distribution.Client.SrcDist          (sdist)

import Distribution.Verbosity   (Verbosity, normal)
import qualified Paths_cabal_install (version)

import System.Environment       (getArgs, getProgName)
import System.Exit              (exitFailure)
import System.FilePath          (splitExtension, takeExtension)
import System.Directory         (doesFileExist)
import Data.List                (intersperse)
import Data.Monoid              (Monoid(..))
import Control.Monad            (unless)

-- | Entry point
--
main :: IO ()
main = getArgs >>= mainWorker

mainWorker :: [String] -> IO ()
mainWorker args = 
  case commandsRun globalCommand commands args of
    CommandHelp   help                 -> printHelp help
    CommandList   opts                 -> printOptionsList opts
    CommandErrors errs                 -> printErrors errs
    CommandReadyToGo (flags, commandParse)  ->
      case commandParse of
        _ | fromFlag (globalVersion flags)        -> printVersion
          | fromFlag (globalNumericVersion flags) -> printNumericVersion
        CommandHelp     help           -> printHelp help
        CommandList     opts           -> printOptionsList opts
        CommandErrors   errs           -> printErrors errs
        CommandReadyToGo action        -> action

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
      ,wrapperAction (Cabal.buildCommand defaultProgramConfiguration)
                     Cabal.buildVerbosity    Cabal.buildDistPref
      ,wrapperAction Cabal.copyCommand
                     Cabal.copyVerbosity     Cabal.copyDistPref
      ,wrapperAction Cabal.haddockCommand
                     Cabal.haddockVerbosity  Cabal.haddockDistPref
      ,wrapperAction Cabal.cleanCommand
                     Cabal.cleanVerbosity    Cabal.cleanDistPref
      ,wrapperAction Cabal.hscolourCommand
                     Cabal.hscolourVerbosity Cabal.hscolourDistPref
      ,wrapperAction Cabal.registerCommand
                     Cabal.regVerbosity      Cabal.regDistPref
      ,wrapperAction Cabal.testCommand
                     Cabal.testVerbosity     Cabal.testDistPref
      ]

wrapperAction :: Monoid flags
              => CommandUI flags
              -> (flags -> Flag Verbosity)
              -> (flags -> Flag String)
              -> Command (IO ())
wrapperAction command verbosityFlag distPrefFlag =
  commandAddAction command $ \flags extraArgs -> do
    let verbosity = fromFlagOrDefault normal (verbosityFlag flags)
        setupScriptOptions = defaultSetupScriptOptions {
          useDistPref = fromFlagOrDefault
                          (useDistPref defaultSetupScriptOptions)
                          (distPrefFlag flags)
        }
    setupWrapper verbosity setupScriptOptions Nothing
                 command (const flags) extraArgs

configureAction :: Cabal.ConfigFlags -> [String] -> IO ()
configureAction flags extraArgs = do
  configFile <- defaultConfigFile --FIXME
  let verbosity = fromFlagOrDefault normal (Cabal.configVerbosity flags)
  config <- loadConfig verbosity configFile
  let flags' = savedConfigToConfigFlags (Cabal.configUserInstall flags) config
               `mappend` flags
  (comp, conf) <- configCompilerAux flags'
  let setupScriptOptions = defaultSetupScriptOptions {
        useCompiler      = Just comp,
        useProgramConfig = conf,
        useDistPref      = fromFlagOrDefault
                             (useDistPref defaultSetupScriptOptions)
                             (Cabal.configDistPref flags)
      }
  setupWrapper verbosity setupScriptOptions Nothing
    configureCommand (const flags') extraArgs

installAction :: (Cabal.ConfigFlags, InstallFlags) -> [String] -> IO ()
installAction (cflags,iflags) _
  | Cabal.fromFlag (installOnly iflags)
  = let verbosity = fromFlagOrDefault normal (Cabal.configVerbosity cflags)
    in setupWrapper verbosity defaultSetupScriptOptions Nothing
         Cabal.installCommand mempty []

installAction (cflags,iflags) extraArgs = do
  pkgs <- either die return (parsePackageArgs extraArgs)
  configFile <- defaultConfigFile --FIXME
  let verbosity = fromFlagOrDefault normal (Cabal.configVerbosity cflags)
  config <- loadConfig verbosity configFile
  let cflags' = savedConfigToConfigFlags (Cabal.configUserInstall cflags) config
               `mappend` cflags
  (comp, conf) <- configCompilerAux cflags'
  install verbosity
          (configPackageDB cflags') (configRepos config)
          comp conf cflags' iflags {
            installSymlinkBinDir = configSymlinkBinDir config
          }
          [ UnresolvedDependency pkg (Cabal.configConfigurationsFlags cflags')
          | pkg <- pkgs ]

listAction :: ListFlags -> [String] -> IO ()
listAction listFlags extraArgs = do
  configFile <- defaultConfigFile --FIXME
  let verbosity = fromFlag (listVerbosity listFlags)
  config <- loadConfig verbosity configFile
  let flags = savedConfigToConfigFlags NoFlag config
  (comp, conf) <- configCompilerAux flags
  list verbosity
       (configPackageDB flags)
       (configRepos config)
       comp
       conf
       listFlags
       extraArgs

updateAction :: Flag Verbosity -> [String] -> IO ()
updateAction verbosityFlag extraArgs = do
  unless (null extraArgs) $ do
    die $ "'update' doesn't take any extra arguments: " ++ unwords extraArgs
  configFile <- defaultConfigFile --FIXME
  let verbosity = fromFlag verbosityFlag
  config <- loadConfig verbosity configFile
  update verbosity (configRepos config)

upgradeAction :: (Cabal.ConfigFlags, InstallFlags) -> [String] -> IO ()
upgradeAction (cflags,iflags) extraArgs = do
  pkgs <- either die return (parsePackageArgs extraArgs)
  configFile <- defaultConfigFile --FIXME
  let verbosity = fromFlagOrDefault normal (Cabal.configVerbosity cflags)
  config <- loadConfig verbosity configFile
  let cflags' = savedConfigToConfigFlags (Cabal.configUserInstall cflags) config
               `mappend` cflags
  (comp, conf) <- configCompilerAux cflags'
  upgrade verbosity
          (configPackageDB cflags') (configRepos config)
          comp conf cflags' iflags {
            installSymlinkBinDir = configSymlinkBinDir config
          }
          [ UnresolvedDependency pkg (Cabal.configConfigurationsFlags cflags')
          | pkg <- pkgs ]

fetchAction :: Flag Verbosity -> [String] -> IO ()
fetchAction verbosityFlag extraArgs = do
  pkgs <- either die return (parsePackageArgs extraArgs)
  configFile <- defaultConfigFile --FIXME
  let verbosity = fromFlag verbosityFlag
  config <- loadConfig verbosity configFile
  let flags = savedConfigToConfigFlags NoFlag config
  (comp, conf) <- configCompilerAux flags
  fetch verbosity
        (configPackageDB flags) (configRepos config)
        comp conf
        [ UnresolvedDependency pkg [] --TODO: flags?
        | pkg <- pkgs ]

uploadAction :: UploadFlags -> [String] -> IO ()
uploadAction flags extraArgs = do
  configFile <- defaultConfigFile --FIXME
  let verbosity = fromFlag (uploadVerbosity flags)
  config <- loadConfig verbosity configFile
  -- FIXME: check that the .tar.gz files exist and report friendly error message if not
  let tarfiles = extraArgs
  checkTarFiles tarfiles
  if fromFlag (uploadCheck flags)
    then Upload.check  verbosity tarfiles
    else upload verbosity 
                (flagToMaybe $ configUploadUsername config
                     `mappend` uploadUsername flags)
                (flagToMaybe $ configUploadPassword config
                     `mappend` uploadPassword flags)
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

checkAction :: Flag Verbosity -> [String] -> IO ()
checkAction verbosityFlag extraArgs = do
  unless (null extraArgs) $ do
    die $ "'check' doesn't take any extra arguments: " ++ unwords extraArgs
  allOk <- Check.check (fromFlag verbosityFlag)
  unless allOk exitFailure


sdistAction :: SDistFlags -> [String] -> IO ()
sdistAction sflags extraArgs = do
  unless (null extraArgs) $ do
    die $ "'sdist' doesn't take any extra arguments: " ++ unwords extraArgs
  sdist sflags

reportAction :: Flag Verbosity -> [String] -> IO ()
reportAction verbosityFlag extraArgs = do
  unless (null extraArgs) $ do
    die $ "'report' doesn't take any extra arguments: " ++ unwords extraArgs

  configFile <- defaultConfigFile --FIXME
  let verbosity = fromFlag verbosityFlag
  config <- loadConfig verbosity configFile

  Upload.report verbosity (configRepos config)
