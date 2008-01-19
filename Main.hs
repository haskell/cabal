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

import Hackage.Setup
import Distribution.PackageDescription (cabalVersion)
import Distribution.Simple.Setup (Flag, fromFlag, fromFlagOrDefault,
                                  flagToMaybe)
import qualified Distribution.Simple.Setup as Cabal
import Distribution.Simple.Program (defaultProgramConfiguration)
import Distribution.Simple.Command
import Distribution.Simple.SetupWrapper (setupWrapper)
import Distribution.Simple.Configure (configCompilerAux)
import Distribution.Simple.Utils (die)
import Hackage.Config           (SavedConfig(..), savedConfigToConfigFlags,
                                 defaultConfigFile, loadConfig, configRepos,
                                 configPackageDB)
import Hackage.List             (list)
import Hackage.Install          (install)
import Hackage.Info             (info)
import Hackage.Update           (update)
import Hackage.Upgrade          (upgrade)
import Hackage.Fetch            (fetch)
--import Hackage.Clean            (clean)
import Hackage.Upload           (upload, check)

import Distribution.Verbosity   (Verbosity, normal)
import Distribution.Version     (showVersion)
import qualified Paths_cabal_install (version)

import System.Environment       (getArgs, getProgName)
import System.Exit              (exitWith, ExitCode(..))
import Data.List                (intersperse)
import Data.Monoid              (Monoid(..))

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
      exitWith (ExitFailure 1)
    printNumericVersion = putStrLn $ showVersion Paths_cabal_install.version
    printVersion        = putStrLn $ "cabal-install version "
                                  ++ showVersion Paths_cabal_install.version
                                  ++ "\nusing version "
                                  ++ showVersion cabalVersion
                                  ++ " of the Cabal library "

    commands =
      [configureCommand       `commandAddAction` configureAction
      ,installCommand         `commandAddAction` installAction
      ,infoCommand            `commandAddAction` infoAction
      ,listCommand            `commandAddAction` listAction
      ,updateCommand          `commandAddAction` updateAction
      ,upgradeCommand         `commandAddAction` upgradeAction
      ,fetchCommand           `commandAddAction` fetchAction
      ,uploadCommand          `commandAddAction` uploadAction

      ,wrapperAction (Cabal.buildCommand     defaultProgramConfiguration)
      ,wrapperAction Cabal.copyCommand
      ,wrapperAction Cabal.haddockCommand
      ,wrapperAction Cabal.cleanCommand
      ,wrapperAction Cabal.sdistCommand
      ,wrapperAction Cabal.hscolourCommand
      ,wrapperAction Cabal.registerCommand
--      ,wrapperAction unregisterCommand
      ,wrapperAction Cabal.testCommand
--      ,wrapperAction programaticaCommand
--      ,wrapperAction makefileCommand
      ]

wrapperAction :: CommandUI flags -> Command (IO ())
wrapperAction command =
  commandAddAction command $ \flags extraArgs ->
  let args = commandName command : commandShowOptions command flags ++ extraArgs
   in setupWrapper args Nothing

configureAction :: Cabal.ConfigFlags -> [String] -> IO ()
configureAction flags extraArgs = do
  configFile <- defaultConfigFile --FIXME
  let verbosity = fromFlagOrDefault normal (Cabal.configVerbose flags)
  config <- loadConfig verbosity configFile
  let flags' = savedConfigToConfigFlags (Cabal.configPackageDB flags) config
               `mappend` flags
      args = commandName configureCommand
           : commandShowOptions configureCommand flags' ++ extraArgs
  setupWrapper args Nothing

installAction :: Cabal.ConfigFlags -> [String] -> IO ()
installAction flags extraArgs = do
  pkgs <- either die return (parsePackageArgs extraArgs)
  configFile <- defaultConfigFile --FIXME
  let verbosity = fromFlagOrDefault normal (Cabal.configVerbose flags)
  config <- loadConfig verbosity configFile
  let flags' = savedConfigToConfigFlags (Cabal.configPackageDB flags) config
               `mappend` flags
  (comp, conf) <- configCompilerAux flags'
  install verbosity
          (fromFlag $ Cabal.configPackageDB flags') (configRepos config)
          comp conf flags' pkgs

infoAction :: Cabal.Flag Verbosity -> [String] -> IO ()
infoAction verbosityFlag extraArgs = do
  pkgs <- either die return (parsePackageArgs extraArgs)
  configFile <- defaultConfigFile --FIXME
  let verbosity = fromFlag verbosityFlag
  config <- loadConfig verbosity configFile
  let flags = savedConfigToConfigFlags (configPackageDB config) config
  (comp, conf) <- configCompilerAux flags
  info verbosity
       (fromFlag $ Cabal.configPackageDB flags) (configRepos config)
       comp conf pkgs

listAction :: Cabal.Flag Verbosity -> [String] -> IO ()
listAction verbosityFlag extraArgs = do
  configFile <- defaultConfigFile --FIXME
  let verbosity = fromFlag verbosityFlag
  config <- loadConfig verbosity configFile
  list verbosity (configRepos config) extraArgs

updateAction :: Flag Verbosity -> [String] -> IO ()
updateAction verbosityFlag _extraArgs = do
  configFile <- defaultConfigFile --FIXME
  let verbosity = fromFlag verbosityFlag
  config <- loadConfig verbosity configFile
  update verbosity (configRepos config)

upgradeAction :: Cabal.ConfigFlags -> [String] -> IO ()
upgradeAction flags _extraArgs = do
  configFile <- defaultConfigFile --FIXME
  let verbosity = fromFlagOrDefault normal (Cabal.configVerbose flags)
  config <- loadConfig verbosity configFile
  let flags' = savedConfigToConfigFlags (Cabal.configPackageDB flags) config
               `mappend` flags
  (comp, conf) <- configCompilerAux flags'
  upgrade verbosity
          (fromFlag $ Cabal.configPackageDB flags') (configRepos config)
          comp conf flags'

fetchAction :: Flag Verbosity -> [String] -> IO ()
fetchAction verbosityFlag extraArgs = do
  pkgs <- either die return (parsePackageArgs extraArgs)
  configFile <- defaultConfigFile --FIXME
  let verbosity = fromFlag verbosityFlag
  config <- loadConfig verbosity configFile
  let flags = savedConfigToConfigFlags (configPackageDB config) config
  (comp, conf) <- configCompilerAux flags
  fetch verbosity
        (fromFlag $ Cabal.configPackageDB flags) (configRepos config)
        comp conf pkgs

uploadAction :: UploadFlags -> [String] -> IO ()
uploadAction flags extraArgs = do
  configFile <- defaultConfigFile --FIXME
  let verbosity = fromFlag (uploadVerbosity flags)
  config <- loadConfig verbosity configFile
  -- FIXME: check that the .tar.gz files exist and report friendly error message if not
  let tarfiles = extraArgs
  if fromFlag (uploadCheck flags)
    then check  verbosity tarfiles
    else upload verbosity 
                (flagToMaybe $ configUploadUsername config
                     `mappend` uploadUsername flags)
                (flagToMaybe $ configUploadPassword config
                     `mappend` uploadPassword flags)
                tarfiles
