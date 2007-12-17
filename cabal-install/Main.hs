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
import Hackage.Types (ConfigFlags(..))
import Distribution.PackageDescription (cabalVersion)
import Distribution.Simple.Setup (Flag, fromFlag, fromFlagOrDefault)
import qualified Distribution.Simple.Setup as Cabal
import Distribution.Simple.Program (defaultProgramConfiguration)
import Distribution.Simple.Command
import Distribution.Simple.SetupWrapper (setupWrapper)
import Distribution.Simple.UserHooks (Args)
import Hackage.Config           (defaultConfigFile, loadConfig, findCompiler)
import Hackage.List             (list)
import Hackage.Install          (install)
import Hackage.Info             (info)
import Hackage.Update           (update)
import Hackage.Fetch            (fetch)
--import Hackage.Clean            (clean)

import Distribution.Verbosity   (Verbosity, normal)
import Distribution.Version     (showVersion)
import qualified Paths_cabal_install (version)

import System.Environment       (getArgs, getProgName)
import System.Exit              (exitWith, ExitCode(..))
import Data.List                (intersperse)

-- | Entry point
--
main :: IO ()
main = getArgs >>= mainWorker

mainWorker :: Args -> IO ()
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
      [installCommand         `commandAddActionWithEmptyFlags` installAction
      ,infoCommand            `commandAddAction` infoAction
      ,listCommand            `commandAddAction` listAction
      ,updateCommand          `commandAddAction` updateAction
      ,fetchCommand           `commandAddAction` fetchAction

      ,wrapperAction (Cabal.configureCommand defaultProgramConfiguration)
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

{-
commandAddActionDebug :: CommandUI flags
                      -> (flags -> [String] -> (IO ()))
                      -> Command (IO ())
commandAddActionDebug command action =
  commandAddAction command $ \flags args -> do
    putStrLn $ commandName command ++ " flags:"
    print (commandShowOptions command flags)
    putStrLn $ commandName command ++ " args:"
    print args
--    action flags args

commandAddActionWithEmptyFlagsDebug
                      :: Monoid flags
                      => CommandUI flags
                      -> (flags -> [String] -> (IO ()))
                      -> Command (IO ())
commandAddActionWithEmptyFlagsDebug command action =
  commandAddActionWithEmptyFlags command $ \flags args -> do
    putStrLn $ commandName command ++ " flags:"
    print (commandShowOptions command flags)
    putStrLn $ commandName command ++ " args:"
    print args
--    action flags args
-}

installAction :: Cabal.ConfigFlags -> Args -> IO ()
installAction flags extraArgs =
  case parsePackageArgs extraArgs of
    Left  err  -> putStrLn err >> exitWith (ExitFailure 1)
    Right pkgs -> do
      configFile <- defaultConfigFile --FIXME
      config0 <- loadConfig configFile
      let config = updateConfig flags config0
      (comp, conf) <- findCompiler config
      install config comp conf flags pkgs

infoAction :: Cabal.Flag Verbosity -> Args -> IO ()
infoAction flags extraArgs = do
  configFile <- defaultConfigFile --FIXME
  config0 <- loadConfig configFile
  let config = config0 { configVerbose = fromFlagOrDefault normal flags }
  (comp, conf) <- findCompiler config
  case parsePackageArgs extraArgs of
    Left  err  -> putStrLn err >> exitWith (ExitFailure 1)
    Right pkgs -> info config comp conf pkgs

listAction :: Cabal.Flag Verbosity -> Args -> IO ()
listAction flags extraArgs = do
  configFile <- defaultConfigFile --FIXME
  config0 <- loadConfig configFile
  let config = config0 { configVerbose = fromFlagOrDefault normal flags }
  list config extraArgs

updateAction :: Flag Verbosity -> Args -> IO ()
updateAction flags _extraArgs = do
  configFile <- defaultConfigFile --FIXME
  config0 <- loadConfig configFile
  let config = config0 { configVerbose = fromFlagOrDefault normal flags }
  update config

fetchAction :: Flag Verbosity -> Args -> IO ()
fetchAction flags extraArgs = do
  configFile <- defaultConfigFile --FIXME
  config0 <- loadConfig configFile
  let config = config0 { configVerbose = fromFlagOrDefault normal flags }
  (comp, conf) <- findCompiler config
  case parsePackageArgs extraArgs of
    Left  err  -> putStrLn err >> exitWith (ExitFailure 1)
    Right pkgs -> fetch config comp conf pkgs
