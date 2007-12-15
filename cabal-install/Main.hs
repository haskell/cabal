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
import qualified Distribution.Simple.Setup as Cabal
import Distribution.Simple.Setup (fromFlag)
import Distribution.Simple.Program
import Distribution.Simple.Command
import Distribution.Simple.UserHooks (Args)
import Hackage.Config           (defaultConfigFile, loadConfig, findCompiler)
import Hackage.List             (list)
import Hackage.Install          (install)
import Hackage.Info             (info)
import Hackage.Update           (update)
import Hackage.Fetch            (fetch)
--import Hackage.Clean            (clean)

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
    CommandErrors errs                 -> printErrors errs
    CommandReadyToGo (flags, commandParse)  ->
      case commandParse of
        _ | fromFlag (globalVersion flags)        -> printVersion
          | fromFlag (globalNumericVersion flags) -> printNumericVersion
        CommandHelp     help           -> printHelp help
        CommandErrors   errs           -> printErrors errs
        CommandReadyToGo action        -> action

  where
    printHelp help = getProgName >>= putStr . help
    printErrors errs = do
      putStr (concat (intersperse "\n" errs))
      exitWith (ExitFailure 1)
    printNumericVersion = putStrLn $ showVersion Paths_cabal_install.version
    printVersion        = putStrLn $ "cabal-install version "
                                  ++ showVersion Paths_cabal_install.version
                                  ++ "\nusing version "
                                  ++ showVersion cabalVersion
                                  ++ " of the Cabal library "

    progs = defaultProgramConfiguration
    commands =
      [installCommand progs   `commandAddActionWithEmptyFlags` installAction
      ,infoCommand            `commandAddAction` infoAction
      ,listCommand            `commandAddAction` listAction
      ,updateCommand          `commandAddAction` updateAction
      ,fetchCommand           `commandAddAction` fetchAction
{-
      ,configureCommand progs `commandAddAction` configureAction args
      ,buildCommand     progs `commandAddAction` buildAction args
      ,copyCommand            `commandAddAction` copyAction args
      ,haddockCommand         `commandAddAction` haddockAction args
      ,cleanCommand           `commandAddAction` cleanAction args
      ,sdistCommand           `commandAddAction` sdistAction args
      ,hscolourCommand        `commandAddAction` hscolourAction args
      ,registerCommand        `commandAddAction` registerAction args
--      ,unregisterCommand      `commandAddAction` unregisterAction
      ,testCommand            `commandAddAction` testAction args
--      ,programaticaCommand    `commandAddAction` programaticaAction
--      ,makefileCommand        `commandAddAction` makefileAction
-}
      ]

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

infoAction :: () -> Args -> IO ()
infoAction _flags extraArgs = do
  configFile <- defaultConfigFile --FIXME
  config <- loadConfig configFile
  (comp, conf) <- findCompiler config
  case parsePackageArgs extraArgs of
    Left  err  -> putStrLn err >> exitWith (ExitFailure 1)
    Right pkgs -> info config comp conf [] pkgs

listAction :: () -> Args -> IO ()
listAction _flags extraArgs = do
  configFile <- defaultConfigFile --FIXME
  config <- loadConfig configFile
  list config extraArgs

updateAction :: () -> Args -> IO ()
updateAction _flags _extraArgs = do
  configFile <- defaultConfigFile --FIXME
  config <- loadConfig configFile
  update config

fetchAction :: () -> Args -> IO ()
fetchAction _flags extraArgs = do
  configFile <- defaultConfigFile --FIXME
  config <- loadConfig configFile
  (comp, conf) <- findCompiler config
  case parsePackageArgs extraArgs of
    Left  err  -> putStrLn err >> exitWith (ExitFailure 1)
    Right pkgs -> fetch config comp conf [] pkgs

{-
configureAction :: Args -> ConfigFlags -> Args -> IO ()
configureAction allArgs flags extraArgs = return ()

buildAction :: Args -> BuildFlags -> Args -> IO ()
buildAction allArgs flags extraArgs = return ()

copyAction :: Args -> CopyFlags -> Args -> IO ()
copyAction allArgs flags extraArgs = return ()

hscolourAction :: Args -> HscolourFlags -> Args -> IO ()
hscolourAction allArgs flags extraArgs = return ()

haddockAction :: Args -> HaddockFlags -> Args -> IO ()
haddockAction allArgs flags extraArgs = return ()

sdistAction :: Args -> SDistFlags -> Args -> IO ()
sdistAction allArgs flags extraArgs = return ()

cleanAction :: Args -> CleanFlags -> Args -> IO ()
cleanAction allArgs flags extraArgs = return ()

testAction :: Args -> () -> Args -> IO ()
testAction allArgs flags extraArgs = return ()

registerAction :: Args -> RegisterFlags -> Args -> IO ()
registerAction flags extraArgs = return ()
-}
