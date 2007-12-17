-----------------------------------------------------------------------------
-- |
-- Module      :  Hackage.Setup
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
--
-----------------------------------------------------------------------------
module Hackage.Setup
    ( globalCommand, Cabal.GlobalFlags(..)
    , installCommand --Cabal.InstallFlags(..)
    , listCommand
    , updateCommand
    , infoCommand
    , fetchCommand

    , parsePackageArgs
    , updateConfig
    ) where

import Distribution.Simple.Program (defaultProgramConfiguration)
import Distribution.Simple.Compiler (PackageDB(..))
import Distribution.Simple.InstallDirs (combineInstallDirs)
import Distribution.Simple.Command
import qualified Distribution.Simple.Setup as Cabal
  (GlobalFlags(..),  {-emptyGlobalFlags,-}   globalCommand,
  ConfigFlags(..),   {-emptyConfigFlags,-}   configureCommand,
{-  CopyFlags(..),     emptyCopyFlags,     copyCommand,
  InstallFlags(..),  emptyInstallFlags,  installCommand,
  HaddockFlags(..),  emptyHaddockFlags,  haddockCommand,
  HscolourFlags(..), emptyHscolourFlags, hscolourCommand,
  BuildFlags(..),    emptyBuildFlags,    buildCommand,
  CleanFlags(..),    emptyCleanFlags,    cleanCommand,
  PFEFlags(..),      emptyPFEFlags,      programaticaCommand,
  MakefileFlags(..), emptyMakefileFlags, makefileCommand,
  RegisterFlags(..), emptyRegisterFlags, registerCommand, unregisterCommand,
  SDistFlags(..),    emptySDistFlags,    sdistCommand,
                                         testCommand-})
import Distribution.Simple.Setup (Flag, toFlag, fromFlagOrDefault,
                                  flagToMaybe, flagToList)
import Distribution.Verbosity (Verbosity, normal, flagToVerbosity, showForCabal)

import Hackage.Types (ConfigFlags(..), UnresolvedDependency(..))
import Hackage.Utils (readPToMaybe, parseDependencyOrPackageId)

import Control.Monad (MonadPlus(mplus))

-- | This function updates the configuration with the cabal configure flags.
updateConfig :: Cabal.ConfigFlags -> ConfigFlags -> ConfigFlags
updateConfig flags conf = conf {
      configCompiler          = override configCompiler Cabal.configHcFlavor,
      configCompilerPath      = configCompilerPath conf
                        `mplus` flagToMaybe (Cabal.configHcPath flags),
      configHcPkgPath         = configHcPkgPath conf
                        `mplus` flagToMaybe (Cabal.configHcPkg flags),
      configUserInstallDirs   = overrideInstallDirs userInstall
                                  (configUserInstallDirs conf),
      configGlobalInstallDirs = overrideInstallDirs (not userInstall)
                                  (configGlobalInstallDirs conf),
--    configCacheDir          =    :: FilePath,
--    configRepos             =    :: [Repo],
      configVerbose           = override configVerbose Cabal.configVerbose,
      configUserInstall       = userInstall
  }
  where override base over = fromFlagOrDefault (base conf) (over flags)
        overrideInstallDirs False base = base
        overrideInstallDirs True base = combineInstallDirs mplus base
                      (fmap flagToMaybe (Cabal.configInstallDirs flags))
        userInstall = case flagToMaybe $ Cabal.configPackageDB flags of
                        Nothing              -> configUserInstall conf
                        Just (UserPackageDB) -> True
                        Just _               -> False

globalCommand :: CommandUI Cabal.GlobalFlags
globalCommand = Cabal.globalCommand {
    commandDescription = Just $ \pname ->
         "Typical step for installing Cabal packages:\n"
      ++ "  " ++ pname ++ " install [PACKAGES]\n"
      ++ "\nOccasionally you need to update the list of available packages:\n"
      ++ "  " ++ pname ++ " update\n"
      ++ "\nFor more information about a command, try '"
          ++ pname ++ " COMMAND --help'."
      ++ "\nThis program is the command line interface to the Haskell Cabal Infrastructure."
      ++ "\nSee http://www.haskell.org/cabal/ for more information.\n"
  }

installCommand :: CommandUI Cabal.ConfigFlags
installCommand = (Cabal.configureCommand defaultProgramConfiguration) {
    commandName     = "install",
    commandSynopsis = "Installs a list of packages.",
    commandUsage    = usagePackages "install"
  }

fetchCommand :: CommandUI (Flag Verbosity)
fetchCommand = CommandUI {
    commandName         = "fetch",
    commandSynopsis     = "Downloads packages for later installation or study.",
    commandDescription  = Nothing,
    commandUsage        = usagePackages "fetch",
    commandDefaultFlags = toFlag normal,
    commandOptions      = \_ -> [optionVerbose id const]
  }

listCommand  :: CommandUI (Flag Verbosity)
listCommand = CommandUI {
    commandName         = "list",
    commandSynopsis     = "List available packages on the server (cached).",
    commandDescription  = Nothing,
    commandUsage        = usagePackages "list",
    commandDefaultFlags = toFlag normal,
    commandOptions      = \_ -> [optionVerbose id const]
  }

updateCommand  :: CommandUI (Flag Verbosity)
updateCommand = CommandUI {
    commandName         = "update",
    commandSynopsis     = "Updates list of known packages",
    commandDescription  = Nothing,
    commandUsage        = usagePackages "update",
    commandDefaultFlags = toFlag normal,
    commandOptions      = \_ -> [optionVerbose id const]
  }

{-
cleanCommand  :: CommandUI ()
cleanCommand = makeCommand name shortDesc longDesc emptyFlags options
  where
    name       = "clean"
    shortDesc  = "Removes downloaded files"
    longDesc   = Nothing
    emptyFlags = ()
    options _  = []
-}

infoCommand  :: CommandUI (Flag Verbosity)
infoCommand = CommandUI {
    commandName         = "info",
    commandSynopsis     = "Emit some info about dependency resolution",
    commandDescription  = Nothing,
    commandUsage        = usagePackages "info",
    commandDefaultFlags = toFlag normal,
    commandOptions      = \_ -> [optionVerbose id const]
  }

optionVerbose :: (flags -> Flag Verbosity)
              -> (Flag Verbosity -> flags -> flags)
              -> Option flags
optionVerbose get set =
  option "v" ["verbose"]
    "Control verbosity (n is 0--3, default verbosity level is 1)"
    get set
    (optArg "n" (toFlag . flagToVerbosity)
                (fmap (Just . showForCabal) . flagToList))

usagePackages :: String -> String -> String
usagePackages pname name =
     "Usage: " ++ pname ++ " " ++ name ++ " [FLAGS]\n"
  ++ "   or: " ++ pname ++ " " ++ name ++ " [PACKAGES]\n\n"
  ++ "Flags for " ++ name ++ ":"

parsePackageArgs :: [String] -> Either String [UnresolvedDependency]
parsePackageArgs = parsePkgArgs []
  where
    parsePkgArgs ds [] = Right (reverse ds)
    parsePkgArgs ds (arg:args) =
      case readPToMaybe parseDependencyOrPackageId arg of
        Just dep -> let d = UnresolvedDependency {
                              dependency = dep,
                              depFlags   = []
                            }
                     in parsePkgArgs (d:ds) args
        Nothing  -> Left ("Failed to parse package dependency: " ++ show arg)
