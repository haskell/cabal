-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Setup
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
--
-----------------------------------------------------------------------------
module Distribution.Client.Setup
    ( globalCommand, Cabal.GlobalFlags(..)
    , configureCommand, filterConfigureFlags
    , installCommand, InstallFlags(..)
    , listCommand, ListFlags(..)
    , updateCommand
    , upgradeCommand
    , infoCommand
    , fetchCommand
    , checkCommand
    , uploadCommand, UploadFlags(..)

    , parsePackageArgs
    ) where

import Distribution.Client.Types
         ( Username(..), Password(..) )

import Distribution.Simple.Program
         ( defaultProgramConfiguration )
import Distribution.Simple.Command hiding (boolOpt)
import qualified Distribution.Simple.Command as Command
import qualified Distribution.Simple.Setup as Cabal
         ( GlobalFlags(..), globalCommand
         , ConfigFlags(..), configureCommand )
import Distribution.Simple.Setup
         ( Flag(..), toFlag, flagToList, flagToMaybe, trueArg, optionVerbosity )
import Distribution.Version
         ( Version(Version), VersionRange(..) )
import Distribution.Package
         ( PackageIdentifier, packageName, packageVersion, Dependency(..) )
import Distribution.Text
         ( Text(parse), display )
import Distribution.ReadE
         ( readP_to_E )
import Distribution.Compat.ReadP
         ( ReadP, readP_to_S, (+++) )
import Distribution.Verbosity
         ( Verbosity, normal )

import Data.Char     (isSpace)
import Data.Maybe    (listToMaybe)
import Data.Monoid   (Monoid(..))
import Control.Monad (liftM)


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

configureCommand :: CommandUI Cabal.ConfigFlags
configureCommand = (Cabal.configureCommand defaultProgramConfiguration) {
    commandDefaultFlags = mempty
  }

filterConfigureFlags :: Cabal.ConfigFlags -> Version -> Cabal.ConfigFlags
filterConfigureFlags flags cabalLibVersion
  | cabalLibVersion >= Version [1,3,10] [] = flags
    -- older Cabal does not grok the constraints flag:
  | otherwise = flags { Cabal.configConstraints = [] }


fetchCommand :: CommandUI (Flag Verbosity)
fetchCommand = CommandUI {
    commandName         = "fetch",
    commandSynopsis     = "Downloads packages for later installation or study.",
    commandDescription  = Nothing,
    commandUsage        = usagePackages "fetch",
    commandDefaultFlags = toFlag normal,
    commandOptions      = \_ -> [optionVerbosity id const]
  }

updateCommand  :: CommandUI (Flag Verbosity)
updateCommand = CommandUI {
    commandName         = "update",
    commandSynopsis     = "Updates list of known packages",
    commandDescription  = Nothing,
    commandUsage        = usagePackages "update",
    commandDefaultFlags = toFlag normal,
    commandOptions      = \_ -> [optionVerbosity id const]
  }

upgradeCommand  :: CommandUI (Cabal.ConfigFlags, InstallFlags)
upgradeCommand = configureCommand {
    commandName         = "upgrade",
    commandSynopsis     = "Upgrades installed packages to the latest available version",
    commandDescription  = Nothing,
    commandUsage        = usagePackages "upgrade",
    commandDefaultFlags = (mempty, defaultInstallFlags),
    commandOptions      = commandOptions installCommand
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
    commandOptions      = \_ -> [optionVerbosity id const]
  }

checkCommand  :: CommandUI (Flag Verbosity)
checkCommand = CommandUI {
    commandName         = "check",
    commandSynopsis     = "Check the package for common mistakes",
    commandDescription  = Nothing,
    commandUsage        = \pname -> "Usage: " ++ pname ++ " check\n",
    commandDefaultFlags = mempty,
    commandOptions      = mempty
  }

-- ------------------------------------------------------------
-- * List flags
-- ------------------------------------------------------------

data ListFlags = ListFlags {
    listInstalled :: Flag Bool,
    listSimpleOutput :: Flag Bool,
    listVerbosity :: Flag Verbosity
  }

defaultListFlags :: ListFlags
defaultListFlags = ListFlags {
    listInstalled = Flag False,
    listSimpleOutput = Flag False,
    listVerbosity = toFlag normal
  }

listCommand  :: CommandUI ListFlags
listCommand = CommandUI {
    commandName         = "list",
    commandSynopsis     = "List available packages on the server (cached).",
    commandDescription  = Nothing,
    commandUsage        = usagePackages "list",
    commandDefaultFlags = defaultListFlags,
    commandOptions      = \_ -> [
        optionVerbosity listVerbosity (\v flags -> flags { listVerbosity = v })

        , option [] ["installed"]
            "Only print installed packages"
            listInstalled (\v flags -> flags { listInstalled = v })
            trueArg

        , option [] ["simple-output"]
            "Print in a easy-to-parse format"
            listSimpleOutput (\v flags -> flags { listSimpleOutput = v })
            trueArg

        ]
  }

instance Monoid ListFlags where
  mempty = defaultListFlags
  mappend a b = ListFlags {
    listInstalled = combine listInstalled,
    listSimpleOutput = combine listSimpleOutput,
    listVerbosity = combine listVerbosity
  }
    where combine field = field a `mappend` field b

-- ------------------------------------------------------------
-- * Install flags
-- ------------------------------------------------------------

-- | Install takes the same flags as configure along with a few extras.
--
data InstallFlags = InstallFlags {
    installDocumentation:: Flag Bool,
    installDryRun       :: Flag Bool,
    installOnly         :: Flag Bool,
    installRootCmd      :: Flag String,
    installCabalVersion :: Flag Version,
    installLogFile      :: Flag FilePath,
    installBuildReports :: Flag Bool,
    installSymlinkBinDir:: Flag FilePath
  }

defaultInstallFlags :: InstallFlags
defaultInstallFlags = InstallFlags {
    installDocumentation= Flag False,
    installDryRun       = Flag False,
    installOnly         = Flag False,
    installRootCmd      = mempty,
    installCabalVersion = mempty,
    installLogFile      = mempty,
    installBuildReports = Flag False,
    installSymlinkBinDir= mempty
  }

installCommand :: CommandUI (Cabal.ConfigFlags, InstallFlags)
installCommand = configureCommand {
  commandName         = "install",
  commandSynopsis     = "Installs a list of packages.",
  commandUsage        = usagePackages "install",
  commandDefaultFlags = (mempty, defaultInstallFlags),
  commandOptions      = \showOrParseArgs ->
    liftOptionsFst (commandOptions configureCommand showOrParseArgs) ++
    liftOptionsSnd
     ([ option "" ["documentation"]
          "building of documentation"
          installDocumentation (\v flags -> flags { installDocumentation = v })
          (boolOpt [] [])

      , option [] ["dry-run"]
          "Do not install anything, only print what would be installed."
          installDryRun (\v flags -> flags { installDryRun = v })
          trueArg

      , option [] ["root-cmd"]
          "Command used to gain root privileges, when installing with --global."
          installRootCmd (\v flags -> flags { installRootCmd = v })
          (reqArg' "COMMAND" toFlag flagToList)

      , option [] ["cabal-lib-version"]
          ("Select which version of the Cabal lib to use to build packages "
          ++ "(useful for testing).")
          installCabalVersion (\v flags -> flags { installCabalVersion = v })
          (reqArg "VERSION" (readP_to_E ("Cannot parse cabal lib version: "++)
                                        (fmap toFlag parse))
                            (map display . flagToList))

      , option [] ["log-builds"]
          "Log all builds to file (name template can use $pkgid, $compiler, $os, $arch)"
          installLogFile (\v flags -> flags { installLogFile = v })
          (reqArg' "FILE" toFlag flagToList)

      , option [] ["build-reports"]
          "Generate detailed build reports. (overrides --log-builds)"
          installBuildReports (\v flags -> flags { installBuildReports = v })
          trueArg

      ] ++ case showOrParseArgs of      -- TODO: remove when "cabal install" avoids
          ParseArgs ->
            option [] ["only"]
              "Only installs the package in the current directory."
              installOnly (\v flags -> flags { installOnly = v })
              trueArg
             : []
          _ -> [])
  }

instance Monoid InstallFlags where
  mempty = defaultInstallFlags
  mappend a b = InstallFlags {
    installDocumentation= combine installDocumentation,
    installDryRun       = combine installDryRun,
    installOnly         = combine installOnly,
    installRootCmd      = combine installRootCmd,
    installCabalVersion = combine installCabalVersion,
    installLogFile      = combine installLogFile,
    installBuildReports = combine installBuildReports,
    installSymlinkBinDir= combine installSymlinkBinDir
  }
    where combine field = field a `mappend` field b

-- ------------------------------------------------------------
-- * Upload flags
-- ------------------------------------------------------------

data UploadFlags = UploadFlags {
    uploadCheck     :: Flag Bool,
    uploadUsername  :: Flag Username,
    uploadPassword  :: Flag Password,
    uploadVerbosity :: Flag Verbosity
  }

defaultUploadFlags :: UploadFlags
defaultUploadFlags = UploadFlags {
    uploadCheck     = toFlag False,
    uploadUsername  = mempty,
    uploadPassword  = mempty,
    uploadVerbosity = toFlag normal
  }

uploadCommand :: CommandUI UploadFlags
uploadCommand = CommandUI {
    commandName         = "upload",
    commandSynopsis     = "Uploads source packages to Hackage",
    commandDescription  = Just $ \_ ->
         "You can store your Hackage login in the ~/.cabal/config file\n",
    commandUsage        = \pname ->
         "Usage: " ++ pname ++ " upload [FLAGS] [TARFILES]\n\n"
      ++ "Flags for upload:",
    commandDefaultFlags = defaultUploadFlags,
    commandOptions      = \_ ->
      [optionVerbosity uploadVerbosity (\v flags -> flags { uploadVerbosity = v })

      ,option ['c'] ["check"]
         "Do not upload, just do QA checks."
        uploadCheck (\v flags -> flags { uploadCheck = v })
        trueArg

      ,option ['u'] ["username"]
        "Hackage username."
        uploadUsername (\v flags -> flags { uploadUsername = v })
        (reqArg' "USERNAME" (toFlag . Username)
                            (flagToList . fmap unUsername))

      ,option ['p'] ["password"]
        "Hackage password."
        uploadPassword (\v flags -> flags { uploadPassword = v })
        (reqArg' "PASSWORD" (toFlag . Password)
                            (flagToList . fmap unPassword))
      ]
  }

instance Monoid UploadFlags where
  mempty = UploadFlags {
    uploadCheck     = mempty,
    uploadUsername  = mempty,
    uploadPassword  = mempty,
    uploadVerbosity = mempty
  }
  mappend a b = UploadFlags {
    uploadCheck     = combine uploadCheck,
    uploadUsername  = combine uploadUsername,
    uploadPassword  = combine uploadPassword,
    uploadVerbosity = combine uploadVerbosity
  }
    where combine field = field a `mappend` field b

-- ------------------------------------------------------------
-- * GetOpt Utils
-- ------------------------------------------------------------

boolOpt :: SFlags -> SFlags -> MkOptDescr (a -> Flag Bool) (Flag Bool -> a -> a) a
boolOpt  = Command.boolOpt  flagToMaybe Flag

liftOptionsFst :: [OptionField a] -> [OptionField (a,b)]
liftOptionsFst = map (liftOption fst (\a (_,b) -> (a,b)))

liftOptionsSnd :: [OptionField b] -> [OptionField (a,b)]
liftOptionsSnd = map (liftOption snd (\b (a,_) -> (a,b)))

usagePackages :: String -> String -> String
usagePackages name pname =
     "Usage: " ++ pname ++ " " ++ name ++ " [FLAGS]\n"
  ++ "   or: " ++ pname ++ " " ++ name ++ " [PACKAGES]\n\n"
  ++ "Flags for " ++ name ++ ":"

--TODO: do we want to allow per-package flags?
parsePackageArgs :: [String] -> Either String [Dependency]
parsePackageArgs = parsePkgArgs []
  where
    parsePkgArgs ds [] = Right (reverse ds)
    parsePkgArgs ds (arg:args) =
      case readPToMaybe parseDependencyOrPackageId arg of
        Just dep -> parsePkgArgs (dep:ds) args
        Nothing  -> Left ("Failed to parse package dependency: " ++ show arg)

readPToMaybe :: ReadP a a -> String -> Maybe a
readPToMaybe p str = listToMaybe [ r | (r,s) <- readP_to_S p str, all isSpace s ]

parseDependencyOrPackageId :: ReadP r Dependency
parseDependencyOrPackageId = parse +++ liftM pkgidToDependency parse
  where
    pkgidToDependency :: PackageIdentifier -> Dependency
    pkgidToDependency p = case packageVersion p of
      Version [] _ -> Dependency (packageName p) AnyVersion
      version      -> Dependency (packageName p) (ThisVersion version)
