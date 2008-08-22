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
    ( globalCommand, GlobalFlags(..), globalRepos
    , configureCommand, Cabal.ConfigFlags(..), filterConfigureFlags, configPackageDB'
    , installCommand, InstallFlags(..), installOptions, defaultInstallFlags
    , listCommand, ListFlags(..)
    , updateCommand
    , upgradeCommand
    , infoCommand
    , fetchCommand
    , checkCommand
    , uploadCommand, UploadFlags(..)
    , reportCommand

    , parsePackageArgs
    --TODO: stop exporting these:
    , showRepo
    , parseRepo
    ) where

import Distribution.Client.Types
         ( Username(..), Password(..), Repo(..), RemoteRepo(..), LocalRepo(..) )

import Distribution.Simple.Program
         ( defaultProgramConfiguration )
import Distribution.Simple.Command hiding (boolOpt)
import qualified Distribution.Simple.Command as Command
import qualified Distribution.Simple.Setup as Cabal
         ( ConfigFlags(..), configureCommand )
import Distribution.Simple.Setup
         ( Flag(..), toFlag, fromFlag, flagToList, flagToMaybe, fromFlagOrDefault
         , optionVerbosity, trueArg )
import Distribution.Simple.Compiler
         ( PackageDB(..) )
import Distribution.Version
         ( Version(Version), VersionRange(..) )
import Distribution.Package
         ( PackageIdentifier, packageName, packageVersion, Dependency(..) )
import Distribution.Text
         ( Text(parse), display )
import Distribution.ReadE
         ( readP_to_E, succeedReadE )
import qualified Distribution.Compat.ReadP as Parse
         ( ReadP, readP_to_S, char, munch1, pfail, (+++) )
import Distribution.Verbosity
         ( Verbosity, normal )

import Data.Char
         ( isSpace, isAlphaNum )
import Data.Maybe
         ( listToMaybe, maybeToList )
import Data.Monoid
         ( Monoid(..) )
import Control.Monad
         ( liftM )
import System.FilePath
         ( (</>) )
import Network.URI
         ( parseAbsoluteURI, uriToString )

-- ------------------------------------------------------------
-- * Global flags
-- ------------------------------------------------------------

-- | Flags that apply at the top level, not to any sub-command.
data GlobalFlags = GlobalFlags {
    globalVersion        :: Flag Bool,
    globalNumericVersion :: Flag Bool,
    globalConfigFile     :: Flag FilePath,
    globalRemoteRepos    :: [RemoteRepo],     -- ^Available Hackage servers.
    globalCacheDir       :: Flag FilePath,
    globalLocalRepos     :: [FilePath]
  }

defaultGlobalFlags :: GlobalFlags
defaultGlobalFlags  = GlobalFlags {
    globalVersion        = Flag False,
    globalNumericVersion = Flag False,
    globalConfigFile     = mempty,
    globalRemoteRepos    = [],
    globalCacheDir       = mempty,
    globalLocalRepos     = mempty
  }

globalCommand :: CommandUI GlobalFlags
globalCommand = CommandUI {
    commandName         = "",
    commandSynopsis     = "",
    commandDescription  = Just $ \pname ->
         "Typical step for installing Cabal packages:\n"
      ++ "  " ++ pname ++ " install [PACKAGES]\n"
      ++ "\nOccasionally you need to update the list of available packages:\n"
      ++ "  " ++ pname ++ " update\n"
      ++ "\nFor more information about a command, try '"
          ++ pname ++ " COMMAND --help'."
      ++ "\nThis program is the command line interface to the Haskell Cabal Infrastructure."
      ++ "\nSee http://www.haskell.org/cabal/ for more information.\n",
    commandUsage        = \_ -> [],
    commandDefaultFlags = defaultGlobalFlags,
    commandOptions      = \showOrParseArgs ->
      (case showOrParseArgs of ShowArgs -> take 2; ParseArgs -> id)
      [option ['V'] ["version"]
         "Print version information"
         globalVersion (\v flags -> flags { globalVersion = v })
         trueArg

      ,option [] ["numeric-version"]
         "Print just the version number"
         globalNumericVersion (\v flags -> flags { globalNumericVersion = v })
         trueArg

      ,option [] ["config-file"]
         "Set an alternate location for the config file"
         globalConfigFile (\v flags -> flags { globalConfigFile = v })
         (reqArgFlag "FILE")

      ,option [] ["remote-repo"]
         "The name and url for a remote repository"
         globalRemoteRepos (\v flags -> flags { globalRemoteRepos = v })
         (reqArg' "NAME:URL" (maybeToList . readRepo) (map showRepo))

      ,option [] ["remote-repo-cache"]
         "The location where downloads from all remote repos are cached"
         globalCacheDir (\v flags -> flags { globalCacheDir = v })
         (reqArgFlag "DIR")

      ,option [] ["local-repo"]
         "The location of a local repository"
         globalLocalRepos (\v flags -> flags { globalLocalRepos = v })
         (reqArg' "DIR" (\x -> [x]) id)
      ]
  }

instance Monoid GlobalFlags where
  mempty = GlobalFlags {
    globalVersion        = mempty,
    globalNumericVersion = mempty,
    globalConfigFile     = mempty,
    globalRemoteRepos    = mempty,
    globalCacheDir       = mempty,
    globalLocalRepos     = mempty
  }
  mappend a b = GlobalFlags {
    globalVersion        = combine globalVersion,
    globalNumericVersion = combine globalNumericVersion,
    globalConfigFile     = combine globalConfigFile,
    globalRemoteRepos    = combine globalRemoteRepos,
    globalCacheDir       = combine globalCacheDir,
    globalLocalRepos     = combine globalLocalRepos
  }
    where combine field = field a `mappend` field b

globalRepos :: GlobalFlags -> [Repo]
globalRepos globalFlags = remoteRepos ++ localRepos
  where
    remoteRepos =
      [ Repo (Left remote) cacheDir
      | remote <- globalRemoteRepos globalFlags
      , let cacheDir = fromFlag (globalCacheDir globalFlags)
                   </> remoteRepoName remote ]
    localRepos =
      [ Repo (Right LocalRepo) local
      | local <- globalLocalRepos globalFlags ]

-- ------------------------------------------------------------
-- * Config flags
-- ------------------------------------------------------------

configureCommand :: CommandUI Cabal.ConfigFlags
configureCommand = (Cabal.configureCommand defaultProgramConfiguration) {
    commandDefaultFlags = mempty
  }

configPackageDB' :: Cabal.ConfigFlags -> PackageDB
configPackageDB' config =
  fromFlagOrDefault defaultDB (Cabal.configPackageDB config)
  where
    defaultDB = case Cabal.configUserInstall config of
      NoFlag     -> UserPackageDB
      Flag True  -> UserPackageDB
      Flag False -> GlobalPackageDB

filterConfigureFlags :: Cabal.ConfigFlags -> Version -> Cabal.ConfigFlags
filterConfigureFlags flags cabalLibVersion
  | cabalLibVersion >= Version [1,3,10] [] = flags
    -- older Cabal does not grok the constraints flag:
  | otherwise = flags { Cabal.configConstraints = [] }

-- ------------------------------------------------------------
-- * Other commands
-- ------------------------------------------------------------

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
    commandDefaultFlags = toFlag normal,
    commandOptions      = mempty
  }

reportCommand :: CommandUI (Flag Verbosity)
reportCommand = CommandUI {
    commandName         = "report",
    commandSynopsis     = "Upload build reports to a remote server.",
    commandDescription  = Nothing,
    commandUsage        = \pname -> "Usage: " ++ pname ++ " report\n",
    commandDefaultFlags = toFlag normal,
    commandOptions      = \_ -> [optionVerbosity id const]
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
    installReinstall    :: Flag Bool,
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
    installReinstall    = Flag False,
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
  commandDefaultFlags = (mempty, mempty),
  commandOptions      = \showOrParseArgs ->
    liftOptionsFst (commandOptions configureCommand showOrParseArgs) ++
    liftOptionsSnd (installOptions showOrParseArgs)
  }

installOptions ::  ShowOrParseArgs -> [OptionField InstallFlags]
installOptions showOrParseArgs =
      [ option "" ["documentation"]
          "building of documentation"
          installDocumentation (\v flags -> flags { installDocumentation = v })
          (boolOpt [] [])

      , option [] ["dry-run"]
          "Do not install anything, only print what would be installed."
          installDryRun (\v flags -> flags { installDryRun = v })
          trueArg

      , option [] ["reinstall"]
          "Install even if it means installing the same version again."
          installReinstall (\v flags -> flags { installReinstall = v })
          trueArg

      , option [] ["root-cmd"]
          "Command used to gain root privileges, when installing with --global."
          installRootCmd (\v flags -> flags { installRootCmd = v })
          (reqArg' "COMMAND" toFlag flagToList)

      , option [] ["symlink-bindir"]
          "Add symlinks to installed executables into this directory."
           installSymlinkBinDir (\v flags -> flags { installSymlinkBinDir = v })
           (reqArgFlag "DIR")

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
          _ -> []

instance Monoid InstallFlags where
  mempty = InstallFlags {
    installDocumentation= mempty,
    installDryRun       = mempty,
    installReinstall    = mempty,
    installOnly         = mempty,
    installRootCmd      = mempty,
    installCabalVersion = mempty,
    installLogFile      = mempty,
    installBuildReports = mempty,
    installSymlinkBinDir= mempty
  }
  mappend a b = InstallFlags {
    installDocumentation= combine installDocumentation,
    installDryRun       = combine installDryRun,
    installReinstall    = combine installReinstall,
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

reqArgFlag :: ArgPlaceHolder -> SFlags -> LFlags -> Description ->
              (b -> Flag String) -> (Flag String -> b -> b) -> OptDescr b
reqArgFlag ad = reqArg ad (succeedReadE Flag) flagToList

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

readPToMaybe :: Parse.ReadP a a -> String -> Maybe a
readPToMaybe p str = listToMaybe [ r | (r,s) <- Parse.readP_to_S p str
                                     , all isSpace s ]

parseDependencyOrPackageId :: Parse.ReadP r Dependency
parseDependencyOrPackageId = parse Parse.+++ liftM pkgidToDependency parse
  where
    pkgidToDependency :: PackageIdentifier -> Dependency
    pkgidToDependency p = case packageVersion p of
      Version [] _ -> Dependency (packageName p) AnyVersion
      version      -> Dependency (packageName p) (ThisVersion version)

showRepo :: RemoteRepo -> String
showRepo repo = remoteRepoName repo ++ ":"
             ++ uriToString id (remoteRepoURI repo) []

readRepo :: String -> Maybe RemoteRepo
readRepo = readPToMaybe parseRepo

parseRepo :: Parse.ReadP r RemoteRepo
parseRepo = do
  name <- Parse.munch1 (\c -> isAlphaNum c || c `elem` "_-.")
  Parse.char ':'
  uriStr <- Parse.munch1 (\c -> isAlphaNum c || c `elem` "+-=._/*()@'$:;&!?~")
  uri <- maybe Parse.pfail return (parseAbsoluteURI uriStr)
  return $ RemoteRepo {
    remoteRepoName = name,
    remoteRepoURI  = uri
  }
