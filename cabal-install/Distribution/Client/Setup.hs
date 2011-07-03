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
    , configureCommand, ConfigFlags(..), filterConfigureFlags
    , configureExCommand, ConfigExFlags(..), defaultConfigExFlags
                        , configureExOptions
    , installCommand, InstallFlags(..), installOptions, defaultInstallFlags
    , listCommand, ListFlags(..)
    , updateCommand
    , upgradeCommand
    , infoCommand, InfoFlags(..)
    , fetchCommand, FetchFlags(..)
    , checkCommand
    , uploadCommand, UploadFlags(..)
    , reportCommand, ReportFlags(..)
    , unpackCommand, UnpackFlags(..)
    , initCommand, IT.InitFlags(..)
    , sdistCommand, SDistFlags(..), SDistExFlags(..), ArchiveFormat(..)

    , parsePackageArgs
    --TODO: stop exporting these:
    , showRepo
    , parseRepo
    ) where

import Distribution.Client.Types
         ( Username(..), Password(..), Repo(..), RemoteRepo(..), LocalRepo(..) )
import Distribution.Client.BuildReports.Types
         ( ReportLevel(..) )
import qualified Distribution.Client.Init.Types as IT
         ( InitFlags(..), PackageType(..) )
import Distribution.Client.Targets
         ( UserConstraint, readUserConstraint )

import Distribution.Simple.Program
         ( defaultProgramConfiguration )
import Distribution.Simple.Command hiding (boolOpt)
import qualified Distribution.Simple.Command as Command
import qualified Distribution.Simple.Setup as Cabal
         ( configureCommand, sdistCommand, haddockCommand )
import Distribution.Simple.Setup
         ( ConfigFlags(..), SDistFlags(..), HaddockFlags(..) )
import Distribution.Simple.Setup
         ( Flag(..), toFlag, fromFlag, flagToList, flagToMaybe
         , optionVerbosity, trueArg, falseArg )
import Distribution.Simple.InstallDirs
         ( PathTemplate, toPathTemplate, fromPathTemplate )
import Distribution.Version
         ( Version(Version), anyVersion, thisVersion )
import Distribution.Package
         ( PackageIdentifier, packageName, packageVersion, Dependency(..) )
import Distribution.Text
         ( Text(parse), display )
import Distribution.ReadE
         ( ReadE(..), readP_to_E, succeedReadE )
import qualified Distribution.Compat.ReadP as Parse
         ( ReadP, readP_to_S, char, munch1, pfail, (+++) )
import Distribution.Verbosity
         ( Verbosity, normal )
import Distribution.Simple.Utils
         ( wrapText )

import Data.Char
         ( isSpace, isAlphaNum )
import Data.Maybe
         ( listToMaybe, maybeToList, fromMaybe )
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
    globalRemoteRepos    :: [RemoteRepo],     -- ^ Available Hackage servers.
    globalCacheDir       :: Flag FilePath,
    globalLocalRepos     :: [FilePath],
    globalLogsDir        :: Flag FilePath,
    globalWorldFile      :: Flag FilePath
  }

defaultGlobalFlags :: GlobalFlags
defaultGlobalFlags  = GlobalFlags {
    globalVersion        = Flag False,
    globalNumericVersion = Flag False,
    globalConfigFile     = mempty,
    globalRemoteRepos    = [],
    globalCacheDir       = mempty,
    globalLocalRepos     = mempty,
    globalLogsDir        = mempty,
    globalWorldFile      = mempty
  }

globalCommand :: CommandUI GlobalFlags
globalCommand = CommandUI {
    commandName         = "",
    commandSynopsis     = "",
    commandUsage        = \_ ->
         "This program is the command line interface "
           ++ "to the Haskell Cabal infrastructure.\n"
      ++ "See http://www.haskell.org/cabal/ for more information.\n",
    commandDescription  = Just $ \pname ->
         "For more information about a command use:\n"
      ++ "  " ++ pname ++ " COMMAND --help\n\n"
      ++ "To install Cabal packages from hackage use:\n"
      ++ "  " ++ pname ++ " install foo [--dry-run]\n\n"
      ++ "Occasionally you need to update the list of available packages:\n"
      ++ "  " ++ pname ++ " update\n",
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

      ,option [] ["logs-dir"]
         "The location to put log files"
         globalLogsDir (\v flags -> flags { globalLogsDir = v })
         (reqArgFlag "DIR")

      ,option [] ["world-file"]
         "The location of the world file"
         globalWorldFile (\v flags -> flags { globalWorldFile = v })
         (reqArgFlag "FILE")
      ]
  }

instance Monoid GlobalFlags where
  mempty = GlobalFlags {
    globalVersion        = mempty,
    globalNumericVersion = mempty,
    globalConfigFile     = mempty,
    globalRemoteRepos    = mempty,
    globalCacheDir       = mempty,
    globalLocalRepos     = mempty,
    globalLogsDir        = mempty,
    globalWorldFile      = mempty
  }
  mappend a b = GlobalFlags {
    globalVersion        = combine globalVersion,
    globalNumericVersion = combine globalNumericVersion,
    globalConfigFile     = combine globalConfigFile,
    globalRemoteRepos    = combine globalRemoteRepos,
    globalCacheDir       = combine globalCacheDir,
    globalLocalRepos     = combine globalLocalRepos,
    globalLogsDir        = combine globalLogsDir,
    globalWorldFile      = combine globalWorldFile
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

configureCommand :: CommandUI ConfigFlags
configureCommand = (Cabal.configureCommand defaultProgramConfiguration) {
    commandDefaultFlags = mempty
  }

configureOptions ::  ShowOrParseArgs -> [OptionField ConfigFlags]
configureOptions = commandOptions configureCommand

filterConfigureFlags :: ConfigFlags -> Version -> ConfigFlags
filterConfigureFlags flags cabalLibVersion
  | cabalLibVersion >= Version [1,3,10] [] = flags
    -- older Cabal does not grok the constraints flag:
  | otherwise = flags { configConstraints = [] }

-- ------------------------------------------------------------
-- * Config extra flags
-- ------------------------------------------------------------

-- | cabal configure takes some extra flags beyond runghc Setup configure
--
data ConfigExFlags = ConfigExFlags {
    configCabalVersion :: Flag Version,
    configExConstraints:: [UserConstraint],
    configPreferences  :: [Dependency]
  }

defaultConfigExFlags :: ConfigExFlags
defaultConfigExFlags = mempty

configureExCommand :: CommandUI (ConfigFlags, ConfigExFlags)
configureExCommand = configureCommand {
    commandDefaultFlags = (mempty, defaultConfigExFlags),
    commandOptions      = \showOrParseArgs ->
         liftOptions fst setFst (filter ((/="constraint") . optionName) $
                                 configureOptions   showOrParseArgs)
      ++ liftOptions snd setSnd (configureExOptions showOrParseArgs)
  }
  where
    setFst a (_,b) = (a,b)
    setSnd b (a,_) = (a,b)

configureExOptions ::  ShowOrParseArgs -> [OptionField ConfigExFlags]
configureExOptions _showOrParseArgs =
  [ option [] ["cabal-lib-version"]
      ("Select which version of the Cabal lib to use to build packages "
      ++ "(useful for testing).")
      configCabalVersion (\v flags -> flags { configCabalVersion = v })
      (reqArg "VERSION" (readP_to_E ("Cannot parse cabal lib version: "++)
                                    (fmap toFlag parse))
                        (map display . flagToList))
  , option [] ["constraint"]
      "Specify constraints on a package (version, installed/source, flags)"
      configExConstraints (\v flags -> flags { configExConstraints = v })
      (reqArg "CONSTRAINT"
              (fmap (\x -> [x]) (ReadE readUserConstraint))
              (map display))

  , option [] ["preference"]
      "Specify preferences (soft constraints) on the version of a package"
      configPreferences (\v flags -> flags { configPreferences = v })
      (reqArg "CONSTRAINT"
              (readP_to_E (const "dependency expected")
                          (fmap (\x -> [x]) parse))
              (map display))
  ]

instance Monoid ConfigExFlags where
  mempty = ConfigExFlags {
    configCabalVersion = mempty,
    configExConstraints= mempty,
    configPreferences  = mempty
  }
  mappend a b = ConfigExFlags {
    configCabalVersion = combine configCabalVersion,
    configExConstraints= combine configExConstraints,
    configPreferences  = combine configPreferences
  }
    where combine field = field a `mappend` field b

-- ------------------------------------------------------------
-- * Fetch command
-- ------------------------------------------------------------

data FetchFlags = FetchFlags {
--    fetchOutput    :: Flag FilePath,
      fetchDeps      :: Flag Bool,
      fetchDryRun    :: Flag Bool,
      fetchVerbosity :: Flag Verbosity
    }

defaultFetchFlags :: FetchFlags
defaultFetchFlags = FetchFlags {
--  fetchOutput    = mempty,
    fetchDeps      = toFlag True,
    fetchDryRun    = toFlag False,
    fetchVerbosity = toFlag normal
   }

fetchCommand :: CommandUI FetchFlags
fetchCommand = CommandUI {
    commandName         = "fetch",
    commandSynopsis     = "Downloads packages for later installation.",
    commandDescription  = Nothing,
    commandUsage        = usagePackages "fetch",
    commandDefaultFlags = defaultFetchFlags,
    commandOptions      = \_ -> [
         optionVerbosity fetchVerbosity (\v flags -> flags { fetchVerbosity = v })

--     , option "o" ["output"]
--         "Put the package(s) somewhere specific rather than the usual cache."
--         fetchOutput (\v flags -> flags { fetchOutput = v })
--         (reqArgFlag "PATH")

       , option [] ["dependencies", "deps"]
           "Resolve and fetch dependencies (default)"
           fetchDeps (\v flags -> flags { fetchDeps = v })
           trueArg

       , option [] ["no-dependencies", "no-deps"]
           "Ignore dependencies"
           fetchDeps (\v flags -> flags { fetchDeps = v })
           falseArg

       , option [] ["dry-run"]
           "Do not install anything, only print what would be installed."
           fetchDryRun (\v flags -> flags { fetchDryRun = v })
           trueArg
       ]
  }

-- ------------------------------------------------------------
-- * Other commands
-- ------------------------------------------------------------

updateCommand  :: CommandUI (Flag Verbosity)
updateCommand = CommandUI {
    commandName         = "update",
    commandSynopsis     = "Updates list of known packages",
    commandDescription  = Nothing,
    commandUsage        = usagePackages "update",
    commandDefaultFlags = toFlag normal,
    commandOptions      = \_ -> [optionVerbosity id const]
  }

upgradeCommand  :: CommandUI (ConfigFlags, ConfigExFlags, InstallFlags, HaddockFlags)
upgradeCommand = configureCommand {
    commandName         = "upgrade",
    commandSynopsis     = "(command disabled, use install instead)",
    commandDescription  = Nothing,
    commandUsage        = usagePackages "upgrade",
    commandDefaultFlags = (mempty, mempty, mempty, mempty),
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

checkCommand  :: CommandUI (Flag Verbosity)
checkCommand = CommandUI {
    commandName         = "check",
    commandSynopsis     = "Check the package for common mistakes",
    commandDescription  = Nothing,
    commandUsage        = \pname -> "Usage: " ++ pname ++ " check\n",
    commandDefaultFlags = toFlag normal,
    commandOptions      = \_ -> []
  }

-- ------------------------------------------------------------
-- * Report flags
-- ------------------------------------------------------------

data ReportFlags = ReportFlags {
    reportUsername  :: Flag Username,
    reportPassword  :: Flag Password,
    reportVerbosity :: Flag Verbosity
  }

defaultReportFlags :: ReportFlags
defaultReportFlags = ReportFlags {
    reportUsername  = mempty,
    reportPassword  = mempty,
    reportVerbosity = toFlag normal
  }

reportCommand :: CommandUI ReportFlags
reportCommand = CommandUI {
    commandName         = "report",
    commandSynopsis     = "Upload build reports to a remote server.",
    commandDescription  = Just $ \_ ->
         "You can store your Hackage login in the ~/.cabal/config file\n",
    commandUsage        = \pname -> "Usage: " ++ pname ++ " report [FLAGS]\n\n"
      ++ "Flags for upload:",
    commandDefaultFlags = defaultReportFlags,
    commandOptions      = \_ ->
      [optionVerbosity reportVerbosity (\v flags -> flags { reportVerbosity = v })

      ,option ['u'] ["username"]
        "Hackage username."
        reportUsername (\v flags -> flags { reportUsername = v })
        (reqArg' "USERNAME" (toFlag . Username)
                            (flagToList . fmap unUsername))

      ,option ['p'] ["password"]
        "Hackage password."
        reportPassword (\v flags -> flags { reportPassword = v })
        (reqArg' "PASSWORD" (toFlag . Password)
                            (flagToList . fmap unPassword))
      ]
  }

instance Monoid ReportFlags where
  mempty = ReportFlags {
    reportUsername  = mempty,
    reportPassword  = mempty,
    reportVerbosity = mempty
  }
  mappend a b = ReportFlags {
    reportUsername  = combine reportUsername,
    reportPassword  = combine reportPassword,
    reportVerbosity = combine reportVerbosity
  }
    where combine field = field a `mappend` field b

-- ------------------------------------------------------------
-- * Unpack flags
-- ------------------------------------------------------------

data UnpackFlags = UnpackFlags {
      unpackDestDir :: Flag FilePath,
      unpackVerbosity :: Flag Verbosity
    }

defaultUnpackFlags :: UnpackFlags
defaultUnpackFlags = UnpackFlags {
    unpackDestDir = mempty,
    unpackVerbosity = toFlag normal
   }

unpackCommand :: CommandUI UnpackFlags
unpackCommand = CommandUI {
    commandName         = "unpack",
    commandSynopsis     = "Unpacks packages for user inspection.",
    commandDescription  = Nothing,
    commandUsage        = usagePackages "unpack",
    commandDefaultFlags = mempty,
    commandOptions      = \_ -> [
        optionVerbosity unpackVerbosity (\v flags -> flags { unpackVerbosity = v })

       ,option "d" ["destdir"]
         "where to unpack the packages, defaults to the current directory."
         unpackDestDir (\v flags -> flags { unpackDestDir = v })
         (reqArgFlag "PATH")
       ]
  }

instance Monoid UnpackFlags where
  mempty = defaultUnpackFlags
  mappend a b = UnpackFlags {
     unpackDestDir = combine unpackDestDir
    ,unpackVerbosity = combine unpackVerbosity
  }
    where combine field = field a `mappend` field b

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
    commandSynopsis     = "List packages matching a search string.",
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
-- * Info flags
-- ------------------------------------------------------------

data InfoFlags = InfoFlags {
    infoVerbosity :: Flag Verbosity
  }

defaultInfoFlags :: InfoFlags
defaultInfoFlags = InfoFlags {
    infoVerbosity = toFlag normal
  }

infoCommand  :: CommandUI InfoFlags
infoCommand = CommandUI {
    commandName         = "info",
    commandSynopsis     = "Display detailed information about a particular package.",
    commandDescription  = Nothing,
    commandUsage        = usagePackages "info",
    commandDefaultFlags = defaultInfoFlags,
    commandOptions      = \_ -> [
        optionVerbosity infoVerbosity (\v flags -> flags { infoVerbosity = v })
        ]
  }

instance Monoid InfoFlags where
  mempty = defaultInfoFlags
  mappend a b = InfoFlags {
    infoVerbosity = combine infoVerbosity
  }
    where combine field = field a `mappend` field b

-- ------------------------------------------------------------
-- * Install flags
-- ------------------------------------------------------------

-- | Install takes the same flags as configure along with a few extras.
--
data InstallFlags = InstallFlags {
    installDocumentation   :: Flag Bool,
    installHaddockIndex    :: Flag PathTemplate,
    installDryRun          :: Flag Bool,
    installReinstall       :: Flag Bool,
    installAvoidReinstalls :: Flag Bool,
    installUpgradeDeps     :: Flag Bool,
    installReorderGoals    :: Flag Bool,
    installOnly            :: Flag Bool,
    installOnlyDeps        :: Flag Bool,
    installRootCmd         :: Flag String,
    installSummaryFile     :: [PathTemplate],
    installLogFile         :: Flag PathTemplate,
    installBuildReports    :: Flag ReportLevel,
    installSymlinkBinDir   :: Flag FilePath,
    installOneShot         :: Flag Bool
  }

defaultInstallFlags :: InstallFlags
defaultInstallFlags = InstallFlags {
    installDocumentation   = Flag False,
    installHaddockIndex    = Flag docIndexFile,
    installDryRun          = Flag False,
    installReinstall       = Flag False,
    installAvoidReinstalls = Flag False,
    installUpgradeDeps     = Flag False,
    installReorderGoals    = Flag False,
    installOnly            = Flag False,
    installOnlyDeps        = Flag False,
    installRootCmd         = mempty,
    installSummaryFile     = mempty,
    installLogFile         = mempty,
    installBuildReports    = Flag NoReports,
    installSymlinkBinDir   = mempty,
    installOneShot         = Flag False
  }
  where
    docIndexFile = toPathTemplate ("$datadir" </> "doc" </> "index.html")

installCommand :: CommandUI (ConfigFlags, ConfigExFlags, InstallFlags, HaddockFlags)
installCommand = CommandUI {
  commandName         = "install",
  commandSynopsis     = "Installs a list of packages.",
  commandUsage        = usagePackages "install",
  commandDescription  = Just $ \pname ->
    let original = case commandDescription configureCommand of
          Just desc -> desc pname ++ "\n"
          Nothing   -> ""
     in original
     ++ "Examples:\n"
     ++ "  " ++ pname ++ " install                 "
     ++ "    Package in the current directory\n"
     ++ "  " ++ pname ++ " install foo             "
     ++ "    Package from the hackage server\n"
     ++ "  " ++ pname ++ " install foo-1.0         "
     ++ "    Specific version of a package\n"
     ++ "  " ++ pname ++ " install 'foo < 2'       "
     ++ "    Constrained package version\n",
  commandDefaultFlags = (mempty, mempty, mempty, mempty),
  commandOptions      = \showOrParseArgs ->
       liftOptions get1 set1 (filter ((/="constraint") . optionName) $
                              configureOptions   showOrParseArgs)
    ++ liftOptions get2 set2 (configureExOptions showOrParseArgs)
    ++ liftOptions get3 set3 (installOptions     showOrParseArgs)
    ++ liftOptions get4 set4 (haddockOptions     showOrParseArgs)
  }
  where
    get1 (a,_,_,_) = a; set1 a (_,b,c,d) = (a,b,c,d)
    get2 (_,b,_,_) = b; set2 b (a,_,c,d) = (a,b,c,d)
    get3 (_,_,c,_) = c; set3 c (a,b,_,d) = (a,b,c,d)
    get4 (_,_,_,d) = d; set4 d (a,b,c,_) = (a,b,c,d)

    haddockOptions showOrParseArgs
      = [ opt { optionName = "haddock-" ++ name,
                optionDescr = [ fmapOptFlags (\(_, lflags) -> ([], map ("haddock-" ++) lflags)) descr
                              | descr <- optionDescr opt] }
        | opt <- commandOptions Cabal.haddockCommand showOrParseArgs
        , let name = optionName opt
        , name `elem` ["hoogle", "html", "html-location",
                       "executables", "internal", "css",
                       "hyperlink-source", "hscolour-css",
                       "contents-location"]
        ]

    fmapOptFlags :: (OptFlags -> OptFlags) -> OptDescr a -> OptDescr a
    fmapOptFlags modify (ReqArg d f p r w)    = ReqArg d (modify f) p r w
    fmapOptFlags modify (OptArg d f p r i w)  = OptArg d (modify f) p r i w
    fmapOptFlags modify (ChoiceOpt xs)        = ChoiceOpt [(d, modify f, i, w) | (d, f, i, w) <- xs]
    fmapOptFlags modify (BoolOpt d f1 f2 r w) = BoolOpt d (modify f1) (modify f2) r w

installOptions ::  ShowOrParseArgs -> [OptionField InstallFlags]
installOptions showOrParseArgs =
      [ option "" ["documentation"]
          "building of documentation"
          installDocumentation (\v flags -> flags { installDocumentation = v })
          (boolOpt [] [])

      , option [] ["doc-index-file"]
          "A central index of haddock API documentation (template cannot use $pkgid)"
          installHaddockIndex (\v flags -> flags { installHaddockIndex = v })
          (reqArg' "TEMPLATE" (toFlag.toPathTemplate)
                              (flagToList . fmap fromPathTemplate))

      , option [] ["dry-run"]
          "Do not install anything, only print what would be installed."
          installDryRun (\v flags -> flags { installDryRun = v })
          trueArg

      , option [] ["reinstall"]
          "Install even if it means installing the same version again."
          installReinstall (\v flags -> flags { installReinstall = v })
          trueArg

      , option [] ["avoid-reinstalls"]
          "Do not select versions that would destructively overwrite installed packages."
          installAvoidReinstalls (\v flags -> flags { installAvoidReinstalls = v })
          trueArg

      , option [] ["upgrade-dependencies"]
          "Pick the latest version for all dependencies, rather than trying to pick an installed version."
          installUpgradeDeps (\v flags -> flags { installUpgradeDeps = v })
          trueArg

      , option [] ["reorder-goals"]
          "Experimental: Try to reorder goals according to certain heuristics. Slows things down on average, but may make backtracking faster for some packages."
          installReorderGoals (\v flags -> flags { installReorderGoals = v })
          trueArg

      , option [] ["only-dependencies"]
          "Install only the dependencies necessary to build the given packages"
          installOnlyDeps (\v flags -> flags { installOnlyDeps = v })
          trueArg


      , option [] ["root-cmd"]
          "Command used to gain root privileges, when installing with --global."
          installRootCmd (\v flags -> flags { installRootCmd = v })
          (reqArg' "COMMAND" toFlag flagToList)

      , option [] ["symlink-bindir"]
          "Add symlinks to installed executables into this directory."
           installSymlinkBinDir (\v flags -> flags { installSymlinkBinDir = v })
           (reqArgFlag "DIR")

      , option [] ["build-summary"]
          "Save build summaries to file (name template can use $pkgid, $compiler, $os, $arch)"
          installSummaryFile (\v flags -> flags { installSummaryFile = v })
          (reqArg' "TEMPLATE" (\x -> [toPathTemplate x]) (map fromPathTemplate))

      , option [] ["build-log"]
          "Log all builds to file (name template can use $pkgid, $compiler, $os, $arch)"
          installLogFile (\v flags -> flags { installLogFile = v })
          (reqArg' "TEMPLATE" (toFlag.toPathTemplate)
                              (flagToList . fmap fromPathTemplate))

      , option [] ["remote-build-reporting"]
          "Generate build reports to send to a remote server (none, anonymous or detailed)."
          installBuildReports (\v flags -> flags { installBuildReports = v })
          (reqArg "LEVEL" (readP_to_E (const $ "report level must be 'none', "
                                            ++ "'anonymous' or 'detailed'")
                                      (toFlag `fmap` parse))
                          (flagToList . fmap display))

      , option [] ["one-shot"]
          "Do not record the packages in the world file."
          installOneShot (\v flags -> flags { installOneShot = v })
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
    installDocumentation   = mempty,
    installHaddockIndex    = mempty,
    installDryRun          = mempty,
    installReinstall       = mempty,
    installAvoidReinstalls = mempty,
    installUpgradeDeps     = mempty,
    installReorderGoals    = mempty,
    installOnly            = mempty,
    installOnlyDeps        = mempty,
    installRootCmd         = mempty,
    installSummaryFile     = mempty,
    installLogFile         = mempty,
    installBuildReports    = mempty,
    installSymlinkBinDir   = mempty,
    installOneShot         = mempty
  }
  mappend a b = InstallFlags {
    installDocumentation   = combine installDocumentation,
    installHaddockIndex    = combine installHaddockIndex,
    installDryRun          = combine installDryRun,
    installReinstall       = combine installReinstall,
    installAvoidReinstalls = combine installAvoidReinstalls,
    installUpgradeDeps     = combine installUpgradeDeps,
    installReorderGoals    = combine installReorderGoals,
    installOnly            = combine installOnly,
    installOnlyDeps        = combine installOnlyDeps,
    installRootCmd         = combine installRootCmd,
    installSummaryFile     = combine installSummaryFile,
    installLogFile         = combine installLogFile,
    installBuildReports    = combine installBuildReports,
    installSymlinkBinDir   = combine installSymlinkBinDir,
    installOneShot         = combine installOneShot
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
-- * Init flags
-- ------------------------------------------------------------

emptyInitFlags :: IT.InitFlags
emptyInitFlags  = mempty

defaultInitFlags :: IT.InitFlags
defaultInitFlags  = emptyInitFlags

initCommand :: CommandUI IT.InitFlags
initCommand = CommandUI {
    commandName = "init",
    commandSynopsis = "Interactively create a .cabal file.",
    commandDescription = Just $ \_ -> wrapText $
         "Cabalise a project by creating a .cabal, Setup.hs, and "
      ++ "optionally a LICENSE file.\n\n"
      ++ "Calling init with no arguments (recommended) uses an "
      ++ "interactive mode, which will try to guess as much as "
      ++ "possible and prompt you for the rest.  Command-line "
      ++ "arguments are provided for scripting purposes. "
      ++ "If you don't want interactive mode, be sure to pass "
      ++ "the -n flag.\n",
    commandUsage = \pname ->
         "Usage: " ++ pname ++ " init [FLAGS]\n\n"
      ++ "Flags for init:",
    commandDefaultFlags = defaultInitFlags,
    commandOptions = \_ ->
      [ option ['n'] ["non-interactive"]
        "Non-interactive mode."
        IT.nonInteractive (\v flags -> flags { IT.nonInteractive = v })
        trueArg

      , option ['q'] ["quiet"]
        "Do not generate log messages to stdout."
        IT.quiet (\v flags -> flags { IT.quiet = v })
        trueArg

      , option [] ["no-comments"]
        "Do not generate explanatory comments in the .cabal file."
        IT.noComments (\v flags -> flags { IT.noComments = v })
        trueArg

      , option ['m'] ["minimal"]
        "Generate a minimal .cabal file, that is, do not include extra empty fields.  Also implies --no-comments."
        IT.minimal (\v flags -> flags { IT.minimal = v })
        trueArg

      , option [] ["package-dir"]
        "Root directory of the package (default = current directory)."
        IT.packageDir (\v flags -> flags { IT.packageDir = v })
        (reqArgFlag "DIRECTORY")

      , option ['p'] ["package-name"]
        "Name of the Cabal package to create."
        IT.packageName (\v flags -> flags { IT.packageName = v })
        (reqArgFlag "PACKAGE")

      , option [] ["version"]
        "Initial version of the package."
        IT.version (\v flags -> flags { IT.version = v })
        (reqArg "VERSION" (readP_to_E ("Cannot parse package version: "++)
                                      (toFlag `fmap` parse))
                          (flagToList . fmap display))

      , option [] ["cabal-version"]
        "Required version of the Cabal library."
        IT.cabalVersion (\v flags -> flags { IT.cabalVersion = v })
        (reqArg "VERSION_RANGE" (readP_to_E ("Cannot parse Cabal version range: "++)
                                            (toFlag `fmap` parse))
                                (flagToList . fmap display))

      , option ['l'] ["license"]
        "Project license."
        IT.license (\v flags -> flags { IT.license = v })
        (reqArg "LICENSE" (readP_to_E ("Cannot parse license: "++)
                                      (toFlag `fmap` parse))
                          (flagToList . fmap display))

      , option ['a'] ["author"]
        "Name of the project's author."
        IT.author (\v flags -> flags { IT.author = v })
        (reqArgFlag "NAME")

      , option ['e'] ["email"]
        "Email address of the maintainer."
        IT.email (\v flags -> flags { IT.email = v })
        (reqArgFlag "EMAIL")

      , option ['u'] ["homepage"]
        "Project homepage and/or repository."
        IT.homepage (\v flags -> flags { IT.homepage = v })
        (reqArgFlag "URL")

      , option ['s'] ["synopsis"]
        "Short project synopsis."
        IT.synopsis (\v flags -> flags { IT.synopsis = v })
        (reqArgFlag "TEXT")

      , option ['c'] ["category"]
        "Project category."
        IT.category (\v flags -> flags { IT.category = v })
        (reqArg' "CATEGORY" (\s -> toFlag $ maybe (Left s) Right (readMaybe s))
                            (flagToList . fmap (either id show)))

      , option [] ["is-library"]
        "Build a library."
        IT.packageType (\v flags -> flags { IT.packageType = v })
        (noArg (Flag IT.Library))

      , option [] ["is-executable"]
        "Build an executable."
        IT.packageType
        (\v flags -> flags { IT.packageType = v })
        (noArg (Flag IT.Executable))

      , option ['o'] ["expose-module"]
        "Export a module from the package."
        IT.exposedModules
        (\v flags -> flags { IT.exposedModules = v })
        (reqArg "MODULE" (readP_to_E ("Cannot parse module name: "++)
                                     ((Just . (:[])) `fmap` parse))
                         (fromMaybe [] . fmap (fmap display)))

      , option ['d'] ["dependency"]
        "Package dependency."
        IT.dependencies (\v flags -> flags { IT.dependencies = v })
        (reqArg "PACKAGE" (readP_to_E ("Cannot parse dependency: "++)
                                      ((Just . (:[])) `fmap` parse))
                          (fromMaybe [] . fmap (fmap display)))

      , option [] ["source-dir"]
        "Directory containing package source."
        IT.sourceDirs (\v flags -> flags { IT.sourceDirs = v })
        (reqArg' "DIR" (Just . (:[]))
                       (fromMaybe []))

      , option [] ["build-tool"]
        "Required external build tool."
        IT.buildTools (\v flags -> flags { IT.buildTools = v })
        (reqArg' "TOOL" (Just . (:[]))
                        (fromMaybe []))
      ]
  }
  where readMaybe s = case reads s of
                        [(x,"")]  -> Just x
                        _         -> Nothing

-- ------------------------------------------------------------
-- * SDist flags
-- ------------------------------------------------------------

-- | Extra flags to @sdist@ beyond runghc Setup sdist
--
data SDistExFlags = SDistExFlags {
    sDistFormat    :: Flag ArchiveFormat
  }
  deriving Show

data ArchiveFormat = TargzFormat | ZipFormat -- | ...
  deriving (Show, Eq)

defaultSDistExFlags :: SDistExFlags
defaultSDistExFlags = SDistExFlags {
    sDistFormat  = Flag TargzFormat
  }

sdistCommand :: CommandUI (SDistFlags, SDistExFlags)
sdistCommand = Cabal.sdistCommand {
    commandDefaultFlags = (commandDefaultFlags Cabal.sdistCommand, defaultSDistExFlags),
    commandOptions      = \showOrParseArgs ->
         liftOptions fst setFst (commandOptions Cabal.sdistCommand showOrParseArgs)
      ++ liftOptions snd setSnd sdistExOptions
  }
  where
    setFst a (_,b) = (a,b)
    setSnd b (a,_) = (a,b)

    sdistExOptions =
      [option [] ["archive-format"] "archive-format"
         sDistFormat (\v flags -> flags { sDistFormat = v })
         (choiceOpt
            [ (Flag TargzFormat, ([], ["targz"]),
                 "Produce a '.tar.gz' format archive (default and required for uploading to hackage)")
            , (Flag ZipFormat,   ([], ["zip"]),
                 "Produce a '.zip' format archive")
            ])
      ]

instance Monoid SDistExFlags where
  mempty = SDistExFlags {
    sDistFormat  = mempty
  }
  mappend a b = SDistExFlags {
    sDistFormat  = combine sDistFormat
  }
    where
      combine field = field a `mappend` field b

-- ------------------------------------------------------------
-- * GetOpt Utils
-- ------------------------------------------------------------

boolOpt :: SFlags -> SFlags -> MkOptDescr (a -> Flag Bool) (Flag Bool -> a -> a) a
boolOpt  = Command.boolOpt  flagToMaybe Flag

reqArgFlag :: ArgPlaceHolder -> SFlags -> LFlags -> Description ->
              (b -> Flag String) -> (Flag String -> b -> b) -> OptDescr b
reqArgFlag ad = reqArg ad (succeedReadE Flag) flagToList

liftOptions :: (b -> a) -> (a -> b -> b)
            -> [OptionField a] -> [OptionField b]
liftOptions get set = map (liftOption get set)

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
        Nothing  -> Left $
         show arg ++ " is not valid syntax for a package name or"
                  ++ " package dependency."

readPToMaybe :: Parse.ReadP a a -> String -> Maybe a
readPToMaybe p str = listToMaybe [ r | (r,s) <- Parse.readP_to_S p str
                                     , all isSpace s ]

parseDependencyOrPackageId :: Parse.ReadP r Dependency
parseDependencyOrPackageId = parse Parse.+++ liftM pkgidToDependency parse
  where
    pkgidToDependency :: PackageIdentifier -> Dependency
    pkgidToDependency p = case packageVersion p of
      Version [] _ -> Dependency (packageName p) anyVersion
      version      -> Dependency (packageName p) (thisVersion version)

showRepo :: RemoteRepo -> String
showRepo repo = remoteRepoName repo ++ ":"
             ++ uriToString id (remoteRepoURI repo) []

readRepo :: String -> Maybe RemoteRepo
readRepo = readPToMaybe parseRepo

parseRepo :: Parse.ReadP r RemoteRepo
parseRepo = do
  name <- Parse.munch1 (\c -> isAlphaNum c || c `elem` "_-.")
  _ <- Parse.char ':'
  uriStr <- Parse.munch1 (\c -> isAlphaNum c || c `elem` "+-=._/*()@'$:;&!?~")
  uri <- maybe Parse.pfail return (parseAbsoluteURI uriStr)
  return $ RemoteRepo {
    remoteRepoName = name,
    remoteRepoURI  = uri
  }
