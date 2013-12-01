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
    , buildCommand, BuildFlags(..), BuildExFlags(..), SkipAddSourceDepsCheck(..)
    , testCommand, benchmarkCommand
    , installCommand, InstallFlags(..), installOptions, defaultInstallFlags
    , listCommand, ListFlags(..)
    , updateCommand
    , upgradeCommand
    , infoCommand, InfoFlags(..)
    , fetchCommand, FetchFlags(..)
    , getCommand, unpackCommand, GetFlags(..)
    , checkCommand
    , uploadCommand, UploadFlags(..)
    , reportCommand, ReportFlags(..)
    , runCommand
    , initCommand, IT.InitFlags(..)
    , sdistCommand, SDistFlags(..), SDistExFlags(..), ArchiveFormat(..)
    , win32SelfUpgradeCommand, Win32SelfUpgradeFlags(..)
    , sandboxCommand, defaultSandboxLocation, SandboxFlags(..)

    , parsePackageArgs
    --TODO: stop exporting these:
    , showRepo
    , parseRepo
    ) where

import Distribution.Client.Types
         ( Username(..), Password(..), Repo(..), RemoteRepo(..), LocalRepo(..) )
import Distribution.Client.BuildReports.Types
         ( ReportLevel(..) )
import Distribution.Client.Dependency.Types
         ( PreSolver(..) )
import qualified Distribution.Client.Init.Types as IT
         ( InitFlags(..), PackageType(..) )
import Distribution.Client.Targets
         ( UserConstraint, readUserConstraint )

import Distribution.Simple.Program
         ( defaultProgramConfiguration )
import Distribution.Simple.Command hiding (boolOpt)
import qualified Distribution.Simple.Setup as Cabal
import Distribution.Simple.Setup
         ( ConfigFlags(..), BuildFlags(..), TestFlags(..), BenchmarkFlags(..)
         , SDistFlags(..), HaddockFlags(..)
         , Flag(..), toFlag, fromFlag, flagToMaybe, flagToList
         , optionVerbosity, boolOpt, trueArg, falseArg, numJobsParser )
import Distribution.Simple.InstallDirs
         ( PathTemplate, InstallDirs(sysconfdir)
         , toPathTemplate, fromPathTemplate )
import Distribution.Version
         ( Version(Version), anyVersion, thisVersion )
import Distribution.Package
         ( PackageIdentifier, packageName, packageVersion, Dependency(..) )
import Distribution.PackageDescription
         ( RepoKind(..) )
import Distribution.Text
         ( Text(..), display )
import Distribution.ReadE
         ( ReadE(..), readP_to_E, succeedReadE )
import qualified Distribution.Compat.ReadP as Parse
         ( ReadP, readP_to_S, readS_to_P, char, munch1, pfail, (+++) )
import Distribution.Verbosity
         ( Verbosity, normal )
import Distribution.Simple.Utils
         ( wrapText )

import Data.Char
         ( isSpace, isAlphaNum )
import Data.List
         ( intercalate )
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
    globalVersion           :: Flag Bool,
    globalNumericVersion    :: Flag Bool,
    globalConfigFile        :: Flag FilePath,
    globalSandboxConfigFile :: Flag FilePath,
    globalRemoteRepos       :: [RemoteRepo],     -- ^ Available Hackage servers.
    globalCacheDir          :: Flag FilePath,
    globalLocalRepos        :: [FilePath],
    globalLogsDir           :: Flag FilePath,
    globalWorldFile         :: Flag FilePath
  }

defaultGlobalFlags :: GlobalFlags
defaultGlobalFlags  = GlobalFlags {
    globalVersion           = Flag False,
    globalNumericVersion    = Flag False,
    globalConfigFile        = mempty,
    globalSandboxConfigFile = mempty,
    globalRemoteRepos       = [],
    globalCacheDir          = mempty,
    globalLocalRepos        = mempty,
    globalLogsDir           = mempty,
    globalWorldFile         = mempty
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
      (case showOrParseArgs of ShowArgs -> take 4; ParseArgs -> id)
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

      ,option [] ["sandbox-config-file"]
         "Set an alternate location for the sandbox config file \
         \(default: './cabal.sandbox.config')"
         globalConfigFile (\v flags -> flags { globalSandboxConfigFile = v })
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
    globalVersion           = mempty,
    globalNumericVersion    = mempty,
    globalConfigFile        = mempty,
    globalSandboxConfigFile = mempty,
    globalRemoteRepos       = mempty,
    globalCacheDir          = mempty,
    globalLocalRepos        = mempty,
    globalLogsDir           = mempty,
    globalWorldFile         = mempty
  }
  mappend a b = GlobalFlags {
    globalVersion           = combine globalVersion,
    globalNumericVersion    = combine globalNumericVersion,
    globalConfigFile        = combine globalConfigFile,
    globalSandboxConfigFile = combine globalConfigFile,
    globalRemoteRepos       = combine globalRemoteRepos,
    globalCacheDir          = combine globalCacheDir,
    globalLocalRepos        = combine globalLocalRepos,
    globalLogsDir           = combine globalLogsDir,
    globalWorldFile         = combine globalWorldFile
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
  | cabalLibVersion >= Version [1,19,1] [] = flags_latest
  | cabalLibVersion <  Version [1,3,10] [] = flags_1_3_10
  | cabalLibVersion <  Version [1,10,0] [] = flags_1_10_0
  | cabalLibVersion <  Version [1,14,0] [] = flags_1_14_0
  | cabalLibVersion <  Version [1,18,0] [] = flags_1_18_0
  | cabalLibVersion <  Version [1,19,1] [] = flags_1_19_0
  | otherwise = flags_latest
  where
    -- Cabal >= 1.19.1 uses --dependency and does not need --constraint
    flags_latest = flags        { configConstraints = [] }

    -- Cabal < 1.19.1 does not grok the --dependency flag.
    flags_1_19_0 = flags        { configDependencies = [] }
    -- Cabal < 1.18.0 doesn't know about --extra-prog-path and --sysconfdir.
    flags_1_18_0 = flags_1_19_0 { configProgramPathExtra = []
                                , configInstallDirs = configInstallDirs_1_18_0}
    configInstallDirs_1_18_0 = (configInstallDirs flags) { sysconfdir = NoFlag }
    -- Cabal < 1.14.0 doesn't know about --disable-benchmarks.
    flags_1_14_0 = flags_1_18_0 { configBenchmarks  = NoFlag }
    -- Cabal < 1.10.0 doesn't know about --disable-tests.
    flags_1_10_0 = flags_1_14_0 { configTests       = NoFlag }
    -- Cabal < 1.3.10 does not grok the --constraints flag.
    flags_1_3_10 = flags_1_10_0 { configConstraints = [] }

-- ------------------------------------------------------------
-- * Config extra flags
-- ------------------------------------------------------------

-- | cabal configure takes some extra flags beyond runghc Setup configure
--
data ConfigExFlags = ConfigExFlags {
    configCabalVersion :: Flag Version,
    configExConstraints:: [UserConstraint],
    configPreferences  :: [Dependency],
    configSolver       :: Flag PreSolver
  }

defaultConfigExFlags :: ConfigExFlags
defaultConfigExFlags = mempty { configSolver = Flag defaultSolver }

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

  , optionSolver configSolver (\v flags -> flags { configSolver = v })
  ]

instance Monoid ConfigExFlags where
  mempty = ConfigExFlags {
    configCabalVersion = mempty,
    configExConstraints= mempty,
    configPreferences  = mempty,
    configSolver       = mempty
  }
  mappend a b = ConfigExFlags {
    configCabalVersion = combine configCabalVersion,
    configExConstraints= combine configExConstraints,
    configPreferences  = combine configPreferences,
    configSolver       = combine configSolver
  }
    where combine field = field a `mappend` field b

-- ------------------------------------------------------------
-- * Build flags
-- ------------------------------------------------------------

data SkipAddSourceDepsCheck =
  SkipAddSourceDepsCheck | DontSkipAddSourceDepsCheck
  deriving Eq

data BuildExFlags = BuildExFlags {
  buildOnly     :: Flag SkipAddSourceDepsCheck
}

buildExOptions :: ShowOrParseArgs -> [OptionField BuildExFlags]
buildExOptions _showOrParseArgs =
  option [] ["only"]
  "Don't reinstall add-source dependencies (sandbox-only)"
  buildOnly (\v flags -> flags { buildOnly = v })
  (noArg (Flag SkipAddSourceDepsCheck))

  : []

buildCommand :: CommandUI (BuildFlags, BuildExFlags)
buildCommand = parent {
    commandDefaultFlags = (commandDefaultFlags parent, mempty),
    commandOptions      =
      \showOrParseArgs -> liftOptions fst setFst
                          (commandOptions parent showOrParseArgs)
                          ++
                          liftOptions snd setSnd (buildExOptions showOrParseArgs)
  }
  where
    setFst a (_,b) = (a,b)
    setSnd b (a,_) = (a,b)

    parent = Cabal.buildCommand defaultProgramConfiguration

instance Monoid BuildExFlags where
  mempty = BuildExFlags {
    buildOnly    = mempty
  }
  mappend a b = BuildExFlags {
    buildOnly    = combine buildOnly
  }
    where combine field = field a `mappend` field b

-- ------------------------------------------------------------
-- * Test command
-- ------------------------------------------------------------

testCommand :: CommandUI (TestFlags, BuildFlags, BuildExFlags)
testCommand = parent {
  commandDefaultFlags = (commandDefaultFlags parent,
                         Cabal.defaultBuildFlags, mempty),
  commandOptions      =
    \showOrParseArgs -> liftOptions get1 set1
                        (commandOptions parent showOrParseArgs)
                        ++
                        liftOptions get2 set2
                        (Cabal.buildOptions progConf showOrParseArgs)
                        ++
                        liftOptions get3 set3 (buildExOptions showOrParseArgs)
  }
  where
    get1 (a,_,_) = a; set1 a (_,b,c) = (a,b,c)
    get2 (_,b,_) = b; set2 b (a,_,c) = (a,b,c)
    get3 (_,_,c) = c; set3 c (a,b,_) = (a,b,c)

    parent   = Cabal.testCommand
    progConf = defaultProgramConfiguration

-- ------------------------------------------------------------
-- * Bench command
-- ------------------------------------------------------------

benchmarkCommand :: CommandUI (BenchmarkFlags, BuildFlags, BuildExFlags)
benchmarkCommand = parent {
  commandDefaultFlags = (commandDefaultFlags parent,
                         Cabal.defaultBuildFlags, mempty),
  commandOptions      =
    \showOrParseArgs -> liftOptions get1 set1
                        (commandOptions parent showOrParseArgs)
                        ++
                        liftOptions get2 set2
                        (Cabal.buildOptions progConf showOrParseArgs)
                        ++
                        liftOptions get3 set3 (buildExOptions showOrParseArgs)
  }
  where
    get1 (a,_,_) = a; set1 a (_,b,c) = (a,b,c)
    get2 (_,b,_) = b; set2 b (a,_,c) = (a,b,c)
    get3 (_,_,c) = c; set3 c (a,b,_) = (a,b,c)

    parent   = Cabal.benchmarkCommand
    progConf = defaultProgramConfiguration

-- ------------------------------------------------------------
-- * Fetch command
-- ------------------------------------------------------------

data FetchFlags = FetchFlags {
--    fetchOutput    :: Flag FilePath,
      fetchDeps      :: Flag Bool,
      fetchDryRun    :: Flag Bool,
      fetchSolver           :: Flag PreSolver,
      fetchMaxBackjumps     :: Flag Int,
      fetchReorderGoals     :: Flag Bool,
      fetchIndependentGoals :: Flag Bool,
      fetchShadowPkgs       :: Flag Bool,
      fetchVerbosity :: Flag Verbosity
    }

defaultFetchFlags :: FetchFlags
defaultFetchFlags = FetchFlags {
--  fetchOutput    = mempty,
    fetchDeps      = toFlag True,
    fetchDryRun    = toFlag False,
    fetchSolver           = Flag defaultSolver,
    fetchMaxBackjumps     = Flag defaultMaxBackjumps,
    fetchReorderGoals     = Flag False,
    fetchIndependentGoals = Flag False,
    fetchShadowPkgs       = Flag False,
    fetchVerbosity = toFlag normal
   }

fetchCommand :: CommandUI FetchFlags
fetchCommand = CommandUI {
    commandName         = "fetch",
    commandSynopsis     = "Downloads packages for later installation.",
    commandDescription  = Nothing,
    commandUsage        = usagePackages "fetch",
    commandDefaultFlags = defaultFetchFlags,
    commandOptions      = \ showOrParseArgs -> [
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

       ] ++

       optionSolver      fetchSolver           (\v flags -> flags { fetchSolver           = v }) :
       optionSolverFlags showOrParseArgs
                         fetchMaxBackjumps     (\v flags -> flags { fetchMaxBackjumps     = v })
                         fetchReorderGoals     (\v flags -> flags { fetchReorderGoals     = v })
                         fetchIndependentGoals (\v flags -> flags { fetchIndependentGoals = v })
                         fetchShadowPkgs       (\v flags -> flags { fetchShadowPkgs       = v })

  }

-- ------------------------------------------------------------
-- * Other commands
-- ------------------------------------------------------------

updateCommand  :: CommandUI (Flag Verbosity)
updateCommand = CommandUI {
    commandName         = "update",
    commandSynopsis     = "Updates list of known packages.",
    commandDescription  = Nothing,
    commandUsage        = usageFlags "update",
    commandDefaultFlags = toFlag normal,
    commandOptions      = \_ -> [optionVerbosity id const]
  }

upgradeCommand  :: CommandUI (ConfigFlags, ConfigExFlags, InstallFlags, HaddockFlags)
upgradeCommand = configureCommand {
    commandName         = "upgrade",
    commandSynopsis     = "(command disabled, use install instead)",
    commandDescription  = Nothing,
    commandUsage        = usageFlagsOrPackages "upgrade",
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
    commandSynopsis     = "Check the package for common mistakes.",
    commandDescription  = Nothing,
    commandUsage        = \pname -> "Usage: " ++ pname ++ " check\n",
    commandDefaultFlags = toFlag normal,
    commandOptions      = \_ -> []
  }

runCommand :: CommandUI (BuildFlags, BuildExFlags)
runCommand = CommandUI {
    commandName         = "run",
    commandSynopsis     = "Runs the compiled executable.",
    commandDescription  = Nothing,
    commandUsage        =
      \pname -> "Usage: " ++ pname
                ++ " run [FLAGS] [EXECUTABLE] [-- EXECUTABLE_FLAGS]\n\n"
                ++ "Flags for run:",
    commandDefaultFlags = mempty,
    commandOptions      =
      \showOrParseArgs -> liftOptions fst setFst
                          (Cabal.buildOptions progConf showOrParseArgs)
                          ++
                          liftOptions snd setSnd
                          (buildExOptions showOrParseArgs)
  }
  where
    setFst a (_,b) = (a,b)
    setSnd b (a,_) = (a,b)

    progConf = defaultProgramConfiguration

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
-- * Get flags
-- ------------------------------------------------------------

data GetFlags = GetFlags {
    getDestDir          :: Flag FilePath,
    getPristine         :: Flag Bool,
    getSourceRepository :: Flag (Maybe RepoKind),
    getVerbosity        :: Flag Verbosity
  }

defaultGetFlags :: GetFlags
defaultGetFlags = GetFlags {
    getDestDir          = mempty,
    getPristine         = mempty,
    getSourceRepository = mempty,
    getVerbosity        = toFlag normal
   }

getCommand :: CommandUI GetFlags
getCommand = CommandUI {
    commandName         = "get",
    commandSynopsis     = "Gets a package's source code.",
    commandDescription  = Just $ \_ ->
          "Creates a local copy of a package's source code. By default it gets "
       ++ "the source\ntarball and unpacks it in a local subdirectory. "
       ++ "Alternatively, with -s it will\nget the code from the source "
       ++ "repository specified by the package.\n",
    commandUsage        = usagePackages "get",
    commandDefaultFlags = mempty,
    commandOptions      = \_ -> [
        optionVerbosity getVerbosity (\v flags -> flags { getVerbosity = v })

       ,option "d" ["destdir"]
         "Where to place the package source, defaults to the current directory."
         getDestDir (\v flags -> flags { getDestDir = v })
         (reqArgFlag "PATH")

       ,option "s" ["source-repository"]
         "Copy the package's source repository (ie git clone, darcs get, etc as appropriate)."
         getSourceRepository (\v flags -> flags { getSourceRepository = v })
        (optArg "[head|this|...]" (readP_to_E (const "invalid source-repository")
                                              (fmap (toFlag . Just) parse))
                                  (Flag Nothing)
                                  (map (fmap show) . flagToList))

       , option [] ["pristine"]
           ("Unpack the original pristine tarball, rather than updating the "
           ++ ".cabal file with the latest revision from the package archive.")
           getPristine (\v flags -> flags { getPristine = v })
           trueArg
       ]
  }

-- 'cabal unpack' is a deprecated alias for 'cabal get'.
unpackCommand :: CommandUI GetFlags
unpackCommand = getCommand {
  commandName  = "unpack",
  commandUsage = usagePackages "unpack"
  }

instance Monoid GetFlags where
  mempty = defaultGetFlags
  mappend a b = GetFlags {
    getDestDir          = combine getDestDir,
    getPristine         = combine getPristine,
    getSourceRepository = combine getSourceRepository,
    getVerbosity        = combine getVerbosity
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
    commandUsage        = usageFlagsOrPackages "list",
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
    installDocumentation    :: Flag Bool,
    installHaddockIndex     :: Flag PathTemplate,
    installDryRun           :: Flag Bool,
    installMaxBackjumps     :: Flag Int,
    installReorderGoals     :: Flag Bool,
    installIndependentGoals :: Flag Bool,
    installShadowPkgs       :: Flag Bool,
    installReinstall        :: Flag Bool,
    installAvoidReinstalls  :: Flag Bool,
    installOverrideReinstall :: Flag Bool,
    installUpgradeDeps      :: Flag Bool,
    installOnly             :: Flag Bool,
    installOnlyDeps         :: Flag Bool,
    installRootCmd          :: Flag String,
    installSummaryFile      :: [PathTemplate],
    installLogFile          :: Flag PathTemplate,
    installBuildReports     :: Flag ReportLevel,
    installSymlinkBinDir    :: Flag FilePath,
    installOneShot          :: Flag Bool,
    installNumJobs          :: Flag (Maybe Int)
  }

defaultInstallFlags :: InstallFlags
defaultInstallFlags = InstallFlags {
    installDocumentation   = Flag False,
    installHaddockIndex    = Flag docIndexFile,
    installDryRun          = Flag False,
    installMaxBackjumps    = Flag defaultMaxBackjumps,
    installReorderGoals    = Flag False,
    installIndependentGoals= Flag False,
    installShadowPkgs      = Flag False,
    installReinstall       = Flag False,
    installAvoidReinstalls = Flag False,
    installOverrideReinstall = Flag False,
    installUpgradeDeps     = Flag False,
    installOnly            = Flag False,
    installOnlyDeps        = Flag False,
    installRootCmd         = mempty,
    installSummaryFile     = mempty,
    installLogFile         = mempty,
    installBuildReports    = Flag NoReports,
    installSymlinkBinDir   = mempty,
    installOneShot         = Flag False,
    installNumJobs         = mempty
  }
  where
    docIndexFile = toPathTemplate ("$datadir" </> "doc" </> "index.html")

defaultMaxBackjumps :: Int
defaultMaxBackjumps = 200

defaultSolver :: PreSolver
defaultSolver = Choose

allSolvers :: String
allSolvers = intercalate ", " (map display ([minBound .. maxBound] :: [PreSolver]))

installCommand :: CommandUI (ConfigFlags, ConfigExFlags, InstallFlags, HaddockFlags)
installCommand = CommandUI {
  commandName         = "install",
  commandSynopsis     = "Installs a list of packages.",
  commandUsage        = usageFlagsOrPackages "install",
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
       liftOptions get1 set1
       (filter ((`notElem` ["constraint", "dependency"]) . optionName) $
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

haddockOptions :: ShowOrParseArgs -> [OptionField HaddockFlags]
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
  where
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
      ] ++

      optionSolverFlags showOrParseArgs
                        installMaxBackjumps     (\v flags -> flags { installMaxBackjumps     = v })
                        installReorderGoals     (\v flags -> flags { installReorderGoals     = v })
                        installIndependentGoals (\v flags -> flags { installIndependentGoals = v })
                        installShadowPkgs       (\v flags -> flags { installShadowPkgs       = v }) ++

      [ option [] ["reinstall"]
          "Install even if it means installing the same version again."
          installReinstall (\v flags -> flags { installReinstall = v })
          (yesNoOpt showOrParseArgs)

      , option [] ["avoid-reinstalls"]
          "Do not select versions that would destructively overwrite installed packages."
          installAvoidReinstalls (\v flags -> flags { installAvoidReinstalls = v })
          (yesNoOpt showOrParseArgs)

      , option [] ["force-reinstalls"]
          "Reinstall packages even if they will most likely break other installed packages."
          installOverrideReinstall (\v flags -> flags { installOverrideReinstall = v })
          (yesNoOpt showOrParseArgs)

      , option [] ["upgrade-dependencies"]
          "Pick the latest version for all dependencies, rather than trying to pick an installed version."
          installUpgradeDeps (\v flags -> flags { installUpgradeDeps = v })
          (yesNoOpt showOrParseArgs)

      , option [] ["only-dependencies"]
          "Install only the dependencies necessary to build the given packages"
          installOnlyDeps (\v flags -> flags { installOnlyDeps = v })
          (yesNoOpt showOrParseArgs)

      , option [] ["dependencies-only"]
          "A synonym for --only-dependencies"
          installOnlyDeps (\v flags -> flags { installOnlyDeps = v })
          (yesNoOpt showOrParseArgs)

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
          (yesNoOpt showOrParseArgs)

      , option "j" ["jobs"]
        "Run NUM jobs simultaneously (or '$ncpus' if no NUM is given)."
        installNumJobs (\v flags -> flags { installNumJobs = v })
        (optArg "NUM" (fmap Flag numJobsParser)
                      (Flag Nothing)
                      (map (Just . maybe "$ncpus" show) . flagToList))
      ] ++ case showOrParseArgs of      -- TODO: remove when "cabal install"
                                        -- avoids
          ParseArgs ->
            [ option [] ["only"]
              "Only installs the package in the current directory."
              installOnly (\v flags -> flags { installOnly = v })
              trueArg ]
          _ -> []


instance Monoid InstallFlags where
  mempty = InstallFlags {
    installDocumentation   = mempty,
    installHaddockIndex    = mempty,
    installDryRun          = mempty,
    installReinstall       = mempty,
    installAvoidReinstalls = mempty,
    installOverrideReinstall = mempty,
    installMaxBackjumps    = mempty,
    installUpgradeDeps     = mempty,
    installReorderGoals    = mempty,
    installIndependentGoals= mempty,
    installShadowPkgs      = mempty,
    installOnly            = mempty,
    installOnlyDeps        = mempty,
    installRootCmd         = mempty,
    installSummaryFile     = mempty,
    installLogFile         = mempty,
    installBuildReports    = mempty,
    installSymlinkBinDir   = mempty,
    installOneShot         = mempty,
    installNumJobs         = mempty
  }
  mappend a b = InstallFlags {
    installDocumentation   = combine installDocumentation,
    installHaddockIndex    = combine installHaddockIndex,
    installDryRun          = combine installDryRun,
    installReinstall       = combine installReinstall,
    installAvoidReinstalls = combine installAvoidReinstalls,
    installOverrideReinstall = combine installOverrideReinstall,
    installMaxBackjumps    = combine installMaxBackjumps,
    installUpgradeDeps     = combine installUpgradeDeps,
    installReorderGoals    = combine installReorderGoals,
    installIndependentGoals= combine installIndependentGoals,
    installShadowPkgs      = combine installShadowPkgs,
    installOnly            = combine installOnly,
    installOnlyDeps        = combine installOnlyDeps,
    installRootCmd         = combine installRootCmd,
    installSummaryFile     = combine installSummaryFile,
    installLogFile         = combine installLogFile,
    installBuildReports    = combine installBuildReports,
    installSymlinkBinDir   = combine installSymlinkBinDir,
    installOneShot         = combine installOneShot,
    installNumJobs         = combine installNumJobs
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
    commandSynopsis     = "Uploads source packages to Hackage.",
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
defaultInitFlags  = emptyInitFlags { IT.initVerbosity = toFlag normal }

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

      , option [] ["overwrite"]
        "Overwrite any existing .cabal, LICENSE, or Setup.hs files without warning."
        IT.overwrite (\v flags -> flags { IT.overwrite = v })
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

      , option ['x'] ["extra-source-file"]
        "Extra source file to be distributed with tarball."
        IT.extraSrc (\v flags -> flags { IT.extraSrc = v })
        (reqArg' "FILE" (Just . (:[]))
                        (fromMaybe []))

      , option [] ["is-library"]
        "Build a library."
        IT.packageType (\v flags -> flags { IT.packageType = v })
        (noArg (Flag IT.Library))

      , option [] ["is-executable"]
        "Build an executable."
        IT.packageType
        (\v flags -> flags { IT.packageType = v })
        (noArg (Flag IT.Executable))

      , option [] ["language"]
        "Specify the default language."
        IT.language
        (\v flags -> flags { IT.language = v })
        (reqArg "LANGUAGE" (readP_to_E ("Cannot parse language: "++)
                                       (toFlag `fmap` parse))
                          (flagToList . fmap display))

      , option ['o'] ["expose-module"]
        "Export a module from the package."
        IT.exposedModules
        (\v flags -> flags { IT.exposedModules = v })
        (reqArg "MODULE" (readP_to_E ("Cannot parse module name: "++)
                                     ((Just . (:[])) `fmap` parse))
                         (maybe [] (fmap display)))

      , option [] ["extension"]
        "Use a LANGUAGE extension (in the other-extensions field)."
        IT.otherExts
        (\v flags -> flags { IT.otherExts = v })
        (reqArg "EXTENSION" (readP_to_E ("Cannot parse extension: "++)
                                        ((Just . (:[])) `fmap` parse))
                            (maybe [] (fmap display)))

      , option ['d'] ["dependency"]
        "Package dependency."
        IT.dependencies (\v flags -> flags { IT.dependencies = v })
        (reqArg "PACKAGE" (readP_to_E ("Cannot parse dependency: "++)
                                      ((Just . (:[])) `fmap` parse))
                          (maybe [] (fmap display)))

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

      , optionVerbosity IT.initVerbosity (\v flags -> flags { IT.initVerbosity = v })
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
-- * Win32SelfUpgrade flags
-- ------------------------------------------------------------

data Win32SelfUpgradeFlags = Win32SelfUpgradeFlags {
  win32SelfUpgradeVerbosity :: Flag Verbosity
}

defaultWin32SelfUpgradeFlags :: Win32SelfUpgradeFlags
defaultWin32SelfUpgradeFlags = Win32SelfUpgradeFlags {
  win32SelfUpgradeVerbosity = toFlag normal
}

win32SelfUpgradeCommand :: CommandUI Win32SelfUpgradeFlags
win32SelfUpgradeCommand = CommandUI {
  commandName         = "win32selfupgrade",
  commandSynopsis     = "Self-upgrade the executable on Windows",
  commandDescription  = Nothing,
  commandUsage        = \pname ->
    "Usage: " ++ pname ++ " win32selfupgrade PID PATH\n\n"
     ++ "Flags for win32selfupgrade:",
  commandDefaultFlags = defaultWin32SelfUpgradeFlags,
  commandOptions      = \_ ->
      [optionVerbosity win32SelfUpgradeVerbosity
       (\v flags -> flags { win32SelfUpgradeVerbosity = v})
      ]
}

instance Monoid Win32SelfUpgradeFlags where
  mempty = defaultWin32SelfUpgradeFlags
  mappend a b = Win32SelfUpgradeFlags {
    win32SelfUpgradeVerbosity = combine win32SelfUpgradeVerbosity
  }
    where combine field = field a `mappend` field b

-- ------------------------------------------------------------
-- * Sandbox-related flags
-- ------------------------------------------------------------

data SandboxFlags = SandboxFlags {
  sandboxVerbosity :: Flag Verbosity,
  sandboxSnapshot  :: Flag Bool, -- FIXME: this should be an 'add-source'-only
                                 -- flag.
  sandboxLocation  :: Flag FilePath
}

defaultSandboxLocation :: FilePath
defaultSandboxLocation = ".cabal-sandbox"

defaultSandboxFlags :: SandboxFlags
defaultSandboxFlags = SandboxFlags {
  sandboxVerbosity = toFlag normal,
  sandboxSnapshot  = toFlag False,
  sandboxLocation  = toFlag defaultSandboxLocation
  }

sandboxCommand :: CommandUI SandboxFlags
sandboxCommand = CommandUI {
  commandName         = "sandbox",
  commandSynopsis     = "Create/modify/delete a sandbox.",
  commandDescription  = Nothing,
  commandUsage        = \pname ->
       "Usage: " ++ pname ++ " sandbox init\n"
    ++ "   or: " ++ pname ++ " sandbox delete\n"
    ++ "   or: " ++ pname ++ " sandbox add-source  [PATHS]\n\n"
    ++ "   or: " ++ pname ++ " sandbox hc-pkg      -- [ARGS]\n"
    ++ "   or: " ++ pname ++ " sandbox list-sources\n\n"
    ++ "Flags for sandbox:",

  commandDefaultFlags = defaultSandboxFlags,
  commandOptions      = \_ ->
    [ optionVerbosity sandboxVerbosity
      (\v flags -> flags { sandboxVerbosity = v })

    , option [] ["snapshot"]
      "Take a snapshot instead of creating a link (only applies to 'add-source')"
      sandboxSnapshot (\v flags -> flags { sandboxSnapshot = v })
      trueArg

    , option [] ["sandbox"]
      "Sandbox location (default: './.cabal-sandbox')."
      sandboxLocation (\v flags -> flags { sandboxLocation = v })
      (reqArgFlag "DIR")
    ]
  }

instance Monoid SandboxFlags where
  mempty = SandboxFlags {
    sandboxVerbosity = mempty,
    sandboxSnapshot  = mempty,
    sandboxLocation  = mempty
    }
  mappend a b = SandboxFlags {
    sandboxVerbosity = combine sandboxVerbosity,
    sandboxSnapshot  = combine sandboxSnapshot,
    sandboxLocation  = combine sandboxLocation
    }
    where combine field = field a `mappend` field b

-- ------------------------------------------------------------
-- * GetOpt Utils
-- ------------------------------------------------------------

reqArgFlag :: ArgPlaceHolder ->
              MkOptDescr (b -> Flag String) (Flag String -> b -> b) b
reqArgFlag ad = reqArg ad (succeedReadE Flag) flagToList

liftOptions :: (b -> a) -> (a -> b -> b)
            -> [OptionField a] -> [OptionField b]
liftOptions get set = map (liftOption get set)

yesNoOpt :: ShowOrParseArgs -> MkOptDescr (b -> Flag Bool) (Flag Bool -> b -> b) b
yesNoOpt ShowArgs sf lf = trueArg sf lf
yesNoOpt _        sf lf = boolOpt' flagToMaybe Flag (sf, lf) ([], map ("no-" ++) lf) sf lf

optionSolver :: (flags -> Flag PreSolver)
             -> (Flag PreSolver -> flags -> flags)
             -> OptionField flags
optionSolver get set =
  option [] ["solver"]
    ("Select dependency solver to use (default: " ++ display defaultSolver ++ "). Choices: " ++ allSolvers ++ ", where 'choose' chooses between 'topdown' and 'modular' based on compiler version.")
    get set
    (reqArg "SOLVER" (readP_to_E (const $ "solver must be one of: " ++ allSolvers)
                                 (toFlag `fmap` parse))
                     (flagToList . fmap display))

optionSolverFlags :: ShowOrParseArgs
                  -> (flags -> Flag Int   ) -> (Flag Int    -> flags -> flags)
                  -> (flags -> Flag Bool  ) -> (Flag Bool   -> flags -> flags)
                  -> (flags -> Flag Bool  ) -> (Flag Bool   -> flags -> flags)
                  -> (flags -> Flag Bool  ) -> (Flag Bool   -> flags -> flags)
                  -> [OptionField flags]
optionSolverFlags showOrParseArgs getmbj setmbj getrg setrg _getig _setig getsip setsip =
  [ option [] ["max-backjumps"]
      ("Maximum number of backjumps allowed while solving (default: " ++ show defaultMaxBackjumps ++ "). Use a negative number to enable unlimited backtracking. Use 0 to disable backtracking completely.")
      getmbj setmbj
      (reqArg "NUM" (readP_to_E ("Cannot parse number: "++)
                                (fmap toFlag (Parse.readS_to_P reads)))
                    (map show . flagToList))
  , option [] ["reorder-goals"]
      "Try to reorder goals according to certain heuristics. Slows things down on average, but may make backtracking faster for some packages."
      getrg setrg
      (yesNoOpt showOrParseArgs)
  -- TODO: Disabled for now because it does not work as advertised (yet).
{-
  , option [] ["independent-goals"]
      "Treat several goals on the command line as independent. If several goals depend on the same package, different versions can be chosen."
      getig setig
      (yesNoOpt showOrParseArgs)
-}
  , option [] ["shadow-installed-packages"]
      "If multiple package instances of the same version are installed, treat all but one as shadowed."
      getsip setsip
      trueArg
  ]


usageFlagsOrPackages :: String -> String -> String
usageFlagsOrPackages name pname =
     "Usage: " ++ pname ++ " " ++ name ++ " [FLAGS]\n"
  ++ "   or: " ++ pname ++ " " ++ name ++ " [PACKAGES]\n\n"
  ++ "Flags for " ++ name ++ ":"

usagePackages :: String -> String -> String
usagePackages name pname =
     "Usage: " ++ pname ++ " " ++ name ++ " [PACKAGES]\n\n"
  ++ "Flags for " ++ name ++ ":"

usageFlags :: String -> String -> String
usageFlags name pname =
  "Usage: " ++ pname ++ " " ++ name ++ " [FLAGS]\n\n"
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
