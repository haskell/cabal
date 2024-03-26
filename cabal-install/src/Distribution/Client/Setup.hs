{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.Client.Setup
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
module Distribution.Client.Setup
  ( globalCommand
  , GlobalFlags (..)
  , defaultGlobalFlags
  , RepoContext (..)
  , withRepoContext
  , configureCommand
  , CommonSetupFlags (..)
  , ConfigFlags (..)
  , configureOptions
  , filterCommonFlags
  , filterConfigureFlags
  , configPackageDB'
  , configCompilerAux'
  , configureExCommand
  , ConfigExFlags (..)
  , defaultConfigExFlags
  , buildCommand
  , BuildFlags (..)
  , filterTestFlags
  , replCommand
  , testCommand
  , benchmarkCommand
  , testOptions
  , benchmarkOptions
  , configureExOptions
  , reconfigureCommand
  , installCommand
  , InstallFlags (..)
  , installOptions
  , defaultInstallFlags
  , filterHaddockArgs
  , filterHaddockFlags
  , haddockOptions
  , defaultSolver
  , defaultMaxBackjumps
  , listCommand
  , ListFlags (..)
  , listNeedsCompiler
  , UpdateFlags (..)
  , defaultUpdateFlags
  , infoCommand
  , InfoFlags (..)
  , fetchCommand
  , FetchFlags (..)
  , freezeCommand
  , FreezeFlags (..)
  , genBoundsCommand
  , getCommand
  , unpackCommand
  , GetFlags (..)
  , checkCommand
  , CheckFlags (..)
  , formatCommand
  , uploadCommand
  , UploadFlags (..)
  , IsCandidate (..)
  , reportCommand
  , ReportFlags (..)
  , runCommand
  , initCommand
  , initOptions
  , IT.InitFlags (..)
  , actAsSetupCommand
  , ActAsSetupFlags (..)
  , userConfigCommand
  , UserConfigFlags (..)
  , manpageCommand
  , haddockCommand
  , cleanCommand
  , copyCommand
  , registerCommand
  , Path (..)
  , pathName
  , PathFlags (..)
  , pathCommand
  , liftOptions
  , yesNoOpt
  ) where

import Distribution.Client.Compat.Prelude hiding (get)
import Prelude ()

import Distribution.Client.Types.AllowNewer (AllowNewer (..), AllowOlder (..), RelaxDeps (..))
import Distribution.Client.Types.Credentials (Password (..), Token (..), Username (..))
import Distribution.Client.Types.Repo (LocalRepo (..), RemoteRepo (..))
import Distribution.Client.Types.WriteGhcEnvironmentFilesPolicy

import Distribution.Client.BuildReports.Types
  ( ReportLevel (..)
  )
import Distribution.Client.Dependency.Types
  ( PreSolver (..)
  )
import Distribution.Client.IndexUtils.ActiveRepos
  ( ActiveRepos
  )
import Distribution.Client.IndexUtils.IndexState
  ( TotalIndexState
  , headTotalIndexState
  )
import qualified Distribution.Client.Init.Defaults as IT
import qualified Distribution.Client.Init.Types as IT
import Distribution.Client.Targets
  ( UserConstraint
  , readUserConstraint
  )
import Distribution.Deprecated.ParseUtils (parseSpaceList, parseTokenQ)
import Distribution.Deprecated.ReadP (readP_to_E)
import Distribution.Utils.NubList
  ( NubList
  , fromNubList
  , toNubList
  )

import Distribution.Solver.Types.ConstraintSource
import Distribution.Solver.Types.Settings

import Distribution.Client.GlobalFlags
  ( GlobalFlags (..)
  , RepoContext (..)
  , defaultGlobalFlags
  , withRepoContext
  )
import Distribution.Client.ManpageFlags (ManpageFlags, defaultManpageFlags, manpageOptions)
import qualified Distribution.Compat.CharParsing as P
import Distribution.FieldGrammar.Newtypes (SpecVersion (..))
import Distribution.PackageDescription
  ( BuildType (..)
  , Dependency
  , LibraryName (..)
  , RepoKind (..)
  )
import Distribution.PackageDescription.Check (CheckExplanationIDString)
import Distribution.Parsec
  ( parsecCommaList
  )
import Distribution.ReadE
  ( ReadE (..)
  , parsecToReadE
  , parsecToReadEErr
  , succeedReadE
  , unexpectMsgString
  )
import Distribution.Simple.Command hiding (boolOpt, boolOpt')
import qualified Distribution.Simple.Command as Command
import Distribution.Simple.Compiler (Compiler, PackageDB, PackageDBStack)
import Distribution.Simple.Configure
  ( computeEffectiveProfiling
  , configCompilerAuxEx
  , interpretPackageDbFlags
  )
import Distribution.Simple.Flag
  ( Flag (..)
  , flagElim
  , flagToList
  , flagToMaybe
  , fromFlagOrDefault
  , maybeToFlag
  , mergeListFlag
  , toFlag
  )
import Distribution.Simple.InstallDirs
  ( InstallDirs (..)
  , PathTemplate
  , combinePathTemplate
  , fromPathTemplate
  , toPathTemplate
  )
import Distribution.Simple.Program (ProgramDb, defaultProgramDb)
import Distribution.Simple.Setup
  ( BenchmarkFlags
  , BooleanFlag (..)
  , BuildFlags (..)
  , CleanFlags (..)
  , CommonSetupFlags (..)
  , ConfigFlags (..)
  , CopyFlags (..)
  , HaddockFlags (..)
  , RegisterFlags (..)
  , ReplFlags
  , TestFlags
  , boolOpt
  , boolOpt'
  , falseArg
  , optionVerbosity
  , readPackageDbList
  , showPackageDbList
  , testCommonFlags
  , trueArg
  )
import qualified Distribution.Simple.Setup as Cabal
import Distribution.Simple.Utils
  ( wrapText
  )
import Distribution.System (Platform)
import Distribution.Types.GivenComponent
  ( GivenComponent (..)
  )
import Distribution.Types.PackageVersionConstraint
  ( PackageVersionConstraint (..)
  )
import Distribution.Types.UnqualComponentName
  ( unqualComponentNameToPackageName
  )
import Distribution.Verbosity
  ( lessVerbose
  , normal
  , verboseNoFlags
  , verboseNoTimestamp
  )
import Distribution.Version
  ( Version
  , mkVersion
  )

import Control.Exception
  ( assert
  )
import Data.List
  ( deleteFirstsBy
  )
import System.FilePath
  ( (</>)
  )

globalCommand :: [Command action] -> CommandUI GlobalFlags
globalCommand commands =
  CommandUI
    { commandName = ""
    , commandSynopsis =
        "Command line interface to the Haskell Cabal infrastructure."
    , commandUsage = \pname ->
        "See http://www.haskell.org/cabal/ for more information.\n"
          ++ "\n"
          ++ "Usage: "
          ++ pname
          ++ " [GLOBAL FLAGS] [COMMAND [FLAGS]]\n"
    , commandDescription = Just $ \pname ->
        let
          commands' = commands ++ [commandAddAction helpCommandUI undefined]
          cmdDescs = getNormalCommandDescriptions commands'
          -- if new commands are added, we want them to appear even if they
          -- are not included in the custom listing below. Thus, we calculate
          -- the `otherCmds` list and append it under the `other` category.
          -- Alternatively, a new testcase could be added that ensures that
          -- the set of commands listed here is equal to the set of commands
          -- that are actually available.
          otherCmds =
            deleteFirstsBy
              (==)
              (map fst cmdDescs)
              [ "help"
              , "update"
              , "install"
              , "fetch"
              , "list"
              , "info"
              , "user-config"
              , "get"
              , "unpack"
              , "init"
              , "configure"
              , "build"
              , "clean"
              , "run"
              , "repl"
              , "test"
              , "bench"
              , "check"
              , "sdist"
              , "upload"
              , "report"
              , "freeze"
              , "gen-bounds"
              , "outdated"
              , "haddock"
              , "haddock-project"
              , "hscolour"
              , "exec"
              , "path"
              , "new-build"
              , "new-configure"
              , "new-repl"
              , "new-freeze"
              , "new-run"
              , "new-test"
              , "new-bench"
              , "new-haddock"
              , "new-exec"
              , "new-update"
              , "new-install"
              , "new-clean"
              , "new-sdist"
              , "new-haddock-project"
              , "list-bin"
              , -- v1 commands, stateful style
                "v1-build"
              , "v1-configure"
              , "v1-repl"
              , "v1-freeze"
              , "v1-run"
              , "v1-test"
              , "v1-bench"
              , "v1-haddock"
              , "v1-exec"
              , "v1-update"
              , "v1-install"
              , "v1-clean"
              , "v1-sdist"
              , "v1-doctest"
              , "v1-copy"
              , "v1-register"
              , "v1-reconfigure"
              , -- v2 commands, nix-style
                "v2-build"
              , "v2-configure"
              , "v2-repl"
              , "v2-freeze"
              , "v2-run"
              , "v2-test"
              , "v2-bench"
              , "v2-haddock"
              , "v2-haddock-project"
              , "v2-exec"
              , "v2-update"
              , "v2-install"
              , "v2-clean"
              , "v2-sdist"
              ]
          maxlen = maximum $ [length name | (name, _) <- cmdDescs]
          align str = str ++ replicate (maxlen - length str) ' '
          startGroup n = " [" ++ n ++ "]"
          par = ""
          addCmd n = case lookup n cmdDescs of
            Nothing -> ""
            Just d -> "  " ++ align n ++ "    " ++ d
         in
          "Commands:\n"
            ++ unlines
              ( [ startGroup "global"
                , addCmd "user-config"
                , addCmd "path"
                , addCmd "help"
                , par
                , startGroup "package database"
                , addCmd "update"
                , addCmd "list"
                , addCmd "info"
                , par
                , startGroup "initialization and download"
                , addCmd "init"
                , addCmd "fetch"
                , addCmd "get"
                , par
                , startGroup "project configuration"
                , addCmd "configure"
                , addCmd "freeze"
                , addCmd "gen-bounds"
                , addCmd "outdated"
                , par
                , startGroup "project building and installing"
                , addCmd "build"
                , addCmd "install"
                , addCmd "haddock"
                , addCmd "haddock-project"
                , addCmd "clean"
                , par
                , startGroup "running and testing"
                , addCmd "list-bin"
                , addCmd "repl"
                , addCmd "run"
                , addCmd "bench"
                , addCmd "test"
                , addCmd "exec"
                , par
                , startGroup "sanity checks and shipping"
                , addCmd "check"
                , addCmd "sdist"
                , addCmd "upload"
                , addCmd "report"
                , par
                , startGroup "deprecated"
                , addCmd "unpack"
                , addCmd "hscolour"
                , par
                , startGroup "new-style projects (forwards-compatible aliases)"
                , addCmd "v2-build"
                , addCmd "v2-configure"
                , addCmd "v2-repl"
                , addCmd "v2-run"
                , addCmd "v2-test"
                , addCmd "v2-bench"
                , addCmd "v2-freeze"
                , addCmd "v2-haddock"
                , addCmd "v2-exec"
                , addCmd "v2-update"
                , addCmd "v2-install"
                , addCmd "v2-clean"
                , addCmd "v2-sdist"
                , par
                , startGroup "legacy command aliases"
                , addCmd "v1-build"
                , addCmd "v1-configure"
                , addCmd "v1-repl"
                , addCmd "v1-run"
                , addCmd "v1-test"
                , addCmd "v1-bench"
                , addCmd "v1-freeze"
                , addCmd "v1-haddock"
                , addCmd "v1-install"
                , addCmd "v1-clean"
                , addCmd "v1-copy"
                , addCmd "v1-register"
                , addCmd "v1-reconfigure"
                ]
                  ++ if null otherCmds
                    then []
                    else
                      par
                        : startGroup "other"
                        : [addCmd n | n <- otherCmds]
              )
            ++ "\n"
            ++ "For more information about a command use:\n"
            ++ "   "
            ++ pname
            ++ " COMMAND --help\n"
            ++ "or "
            ++ pname
            ++ " help COMMAND\n"
            ++ "\n"
            ++ "To install Cabal packages from hackage use:\n"
            ++ "  "
            ++ pname
            ++ " install foo [--dry-run]\n"
            ++ "\n"
            ++ "Occasionally you need to update the list of available packages:\n"
            ++ "  "
            ++ pname
            ++ " update\n"
    , commandNotes = Nothing
    , commandDefaultFlags = mempty
    , commandOptions = args
    }
  where
    args :: ShowOrParseArgs -> [OptionField GlobalFlags]
    args ShowArgs = argsShown
    args ParseArgs = argsShown ++ argsNotShown

    -- arguments we want to show in the help
    argsShown :: [OptionField GlobalFlags]
    argsShown =
      [ option
          ['V']
          ["version"]
          "Print version information"
          globalVersion
          (\v flags -> flags{globalVersion = v})
          trueArg
      , option
          []
          ["numeric-version"]
          "Print just the version number"
          globalNumericVersion
          (\v flags -> flags{globalNumericVersion = v})
          trueArg
      , option
          []
          ["config-file"]
          "Set an alternate location for the config file"
          globalConfigFile
          (\v flags -> flags{globalConfigFile = v})
          (reqArgFlag "FILE")
      , option
          []
          ["ignore-expiry"]
          "Ignore expiry dates on signed metadata (use only in exceptional circumstances)"
          globalIgnoreExpiry
          (\v flags -> flags{globalIgnoreExpiry = v})
          trueArg
      , option
          []
          ["http-transport"]
          "Set a transport for http(s) requests. Accepts 'curl', 'wget', 'powershell', and 'plain-http'. (default: 'curl')"
          globalHttpTransport
          (\v flags -> flags{globalHttpTransport = v})
          (reqArgFlag "HttpTransport")
      , multiOption
          "nix"
          globalNix
          (\v flags -> flags{globalNix = v})
          [ optArg'
              "(True or False)"
              (maybeToFlag . (readMaybe =<<))
              ( \case
                  Flag True -> [Just "enable"]
                  Flag False -> [Just "disable"]
                  NoFlag -> []
              )
              ""
              ["nix"] -- Must be empty because we need to return PP.empty from viewAsFieldDescr
              "[DEPRECATED] Nix integration: run commands through nix-shell if a 'shell.nix' file exists (default is False)"
          , noArg
              (Flag True)
              []
              ["enable-nix"]
              "[DEPRECATED] Enable Nix integration: run commands through nix-shell if a 'shell.nix' file exists"
          , noArg
              (Flag False)
              []
              ["disable-nix"]
              "[DEPRECATED] Disable Nix integration"
          ]
      , option
          []
          ["store-dir", "storedir"]
          "The location of the build store"
          globalStoreDir
          (\v flags -> flags{globalStoreDir = v})
          (reqArgFlag "DIR")
      , option
          []
          ["active-repositories"]
          "The active package repositories (set to ':none' to disable all repositories)"
          globalActiveRepos
          (\v flags -> flags{globalActiveRepos = v})
          ( reqArg
              "REPOS"
              ( parsecToReadE
                  (\err -> "Error parsing active-repositories: " ++ err)
                  (toFlag `fmap` parsec)
              )
              (map prettyShow . flagToList)
          )
      ]

    -- arguments we don't want shown in the help
    -- the remote repo flags are not useful compared to the more general "active-repositories" flag.
    -- the global logs directory was only used in v1, while in v2 we have specific project config logs dirs
    -- default-user-config is support for a relatively obscure workflow for v1-freeze.
    argsNotShown :: [OptionField GlobalFlags]
    argsNotShown =
      [ option
          []
          ["remote-repo"]
          "The name and url for a remote repository"
          globalRemoteRepos
          (\v flags -> flags{globalRemoteRepos = v})
          (reqArg' "NAME:URL" (toNubList . maybeToList . readRemoteRepo) (map showRemoteRepo . fromNubList))
      , option
          []
          ["local-no-index-repo"]
          "The name and a path for a local no-index repository"
          globalLocalNoIndexRepos
          (\v flags -> flags{globalLocalNoIndexRepos = v})
          (reqArg' "NAME:PATH" (toNubList . maybeToList . readLocalRepo) (map showLocalRepo . fromNubList))
      , option
          []
          ["remote-repo-cache"]
          "The location where downloads from all remote repos are cached"
          globalCacheDir
          (\v flags -> flags{globalCacheDir = v})
          (reqArgFlag "DIR")
      , option
          []
          ["logs-dir", "logsdir"]
          "The location to put log files"
          globalLogsDir
          (\v flags -> flags{globalLogsDir = v})
          (reqArgFlag "DIR")
      , option
          []
          ["default-user-config"]
          "Set a location for a cabal.config file for projects without their own cabal.config freeze file."
          globalConstraintsFile
          (\v flags -> flags{globalConstraintsFile = v})
          (reqArgFlag "FILE")
      ]

-- ------------------------------------------------------------

-- * Config flags

-- ------------------------------------------------------------

configureCommand :: CommandUI ConfigFlags
configureCommand =
  c
    { commandName = "configure"
    , commandDefaultFlags = mempty
    , commandDescription = Just $ \_ ->
        wrapText $
          "Configure how the package is built by setting "
            ++ "package (and other) flags.\n"
            ++ "\n"
            ++ "The configuration affects several other commands, "
            ++ "including v1-build, v1-test, v1-bench, v1-run, v1-repl.\n"
    , commandUsage = \pname ->
        "Usage: " ++ pname ++ " v1-configure [FLAGS]\n"
    , commandNotes = Just $ \pname ->
        (Cabal.programFlagsDescription defaultProgramDb ++ "\n")
          ++ "Examples:\n"
          ++ "  "
          ++ pname
          ++ " v1-configure\n"
          ++ "    Configure with defaults;\n"
          ++ "  "
          ++ pname
          ++ " v1-configure --enable-tests -fcustomflag\n"
          ++ "    Configure building package including tests,\n"
          ++ "    with some package-specific flag.\n"
    }
  where
    c = Cabal.configureCommand defaultProgramDb

configureOptions :: ShowOrParseArgs -> [OptionField ConfigFlags]
configureOptions = commandOptions configureCommand

filterCommonFlags :: CommonSetupFlags -> Version -> CommonSetupFlags
filterCommonFlags flags cabalLibVersion
  -- NB: we expect the latest version to be the most common case,
  -- so test it first.
  | cabalLibVersion >= mkVersion [3, 11, 0] = flags_latest
  | cabalLibVersion < mkVersion [1, 2, 5] = flags_1_2_5
  | cabalLibVersion < mkVersion [2, 1, 0] = flags_2_1_0
  | cabalLibVersion < mkVersion [3, 11, 0] = flags_3_11_0
  | otherwise = error "the impossible just happened" -- see first guard
  where
    flags_latest = flags
    flags_3_11_0 =
      flags_latest
        { setupWorkingDir = NoFlag
        }
    -- Cabal < 3.11 does not support the --working-dir flag.
    flags_2_1_0 =
      flags_3_11_0
        { -- Cabal < 2.1 doesn't know about -v +timestamp modifier
          setupVerbosity = fmap verboseNoTimestamp (setupVerbosity flags_3_11_0)
        }
    flags_1_2_5 =
      flags_2_1_0
        { -- Cabal < 1.25 doesn't have extended verbosity syntax
          setupVerbosity =
            fmap verboseNoFlags (setupVerbosity flags_2_1_0)
        }

-- | Given some 'ConfigFlags' for the version of Cabal that
-- cabal-install was built with, and a target older 'Version' of
-- Cabal that we want to pass these flags to, convert the
-- flags into a form that will be accepted by the older
-- Setup script.  Generally speaking, this just means filtering
-- out flags that the old Cabal library doesn't understand, but
-- in some cases it may also mean "emulating" a feature using
-- some more legacy flags.
filterConfigureFlags :: ConfigFlags -> Version -> ConfigFlags
filterConfigureFlags flags cabalLibVersion =
  let flags' = filterConfigureFlags' flags cabalLibVersion
   in flags'
        { configCommonFlags =
            filterCommonFlags (configCommonFlags flags') cabalLibVersion
        }

filterConfigureFlags' :: ConfigFlags -> Version -> ConfigFlags
filterConfigureFlags' flags cabalLibVersion
  -- NB: we expect the latest version to be the most common case,
  -- so test it first.
  | cabalLibVersion >= mkVersion [3, 11, 0] = flags_latest
  -- The naming convention is that flags_version gives flags with
  -- all flags *introduced* in version eliminated.
  -- It is NOT the latest version of Cabal library that
  -- these flags work for; version of introduction is a more
  -- natural metric.
  | cabalLibVersion < mkVersion [1, 3, 10] = flags_1_3_10
  | cabalLibVersion < mkVersion [1, 10, 0] = flags_1_10_0
  | cabalLibVersion < mkVersion [1, 12, 0] = flags_1_12_0
  | cabalLibVersion < mkVersion [1, 14, 0] = flags_1_14_0
  | cabalLibVersion < mkVersion [1, 18, 0] = flags_1_18_0
  | cabalLibVersion < mkVersion [1, 19, 1] = flags_1_19_1
  | cabalLibVersion < mkVersion [1, 19, 2] = flags_1_19_2
  | cabalLibVersion < mkVersion [1, 21, 1] = flags_1_21_1
  | cabalLibVersion < mkVersion [1, 22, 0] = flags_1_22_0
  | cabalLibVersion < mkVersion [1, 22, 1] = flags_1_22_1
  | cabalLibVersion < mkVersion [1, 23, 0] = flags_1_23_0
  | cabalLibVersion < mkVersion [1, 25, 0] = flags_1_25_0
  | cabalLibVersion < mkVersion [2, 1, 0] = flags_2_1_0
  | cabalLibVersion < mkVersion [2, 5, 0] = flags_2_5_0
  | cabalLibVersion < mkVersion [3, 7, 0] = flags_3_7_0
  | cabalLibVersion < mkVersion [3, 11, 0] = flags_3_11_0
  | otherwise = error "the impossible just happened" -- see first guard
  where
    flags_latest =
      flags
        { -- Cabal >= 1.19.1 uses '--dependency' and does not need '--constraint'.
          -- Note: this is not in the wrong place. configConstraints gets
          -- repopulated in flags_1_19_1 but it needs to be set to empty for
          -- newer versions first.
          configConstraints = []
        }

    flags_3_11_0 =
      flags_latest
        { -- It's too late to convert configPromisedDependencies to anything
          -- meaningful, so we just assert that it's empty.
          -- We add a Cabal>=3.11 constraint before solving when multi-repl is
          -- enabled, so this should never trigger.
          configPromisedDependencies = assert (null $ configPromisedDependencies flags) []
        , -- Cabal < 3.11 does not understand '--coverage-for', which is OK
          -- because previous versions of Cabal using coverage implied
          -- whole-package builds (cuz_coverage), and determine the path to
          -- libraries mix dirs from the testsuite root with a small hack.
          configCoverageFor = NoFlag
        }

    flags_3_7_0 =
      flags_3_11_0
        { -- Cabal < 3.7 does not know about --extra-lib-dirs-static
          configExtraLibDirsStatic = []
        , -- Cabal < 3.7 does not understand '--enable-build-info' or '--disable-build-info'
          configDumpBuildInfo = NoFlag
        }

    flags_2_5_0 =
      flags_3_7_0
        { -- Cabal < 2.5 does not understand --dependency=pkg:component=cid
          -- (public sublibraries), so we convert it to the legacy
          -- --dependency=pkg_or_internal_component=cid
          configDependencies =
            let convertToLegacyInternalDep (GivenComponent _ (LSubLibName cn) cid) =
                  Just $
                    GivenComponent
                      (unqualComponentNameToPackageName cn)
                      LMainLibName
                      cid
                convertToLegacyInternalDep (GivenComponent pn LMainLibName cid) =
                  Just $ GivenComponent pn LMainLibName cid
             in catMaybes $ convertToLegacyInternalDep <$> configDependencies flags
        , -- Cabal < 2.5 doesn't know about '--allow-depending-on-private-libs'.
          configAllowDependingOnPrivateLibs = NoFlag
        , -- Cabal < 2.5 doesn't know about '--enable/disable-executable-static'.
          configFullyStaticExe = NoFlag
        }

    flags_2_1_0 =
      flags_2_5_0
        { -- Cabal < 2.1 doesn't know about --<enable|disable>-static
          configStaticLib = NoFlag
        , configSplitSections = NoFlag
        }

    flags_1_25_0 =
      flags_2_1_0
        { -- Cabal < 1.25.0 doesn't know about --dynlibdir.
          configInstallDirs = configInstallDirs_1_25_0
        , -- Cabal < 1.25 doesn't support --deterministic
          configDeterministic = mempty
        }
    configInstallDirs_1_25_0 =
      let dirs = configInstallDirs flags
       in dirs
            { dynlibdir = NoFlag
            , libexecsubdir = NoFlag
            , libexecdir =
                maybeToFlag $
                  combinePathTemplate
                    <$> flagToMaybe (libexecdir dirs)
                    <*> flagToMaybe (libexecsubdir dirs)
            }
    -- Cabal < 1.23 doesn't know about '--profiling-detail'.
    -- Cabal < 1.23 has a hacked up version of 'enable-profiling'
    -- which we shouldn't use.
    (tryLibProfiling, tryExeProfiling) = computeEffectiveProfiling flags
    flags_1_23_0 =
      flags_1_25_0
        { configProfDetail = NoFlag
        , configProfLibDetail = NoFlag
        , configIPID = NoFlag
        , configProf = NoFlag
        , configProfExe = Flag tryExeProfiling
        , configProfLib = Flag tryLibProfiling
        }

    -- Cabal == 1.22.0.* had a discontinuity (see #5946 or e9a8d48a3adce34d)
    -- due to temporary amnesia of the --*-executable-profiling flags
    flags_1_22_1 =
      flags_1_23_0
        { configDebugInfo = NoFlag
        , configProfExe = NoFlag
        }

    -- Cabal < 1.22 doesn't know about '--disable-debug-info'.
    flags_1_22_0 = flags_1_23_0{configDebugInfo = NoFlag}

    -- Cabal < 1.21.1 doesn't know about 'disable-relocatable'
    -- Cabal < 1.21.1 doesn't know about 'enable-profiling'
    -- (but we already dealt with it in flags_1_23_0)
    flags_1_21_1 =
      flags_1_22_0
        { configRelocatable = NoFlag
        , configCoverage = NoFlag
        , configLibCoverage = configCoverage flags
        }
    -- Cabal < 1.19.2 doesn't know about '--exact-configuration' and
    -- '--enable-library-stripping'.
    flags_1_19_2 =
      flags_1_21_1
        { configExactConfiguration = NoFlag
        , configStripLibs = NoFlag
        }
    -- Cabal < 1.19.1 uses '--constraint' instead of '--dependency'.
    flags_1_19_1 =
      flags_1_19_2
        { configDependencies = []
        , configConstraints = configConstraints flags
        }
    -- Cabal < 1.18.0 doesn't know about --extra-prog-path and --sysconfdir.
    flags_1_18_0 =
      flags_1_19_1
        { configProgramPathExtra = toNubList []
        , configInstallDirs = configInstallDirs_1_18_0
        }
    configInstallDirs_1_18_0 = (configInstallDirs flags_1_19_1){sysconfdir = NoFlag}
    -- Cabal < 1.14.0 doesn't know about '--disable-benchmarks'.
    flags_1_14_0 = flags_1_18_0{configBenchmarks = NoFlag}
    -- Cabal < 1.12.0 doesn't know about '--enable/disable-executable-dynamic'
    -- and '--enable/disable-library-coverage'.
    flags_1_12_0 =
      flags_1_14_0
        { configLibCoverage = NoFlag
        , configDynExe = NoFlag
        }
    -- Cabal < 1.10.0 doesn't know about '--disable-tests'.
    flags_1_10_0 = flags_1_12_0{configTests = NoFlag}
    -- Cabal < 1.3.10 does not grok the '--constraints' flag.
    flags_1_3_10 = flags_1_10_0{configConstraints = []}

-- | Get the package database settings from 'ConfigFlags', accounting for
-- @--package-db@ and @--user@ flags.
configPackageDB' :: ConfigFlags -> PackageDBStack
configPackageDB' cfg =
  interpretPackageDbFlags userInstall (configPackageDBs cfg)
  where
    userInstall = Cabal.fromFlagOrDefault True (configUserInstall cfg)

-- | Configure the compiler, but reduce verbosity during this step.
configCompilerAux' :: ConfigFlags -> IO (Compiler, Platform, ProgramDb)
configCompilerAux' configFlags = do
  let commonFlags = configCommonFlags configFlags
  configCompilerAuxEx
    configFlags
      { -- FIXME: make configCompilerAux use a sensible verbosity
        configCommonFlags =
          commonFlags
            { setupVerbosity = fmap lessVerbose (setupVerbosity commonFlags)
            }
      }

-- ------------------------------------------------------------

-- * Config extra flags

-- ------------------------------------------------------------

-- | cabal configure takes some extra flags beyond runghc Setup configure
data ConfigExFlags = ConfigExFlags
  { configCabalVersion :: Flag Version
  , configAppend :: Flag Bool
  , configBackup :: Flag Bool
  , configExConstraints :: [(UserConstraint, ConstraintSource)]
  , configPreferences :: [PackageVersionConstraint]
  , configSolver :: Flag PreSolver
  , configAllowNewer :: Maybe AllowNewer
  , configAllowOlder :: Maybe AllowOlder
  , configWriteGhcEnvironmentFilesPolicy
      :: Flag WriteGhcEnvironmentFilesPolicy
  }
  deriving (Eq, Show, Generic)

defaultConfigExFlags :: ConfigExFlags
defaultConfigExFlags = mempty{configSolver = Flag defaultSolver}

configureExCommand :: CommandUI (ConfigFlags, ConfigExFlags)
configureExCommand =
  configureCommand
    { commandDefaultFlags = (mempty, defaultConfigExFlags)
    , commandOptions = \showOrParseArgs ->
        liftOptions
          fst
          setFst
          ( filter
              ( (`notElem` ["constraint", "dependency", "promised-dependency", "exact-configuration"])
                  . optionName
              )
              $ configureOptions showOrParseArgs
          )
          ++ liftOptions
            snd
            setSnd
            (configureExOptions showOrParseArgs ConstraintSourceCommandlineFlag)
    }
  where
    setFst a (_, b) = (a, b)
    setSnd b (a, _) = (a, b)

configureExOptions
  :: ShowOrParseArgs
  -> ConstraintSource
  -> [OptionField ConfigExFlags]
configureExOptions _showOrParseArgs src =
  [ option
      []
      ["cabal-lib-version"]
      ( "Select which version of the Cabal lib to use to build packages "
          ++ "(useful for testing)."
      )
      configCabalVersion
      (\v flags -> flags{configCabalVersion = v})
      ( reqArg
          "VERSION"
          ( parsecToReadE
              ("Cannot parse cabal lib version: " ++)
              (fmap toFlag parsec)
          )
          (map prettyShow . flagToList)
      )
  , option
      ""
      ["append"]
      "appending the new config to the old config file"
      configAppend
      (\v flags -> flags{configAppend = v})
      (boolOpt [] [])
  , option
      ""
      ["backup"]
      "the backup of the config file before any alterations"
      configBackup
      (\v flags -> flags{configBackup = v})
      (boolOpt [] [])
  , option
      "c"
      ["constraint"]
      "Specify constraints on a package (version, installed/source, flags)"
      configExConstraints
      (\v flags -> flags{configExConstraints = v})
      ( reqArg
          "CONSTRAINT"
          ((\x -> [(x, src)]) `fmap` ReadE readUserConstraint)
          (map $ prettyShow . fst)
      )
  , option
      []
      ["preference"]
      "Specify preferences (soft constraints) on the version of a package"
      configPreferences
      (\v flags -> flags{configPreferences = v})
      ( reqArg
          "CONSTRAINT"
          ( parsecToReadE
              (const "dependency expected")
              (fmap (\x -> [x]) parsec)
          )
          (map prettyShow)
      )
  , optionSolver configSolver (\v flags -> flags{configSolver = v})
  , option
      []
      ["allow-older"]
      ("Ignore lower bounds in all dependencies or DEPS")
      (fmap unAllowOlder . configAllowOlder)
      (\v flags -> flags{configAllowOlder = fmap AllowOlder v})
      ( optArg
          "DEPS"
          (parsecToReadEErr unexpectMsgString relaxDepsParser)
          (show RelaxDepsAll, Just RelaxDepsAll)
          relaxDepsPrinter
      )
  , option
      []
      ["allow-newer"]
      ("Ignore upper bounds in all dependencies or DEPS")
      (fmap unAllowNewer . configAllowNewer)
      (\v flags -> flags{configAllowNewer = fmap AllowNewer v})
      ( optArg
          "DEPS"
          (parsecToReadEErr unexpectMsgString relaxDepsParser)
          (show RelaxDepsAll, Just RelaxDepsAll)
          relaxDepsPrinter
      )
  , option
      []
      ["write-ghc-environment-files"]
      ( "Whether to create a .ghc.environment file after a successful build"
          ++ " (v2-build only)"
      )
      configWriteGhcEnvironmentFilesPolicy
      (\v flags -> flags{configWriteGhcEnvironmentFilesPolicy = v})
      ( reqArg
          "always|never|ghc8.4.4+"
          writeGhcEnvironmentFilesPolicyParser
          writeGhcEnvironmentFilesPolicyPrinter
      )
  ]

writeGhcEnvironmentFilesPolicyParser :: ReadE (Flag WriteGhcEnvironmentFilesPolicy)
writeGhcEnvironmentFilesPolicyParser = ReadE $ \case
  "always" -> Right $ Flag AlwaysWriteGhcEnvironmentFiles
  "never" -> Right $ Flag NeverWriteGhcEnvironmentFiles
  "ghc8.4.4+" -> Right $ Flag WriteGhcEnvironmentFilesOnlyForGhc844AndNewer
  policy ->
    Left $
      "Cannot parse the GHC environment file write policy '"
        <> policy
        <> "'"

writeGhcEnvironmentFilesPolicyPrinter
  :: Flag WriteGhcEnvironmentFilesPolicy -> [String]
writeGhcEnvironmentFilesPolicyPrinter = \case
  (Flag AlwaysWriteGhcEnvironmentFiles) -> ["always"]
  (Flag NeverWriteGhcEnvironmentFiles) -> ["never"]
  (Flag WriteGhcEnvironmentFilesOnlyForGhc844AndNewer) -> ["ghc8.4.4+"]
  NoFlag -> []

relaxDepsParser :: CabalParsing m => m (Maybe RelaxDeps)
relaxDepsParser = do
  rs <- P.sepBy parsec (P.char ',')
  if null rs
    then
      fail $
        "empty argument list is not allowed. "
          ++ "Note: use --allow-newer without the equals sign to permit all "
          ++ "packages to use newer versions."
    else return . Just . RelaxDepsSome . toList $ rs

relaxDepsPrinter :: (Maybe RelaxDeps) -> [Maybe String]
relaxDepsPrinter Nothing = []
relaxDepsPrinter (Just RelaxDepsAll) = [Nothing]
relaxDepsPrinter (Just (RelaxDepsSome pkgs)) = map (Just . prettyShow) $ pkgs

instance Monoid ConfigExFlags where
  mempty = gmempty
  mappend = (<>)

instance Semigroup ConfigExFlags where
  (<>) = gmappend

reconfigureCommand :: CommandUI (ConfigFlags, ConfigExFlags)
reconfigureCommand =
  configureExCommand
    { commandName = "reconfigure"
    , commandSynopsis = "Reconfigure the package if necessary."
    , commandDescription = Just $ \pname ->
        wrapText $
          "Run `configure` with the most recently used flags, or append FLAGS "
            ++ "to the most recently used configuration. "
            ++ "Accepts the same flags as `"
            ++ pname
            ++ " v1-configure'. "
            ++ "If the package has never been configured, the default flags are "
            ++ "used."
    , commandNotes = Just $ \pname ->
        "Examples:\n"
          ++ "  "
          ++ pname
          ++ " v1-reconfigure\n"
          ++ "    Configure with the most recently used flags.\n"
          ++ "  "
          ++ pname
          ++ " v1-reconfigure -w PATH\n"
          ++ "    Reconfigure with the most recently used flags,\n"
          ++ "    but use the compiler at PATH.\n\n"
    , commandUsage = usageAlternatives "v1-reconfigure" ["[FLAGS]"]
    , commandDefaultFlags = mempty
    }

-- ------------------------------------------------------------

-- * Build flags

-- ------------------------------------------------------------

buildCommand :: CommandUI BuildFlags
buildCommand =
  parent
    { commandName = "build"
    , commandDescription = Just $ \_ ->
        wrapText $
          "Components encompass executables, tests, and benchmarks.\n"
            ++ "\n"
            ++ "Affected by configuration options, see `v1-configure`.\n"
    , commandDefaultFlags = commandDefaultFlags parent
    , commandUsage =
        usageAlternatives "v1-build" $
          ["[FLAGS]", "COMPONENTS [FLAGS]"]
    , commandOptions = commandOptions parent
    , commandNotes = Just $ \pname ->
        "Examples:\n"
          ++ "  "
          ++ pname
          ++ " v1-build           "
          ++ "    All the components in the package\n"
          ++ "  "
          ++ pname
          ++ " v1-build foo       "
          ++ "    A component (i.e. lib, exe, test suite)\n\n"
          ++ Cabal.programFlagsDescription defaultProgramDb
    }
  where
    parent = Cabal.buildCommand defaultProgramDb

-- ------------------------------------------------------------

-- * Test flags

-- ------------------------------------------------------------

-- | Given some 'TestFlags' for the version of Cabal that
-- cabal-install was built with, and a target older 'Version' of
-- Cabal that we want to pass these flags to, convert the
-- flags into a form that will be accepted by the older
-- Setup script.  Generally speaking, this just means filtering
-- out flags that the old Cabal library doesn't understand, but
-- in some cases it may also mean "emulating" a feature using
-- some more legacy flags.
filterTestFlags :: TestFlags -> Version -> TestFlags
filterTestFlags flags cabalLibVersion =
  let flags' = filterTestFlags' flags cabalLibVersion
   in flags'
        { testCommonFlags =
            filterCommonFlags (testCommonFlags flags') cabalLibVersion
        }

filterTestFlags' :: TestFlags -> Version -> TestFlags
filterTestFlags' flags cabalLibVersion
  -- NB: we expect the latest version to be the most common case,
  -- so test it first.
  | cabalLibVersion >= mkVersion [3, 0, 0] = flags_latest
  -- The naming convention is that flags_version gives flags with
  -- all flags *introduced* in version eliminated.
  -- It is NOT the latest version of Cabal library that
  -- these flags work for; version of introduction is a more
  -- natural metric.
  | cabalLibVersion < mkVersion [3, 0, 0] = flags_3_0_0
  | otherwise = error "the impossible just happened" -- see first guard
  where
    flags_latest = flags
    flags_3_0_0 =
      flags_latest
        { -- Cabal < 3.0 doesn't know about --test-wrapper
          Cabal.testWrapper = NoFlag
        }

-- ------------------------------------------------------------

-- * Repl command

-- ------------------------------------------------------------

replCommand :: CommandUI ReplFlags
replCommand =
  parent
    { commandName = "repl"
    , commandDescription = Just $ \pname ->
        wrapText $
          "If the current directory contains no package, ignores COMPONENT "
            ++ "parameters and opens an interactive interpreter session;\n"
            ++ "\n"
            ++ "Otherwise, (re)configures with the given or default flags, and "
            ++ "loads the interpreter with the relevant modules. For executables, "
            ++ "tests and benchmarks, loads the main module (and its "
            ++ "dependencies); for libraries all exposed/other modules.\n"
            ++ "\n"
            ++ "The default component is the library itself, or the executable "
            ++ "if that is the only component.\n"
            ++ "\n"
            ++ "Support for loading specific modules is planned but not "
            ++ "implemented yet. For certain scenarios, `"
            ++ pname
            ++ " v1-exec -- ghci :l Foo` may be used instead. Note that `v1-exec` will "
            ++ "not (re)configure and you will have to specify the location of "
            ++ "other modules, if required.\n"
    , commandUsage = \pname -> "Usage: " ++ pname ++ " v1-repl [COMPONENT] [FLAGS]\n"
    , commandDefaultFlags = commandDefaultFlags parent
    , commandOptions = commandOptions parent
    , commandNotes = Just $ \pname ->
        "Examples:\n"
          ++ "  "
          ++ pname
          ++ " v1-repl           "
          ++ "    The first component in the package\n"
          ++ "  "
          ++ pname
          ++ " v1-repl foo       "
          ++ "    A named component (i.e. lib, exe, test suite)\n"
          ++ "  "
          ++ pname
          ++ " v1-repl --ghc-options=\"-lstdc++\""
          ++ "  Specifying flags for interpreter\n"
    }
  where
    parent = Cabal.replCommand defaultProgramDb

-- ------------------------------------------------------------

-- * Test command

-- ------------------------------------------------------------

testCommand :: CommandUI (BuildFlags, TestFlags)
testCommand =
  parent
    { commandName = "test"
    , commandDescription = Just $ \pname ->
        wrapText $
          "If necessary (re)configures with `--enable-tests` flag and builds"
            ++ " the test suite.\n"
            ++ "\n"
            ++ "Remember that the tests' dependencies must be installed if there"
            ++ " are additional ones; e.g. with `"
            ++ pname
            ++ " v1-install --only-dependencies --enable-tests`.\n"
            ++ "\n"
            ++ "By defining UserHooks in a custom Setup.hs, the package can"
            ++ " define actions to be executed before and after running tests.\n"
    , commandUsage =
        usageAlternatives
          "v1-test"
          ["[FLAGS]", "TESTCOMPONENTS [FLAGS]"]
    , commandDefaultFlags = (Cabal.defaultBuildFlags, commandDefaultFlags parent)
    , commandOptions =
        \showOrParseArgs ->
          liftOptions
            get1
            set1
            (Cabal.buildOptions progDb showOrParseArgs)
            ++ liftOptions
              get2
              set2
              (commandOptions parent showOrParseArgs)
    }
  where
    get1 (a, _) = a
    set1 a (_, b) = (a, b)
    get2 (_, b) = b
    set2 b (a, _) = (a, b)

    parent = Cabal.testCommand
    progDb = defaultProgramDb

-- ------------------------------------------------------------

-- * Bench command

-- ------------------------------------------------------------

benchmarkCommand :: CommandUI (BuildFlags, BenchmarkFlags)
benchmarkCommand =
  parent
    { commandName = "bench"
    , commandUsage =
        usageAlternatives
          "v1-bench"
          ["[FLAGS]", "BENCHCOMPONENTS [FLAGS]"]
    , commandDescription = Just $ \pname ->
        wrapText $
          "If necessary (re)configures with `--enable-benchmarks` flag and"
            ++ " builds the benchmarks.\n"
            ++ "\n"
            ++ "Remember that the benchmarks' dependencies must be installed if"
            ++ " there are additional ones; e.g. with `"
            ++ pname
            ++ " v1-install --only-dependencies --enable-benchmarks`.\n"
            ++ "\n"
            ++ "By defining UserHooks in a custom Setup.hs, the package can"
            ++ " define actions to be executed before and after running"
            ++ " benchmarks.\n"
    , commandDefaultFlags = (Cabal.defaultBuildFlags, commandDefaultFlags parent)
    , commandOptions =
        \showOrParseArgs ->
          liftOptions
            get1
            set1
            (Cabal.buildOptions progDb showOrParseArgs)
            ++ liftOptions
              get2
              set2
              (commandOptions parent showOrParseArgs)
    }
  where
    get1 (a, _) = a
    set1 a (_, b) = (a, b)
    get2 (_, b) = b
    set2 b (a, _) = (a, b)

    parent = Cabal.benchmarkCommand
    progDb = defaultProgramDb

-- ------------------------------------------------------------

-- * Fetch command

-- ------------------------------------------------------------

data FetchFlags = FetchFlags
  { --    fetchOutput    :: Flag FilePath,
    fetchDeps :: Flag Bool
  , fetchDryRun :: Flag Bool
  , fetchSolver :: Flag PreSolver
  , fetchMaxBackjumps :: Flag Int
  , fetchReorderGoals :: Flag ReorderGoals
  , fetchCountConflicts :: Flag CountConflicts
  , fetchFineGrainedConflicts :: Flag FineGrainedConflicts
  , fetchMinimizeConflictSet :: Flag MinimizeConflictSet
  , fetchIndependentGoals :: Flag IndependentGoals
  , fetchPreferOldest :: Flag PreferOldest
  , fetchShadowPkgs :: Flag ShadowPkgs
  , fetchStrongFlags :: Flag StrongFlags
  , fetchAllowBootLibInstalls :: Flag AllowBootLibInstalls
  , fetchOnlyConstrained :: Flag OnlyConstrained
  , fetchTests :: Flag Bool
  , fetchBenchmarks :: Flag Bool
  , fetchVerbosity :: Flag Verbosity
  }

defaultFetchFlags :: FetchFlags
defaultFetchFlags =
  FetchFlags
    { --  fetchOutput    = mempty,
      fetchDeps = toFlag True
    , fetchDryRun = toFlag False
    , fetchSolver = Flag defaultSolver
    , fetchMaxBackjumps = Flag defaultMaxBackjumps
    , fetchReorderGoals = Flag (ReorderGoals False)
    , fetchCountConflicts = Flag (CountConflicts True)
    , fetchFineGrainedConflicts = Flag (FineGrainedConflicts True)
    , fetchMinimizeConflictSet = Flag (MinimizeConflictSet False)
    , fetchIndependentGoals = Flag (IndependentGoals False)
    , fetchPreferOldest = Flag (PreferOldest False)
    , fetchShadowPkgs = Flag (ShadowPkgs False)
    , fetchStrongFlags = Flag (StrongFlags False)
    , fetchAllowBootLibInstalls = Flag (AllowBootLibInstalls False)
    , fetchOnlyConstrained = Flag OnlyConstrainedNone
    , fetchTests = toFlag False
    , fetchBenchmarks = toFlag False
    , fetchVerbosity = toFlag normal
    }

fetchCommand :: CommandUI FetchFlags
fetchCommand =
  CommandUI
    { commandName = "fetch"
    , commandSynopsis = "Downloads packages for later installation."
    , commandUsage =
        usageAlternatives
          "fetch"
          [ "[FLAGS] PACKAGES"
          ]
    , commandDescription = Just $ \_ ->
        "Note that it currently is not possible to fetch the dependencies for a\n"
          ++ "package in the current directory.\n"
    , commandNotes = Nothing
    , commandDefaultFlags = defaultFetchFlags
    , commandOptions = \showOrParseArgs ->
        [ optionVerbosity fetchVerbosity (\v flags -> flags{fetchVerbosity = v})
        , --     , option "o" ["output"]
          --         "Put the package(s) somewhere specific rather than the usual cache."
          --         fetchOutput (\v flags -> flags { fetchOutput = v })
          --         (reqArgFlag "PATH")

          option
            []
            ["dependencies", "deps"]
            "Resolve and fetch dependencies (default)"
            fetchDeps
            (\v flags -> flags{fetchDeps = v})
            trueArg
        , option
            []
            ["no-dependencies", "no-deps"]
            "Ignore dependencies"
            fetchDeps
            (\v flags -> flags{fetchDeps = v})
            falseArg
        , option
            []
            ["dry-run"]
            "Do not install anything, only print what would be installed."
            fetchDryRun
            (\v flags -> flags{fetchDryRun = v})
            trueArg
        , option
            ""
            ["tests"]
            "dependency checking and compilation for test suites listed in the package description file."
            fetchTests
            (\v flags -> flags{fetchTests = v})
            (boolOpt [] [])
        , option
            ""
            ["benchmarks"]
            "dependency checking and compilation for benchmarks listed in the package description file."
            fetchBenchmarks
            (\v flags -> flags{fetchBenchmarks = v})
            (boolOpt [] [])
        ]
          ++ optionSolver fetchSolver (\v flags -> flags{fetchSolver = v})
          : optionSolverFlags
            showOrParseArgs
            fetchMaxBackjumps
            (\v flags -> flags{fetchMaxBackjumps = v})
            fetchReorderGoals
            (\v flags -> flags{fetchReorderGoals = v})
            fetchCountConflicts
            (\v flags -> flags{fetchCountConflicts = v})
            fetchFineGrainedConflicts
            (\v flags -> flags{fetchFineGrainedConflicts = v})
            fetchMinimizeConflictSet
            (\v flags -> flags{fetchMinimizeConflictSet = v})
            fetchIndependentGoals
            (\v flags -> flags{fetchIndependentGoals = v})
            fetchPreferOldest
            (\v flags -> flags{fetchPreferOldest = v})
            fetchShadowPkgs
            (\v flags -> flags{fetchShadowPkgs = v})
            fetchStrongFlags
            (\v flags -> flags{fetchStrongFlags = v})
            fetchAllowBootLibInstalls
            (\v flags -> flags{fetchAllowBootLibInstalls = v})
            fetchOnlyConstrained
            (\v flags -> flags{fetchOnlyConstrained = v})
    }

-- ------------------------------------------------------------

-- * Freeze command

-- ------------------------------------------------------------

data FreezeFlags = FreezeFlags
  { freezeDryRun :: Flag Bool
  , freezeTests :: Flag Bool
  , freezeBenchmarks :: Flag Bool
  , freezeSolver :: Flag PreSolver
  , freezeMaxBackjumps :: Flag Int
  , freezeReorderGoals :: Flag ReorderGoals
  , freezeCountConflicts :: Flag CountConflicts
  , freezeFineGrainedConflicts :: Flag FineGrainedConflicts
  , freezeMinimizeConflictSet :: Flag MinimizeConflictSet
  , freezeIndependentGoals :: Flag IndependentGoals
  , freezePreferOldest :: Flag PreferOldest
  , freezeShadowPkgs :: Flag ShadowPkgs
  , freezeStrongFlags :: Flag StrongFlags
  , freezeAllowBootLibInstalls :: Flag AllowBootLibInstalls
  , freezeOnlyConstrained :: Flag OnlyConstrained
  , freezeVerbosity :: Flag Verbosity
  }

defaultFreezeFlags :: FreezeFlags
defaultFreezeFlags =
  FreezeFlags
    { freezeDryRun = toFlag False
    , freezeTests = toFlag False
    , freezeBenchmarks = toFlag False
    , freezeSolver = Flag defaultSolver
    , freezeMaxBackjumps = Flag defaultMaxBackjumps
    , freezeReorderGoals = Flag (ReorderGoals False)
    , freezeCountConflicts = Flag (CountConflicts True)
    , freezeFineGrainedConflicts = Flag (FineGrainedConflicts True)
    , freezeMinimizeConflictSet = Flag (MinimizeConflictSet False)
    , freezeIndependentGoals = Flag (IndependentGoals False)
    , freezePreferOldest = Flag (PreferOldest False)
    , freezeShadowPkgs = Flag (ShadowPkgs False)
    , freezeStrongFlags = Flag (StrongFlags False)
    , freezeAllowBootLibInstalls = Flag (AllowBootLibInstalls False)
    , freezeOnlyConstrained = Flag OnlyConstrainedNone
    , freezeVerbosity = toFlag normal
    }

freezeCommand :: CommandUI FreezeFlags
freezeCommand =
  CommandUI
    { commandName = "freeze"
    , commandSynopsis = "Freeze dependencies."
    , commandDescription = Just $ \_ ->
        wrapText $
          "Calculates a valid set of dependencies and their exact versions. "
            ++ "If successful, saves the result to the file `cabal.config`.\n"
            ++ "\n"
            ++ "The package versions specified in `cabal.config` will be used for "
            ++ "any future installs.\n"
            ++ "\n"
            ++ "An existing `cabal.config` is ignored and overwritten.\n"
    , commandNotes = Nothing
    , commandUsage = usageFlags "freeze"
    , commandDefaultFlags = defaultFreezeFlags
    , commandOptions = \showOrParseArgs ->
        [ optionVerbosity
            freezeVerbosity
            (\v flags -> flags{freezeVerbosity = v})
        , option
            []
            ["dry-run"]
            "Do not freeze anything, only print what would be frozen"
            freezeDryRun
            (\v flags -> flags{freezeDryRun = v})
            trueArg
        , option
            []
            ["tests"]
            ( "freezing of the dependencies of any tests suites "
                ++ "in the package description file."
            )
            freezeTests
            (\v flags -> flags{freezeTests = v})
            (boolOpt [] [])
        , option
            []
            ["benchmarks"]
            ( "freezing of the dependencies of any benchmarks suites "
                ++ "in the package description file."
            )
            freezeBenchmarks
            (\v flags -> flags{freezeBenchmarks = v})
            (boolOpt [] [])
        ]
          ++ optionSolver
            freezeSolver
            (\v flags -> flags{freezeSolver = v})
          : optionSolverFlags
            showOrParseArgs
            freezeMaxBackjumps
            (\v flags -> flags{freezeMaxBackjumps = v})
            freezeReorderGoals
            (\v flags -> flags{freezeReorderGoals = v})
            freezeCountConflicts
            (\v flags -> flags{freezeCountConflicts = v})
            freezeFineGrainedConflicts
            (\v flags -> flags{freezeFineGrainedConflicts = v})
            freezeMinimizeConflictSet
            (\v flags -> flags{freezeMinimizeConflictSet = v})
            freezeIndependentGoals
            (\v flags -> flags{freezeIndependentGoals = v})
            freezePreferOldest
            (\v flags -> flags{freezePreferOldest = v})
            freezeShadowPkgs
            (\v flags -> flags{freezeShadowPkgs = v})
            freezeStrongFlags
            (\v flags -> flags{freezeStrongFlags = v})
            freezeAllowBootLibInstalls
            (\v flags -> flags{freezeAllowBootLibInstalls = v})
            freezeOnlyConstrained
            (\v flags -> flags{freezeOnlyConstrained = v})
    }

-- ------------------------------------------------------------

-- * 'gen-bounds' command

-- ------------------------------------------------------------

genBoundsCommand :: CommandUI FreezeFlags
genBoundsCommand =
  CommandUI
    { commandName = "gen-bounds"
    , commandSynopsis = "Generate dependency bounds."
    , commandDescription = Just $ \_ ->
        wrapText $
          "Generates bounds for all dependencies that do not currently have them. "
            ++ "Generated bounds are printed to stdout.  "
            ++ "You can then paste them into your .cabal file.\n"
            ++ "\n"
    , commandNotes = Nothing
    , commandUsage = usageFlags "gen-bounds"
    , commandDefaultFlags = defaultFreezeFlags
    , commandOptions = \_ ->
        [ optionVerbosity freezeVerbosity (\v flags -> flags{freezeVerbosity = v})
        ]
    }

-- ------------------------------------------------------------
-- Check command
-- ------------------------------------------------------------

data CheckFlags = CheckFlags
  { checkVerbosity :: Flag Verbosity
  , checkIgnore :: [CheckExplanationIDString]
  }
  deriving (Show, Typeable)

defaultCheckFlags :: CheckFlags
defaultCheckFlags =
  CheckFlags
    { checkVerbosity = Flag normal
    , checkIgnore = []
    }

checkCommand :: CommandUI CheckFlags
checkCommand =
  CommandUI
    { commandName = "check"
    , commandSynopsis = "Check the package for common mistakes."
    , commandDescription = Just $ \_ ->
        wrapText $
          "Expects a .cabal package file in the current directory.\n"
            ++ "\n"
            ++ "Some checks correspond to the requirements to packages on Hackage. "
            ++ "If no `Error` is reported, Hackage should accept the "
            ++ "package. If errors are present, `check` exits with 1 and Hackage "
            ++ "will refuse the package.\n"
    , commandNotes = Nothing
    , commandUsage = usageFlags "check"
    , commandDefaultFlags = defaultCheckFlags
    , commandOptions = checkOptions'
    }

checkOptions' :: ShowOrParseArgs -> [OptionField CheckFlags]
checkOptions' _showOrParseArgs =
  [ optionVerbosity
      checkVerbosity
      (\v flags -> flags{checkVerbosity = v})
  , option
      ['i']
      ["ignore"]
      "ignore a specific warning (e.g. --ignore=missing-upper-bounds)"
      checkIgnore
      (\v c -> c{checkIgnore = v ++ checkIgnore c})
      (reqArg' "WARNING" (: []) (const []))
  ]

-- ------------------------------------------------------------

-- * Update command

-- ------------------------------------------------------------

data UpdateFlags = UpdateFlags
  { updateVerbosity :: Flag Verbosity
  , updateIndexState :: Flag TotalIndexState
  }
  deriving (Generic)

defaultUpdateFlags :: UpdateFlags
defaultUpdateFlags =
  UpdateFlags
    { updateVerbosity = toFlag normal
    , updateIndexState = toFlag headTotalIndexState
    }

-- ------------------------------------------------------------

-- * Other commands

-- ------------------------------------------------------------

cleanCommand :: CommandUI CleanFlags
cleanCommand =
  Cabal.cleanCommand
    { commandUsage = \pname ->
        "Usage: " ++ pname ++ " v1-clean [FLAGS]\n"
    }

formatCommand :: CommandUI (Flag Verbosity)
formatCommand =
  CommandUI
    { commandName = "format"
    , commandSynopsis = "Reformat the .cabal file using the standard style."
    , commandDescription = Nothing
    , commandNotes = Nothing
    , commandUsage = usageAlternatives "format" ["[FILE]"]
    , commandDefaultFlags = toFlag normal
    , commandOptions = \_ -> []
    }

manpageCommand :: CommandUI ManpageFlags
manpageCommand =
  CommandUI
    { commandName = "man"
    , commandSynopsis = "Outputs manpage source."
    , commandDescription = Just $ \_ ->
        "Output manpage source to STDOUT.\n"
    , commandNotes = Nothing
    , commandUsage = usageFlags "man"
    , commandDefaultFlags = defaultManpageFlags
    , commandOptions = manpageOptions
    }

runCommand :: CommandUI BuildFlags
runCommand =
  CommandUI
    { commandName = "run"
    , commandSynopsis = "Builds and runs an executable."
    , commandDescription = Just $ \pname ->
        wrapText $
          "Builds and then runs the specified executable. If no executable is "
            ++ "specified, but the package contains just one executable, that one "
            ++ "is built and executed.\n"
            ++ "\n"
            ++ "Use `"
            ++ pname
            ++ " v1-test --show-details=streaming` to run a "
            ++ "test-suite and get its full output.\n"
    , commandNotes = Just $ \pname ->
        "Examples:\n"
          ++ "  "
          ++ pname
          ++ " v1-run\n"
          ++ "    Run the only executable in the current package;\n"
          ++ "  "
          ++ pname
          ++ " v1-run foo -- --fooflag\n"
          ++ "    Works similar to `./foo --fooflag`.\n"
    , commandUsage =
        usageAlternatives
          "v1-run"
          ["[FLAGS] [EXECUTABLE] [-- EXECUTABLE_FLAGS]"]
    , commandDefaultFlags = mempty
    , commandOptions = commandOptions parent
    }
  where
    parent = Cabal.buildCommand defaultProgramDb

-- ------------------------------------------------------------

-- * Report flags

-- ------------------------------------------------------------

data ReportFlags = ReportFlags
  { reportToken :: Flag Token
  , reportUsername :: Flag Username
  , reportPassword :: Flag Password
  , reportVerbosity :: Flag Verbosity
  }
  deriving (Generic)

defaultReportFlags :: ReportFlags
defaultReportFlags =
  ReportFlags
    { reportToken = mempty
    , reportUsername = mempty
    , reportPassword = mempty
    , reportVerbosity = toFlag normal
    }

reportCommand :: CommandUI ReportFlags
reportCommand =
  CommandUI
    { commandName = "report"
    , commandSynopsis = "Upload build reports to a remote server."
    , commandDescription = Nothing
    , commandNotes = Just $ \_ ->
        "You can store your Hackage login in the ~/.config/cabal/config file\n"
          ++ "(the %APPDATA%\\cabal\\config file on Windows)\n"
    , commandUsage = usageAlternatives "report" ["[FLAGS]"]
    , commandDefaultFlags = defaultReportFlags
    , commandOptions = \_ ->
        [ optionVerbosity reportVerbosity (\v flags -> flags{reportVerbosity = v})
        , option
            ['t']
            ["token"]
            "Hackage authentication Token."
            reportToken
            (\v flags -> flags{reportToken = v})
            ( reqArg'
                "TOKEN"
                (toFlag . Token)
                (flagToList . fmap unToken)
            )
        , option
            ['u']
            ["username"]
            "Hackage username."
            reportUsername
            (\v flags -> flags{reportUsername = v})
            ( reqArg'
                "USERNAME"
                (toFlag . Username)
                (flagToList . fmap unUsername)
            )
        , option
            ['p']
            ["password"]
            "Hackage password."
            reportPassword
            (\v flags -> flags{reportPassword = v})
            ( reqArg'
                "PASSWORD"
                (toFlag . Password)
                (flagToList . fmap unPassword)
            )
        ]
    }

instance Monoid ReportFlags where
  mempty = gmempty
  mappend = (<>)

instance Semigroup ReportFlags where
  (<>) = gmappend

-- ------------------------------------------------------------

-- * Get flags

-- ------------------------------------------------------------

data GetFlags = GetFlags
  { getDestDir :: Flag FilePath
  , getOnlyPkgDescr :: Flag Bool
  , getPristine :: Flag Bool
  , getIndexState :: Flag TotalIndexState
  , getActiveRepos :: Flag ActiveRepos
  , getSourceRepository :: Flag (Maybe RepoKind)
  , getVerbosity :: Flag Verbosity
  }
  deriving (Generic)

defaultGetFlags :: GetFlags
defaultGetFlags =
  GetFlags
    { getDestDir = mempty
    , getOnlyPkgDescr = mempty
    , getPristine = mempty
    , getIndexState = mempty
    , getActiveRepos = mempty
    , getSourceRepository = mempty
    , getVerbosity = toFlag normal
    }

getCommand :: CommandUI GetFlags
getCommand =
  CommandUI
    { commandName = "get"
    , commandSynopsis = "Download/Extract a package's source code (repository)."
    , commandDescription = Just $ \_ -> wrapText $ unlines descriptionOfGetCommand
    , commandNotes = Just $ \pname -> unlines $ notesOfGetCommand "get" pname
    , commandUsage = usagePackages "get"
    , commandDefaultFlags = defaultGetFlags
    , commandOptions = \_ ->
        [ optionVerbosity getVerbosity (\v flags -> flags{getVerbosity = v})
        , option
            "d"
            ["destdir"]
            "Where to place the package source, defaults to the current directory."
            getDestDir
            (\v flags -> flags{getDestDir = v})
            (reqArgFlag "PATH")
        , option
            "s"
            ["source-repository"]
            "Copy the package's source repository (ie git clone, darcs get, etc as appropriate)."
            getSourceRepository
            (\v flags -> flags{getSourceRepository = v})
            ( optArg
                "[head|this|...]"
                ( parsecToReadE
                    (const "invalid source-repository")
                    (fmap (toFlag . Just) parsec)
                )
                ("", Flag Nothing)
                (map (fmap show) . flagToList)
            )
        , option
            []
            ["index-state"]
            ( "Use source package index state as it existed at a previous time. "
                ++ "Accepts unix-timestamps (e.g. '@1474732068'), ISO8601 UTC timestamps "
                ++ "(e.g. '2016-09-24T17:47:48Z'), or 'HEAD' (default: 'HEAD'). "
                ++ "This determines which package versions are available as well as "
                ++ ".cabal file revision is selected (unless --pristine is used)."
            )
            getIndexState
            (\v flags -> flags{getIndexState = v})
            ( reqArg
                "STATE"
                ( parsecToReadE
                    ( const $
                        "index-state must be a  "
                          ++ "unix-timestamps (e.g. '@1474732068'), "
                          ++ "a ISO8601 UTC timestamp "
                          ++ "(e.g. '2016-09-24T17:47:48Z'), or 'HEAD'"
                    )
                    (toFlag `fmap` parsec)
                )
                (flagToList . fmap prettyShow)
            )
        , option
            []
            ["only-package-description"]
            "Unpack only the package description file."
            getOnlyPkgDescr
            (\v flags -> flags{getOnlyPkgDescr = v})
            trueArg
        , option
            []
            ["package-description-only"]
            "A synonym for --only-package-description."
            getOnlyPkgDescr
            (\v flags -> flags{getOnlyPkgDescr = v})
            trueArg
        , option
            []
            ["pristine"]
            ( "Unpack the original pristine tarball, rather than updating the "
                ++ ".cabal file with the latest revision from the package archive."
            )
            getPristine
            (\v flags -> flags{getPristine = v})
            trueArg
        ]
    }

-- | List of lines describing command @get@.
descriptionOfGetCommand :: [String]
descriptionOfGetCommand =
  [ "Creates a local copy of a package's source code. By default it gets the source"
  , "tarball and unpacks it in a local subdirectory. Alternatively, with -s it will"
  , "get the code from the source repository specified by the package."
  ]

-- | Notes for the command @get@.
notesOfGetCommand
  :: String
  -- ^ Either @"get"@ or @"unpack"@.
  -> String
  -- ^ E.g. @"cabal"@.
  -> [String]
  -- ^ List of lines.
notesOfGetCommand cmd pname =
  [ "Examples:"
  , "  " ++ unwords [pname, cmd, "hlint"]
  , "    Download the latest stable version of hlint;"
  , "  " ++ unwords [pname, cmd, "lens --source-repository=head"]
  , "    Download the source repository of lens (i.e. git clone from github)."
  ]

-- 'cabal unpack' is a deprecated alias for 'cabal get'.
unpackCommand :: CommandUI GetFlags
unpackCommand =
  getCommand
    { commandName = "unpack"
    , commandSynopsis = synopsis
    , commandNotes = Just $ \pname ->
        unlines $
          notesOfGetCommand "unpack" pname
    , commandUsage = usagePackages "unpack"
    }
  where
    synopsis = "Deprecated alias for 'get'."

instance Monoid GetFlags where
  mempty = gmempty
  mappend = (<>)

instance Semigroup GetFlags where
  (<>) = gmappend

-- ------------------------------------------------------------

-- * List flags

-- ------------------------------------------------------------

data ListFlags = ListFlags
  { listInstalled :: Flag Bool
  , listSimpleOutput :: Flag Bool
  , listCaseInsensitive :: Flag Bool
  , listVerbosity :: Flag Verbosity
  , listPackageDBs :: [Maybe PackageDB]
  , listHcPath :: Flag FilePath
  }
  deriving (Generic)

defaultListFlags :: ListFlags
defaultListFlags =
  ListFlags
    { listInstalled = Flag False
    , listSimpleOutput = Flag False
    , listCaseInsensitive = Flag True
    , listVerbosity = toFlag normal
    , listPackageDBs = []
    , listHcPath = mempty
    }

listCommand :: CommandUI ListFlags
listCommand =
  CommandUI
    { commandName = "list"
    , commandSynopsis = "List packages matching a search string."
    , commandDescription = Just $ \_ ->
        wrapText $
          "List all packages, or all packages matching one of the search"
            ++ " strings.\n"
            ++ "\n"
            ++ "Use the package database specified with --package-db. "
            ++ "If not specified, use the user package database.\n"
    , commandNotes = Just $ \pname ->
        "Examples:\n"
          ++ "  "
          ++ pname
          ++ " list pandoc\n"
          ++ "    Will find pandoc, pandoc-citeproc, pandoc-lens, ...\n"
    , commandUsage =
        usageAlternatives
          "list"
          [ "[FLAGS]"
          , "[FLAGS] STRINGS"
          ]
    , commandDefaultFlags = defaultListFlags
    , commandOptions = const listOptions
    }

listOptions :: [OptionField ListFlags]
listOptions =
  [ optionVerbosity listVerbosity (\v flags -> flags{listVerbosity = v})
  , option
      []
      ["installed"]
      "Only print installed packages"
      listInstalled
      (\v flags -> flags{listInstalled = v})
      trueArg
  , option
      []
      ["simple-output"]
      "Print in a easy-to-parse format"
      listSimpleOutput
      (\v flags -> flags{listSimpleOutput = v})
      trueArg
  , option
      ['i']
      ["ignore-case"]
      "Ignore case distinctions"
      listCaseInsensitive
      (\v flags -> flags{listCaseInsensitive = v})
      (boolOpt' (['i'], ["ignore-case"]) (['I'], ["strict-case"]))
  , option
      ""
      ["package-db"]
      ( "Append the given package database to the list of package"
          ++ " databases used (to satisfy dependencies and register into)."
          ++ " May be a specific file, 'global' or 'user'. The initial list"
          ++ " is ['global'], ['global', 'user'],"
          ++ " depending on context. Use 'clear' to reset the list to empty."
          ++ " See the user guide for details."
      )
      listPackageDBs
      (\v flags -> flags{listPackageDBs = v})
      (reqArg' "DB" readPackageDbList showPackageDbList)
  , option
      "w"
      ["with-compiler"]
      "give the path to a particular compiler"
      listHcPath
      (\v flags -> flags{listHcPath = v})
      (reqArgFlag "PATH")
  ]

listNeedsCompiler :: ListFlags -> Bool
listNeedsCompiler f =
  flagElim False (const True) (listHcPath f)
    || fromFlagOrDefault False (listInstalled f)

instance Monoid ListFlags where
  mempty = gmempty
  mappend = (<>)

instance Semigroup ListFlags where
  (<>) = gmappend

-- ------------------------------------------------------------

-- * Info flags

-- ------------------------------------------------------------

data InfoFlags = InfoFlags
  { infoVerbosity :: Flag Verbosity
  , infoPackageDBs :: [Maybe PackageDB]
  }
  deriving (Generic)

defaultInfoFlags :: InfoFlags
defaultInfoFlags =
  InfoFlags
    { infoVerbosity = toFlag normal
    , infoPackageDBs = []
    }

infoCommand :: CommandUI InfoFlags
infoCommand =
  CommandUI
    { commandName = "info"
    , commandSynopsis = "Display detailed information about a particular package."
    , commandDescription = Just $ \_ ->
        wrapText $
          "Use the package database specified with --package-db. "
            ++ "If not specified, use the user package database.\n"
    , commandNotes = Nothing
    , commandUsage = usageAlternatives "info" ["[FLAGS] PACKAGES"]
    , commandDefaultFlags = defaultInfoFlags
    , commandOptions = \_ ->
        [ optionVerbosity infoVerbosity (\v flags -> flags{infoVerbosity = v})
        , option
            ""
            ["package-db"]
            ( "Append the given package database to the list of package"
                ++ " databases used (to satisfy dependencies and register into)."
                ++ " May be a specific file, 'global' or 'user'. The initial list"
                ++ " is ['global'], ['global', 'user'],"
                ++ " depending on context. Use 'clear' to reset the list to empty."
                ++ " See the user guide for details."
            )
            infoPackageDBs
            (\v flags -> flags{infoPackageDBs = v})
            (reqArg' "DB" readPackageDbList showPackageDbList)
        ]
    }

instance Monoid InfoFlags where
  mempty = gmempty
  mappend = (<>)

instance Semigroup InfoFlags where
  (<>) = gmappend

-- ------------------------------------------------------------

-- * Install flags

-- ------------------------------------------------------------

-- | Install takes the same flags as configure along with a few extras.
data InstallFlags = InstallFlags
  { installDocumentation :: Flag Bool
  , installHaddockIndex :: Flag PathTemplate
  , installDest :: Flag Cabal.CopyDest
  , installDryRun :: Flag Bool
  , installOnlyDownload :: Flag Bool
  , installMaxBackjumps :: Flag Int
  , installReorderGoals :: Flag ReorderGoals
  , installCountConflicts :: Flag CountConflicts
  , installFineGrainedConflicts :: Flag FineGrainedConflicts
  , installMinimizeConflictSet :: Flag MinimizeConflictSet
  , installIndependentGoals :: Flag IndependentGoals
  , installPreferOldest :: Flag PreferOldest
  , installShadowPkgs :: Flag ShadowPkgs
  , installStrongFlags :: Flag StrongFlags
  , installAllowBootLibInstalls :: Flag AllowBootLibInstalls
  , installOnlyConstrained :: Flag OnlyConstrained
  , installReinstall :: Flag Bool
  , installAvoidReinstalls :: Flag AvoidReinstalls
  , installOverrideReinstall :: Flag Bool
  , installUpgradeDeps :: Flag Bool
  , installOnly :: Flag Bool
  , installOnlyDeps :: Flag Bool
  , installIndexState :: Flag TotalIndexState
  , installRootCmd :: Flag String
  , installSummaryFile :: NubList PathTemplate
  , installLogFile :: Flag PathTemplate
  , installBuildReports :: Flag ReportLevel
  , installReportPlanningFailure :: Flag Bool
  , -- Note: symlink-bindir is no longer used by v2-install and can be removed
    -- when removing v1 commands
    installSymlinkBinDir :: Flag FilePath
  , installPerComponent :: Flag Bool
  , installNumJobs :: Flag (Maybe Int)
  , installUseSemaphore :: Flag Bool
  , installKeepGoing :: Flag Bool
  , installRunTests :: Flag Bool
  , installOfflineMode :: Flag Bool
  }
  deriving (Eq, Show, Generic)

instance Binary InstallFlags

defaultInstallFlags :: InstallFlags
defaultInstallFlags =
  InstallFlags
    { installDocumentation = Flag False
    , installHaddockIndex = Flag docIndexFile
    , installDest = Flag Cabal.NoCopyDest
    , installDryRun = Flag False
    , installOnlyDownload = Flag False
    , installMaxBackjumps = Flag defaultMaxBackjumps
    , installReorderGoals = Flag (ReorderGoals False)
    , installCountConflicts = Flag (CountConflicts True)
    , installFineGrainedConflicts = Flag (FineGrainedConflicts True)
    , installMinimizeConflictSet = Flag (MinimizeConflictSet False)
    , installIndependentGoals = Flag (IndependentGoals False)
    , installPreferOldest = Flag (PreferOldest False)
    , installShadowPkgs = Flag (ShadowPkgs False)
    , installStrongFlags = Flag (StrongFlags False)
    , installAllowBootLibInstalls = Flag (AllowBootLibInstalls False)
    , installOnlyConstrained = Flag OnlyConstrainedNone
    , installReinstall = Flag False
    , installAvoidReinstalls = Flag (AvoidReinstalls False)
    , installOverrideReinstall = Flag False
    , installUpgradeDeps = Flag False
    , installOnly = Flag False
    , installOnlyDeps = Flag False
    , installIndexState = mempty
    , installRootCmd = mempty
    , installSummaryFile = mempty
    , installLogFile = mempty
    , installBuildReports = Flag NoReports
    , installReportPlanningFailure = Flag False
    , installSymlinkBinDir = mempty
    , installPerComponent = Flag True
    , installNumJobs = mempty
    , installUseSemaphore = Flag False
    , installKeepGoing = Flag False
    , installRunTests = mempty
    , installOfflineMode = Flag False
    }
  where
    docIndexFile =
      toPathTemplate
        ( "$datadir"
            </> "doc"
            </> "$arch-$os-$compiler"
            </> "index.html"
        )

defaultMaxBackjumps :: Int
defaultMaxBackjumps = 4000

defaultSolver :: PreSolver
defaultSolver = AlwaysModular

allSolvers :: String
allSolvers = intercalate ", " (map prettyShow ([minBound .. maxBound] :: [PreSolver]))

installCommand
  :: CommandUI
      ( ConfigFlags
      , ConfigExFlags
      , InstallFlags
      , HaddockFlags
      , TestFlags
      , BenchmarkFlags
      )
installCommand =
  CommandUI
    { commandName = "install"
    , commandSynopsis = "Install packages."
    , commandUsage =
        usageAlternatives
          "v1-install"
          [ "[FLAGS]"
          , "[FLAGS] PACKAGES"
          ]
    , commandDescription = Just $ \_ ->
        wrapText $
          "Installs one or more packages. By default, the installed package"
            ++ " will be registered in the user's package database."
            ++ "\n"
            ++ "If PACKAGES are specified, downloads and installs those packages."
            ++ " Otherwise, install the package in the current directory (and/or its"
            ++ " dependencies) (there must be exactly one .cabal file in the current"
            ++ " directory).\n"
            ++ "\n"
            ++ "The flags to `v1-install` are saved and"
            ++ " affect future commands such as `v1-build` and `v1-repl`. See the help for"
            ++ " `v1-configure` for a list of commands being affected.\n"
            ++ "\n"
            ++ "Installed executables will by default"
            ++ " be put into `~/.local/bin/`."
            ++ " If you want installed executable to be available globally, make"
            ++ " sure that the PATH environment variable contains that directory.\n"
            ++ "\n"
    , commandNotes = Just $ \pname ->
        ( case commandNotes $
            Cabal.configureCommand defaultProgramDb of
            Just desc -> desc pname ++ "\n"
            Nothing -> ""
        )
          ++ "Examples:\n"
          ++ "  "
          ++ pname
          ++ " v1-install                 "
          ++ "    Package in the current directory\n"
          ++ "  "
          ++ pname
          ++ " v1-install foo             "
          ++ "    Package from the hackage server\n"
          ++ "  "
          ++ pname
          ++ " v1-install foo-1.0         "
          ++ "    Specific version of a package\n"
          ++ "  "
          ++ pname
          ++ " v1-install 'foo < 2'       "
          ++ "    Constrained package version\n"
          ++ "  "
          ++ pname
          ++ " v1-install haddock --bindir=$HOME/hask-bin/ --datadir=$HOME/hask-data/\n"
          ++ "  "
          ++ (map (const ' ') pname)
          ++ "                         "
          ++ "    Change installation destination\n"
    , commandDefaultFlags = (mempty, mempty, mempty, mempty, mempty, mempty)
    , commandOptions = \showOrParseArgs ->
        liftOptions
          get1
          set1
          -- Note: [Hidden Flags]
          -- hide "constraint", "dependency", "promised-dependency" and
          -- "exact-configuration" from the configure options.
          ( filter
              ( ( `notElem`
                    [ "constraint"
                    , "dependency"
                    , "promised-dependency"
                    , "exact-configuration"
                    ]
                )
                  . optionName
              )
              $ configureOptions showOrParseArgs
          )
          ++ liftOptions get2 set2 (configureExOptions showOrParseArgs ConstraintSourceCommandlineFlag)
          ++ liftOptions
            get3
            set3
            -- hide "target-package-db" flag from the
            -- install options.
            ( filter
                ( (`notElem` ["target-package-db"])
                    . optionName
                )
                $ installOptions showOrParseArgs
            )
          ++ liftOptions get4 set4 (haddockOptions showOrParseArgs)
          ++ liftOptions get5 set5 (testOptions showOrParseArgs)
          ++ liftOptions get6 set6 (benchmarkOptions showOrParseArgs)
    }
  where
    get1 (a, _, _, _, _, _) = a
    set1 a (_, b, c, d, e, f) = (a, b, c, d, e, f)
    get2 (_, b, _, _, _, _) = b
    set2 b (a, _, c, d, e, f) = (a, b, c, d, e, f)
    get3 (_, _, c, _, _, _) = c
    set3 c (a, b, _, d, e, f) = (a, b, c, d, e, f)
    get4 (_, _, _, d, _, _) = d
    set4 d (a, b, c, _, e, f) = (a, b, c, d, e, f)
    get5 (_, _, _, _, e, _) = e
    set5 e (a, b, c, d, _, f) = (a, b, c, d, e, f)
    get6 (_, _, _, _, _, f) = f
    set6 f (a, b, c, d, e, _) = (a, b, c, d, e, f)

haddockCommand :: CommandUI HaddockFlags
haddockCommand =
  Cabal.haddockCommand
    { commandUsage =
        usageAlternatives "v1-haddock" $
          ["[FLAGS]", "COMPONENTS [FLAGS]"]
    }

filterHaddockArgs :: [String] -> Version -> [String]
filterHaddockArgs args cabalLibVersion
  | cabalLibVersion >= mkVersion [2, 3, 0] = args_latest
  | cabalLibVersion < mkVersion [2, 3, 0] = args_2_3_0
  | otherwise = args_latest
  where
    args_latest = args

    -- Cabal < 2.3 doesn't know about per-component haddock
    args_2_3_0 = []

filterHaddockFlags :: HaddockFlags -> Version -> HaddockFlags
filterHaddockFlags flags cabalLibVersion =
  let flags' = filterHaddockFlags' flags cabalLibVersion
   in flags'
        { haddockCommonFlags =
            filterCommonFlags (haddockCommonFlags flags') cabalLibVersion
        }

filterHaddockFlags' :: HaddockFlags -> Version -> HaddockFlags
filterHaddockFlags' flags cabalLibVersion
  | cabalLibVersion >= mkVersion [2, 3, 0] = flags_latest
  | cabalLibVersion < mkVersion [2, 3, 0] = flags_2_3_0
  | otherwise = flags_latest
  where
    flags_latest = flags

    flags_2_3_0 =
      flags_latest
        { -- Cabal < 2.3 doesn't know about per-component haddock
          haddockCommonFlags =
            (haddockCommonFlags flags_latest)
              { setupTargets = []
              }
        }

haddockOptions :: ShowOrParseArgs -> [OptionField HaddockFlags]
haddockOptions showOrParseArgs =
  [ opt
    { optionName = "haddock-" ++ name
    , optionDescr =
        [ fmapOptFlags (\(_, lflags) -> ([], map ("haddock-" ++) lflags)) descr
        | descr <- optionDescr opt
        ]
    }
  | opt <- commandOptions Cabal.haddockCommand showOrParseArgs
  , let name = optionName opt
  , name
      `elem` [ "hoogle"
             , "html"
             , "html-location"
             , "executables"
             , "tests"
             , "benchmarks"
             , "all"
             , "internal"
             , "css"
             , "hyperlink-source"
             , "quickjump"
             , "hscolour-css"
             , "contents-location"
             , "use-index"
             , "for-hackage"
             , "base-url"
             , "lib"
             , "output-dir"
             ]
  ]

testOptions :: ShowOrParseArgs -> [OptionField TestFlags]
testOptions showOrParseArgs =
  [ opt
    { optionName = prefixTest name
    , optionDescr =
        [ fmapOptFlags (\(_, lflags) -> ([], map prefixTest lflags)) descr
        | descr <- optionDescr opt
        ]
    }
  | opt <- commandOptions Cabal.testCommand showOrParseArgs
  , let name = optionName opt
  , name
      `elem` [ "log"
             , "machine-log"
             , "show-details"
             , "keep-tix-files"
             , "fail-when-no-test-suites"
             , "test-options"
             , "test-option"
             , "test-wrapper"
             ]
  ]
  where
    prefixTest name
      | "test-" `isPrefixOf` name = name
      | otherwise = "test-" ++ name

benchmarkOptions :: ShowOrParseArgs -> [OptionField BenchmarkFlags]
benchmarkOptions showOrParseArgs =
  [ opt
    { optionName = prefixBenchmark name
    , optionDescr =
        [ fmapOptFlags (\(_, lflags) -> ([], map prefixBenchmark lflags)) descr
        | descr <- optionDescr opt
        ]
    }
  | opt <- commandOptions Cabal.benchmarkCommand showOrParseArgs
  , let name = optionName opt
  , name `elem` ["benchmark-options", "benchmark-option"]
  ]
  where
    prefixBenchmark name
      | "benchmark-" `isPrefixOf` name = name
      | otherwise = "benchmark-" ++ name

fmapOptFlags :: (OptFlags -> OptFlags) -> OptDescr a -> OptDescr a
fmapOptFlags modify (ReqArg d f p r w) = ReqArg d (modify f) p r w
fmapOptFlags modify (OptArg d f p r i w) = OptArg d (modify f) p r i w
fmapOptFlags modify (ChoiceOpt xs) = ChoiceOpt [(d, modify f, i, w) | (d, f, i, w) <- xs]
fmapOptFlags modify (BoolOpt d f1 f2 r w) = BoolOpt d (modify f1) (modify f2) r w

installOptions :: ShowOrParseArgs -> [OptionField InstallFlags]
installOptions showOrParseArgs =
  [ option
      ""
      ["documentation"]
      "building of documentation"
      installDocumentation
      (\v flags -> flags{installDocumentation = v})
      (boolOpt [] [])
  , option
      []
      ["doc-index-file"]
      "A central index of haddock API documentation (template cannot use $pkgid)"
      installHaddockIndex
      (\v flags -> flags{installHaddockIndex = v})
      ( reqArg'
          "TEMPLATE"
          (toFlag . toPathTemplate)
          (flagToList . fmap fromPathTemplate)
      )
  , option
      []
      ["dry-run"]
      "Do not install anything, only print what would be installed."
      installDryRun
      (\v flags -> flags{installDryRun = v})
      trueArg
  , option
      []
      ["only-download"]
      "Do not build anything, only fetch the packages."
      installOnlyDownload
      (\v flags -> flags{installOnlyDownload = v})
      trueArg
  , option
      ""
      ["target-package-db"]
      "package database to install into. Required when using ${pkgroot} prefix."
      installDest
      (\v flags -> flags{installDest = v})
      ( reqArg
          "DATABASE"
          (succeedReadE (Flag . Cabal.CopyToDb))
          (\f -> case f of Flag (Cabal.CopyToDb p) -> [p]; _ -> [])
      )
  ]
    ++ optionSolverFlags
      showOrParseArgs
      installMaxBackjumps
      (\v flags -> flags{installMaxBackjumps = v})
      installReorderGoals
      (\v flags -> flags{installReorderGoals = v})
      installCountConflicts
      (\v flags -> flags{installCountConflicts = v})
      installFineGrainedConflicts
      (\v flags -> flags{installFineGrainedConflicts = v})
      installMinimizeConflictSet
      (\v flags -> flags{installMinimizeConflictSet = v})
      installIndependentGoals
      (\v flags -> flags{installIndependentGoals = v})
      installPreferOldest
      (\v flags -> flags{installPreferOldest = v})
      installShadowPkgs
      (\v flags -> flags{installShadowPkgs = v})
      installStrongFlags
      (\v flags -> flags{installStrongFlags = v})
      installAllowBootLibInstalls
      (\v flags -> flags{installAllowBootLibInstalls = v})
      installOnlyConstrained
      (\v flags -> flags{installOnlyConstrained = v})
    ++ [ option
          []
          ["reinstall"]
          "Install even if it means installing the same version again."
          installReinstall
          (\v flags -> flags{installReinstall = v})
          (yesNoOpt showOrParseArgs)
       , option
          []
          ["avoid-reinstalls"]
          "Do not select versions that would destructively overwrite installed packages."
          (fmap asBool . installAvoidReinstalls)
          (\v flags -> flags{installAvoidReinstalls = fmap AvoidReinstalls v})
          (yesNoOpt showOrParseArgs)
       , option
          []
          ["force-reinstalls"]
          "Reinstall packages even if they will most likely break other installed packages."
          installOverrideReinstall
          (\v flags -> flags{installOverrideReinstall = v})
          (yesNoOpt showOrParseArgs)
       , option
          []
          ["upgrade-dependencies"]
          "Pick the latest version for all dependencies, rather than trying to pick an installed version."
          installUpgradeDeps
          (\v flags -> flags{installUpgradeDeps = v})
          (yesNoOpt showOrParseArgs)
       , option
          []
          ["only-dependencies"]
          "Install only the dependencies necessary to build the given packages"
          installOnlyDeps
          (\v flags -> flags{installOnlyDeps = v})
          (yesNoOpt showOrParseArgs)
       , option
          []
          ["dependencies-only"]
          "A synonym for --only-dependencies"
          installOnlyDeps
          (\v flags -> flags{installOnlyDeps = v})
          (yesNoOpt showOrParseArgs)
       , option
          []
          ["index-state"]
          ( "Use source package index state as it existed at a previous time. "
              ++ "Accepts unix-timestamps (e.g. '@1474732068'), ISO8601 UTC timestamps "
              ++ "(e.g. '2016-09-24T17:47:48Z'), or 'HEAD' (default: 'HEAD')."
          )
          installIndexState
          (\v flags -> flags{installIndexState = v})
          ( reqArg
              "STATE"
              ( parsecToReadE
                  ( const $
                      "index-state must be a  "
                        ++ "unix-timestamps (e.g. '@1474732068'), "
                        ++ "a ISO8601 UTC timestamp "
                        ++ "(e.g. '2016-09-24T17:47:48Z'), or 'HEAD'"
                  )
                  (toFlag `fmap` parsec)
              )
              (flagToList . fmap prettyShow)
          )
       , option
          []
          ["root-cmd"]
          "(No longer supported, do not use.)"
          installRootCmd
          (\v flags -> flags{installRootCmd = v})
          (reqArg' "COMMAND" toFlag flagToList)
       , option
          []
          ["symlink-bindir"]
          "Add symlinks to installed executables into this directory."
          installSymlinkBinDir
          (\v flags -> flags{installSymlinkBinDir = v})
          (reqArgFlag "DIR")
       , option
          []
          ["build-summary"]
          "Save build summaries to file (name template can use $pkgid, $compiler, $os, $arch)"
          installSummaryFile
          (\v flags -> flags{installSummaryFile = v})
          (reqArg' "TEMPLATE" (\x -> toNubList [toPathTemplate x]) (map fromPathTemplate . fromNubList))
       , option
          []
          ["build-log"]
          "Log all builds to file (name template can use $pkgid, $compiler, $os, $arch)"
          installLogFile
          (\v flags -> flags{installLogFile = v})
          ( reqArg'
              "TEMPLATE"
              (toFlag . toPathTemplate)
              (flagToList . fmap fromPathTemplate)
          )
       , option
          []
          ["remote-build-reporting"]
          "Generate build reports to send to a remote server (none, anonymous or detailed)."
          installBuildReports
          (\v flags -> flags{installBuildReports = v})
          ( reqArg
              "LEVEL"
              ( parsecToReadE
                  ( const $
                      "report level must be 'none', "
                        ++ "'anonymous' or 'detailed'"
                  )
                  (toFlag `fmap` parsec)
              )
              (flagToList . fmap prettyShow)
          )
       , option
          []
          ["report-planning-failure"]
          "Generate build reports when the dependency solver fails. This is used by the Hackage build bot."
          installReportPlanningFailure
          (\v flags -> flags{installReportPlanningFailure = v})
          trueArg
       , option
          ""
          ["per-component"]
          "Per-component builds when possible"
          installPerComponent
          (\v flags -> flags{installPerComponent = v})
          (boolOpt [] [])
       , option
          []
          ["run-tests"]
          "Run package test suites during installation."
          installRunTests
          (\v flags -> flags{installRunTests = v})
          trueArg
       , option
          []
          ["semaphore"]
          "Use a semaphore so GHC can compile components in parallel"
          installUseSemaphore
          (\v flags -> flags{installUseSemaphore = v})
          (yesNoOpt showOrParseArgs)
       , optionNumJobs
          installNumJobs
          (\v flags -> flags{installNumJobs = v})
       , option
          []
          ["keep-going"]
          "After a build failure, continue to build other unaffected packages."
          installKeepGoing
          (\v flags -> flags{installKeepGoing = v})
          trueArg
       , option
          []
          ["offline"]
          "Don't download packages from the Internet."
          installOfflineMode
          (\v flags -> flags{installOfflineMode = v})
          (yesNoOpt showOrParseArgs)
       ]
    ++ case showOrParseArgs of -- TODO: remove when "cabal install"
    -- avoids
      ParseArgs ->
        [ option
            []
            ["only"]
            "Only installs the package in the current directory."
            installOnly
            (\v flags -> flags{installOnly = v})
            trueArg
        ]
      _ -> []

optionNumJobs
  :: (flags -> Flag (Maybe Int))
  -> (Flag (Maybe Int) -> flags -> flags)
  -> OptionField flags
optionNumJobs get set =
  option
    "j"
    ["jobs"]
    "Run NUM jobs simultaneously (or '$ncpus' if no NUM is given)."
    get
    set
    ( optArg
        "NUM"
        (fmap Flag numJobsParser)
        ("", Flag Nothing)
        (map (Just . maybe "$ncpus" show) . flagToList)
    )
  where
    numJobsParser :: ReadE (Maybe Int)
    numJobsParser = ReadE $ \s ->
      case s of
        "$ncpus" -> Right Nothing
        _ -> case reads s of
          [(n, "")]
            | n < 1 -> Left "The number of jobs should be 1 or more."
            | otherwise -> Right (Just n)
          _ -> Left "The jobs value should be a number or '$ncpus'"

instance Monoid InstallFlags where
  mempty = gmempty
  mappend = (<>)

instance Semigroup InstallFlags where
  (<>) = gmappend

-- ------------------------------------------------------------

-- * Upload flags

-- ------------------------------------------------------------

-- | Is this a candidate package or a package to be published?
data IsCandidate = IsCandidate | IsPublished
  deriving (Eq)

data UploadFlags = UploadFlags
  { uploadCandidate :: Flag IsCandidate
  , uploadDoc :: Flag Bool
  , uploadToken :: Flag Token
  , uploadUsername :: Flag Username
  , uploadPassword :: Flag Password
  , uploadPasswordCmd :: Flag [String]
  , uploadVerbosity :: Flag Verbosity
  }
  deriving (Generic)

defaultUploadFlags :: UploadFlags
defaultUploadFlags =
  UploadFlags
    { uploadCandidate = toFlag IsCandidate
    , uploadDoc = toFlag False
    , uploadToken = mempty
    , uploadUsername = mempty
    , uploadPassword = mempty
    , uploadPasswordCmd = mempty
    , uploadVerbosity = toFlag normal
    }

uploadCommand :: CommandUI UploadFlags
uploadCommand =
  CommandUI
    { commandName = "upload"
    , commandSynopsis = "Uploads source packages or documentation to Hackage."
    , commandDescription = Nothing
    , commandNotes = Just $ \_ ->
        "You can store your Hackage login in the ~/.config/cabal/config file\n"
          ++ "(the %APPDATA%\\cabal\\config file on Windows)\n"
          ++ relevantConfigValuesText ["token", "username", "password", "password-command"]
    , commandUsage = \pname ->
        "Usage: " ++ pname ++ " upload [FLAGS] TARFILES\n"
    , commandDefaultFlags = defaultUploadFlags
    , commandOptions = \_ ->
        [ optionVerbosity
            uploadVerbosity
            (\v flags -> flags{uploadVerbosity = v})
        , option
            []
            ["publish"]
            "Publish the package instead of uploading it as a candidate."
            uploadCandidate
            (\v flags -> flags{uploadCandidate = v})
            (noArg (Flag IsPublished))
        , option
            ['d']
            ["documentation"]
            ( "Upload documentation instead of a source package. "
                ++ "By default, this uploads documentation for a package candidate. "
                ++ "To upload documentation for "
                ++ "a published package, combine with --publish."
            )
            uploadDoc
            (\v flags -> flags{uploadDoc = v})
            trueArg
        , option
            ['t']
            ["token"]
            "Hackage authentication token."
            uploadToken
            (\v flags -> flags{uploadToken = v})
            ( reqArg'
                "TOKEN"
                (toFlag . Token)
                (flagToList . fmap unToken)
            )
        , option
            ['u']
            ["username"]
            "Hackage username."
            uploadUsername
            (\v flags -> flags{uploadUsername = v})
            ( reqArg'
                "USERNAME"
                (toFlag . Username)
                (flagToList . fmap unUsername)
            )
        , option
            ['p']
            ["password"]
            "Hackage password."
            uploadPassword
            (\v flags -> flags{uploadPassword = v})
            ( reqArg'
                "PASSWORD"
                (toFlag . Password)
                (flagToList . fmap unPassword)
            )
        , option
            ['P']
            ["password-command"]
            "Command to get Hackage password."
            uploadPasswordCmd
            (\v flags -> flags{uploadPasswordCmd = v})
            ( reqArg
                "COMMAND"
                ( readP_to_E
                    ("Cannot parse command: " ++)
                    (Flag <$> parseSpaceList parseTokenQ)
                )
                (flagElim [] (pure . unwords . fmap show))
            )
        ]
    }

instance Monoid UploadFlags where
  mempty = gmempty
  mappend = (<>)

instance Semigroup UploadFlags where
  (<>) = gmappend

-- ------------------------------------------------------------

-- * Init flags

-- ------------------------------------------------------------

initCommand :: CommandUI IT.InitFlags
initCommand =
  CommandUI
    { commandName = "init"
    , commandSynopsis = "Create a new cabal package."
    , commandDescription = Just $ \_ ->
        wrapText $
          "Create a .cabal, CHANGELOG.md, minimal initial Haskell code and optionally a LICENSE file.\n"
            ++ "\n"
            ++ "Calling init with no arguments runs interactive mode, "
            ++ "which will try to guess as much as possible and prompt you for the rest.\n"
            ++ "Non-interactive mode can be invoked by the -n/--non-interactive flag, "
            ++ "which will let you specify the options via flags and will use the defaults for the rest.\n"
            ++ "It is also possible to call init with a single argument, which denotes the project's desired "
            ++ "root directory.\n"
    , commandNotes = Nothing
    , commandUsage = \pname ->
        "Usage: " ++ pname ++ " init [PROJECT ROOT] [FLAGS]\n"
    , commandDefaultFlags = IT.defaultInitFlags
    , commandOptions = initOptions
    }

initOptions :: ShowOrParseArgs -> [OptionField IT.InitFlags]
initOptions _ =
  [ option
      ['i']
      ["interactive"]
      "interactive mode."
      IT.interactive
      (\v flags -> flags{IT.interactive = v})
      (boolOpt' (['i'], ["interactive"]) (['n'], ["non-interactive"]))
  , option
      ['q']
      ["quiet"]
      "Do not generate log messages to stdout."
      IT.quiet
      (\v flags -> flags{IT.quiet = v})
      trueArg
  , option
      []
      ["no-comments"]
      "Do not generate explanatory comments in the .cabal file."
      IT.noComments
      (\v flags -> flags{IT.noComments = v})
      trueArg
  , option
      ['m']
      ["minimal"]
      "Generate a minimal .cabal file, that is, do not include extra empty fields.  Also implies --no-comments."
      IT.minimal
      (\v flags -> flags{IT.minimal = v})
      trueArg
  , option
      []
      ["overwrite"]
      "Overwrite any existing .cabal, LICENSE, or Setup.hs files without warning."
      IT.overwrite
      (\v flags -> flags{IT.overwrite = v})
      trueArg
  , option
      []
      ["package-dir", "packagedir"]
      "Root directory of the package (default = current directory)."
      IT.packageDir
      (\v flags -> flags{IT.packageDir = v})
      (reqArgFlag "DIRECTORY")
  , option
      ['p']
      ["package-name"]
      "Name of the Cabal package to create."
      IT.packageName
      (\v flags -> flags{IT.packageName = v})
      ( reqArg
          "PACKAGE"
          ( parsecToReadE
              ("Cannot parse package name: " ++)
              (toFlag `fmap` parsec)
          )
          (flagToList . fmap prettyShow)
      )
  , option
      []
      ["version"]
      "Initial version of the package."
      IT.version
      (\v flags -> flags{IT.version = v})
      ( reqArg
          "VERSION"
          ( parsecToReadE
              ("Cannot parse package version: " ++)
              (toFlag `fmap` parsec)
          )
          (flagToList . fmap prettyShow)
      )
  , option
      []
      ["cabal-version"]
      "Version of the Cabal specification."
      IT.cabalVersion
      (\v flags -> flags{IT.cabalVersion = v})
      ( reqArg
          "CABALSPECVERSION"
          ( parsecToReadE
              ("Cannot parse Cabal specification version: " ++)
              (fmap (toFlag . getSpecVersion) parsec)
          )
          (flagToList . fmap (prettyShow . SpecVersion))
      )
  , option
      ['l']
      ["license"]
      "Project license."
      IT.license
      (\v flags -> flags{IT.license = v})
      ( reqArg
          "LICENSE"
          ( parsecToReadE
              ("Cannot parse license: " ++)
              (toFlag `fmap` parsec)
          )
          (flagToList . fmap prettyShow)
      )
  , option
      ['a']
      ["author"]
      "Name of the project's author."
      IT.author
      (\v flags -> flags{IT.author = v})
      (reqArgFlag "NAME")
  , option
      ['e']
      ["email"]
      "Email address of the maintainer."
      IT.email
      (\v flags -> flags{IT.email = v})
      (reqArgFlag "EMAIL")
  , option
      ['u']
      ["homepage"]
      "Project homepage and/or repository."
      IT.homepage
      (\v flags -> flags{IT.homepage = v})
      (reqArgFlag "URL")
  , option
      ['s']
      ["synopsis"]
      "Short project synopsis."
      IT.synopsis
      (\v flags -> flags{IT.synopsis = v})
      (reqArgFlag "TEXT")
  , option
      ['c']
      ["category"]
      "Project category."
      IT.category
      (\v flags -> flags{IT.category = v})
      (reqArgFlag "CATEGORY")
  , option
      ['x']
      ["extra-source-file"]
      "Extra source file to be distributed with tarball."
      IT.extraSrc
      (\v flags -> flags{IT.extraSrc = mergeListFlag (IT.extraSrc flags) v})
      ( reqArg'
          "FILE"
          (Flag . (: []))
          (fromFlagOrDefault [])
      )
  , option
      []
      ["extra-doc-file"]
      "Extra doc file to be distributed with tarball."
      IT.extraDoc
      (\v flags -> flags{IT.extraDoc = mergeListFlag (IT.extraDoc flags) v})
      (reqArg' "FILE" (Flag . (: [])) (fromFlagOrDefault []))
  , option
      []
      ["lib", "is-library"]
      "Build a library."
      IT.packageType
      (\v flags -> flags{IT.packageType = v})
      (noArg (Flag IT.Library))
  , option
      []
      ["exe", "is-executable"]
      "Build an executable."
      IT.packageType
      (\v flags -> flags{IT.packageType = v})
      (noArg (Flag IT.Executable))
  , option
      []
      ["libandexe", "is-libandexe"]
      "Build a library and an executable."
      IT.packageType
      (\v flags -> flags{IT.packageType = v})
      (noArg (Flag IT.LibraryAndExecutable))
  , option
      []
      ["tests"]
      "Generate a test suite, standalone or for a library."
      IT.initializeTestSuite
      (\v flags -> flags{IT.initializeTestSuite = v})
      trueArg
  , option
      []
      ["test-dir"]
      "Directory containing tests."
      IT.testDirs
      ( \v flags ->
          flags{IT.testDirs = mergeListFlag (IT.testDirs flags) v}
      )
      ( reqArg'
          "DIR"
          (Flag . (: []))
          (fromFlagOrDefault [])
      )
  , option
      []
      ["simple"]
      "Create a simple project with sensible defaults."
      IT.simpleProject
      (\v flags -> flags{IT.simpleProject = v})
      trueArg
  , option
      []
      ["main-is"]
      "Specify the main module."
      IT.mainIs
      (\v flags -> flags{IT.mainIs = v})
      (reqArgFlag "FILE")
  , option
      []
      ["language"]
      "Specify the default language."
      IT.language
      (\v flags -> flags{IT.language = v})
      ( reqArg
          "LANGUAGE"
          ( parsecToReadE
              ("Cannot parse language: " ++)
              (toFlag `fmap` parsec)
          )
          (flagToList . fmap prettyShow)
      )
  , option
      ['o']
      ["expose-module"]
      "Export a module from the package."
      IT.exposedModules
      ( \v flags ->
          flags
            { IT.exposedModules =
                mergeListFlag (IT.exposedModules flags) v
            }
      )
      ( reqArg
          "MODULE"
          ( parsecToReadE
              ("Cannot parse module name: " ++)
              (Flag . (: []) <$> parsec)
          )
          (flagElim [] (fmap prettyShow))
      )
  , option
      []
      ["extension"]
      "Use a LANGUAGE extension (in the other-extensions field)."
      IT.otherExts
      ( \v flags ->
          flags
            { IT.otherExts =
                mergeListFlag (IT.otherExts flags) v
            }
      )
      ( reqArg
          "EXTENSION"
          ( parsecToReadE
              ("Cannot parse extension: " ++)
              (Flag . (: []) <$> parsec)
          )
          (flagElim [] (fmap prettyShow))
      )
  , option
      ['d']
      ["dependency"]
      "Package dependencies. Permits comma separated list of dependencies."
      IT.dependencies
      ( \v flags ->
          flags
            { IT.dependencies =
                mergeListFlag (IT.dependencies flags) v
            }
      )
      ( reqArg
          "DEPENDENCIES"
          (fmap Flag dependenciesReadE)
          (fmap prettyShow . fromFlagOrDefault [])
      )
  , option
      []
      ["application-dir"]
      "Directory containing package application executable."
      IT.applicationDirs
      ( \v flags ->
          flags
            { IT.applicationDirs =
                mergeListFlag (IT.applicationDirs flags) v
            }
      )
      ( reqArg'
          "DIR"
          (Flag . (: []))
          (fromFlagOrDefault [])
      )
  , option
      []
      ["source-dir", "sourcedir"]
      "Directory containing package library source."
      IT.sourceDirs
      ( \v flags ->
          flags
            { IT.sourceDirs =
                mergeListFlag (IT.sourceDirs flags) v
            }
      )
      ( reqArg'
          "DIR"
          (Flag . (: []))
          (fromFlagOrDefault [])
      )
  , option
      []
      ["build-tool"]
      "Required external build tool."
      IT.buildTools
      ( \v flags ->
          flags
            { IT.buildTools =
                mergeListFlag (IT.buildTools flags) v
            }
      )
      ( reqArg'
          "TOOL"
          (Flag . (: []))
          (fromFlagOrDefault [])
      )
  , option
      "w"
      ["with-compiler"]
      "give the path to a particular compiler. For 'init', this flag is used \
      \to set the bounds inferred for the 'base' package."
      IT.initHcPath
      (\v flags -> flags{IT.initHcPath = v})
      (reqArgFlag "PATH")
  , optionVerbosity IT.initVerbosity (\v flags -> flags{IT.initVerbosity = v})
  ]
  where
    dependenciesReadE :: ReadE [Dependency]
    dependenciesReadE =
      parsecToReadE
        ("Cannot parse dependencies: " ++)
        (parsecCommaList parsec)

-- ------------------------------------------------------------

-- * Copy and Register

-- ------------------------------------------------------------

copyCommand :: CommandUI CopyFlags
copyCommand =
  Cabal.copyCommand
    { commandNotes = Just $ \pname ->
        "Examples:\n"
          ++ "  "
          ++ pname
          ++ " v1-copy           "
          ++ "    All the components in the package\n"
          ++ "  "
          ++ pname
          ++ " v1-copy foo       "
          ++ "    A component (i.e. lib, exe, test suite)"
    , commandUsage =
        usageAlternatives "v1-copy" $
          [ "[FLAGS]"
          , "COMPONENTS [FLAGS]"
          ]
    }

registerCommand :: CommandUI RegisterFlags
registerCommand =
  Cabal.registerCommand
    { commandUsage = \pname -> "Usage: " ++ pname ++ " v1-register [FLAGS]\n"
    }

-- ------------------------------------------------------------

-- * ActAsSetup flags

-- ------------------------------------------------------------

data ActAsSetupFlags = ActAsSetupFlags
  { actAsSetupBuildType :: Flag BuildType
  }
  deriving (Generic)

defaultActAsSetupFlags :: ActAsSetupFlags
defaultActAsSetupFlags =
  ActAsSetupFlags
    { actAsSetupBuildType = toFlag Simple
    }

actAsSetupCommand :: CommandUI ActAsSetupFlags
actAsSetupCommand =
  CommandUI
    { commandName = "act-as-setup"
    , commandSynopsis = "Run as-if this was a Setup.hs"
    , commandDescription = Nothing
    , commandNotes = Nothing
    , commandUsage = \pname ->
        "Usage: " ++ pname ++ " act-as-setup\n"
    , commandDefaultFlags = defaultActAsSetupFlags
    , commandOptions = \_ ->
        [ option
            ""
            ["build-type"]
            "Use the given build type."
            actAsSetupBuildType
            (\v flags -> flags{actAsSetupBuildType = v})
            ( reqArg
                "BUILD-TYPE"
                ( parsecToReadE
                    ("Cannot parse build type: " ++)
                    (fmap toFlag parsec)
                )
                (map prettyShow . flagToList)
            )
        ]
    }

instance Monoid ActAsSetupFlags where
  mempty = gmempty
  mappend = (<>)

instance Semigroup ActAsSetupFlags where
  (<>) = gmappend

-- ------------------------------------------------------------

-- * UserConfig flags

-- ------------------------------------------------------------

data UserConfigFlags = UserConfigFlags
  { userConfigVerbosity :: Flag Verbosity
  , userConfigForce :: Flag Bool
  , userConfigAppendLines :: Flag [String]
  }
  deriving (Generic)

instance Monoid UserConfigFlags where
  mempty =
    UserConfigFlags
      { userConfigVerbosity = toFlag normal
      , userConfigForce = toFlag False
      , userConfigAppendLines = toFlag []
      }
  mappend = (<>)

instance Semigroup UserConfigFlags where
  (<>) = gmappend

userConfigCommand :: CommandUI UserConfigFlags
userConfigCommand =
  CommandUI
    { commandName = "user-config"
    , commandSynopsis = "Display and update the user's global cabal configuration."
    , commandDescription = Just $ \_ ->
        wrapText $
          "When upgrading cabal, the set of configuration keys and their default"
            ++ " values may change. This command provides means to merge the existing"
            ++ " config in ~/.config/cabal/config"
            ++ " (i.e. all bindings that are actually defined and not commented out)"
            ++ " and the default config of the new version.\n"
            ++ "\n"
            ++ "init: Creates a new config file at either ~/.config/cabal/config or as"
            ++ " specified by --config-file, if given. An existing file won't be "
            ++ " overwritten unless -f or --force is given.\n"
            ++ "diff: Shows a pseudo-diff of the user's ~/.config/cabal/config file and"
            ++ " the default configuration that would be created by cabal if the"
            ++ " config file did not exist.\n"
            ++ "update: Applies the pseudo-diff to the configuration that would be"
            ++ " created by default, and write the result back to ~/.config/cabal/config."
    , commandNotes = Nothing
    , commandUsage = usageAlternatives "user-config" ["init", "diff", "update"]
    , commandDefaultFlags = mempty
    , commandOptions = \_ ->
        [ optionVerbosity userConfigVerbosity (\v flags -> flags{userConfigVerbosity = v})
        , option
            ['f']
            ["force"]
            "Overwrite the config file if it already exists."
            userConfigForce
            (\v flags -> flags{userConfigForce = v})
            trueArg
        , option
            ['a']
            ["augment"]
            "Additional setting to augment the config file (replacing a previous setting if it existed)."
            userConfigAppendLines
            ( \v flags ->
                flags
                  { userConfigAppendLines =
                      Flag $ concat (flagToList (userConfigAppendLines flags) ++ flagToList v)
                  }
            )
            (reqArg' "CONFIGLINE" (Flag . (: [])) (fromMaybe [] . flagToMaybe))
        ]
    }

-- ------------------------------------------------------------

-- * Dirs

-- ------------------------------------------------------------

-- | A path that can be retrieved by the @cabal path@ command.
data Path
  = PathCacheDir
  | PathLogsDir
  | PathStoreDir
  | PathConfigFile
  | PathInstallDir
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | The configuration name for this path.
pathName :: Path -> String
pathName PathCacheDir = "cache-dir"
pathName PathLogsDir = "logs-dir"
pathName PathStoreDir = "store-dir"
pathName PathConfigFile = "config-file"
pathName PathInstallDir = "installdir"

data PathFlags = PathFlags
  { pathVerbosity :: Flag Verbosity
  , pathDirs :: Flag [Path]
  }
  deriving (Generic)

instance Monoid PathFlags where
  mempty =
    PathFlags
      { pathVerbosity = toFlag normal
      , pathDirs = toFlag []
      }
  mappend = (<>)

instance Semigroup PathFlags where
  (<>) = gmappend

pathCommand :: CommandUI PathFlags
pathCommand =
  CommandUI
    { commandName = "path"
    , commandSynopsis = "Display paths used by cabal."
    , commandDescription = Just $ \_ ->
        wrapText $
          "This command prints the directories that are used by cabal,"
            ++ " taking into account the contents of the configuration file and any"
            ++ " environment variables."
    , commandNotes = Nothing
    , commandUsage = \pname -> "Usage: " ++ pname ++ " path\n"
    , commandDefaultFlags = mempty
    , commandOptions = \_ ->
        map pathOption [minBound .. maxBound]
          ++ [optionVerbosity pathVerbosity (\v flags -> flags{pathVerbosity = v})]
    }
  where
    pathOption s =
      option
        []
        [pathName s]
        ("Print " <> pathName s)
        pathDirs
        (\v flags -> flags{pathDirs = Flag $ concat (flagToList (pathDirs flags) ++ flagToList v)})
        (noArg (Flag [s]))

-- ------------------------------------------------------------

-- * GetOpt Utils

-- ------------------------------------------------------------

reqArgFlag
  :: ArgPlaceHolder
  -> MkOptDescr (b -> Flag String) (Flag String -> b -> b) b
reqArgFlag ad = reqArg ad (succeedReadE Flag) flagToList

liftOptions
  :: (b -> a)
  -> (a -> b -> b)
  -> [OptionField a]
  -> [OptionField b]
liftOptions get set = map (liftOption get set)

yesNoOpt :: ShowOrParseArgs -> MkOptDescr (b -> Flag Bool) (Flag Bool -> b -> b) b
yesNoOpt ShowArgs sf lf = trueArg sf lf
yesNoOpt _ sf lf = Command.boolOpt' flagToMaybe Flag (sf, lf) ([], map ("no-" ++) lf) sf lf

optionSolver
  :: (flags -> Flag PreSolver)
  -> (Flag PreSolver -> flags -> flags)
  -> OptionField flags
optionSolver get set =
  option
    []
    ["solver"]
    ("Select dependency solver to use (default: " ++ prettyShow defaultSolver ++ "). Choices: " ++ allSolvers ++ ".")
    get
    set
    ( reqArg
        "SOLVER"
        ( parsecToReadE
            (const $ "solver must be one of: " ++ allSolvers)
            (toFlag `fmap` parsec)
        )
        (flagToList . fmap prettyShow)
    )

optionSolverFlags
  :: ShowOrParseArgs
  -> (flags -> Flag Int)
  -> (Flag Int -> flags -> flags)
  -> (flags -> Flag ReorderGoals)
  -> (Flag ReorderGoals -> flags -> flags)
  -> (flags -> Flag CountConflicts)
  -> (Flag CountConflicts -> flags -> flags)
  -> (flags -> Flag FineGrainedConflicts)
  -> (Flag FineGrainedConflicts -> flags -> flags)
  -> (flags -> Flag MinimizeConflictSet)
  -> (Flag MinimizeConflictSet -> flags -> flags)
  -> (flags -> Flag IndependentGoals)
  -> (Flag IndependentGoals -> flags -> flags)
  -> (flags -> Flag PreferOldest)
  -> (Flag PreferOldest -> flags -> flags)
  -> (flags -> Flag ShadowPkgs)
  -> (Flag ShadowPkgs -> flags -> flags)
  -> (flags -> Flag StrongFlags)
  -> (Flag StrongFlags -> flags -> flags)
  -> (flags -> Flag AllowBootLibInstalls)
  -> (Flag AllowBootLibInstalls -> flags -> flags)
  -> (flags -> Flag OnlyConstrained)
  -> (Flag OnlyConstrained -> flags -> flags)
  -> [OptionField flags]
optionSolverFlags
  showOrParseArgs
  getmbj
  setmbj
  getrg
  setrg
  getcc
  setcc
  getfgc
  setfgc
  getmc
  setmc
  getig
  setig
  getpo
  setpo
  getsip
  setsip
  getstrfl
  setstrfl
  getib
  setib
  getoc
  setoc =
    [ option
        []
        ["max-backjumps"]
        ("Maximum number of backjumps allowed while solving (default: " ++ show defaultMaxBackjumps ++ "). Use a negative number to enable unlimited backtracking. Use 0 to disable backtracking completely.")
        getmbj
        setmbj
        ( reqArg
            "NUM"
            (parsecToReadE ("Cannot parse number: " ++) (fmap toFlag P.signedIntegral))
            (map show . flagToList)
        )
    , option
        []
        ["reorder-goals"]
        "Try to reorder goals according to certain heuristics. Slows things down on average, but may make backtracking faster for some packages."
        (fmap asBool . getrg)
        (setrg . fmap ReorderGoals)
        (yesNoOpt showOrParseArgs)
    , option
        []
        ["count-conflicts"]
        "Try to speed up solving by preferring goals that are involved in a lot of conflicts (default)."
        (fmap asBool . getcc)
        (setcc . fmap CountConflicts)
        (yesNoOpt showOrParseArgs)
    , option
        []
        ["fine-grained-conflicts"]
        "Skip a version of a package if it does not resolve the conflicts encountered in the last version, as a solver optimization (default)."
        (fmap asBool . getfgc)
        (setfgc . fmap FineGrainedConflicts)
        (yesNoOpt showOrParseArgs)
    , option
        []
        ["minimize-conflict-set"]
        ( "When there is no solution, try to improve the error message by finding "
            ++ "a minimal conflict set (default: false). May increase run time "
            ++ "significantly."
        )
        (fmap asBool . getmc)
        (setmc . fmap MinimizeConflictSet)
        (yesNoOpt showOrParseArgs)
    , option
        []
        ["independent-goals"]
        "Treat several goals on the command line as independent. If several goals depend on the same package, different versions can be chosen."
        (fmap asBool . getig)
        (setig . fmap IndependentGoals)
        (yesNoOpt showOrParseArgs)
    , option
        []
        ["prefer-oldest"]
        "Prefer the oldest (instead of the latest) versions of packages available. Useful to determine lower bounds in the build-depends section."
        (fmap asBool . getpo)
        (setpo . fmap PreferOldest)
        (yesNoOpt showOrParseArgs)
    , option
        []
        ["shadow-installed-packages"]
        "If multiple package instances of the same version are installed, treat all but one as shadowed."
        (fmap asBool . getsip)
        (setsip . fmap ShadowPkgs)
        (yesNoOpt showOrParseArgs)
    , option
        []
        ["strong-flags"]
        "Do not defer flag choices (this used to be the default in cabal-install <= 1.20)."
        (fmap asBool . getstrfl)
        (setstrfl . fmap StrongFlags)
        (yesNoOpt showOrParseArgs)
    , option
        []
        ["allow-boot-library-installs"]
        "Allow cabal to install base, ghc-prim, integer-simple, integer-gmp, and template-haskell."
        (fmap asBool . getib)
        (setib . fmap AllowBootLibInstalls)
        (yesNoOpt showOrParseArgs)
    , option
        []
        ["reject-unconstrained-dependencies"]
        "Require these packages to have constraints on them if they are to be selected (default: none)."
        getoc
        setoc
        ( reqArg
            "none|all"
            ( parsecToReadE
                (const "reject-unconstrained-dependencies must be 'none' or 'all'")
                (toFlag `fmap` parsec)
            )
            (flagToList . fmap prettyShow)
        )
    ]

usagePackages :: String -> String -> String
usagePackages name pname =
  "Usage: " ++ pname ++ " " ++ name ++ " [PACKAGES]\n"

usageFlags :: String -> String -> String
usageFlags name pname =
  "Usage: " ++ pname ++ " " ++ name ++ " [FLAGS]\n"

-- ------------------------------------------------------------

-- * Repo helpers

-- ------------------------------------------------------------

showRemoteRepo :: RemoteRepo -> String
showRemoteRepo = prettyShow

readRemoteRepo :: String -> Maybe RemoteRepo
readRemoteRepo = simpleParsec

showLocalRepo :: LocalRepo -> String
showLocalRepo = prettyShow

readLocalRepo :: String -> Maybe LocalRepo
readLocalRepo = simpleParsec

-- ------------------------------------------------------------

-- * Helpers for Documentation

-- ------------------------------------------------------------

relevantConfigValuesText :: [String] -> String
relevantConfigValuesText vs =
  "Relevant global configuration keys:\n"
    ++ concat ["  " ++ v ++ "\n" | v <- vs]
