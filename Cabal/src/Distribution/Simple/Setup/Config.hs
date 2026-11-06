{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      :  Distribution.Simple.Setup.Config
-- Copyright   :  Isaac Jones 2003-2004
--                Duncan Coutts 2007
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Definition of the configure command-line options.
-- See: @Distribution.Simple.Setup@
module Distribution.Simple.Setup.Config
  ( ConfigFlags
      ( ConfigFlags
      , configCommonFlags
      , configTargets
      , configPrograms_
      , configProgramPaths
      , configProgramArgs
      , configProgramPathExtra
      , configHcFlavor
      , configHcPath
      , configHcPkg
      , configVanillaLib
      , configProfLib
      , configSharedLib
      , configStaticLib
      , configBytecodeLib
      , configDynExe
      , configFullyStaticExe
      , configProfExe
      , configProf
      , configProfShared
      , configProfDetail
      , configProfLibDetail
      , configConfigureArgs
      , configOptimization
      , configProgPrefix
      , configProgSuffix
      , configInstallDirs
      , configExtraLibDirs
      , configExtraLibDirsStatic
      , configExtraFrameworkDirs
      , configExtraIncludeDirs
      , configIPID
      , configCID
      , configDeterministic
      , configUserInstall
      , configPackageDBs
      , configGHCiLib
      , configSplitSections
      , configSplitObjs
      , configStripExes
      , configStripLibs
      , configConstraints
      , configDependencies
      , configPromisedDependencies
      , configInstantiateWith
      , configConfigurationsFlags
      , configTests
      , configBenchmarks
      , configCoverage
      , configLibCoverage
      , configExactConfiguration
      , configFlagError
      , configRelocatable
      , configDebugInfo
      , configDumpBuildInfo
      , configUseResponseFiles
      , configAllowDependingOnPrivateLibs
      , configCoverageFor
      , configIgnoreBuildTools
      , ConfigCommonFlags
      , configVerbosity
      , configDistPref
      , configCabalFilePath
      , configWorkingDir
      )
  , emptyConfigFlags
  , defaultConfigFlags
  , configureCommand
  , configPrograms
  , readPackageDb
  , readPackageDbList
  , showPackageDb
  , showPackageDbList
  , configureArgs
  , packageConfigureArgs
  , configureOptions
  , installDirsOptions
  ) where

import Distribution.Compat.Prelude hiding (get)
import Prelude ()

import Data.Semigroup (Last (..))
import qualified Distribution.Compat.CharParsing as P
import Distribution.Compat.Stack
import Distribution.Compiler
import Distribution.ModuleName
import Distribution.PackageDescription
import Distribution.Parsec
import Distribution.Pretty
import Distribution.ReadE
import Distribution.Simple.Command hiding (boolOpt, boolOpt')
import Distribution.Simple.Compiler
import Distribution.Simple.Flag
import Distribution.Simple.InstallDirs
import Distribution.Simple.Program
import Distribution.Simple.Setup.Common
import Distribution.Simple.Utils
import Distribution.Types.ComponentId
import Distribution.Types.ConfigFlags (ConfigFlags, PackageConfigFlags)
import qualified Distribution.Types.ConfigFlags as CF
import Distribution.Types.DumpBuildInfo
import Distribution.Types.GivenComponent
import Distribution.Types.Module
import Distribution.Types.PackageVersionConstraint
import Distribution.Types.UnitId
import Distribution.Utils.NubList
import Distribution.Utils.Path
import Distribution.Verbosity

import qualified Text.PrettyPrint as Disp

-- ------------------------------------------------------------

-- * Config flags

-- ------------------------------------------------------------

-- | A flat view of all the configure flags, regardless of whether they are
-- package-level or component-level (see "Distribution.Types.ConfigFlags").
--
-- Each argument is documented at the corresponding field in
-- "Distribution.Types.ConfigFlags".
pattern ConfigFlags
  :: CommonSetupFlags
  -- ^ 'Distribution.Types.ConfigFlags.commonFlags'
  -> [String]
  -- ^ 'Distribution.Types.ConfigFlags.configTargets'
  -> Maybe (Last ProgramDb)
  -- ^ 'Distribution.Types.ConfigFlags.initialPrograms'
  -> [(String, FilePath)]
  -- ^ 'Distribution.Types.ConfigFlags.configProgramPaths'
  -> [(String, [String])]
  -- ^ 'Distribution.Types.ConfigFlags.configProgramArgs'
  -> NubList FilePath
  -- ^ 'Distribution.Types.ConfigFlags.configProgramPathExtra'
  -> Flag CompilerFlavor
  -- ^ 'Distribution.Types.ConfigFlags.configHcFlavor'
  -> Flag FilePath
  -- ^ 'Distribution.Types.ConfigFlags.configHcPath'
  -> Flag FilePath
  -- ^ 'Distribution.Types.ConfigFlags.configHcPkg'
  -> Flag Bool
  -- ^ 'Distribution.Types.ConfigFlags.configVanillaLib'
  -> Flag Bool
  -- ^ 'Distribution.Types.ConfigFlags.configProfLib'
  -> Flag Bool
  -- ^ 'Distribution.Types.ConfigFlags.configSharedLib'
  -> Flag Bool
  -- ^ 'Distribution.Types.ConfigFlags.configStaticLib'
  -> Flag Bool
  -- ^ 'Distribution.Types.ConfigFlags.configBytecodeLib'
  -> Flag Bool
  -- ^ 'Distribution.Types.ConfigFlags.configDynExe'
  -> Flag Bool
  -- ^ 'Distribution.Types.ConfigFlags.configFullyStaticExe'
  -> Flag Bool
  -- ^ 'Distribution.Types.ConfigFlags.configProfExe'
  -> Flag Bool
  -- ^ 'Distribution.Types.ConfigFlags.configProf'
  -> Flag Bool
  -- ^ 'Distribution.Types.ConfigFlags.configProfShared'
  -> Flag ProfDetailLevel
  -- ^ 'Distribution.Types.ConfigFlags.configProfDetail'
  -> Flag ProfDetailLevel
  -- ^ 'Distribution.Types.ConfigFlags.configProfLibDetail'
  -> [String]
  -- ^ 'Distribution.Types.ConfigFlags.configConfigureArgs'
  -> Flag OptimisationLevel
  -- ^ 'Distribution.Types.ConfigFlags.configOptimization'
  -> Flag PathTemplate
  -- ^ 'Distribution.Types.ConfigFlags.configProgPrefix'
  -> Flag PathTemplate
  -- ^ 'Distribution.Types.ConfigFlags.configProgSuffix'
  -> InstallDirs (Flag PathTemplate)
  -- ^ 'Distribution.Types.ConfigFlags.configInstallDirs'
  -> [SymbolicPath Pkg (Dir Lib)]
  -- ^ 'Distribution.Types.ConfigFlags.configExtraLibDirs'
  -> [SymbolicPath Pkg (Dir Lib)]
  -- ^ 'Distribution.Types.ConfigFlags.configExtraLibDirsStatic'
  -> [SymbolicPath Pkg (Dir Framework)]
  -- ^ 'Distribution.Types.ConfigFlags.configExtraFrameworkDirs'
  -> [SymbolicPath Pkg (Dir Include)]
  -- ^ 'Distribution.Types.ConfigFlags.configExtraIncludeDirs'
  -> Flag String
  -- ^ 'Distribution.Types.ConfigFlags.configIPID'
  -> Flag ComponentId
  -- ^ 'Distribution.Types.ConfigFlags.configCID'
  -> Flag Bool
  -- ^ 'Distribution.Types.ConfigFlags.configDeterministic'
  -> Flag Bool
  -- ^ 'Distribution.Types.ConfigFlags.configUserInstall'
  -> [Maybe PackageDB]
  -- ^ 'Distribution.Types.ConfigFlags.configPackageDBs'
  -> Flag Bool
  -- ^ 'Distribution.Types.ConfigFlags.configGHCiLib'
  -> Flag Bool
  -- ^ 'Distribution.Types.ConfigFlags.configSplitSections'
  -> Flag Bool
  -- ^ 'Distribution.Types.ConfigFlags.configSplitObjs'
  -> Flag Bool
  -- ^ 'Distribution.Types.ConfigFlags.configStripExes'
  -> Flag Bool
  -- ^ 'Distribution.Types.ConfigFlags.configStripLibs'
  -> [PackageVersionConstraint]
  -- ^ 'Distribution.Types.ConfigFlags.configConstraints'
  -> [GivenComponent]
  -- ^ 'Distribution.Types.ConfigFlags.configDependencies'
  -> [PromisedComponent]
  -- ^ 'Distribution.Types.ConfigFlags.configPromisedDependencies'
  -> [(ModuleName, Module)]
  -- ^ 'Distribution.Types.ConfigFlags.configInstantiateWith'
  -> FlagAssignment
  -- ^ 'Distribution.Types.ConfigFlags.configConfigurationsFlags'
  -> Flag Bool
  -- ^ 'Distribution.Types.ConfigFlags.configTests'
  -> Flag Bool
  -- ^ 'Distribution.Types.ConfigFlags.configBenchmarks'
  -> Flag Bool
  -- ^ 'Distribution.Types.ConfigFlags.configCoverage'
  -> Flag Bool
  -- ^ 'Distribution.Types.ConfigFlags.configLibCoverage'
  -> Flag Bool
  -- ^ 'Distribution.Types.ConfigFlags.configExactConfiguration'
  -> Flag String
  -- ^ 'Distribution.Types.ConfigFlags.configFlagError'
  -> Flag Bool
  -- ^ 'Distribution.Types.ConfigFlags.configRelocatable'
  -> Flag DebugInfoLevel
  -- ^ 'Distribution.Types.ConfigFlags.configDebugInfo'
  -> Flag DumpBuildInfo
  -- ^ 'Distribution.Types.ConfigFlags.configDumpBuildInfo'
  -> Flag Bool
  -- ^ 'Distribution.Types.ConfigFlags.configUseResponseFiles'
  -> Flag Bool
  -- ^ 'Distribution.Types.ConfigFlags.configAllowDependingOnPrivateLibs'
  -> Flag [UnitId]
  -- ^ 'Distribution.Types.ConfigFlags.configCoverageFor'
  -> Flag Bool
  -- ^ 'Distribution.Types.ConfigFlags.configIgnoreBuildTools'
  -> ConfigFlags
pattern ConfigFlags
  { configCommonFlags
  , configTargets
  , configPrograms_
  , configProgramPaths
  , configProgramArgs
  , configProgramPathExtra
  , configHcFlavor
  , configHcPath
  , configHcPkg
  , configVanillaLib
  , configProfLib
  , configSharedLib
  , configStaticLib
  , configBytecodeLib
  , configDynExe
  , configFullyStaticExe
  , configProfExe
  , configProf
  , configProfShared
  , configProfDetail
  , configProfLibDetail
  , configConfigureArgs
  , configOptimization
  , configProgPrefix
  , configProgSuffix
  , configInstallDirs
  , configExtraLibDirs
  , configExtraLibDirsStatic
  , configExtraFrameworkDirs
  , configExtraIncludeDirs
  , configIPID
  , configCID
  , configDeterministic
  , configUserInstall
  , configPackageDBs
  , configGHCiLib
  , configSplitSections
  , configSplitObjs
  , configStripExes
  , configStripLibs
  , configConstraints
  , configDependencies
  , configPromisedDependencies
  , configInstantiateWith
  , configConfigurationsFlags
  , configTests
  , configBenchmarks
  , configCoverage
  , configLibCoverage
  , configExactConfiguration
  , configFlagError
  , configRelocatable
  , configDebugInfo
  , configDumpBuildInfo
  , configUseResponseFiles
  , configAllowDependingOnPrivateLibs
  , configCoverageFor
  , configIgnoreBuildTools
  } =
  CF.NewConfigFlags
    { CF.commonFlags = configCommonFlags
    , CF.initialPrograms = configPrograms_
    , CF.packageFlags =
      CF.PackageConfigFlags
        { CF.configProgramPaths = configProgramPaths
        , CF.configProgramArgs = configProgramArgs
        , CF.configProgramPathExtra = configProgramPathExtra
        , CF.configHcFlavor = configHcFlavor
        , CF.configHcPath = configHcPath
        , CF.configHcPkg = configHcPkg
        , CF.configVanillaLib = configVanillaLib
        , CF.configProfLib = configProfLib
        , CF.configSharedLib = configSharedLib
        , CF.configStaticLib = configStaticLib
        , CF.configBytecodeLib = configBytecodeLib
        , CF.configDynExe = configDynExe
        , CF.configFullyStaticExe = configFullyStaticExe
        , CF.configProfExe = configProfExe
        , CF.configProf = configProf
        , CF.configProfShared = configProfShared
        , CF.configProfDetail = configProfDetail
        , CF.configProfLibDetail = configProfLibDetail
        , CF.configConfigureArgs = configConfigureArgs
        , CF.configOptimization = configOptimization
        , CF.configProgPrefix = configProgPrefix
        , CF.configProgSuffix = configProgSuffix
        , CF.configExtraLibDirs = configExtraLibDirs
        , CF.configExtraLibDirsStatic = configExtraLibDirsStatic
        , CF.configExtraFrameworkDirs = configExtraFrameworkDirs
        , CF.configExtraIncludeDirs = configExtraIncludeDirs
        , CF.configDeterministic = configDeterministic
        , CF.configUserInstall = configUserInstall
        , CF.configGHCiLib = configGHCiLib
        , CF.configSplitSections = configSplitSections
        , CF.configSplitObjs = configSplitObjs
        , CF.configStripExes = configStripExes
        , CF.configStripLibs = configStripLibs
        , CF.configConfigurationsFlags = configConfigurationsFlags
        , CF.configCoverage = configCoverage
        , CF.configLibCoverage = configLibCoverage
        , CF.configExactConfiguration = configExactConfiguration
        , CF.configFlagError = configFlagError
        , CF.configRelocatable = configRelocatable
        , CF.configDebugInfo = configDebugInfo
        , CF.configDumpBuildInfo = configDumpBuildInfo
        , CF.configUseResponseFiles = configUseResponseFiles
        , CF.configAllowDependingOnPrivateLibs = configAllowDependingOnPrivateLibs
        , CF.configIgnoreBuildTools = configIgnoreBuildTools
        }
    , CF.componentFlags =
      CF.ComponentConfigFlags
        { CF.configTargets = configTargets
        , CF.configTests = configTests
        , CF.configBenchmarks = configBenchmarks
        , CF.configInstallDirs = configInstallDirs
        , CF.configIPID = configIPID
        , CF.configCID = configCID
        , CF.configConstraints = configConstraints
        , CF.configPackageDBs = configPackageDBs
        , CF.configDependencies = configDependencies
        , CF.configPromisedDependencies = configPromisedDependencies
        , CF.configInstantiateWith = configInstantiateWith
        , CF.configCoverageFor = configCoverageFor
        }
    }

{-# COMPLETE ConfigFlags #-}

pattern ConfigCommonFlags
  :: Flag VerbosityFlags
  -> Flag (SymbolicPath Pkg (Dir Dist))
  -> Flag (SymbolicPath CWD (Dir Pkg))
  -> Flag (SymbolicPath Pkg File)
  -> ConfigFlags
pattern ConfigCommonFlags
  { configVerbosity
  , configDistPref
  , configWorkingDir
  , configCabalFilePath
  } <-
  ( configCommonFlags ->
      CommonSetupFlags
        { setupVerbosity = configVerbosity
        , setupDistPref = configDistPref
        , setupWorkingDir = configWorkingDir
        , setupCabalFilePath = configCabalFilePath
        }
    )

-- | More convenient version of 'configPrograms'. Results in an
-- 'error' if internal invariant is violated.
configPrograms :: WithCallStack (ConfigFlags -> ProgramDb)
configPrograms =
  maybe (error "FIXME: remove configPrograms") getLast . configPrograms_

{- FOURMOLU_DISABLE -}
defaultConfigFlags :: ProgramDb -> ConfigFlags
defaultConfigFlags progDb =
  emptyConfigFlags
    { configCommonFlags = defaultCommonSetupFlags
    , configPrograms_ = Just (Last progDb)
    , configHcFlavor = maybe NoFlag Flag defaultCompilerFlavor
    , configVanillaLib = Flag True
    , configProfLib = NoFlag
    , configSharedLib = NoFlag
    , configStaticLib = NoFlag
    , configBytecodeLib = NoFlag
    , configDynExe = Flag False
    , configFullyStaticExe = Flag False
    , configProfExe = NoFlag
    , configProf = NoFlag
    , configProfDetail = NoFlag
    , configProfLibDetail = NoFlag
    , configOptimization = Flag NormalOptimisation
    , configProgPrefix = Flag (toPathTemplate "")
    , configProgSuffix = Flag (toPathTemplate "")
    , configUserInstall = Flag False -- TODO: reverse this
#if defined(mingw32_HOST_OS)
        -- See #8062 and GHC #21019.
    , configGHCiLib = Flag False
#else
    , configGHCiLib = NoFlag
#endif
    , configSplitSections = Flag False
    , configSplitObjs = Flag False -- takes longer, so turn off by default
    , configStripExes = NoFlag
    , configStripLibs = NoFlag
    , configTests = Flag False
    , configBenchmarks = Flag False
    , configCoverage = Flag False
    , configLibCoverage = NoFlag
    , configExactConfiguration = Flag False
    , configFlagError = NoFlag
    , configRelocatable = Flag False
    , configDebugInfo = Flag NoDebugInfo
    , configDumpBuildInfo = NoFlag
    , configUseResponseFiles = NoFlag
    }
{- FOURMOLU_ENABLE -}

configureCommand :: ProgramDb -> CommandUI ConfigFlags
configureCommand progDb =
  CommandUI
    { commandName = "configure"
    , commandSynopsis = "Prepare to build the package."
    , commandDescription = Just $ \_ ->
        wrapText $
          "Configure how the package is built by setting "
            ++ "package (and other) flags.\n"
            ++ "\n"
            ++ "The configuration affects several other commands, "
            ++ "including build, test, bench, run, repl.\n"
    , commandNotes = Just $ \_pname -> programFlagsDescription progDb
    , commandUsage = \pname ->
        "Usage: " ++ pname ++ " configure [FLAGS]\n"
    , commandDefaultFlags = defaultConfigFlags progDb
    , commandOptions = \showOrParseArgs ->
        configureOptions showOrParseArgs
          ++ programDbPaths
            progDb
            showOrParseArgs
            configProgramPaths
            (\v fs -> fs{configProgramPaths = v})
          ++ programDbOption
            progDb
            showOrParseArgs
            configProgramArgs
            (\v fs -> fs{configProgramArgs = v})
          ++ programDbOptions
            progDb
            showOrParseArgs
            configProgramArgs
            (\v fs -> fs{configProgramArgs = v})
    }

-- | Inverse to 'dispModSubstEntry'.
parsecModSubstEntry :: ParsecParser (ModuleName, Module)
parsecModSubstEntry = do
  k <- parsec
  _ <- P.char '='
  v <- parsec
  return (k, v)

-- | Pretty-print a single entry of a module substitution.
dispModSubstEntry :: (ModuleName, Module) -> Disp.Doc
dispModSubstEntry (k, v) = pretty k <<>> Disp.char '=' <<>> pretty v

configureOptions :: ShowOrParseArgs -> [OptionField ConfigFlags]
configureOptions showOrParseArgs =
  withCommonSetupOptions
    configCommonFlags
    (\c f -> f{configCommonFlags = c})
    showOrParseArgs
    [ option
        []
        ["compiler"]
        "compiler"
        configHcFlavor
        (\v flags -> flags{configHcFlavor = v})
        ( choiceOpt
            [ (Flag GHC, ("g", ["ghc"]), "Compile with GHC")
            , (Flag UHC, ([], ["uhc"]), "Compile with UHC")
            ]
        )
    , option
        "w"
        ["with-compiler"]
        "Give the path to a particular compiler"
        configHcPath
        (\v flags -> flags{configHcPath = v})
        (reqArgFlag "PATH")
    , option
        ""
        ["with-hc-pkg"]
        "Give the path to the package tool"
        configHcPkg
        (\v flags -> flags{configHcPkg = v})
        (reqArgFlag "PATH")
    ]
    ++ map liftInstallDirs installDirsOptions
    ++ [ option
          ""
          ["program-prefix"]
          "Prefix to be applied to installed executables"
          configProgPrefix
          (\v flags -> flags{configProgPrefix = v})
          (reqPathTemplateArgFlag "PREFIX")
       , option
          ""
          ["program-suffix"]
          "Suffix to be applied to installed executables"
          configProgSuffix
          (\v flags -> flags{configProgSuffix = v})
          (reqPathTemplateArgFlag "SUFFIX")
       , option
          ""
          ["library-vanilla"]
          "Vanilla libraries"
          configVanillaLib
          (\v flags -> flags{configVanillaLib = v})
          (boolOpt [] [])
       , option
          "p"
          ["library-profiling"]
          "Library profiling"
          configProfLib
          (\v flags -> flags{configProfLib = v})
          (boolOpt "p" [])
       , option
          ""
          ["shared"]
          "Shared library"
          configSharedLib
          (\v flags -> flags{configSharedLib = v})
          (boolOpt [] [])
       , option
          ""
          ["static"]
          "Static library"
          configStaticLib
          (\v flags -> flags{configStaticLib = v})
          (boolOpt [] [])
       , option
          ""
          ["library-bytecode"]
          "Bytecode library"
          configBytecodeLib
          (\v flags -> flags{configBytecodeLib = v})
          (boolOpt [] [])
       , option
          ""
          ["executable-dynamic"]
          "Executable dynamic linking"
          configDynExe
          (\v flags -> flags{configDynExe = v})
          (boolOpt [] [])
       , option
          ""
          ["executable-static"]
          "Executable fully static linking"
          configFullyStaticExe
          (\v flags -> flags{configFullyStaticExe = v})
          (boolOpt [] [])
       , option
          ""
          ["profiling"]
          "Executable and library profiling"
          configProf
          (\v flags -> flags{configProf = v})
          (boolOpt [] [])
       , option
          ""
          ["profiling-shared"]
          "Build profiling shared libraries"
          configProfShared
          (\v flags -> flags{configProfShared = v})
          (boolOpt [] [])
       , option
          ""
          ["executable-profiling"]
          "Executable profiling (DEPRECATED)"
          configProfExe
          (\v flags -> flags{configProfExe = v})
          (boolOpt [] [])
       , option
          ""
          ["profiling-detail"]
          ( "Profiling detail level for executable and library (default, "
              ++ "none, exported-functions, toplevel-functions,  all-functions, late)."
          )
          configProfDetail
          (\v flags -> flags{configProfDetail = v})
          ( reqArg'
              "level"
              (Flag . flagToProfDetailLevel)
              showProfDetailLevelFlag
          )
       , option
          ""
          ["library-profiling-detail"]
          "Profiling detail level for libraries only."
          configProfLibDetail
          (\v flags -> flags{configProfLibDetail = v})
          ( reqArg'
              "level"
              (Flag . flagToProfDetailLevel)
              showProfDetailLevelFlag
          )
       , multiOption
          "optimization"
          configOptimization
          (\v flags -> flags{configOptimization = v})
          [ optArg'
              "n"
              (Flag . flagToOptimisationLevel)
              ( \case
                  Flag NoOptimisation -> []
                  Flag NormalOptimisation -> [Nothing]
                  Flag MaximumOptimisation -> [Just "2"]
                  _ -> []
              )
              "O"
              ["enable-optimization", "enable-optimisation"]
              "Build with optimization (n is 0--2, default is 1)"
          , noArg
              (Flag NoOptimisation)
              []
              ["disable-optimization", "disable-optimisation"]
              "Build without optimization"
          ]
       , multiOption
          "debug-info"
          configDebugInfo
          (\v flags -> flags{configDebugInfo = v})
          [ optArg'
              "n"
              (Flag . flagToDebugInfoLevel)
              ( \case
                  Flag NoDebugInfo -> []
                  Flag MinimalDebugInfo -> [Just "1"]
                  Flag NormalDebugInfo -> [Nothing]
                  Flag MaximalDebugInfo -> [Just "3"]
                  _ -> []
              )
              ""
              ["enable-debug-info"]
              "Emit debug info (n is 0--3, default is 0)"
          , noArg
              (Flag NoDebugInfo)
              []
              ["disable-debug-info"]
              "Don't emit debug info"
          ]
       , multiOption
          "build-info"
          configDumpBuildInfo
          (\v flags -> flags{configDumpBuildInfo = v})
          [ noArg
              (Flag DumpBuildInfo)
              []
              ["enable-build-info"]
              "Enable build information generation during project building"
          , noArg
              (Flag NoDumpBuildInfo)
              []
              ["disable-build-info"]
              "Disable build information generation during project building"
          ]
       , option
          ""
          ["library-for-ghci"]
          "compile library for use with GHCi"
          configGHCiLib
          (\v flags -> flags{configGHCiLib = v})
          (boolOpt [] [])
       , option
          ""
          ["split-sections"]
          "compile library code such that unneeded definitions can be dropped from the final executable (GHC 7.8+)"
          configSplitSections
          (\v flags -> flags{configSplitSections = v})
          (boolOpt [] [])
       , option
          ""
          ["split-objs"]
          "split library into smaller objects to reduce binary sizes (GHC 6.6+)"
          configSplitObjs
          (\v flags -> flags{configSplitObjs = v})
          (boolOpt [] [])
       , option
          ""
          ["executable-stripping"]
          "strip executables upon installation to reduce binary sizes"
          configStripExes
          (\v flags -> flags{configStripExes = v})
          (boolOpt [] [])
       , option
          ""
          ["library-stripping"]
          "strip libraries upon installation to reduce binary sizes"
          configStripLibs
          (\v flags -> flags{configStripLibs = v})
          (boolOpt [] [])
       , option
          ""
          ["configure-option"]
          "Extra option for configure"
          configConfigureArgs
          (\v flags -> flags{configConfigureArgs = v})
          (reqArg' "OPT" (\x -> [x]) id)
       , option
          ""
          ["user-install"]
          "doing a per-user installation"
          configUserInstall
          (\v flags -> flags{configUserInstall = v})
          (boolOpt' ([], ["user"]) ([], ["global"]))
       , option
          ""
          ["package-db"]
          ( "Append the given package database to the list of package"
              ++ " databases used (to satisfy dependencies and register into)."
              ++ " May be a specific file, 'global' or 'user'. The initial list"
              ++ " is ['global'], ['global', 'user'], or ['global', $sandbox],"
              ++ " depending on context. Use 'clear' to reset the list to empty."
              ++ " See the user guide for details."
          )
          configPackageDBs
          (\v flags -> flags{configPackageDBs = v})
          (reqArg' "DB" readPackageDbList showPackageDbList)
       , option
          "f"
          ["flags"]
          "Force values for the given flags in Cabal conditionals in the .cabal file.  E.g., --flags=\"debug -usebytestrings\" forces the flag \"debug\" to true and \"usebytestrings\" to false."
          configConfigurationsFlags
          (\v flags -> flags{configConfigurationsFlags = v})
          ( reqArg
              "FLAGS"
              (parsecToReadE (\err -> "Invalid flag assignment: " ++ err) legacyParsecFlagAssignment)
              legacyShowFlagAssignment'
          )
       , option
          ""
          ["extra-include-dirs"]
          "A list of directories to search for header files"
          configExtraIncludeDirs
          (\v flags -> flags{configExtraIncludeDirs = v})
          (reqArg' "PATH" (\x -> [makeSymbolicPath x]) (fmap getSymbolicPath))
       , option
          ""
          ["deterministic"]
          "Try to be as deterministic as possible (used by the test suite)"
          configDeterministic
          (\v flags -> flags{configDeterministic = v})
          (boolOpt [] [])
       , option
          ""
          ["ipid"]
          "Installed package ID to compile this package as"
          configIPID
          (\v flags -> flags{configIPID = v})
          (reqArgFlag "IPID")
       , option
          ""
          ["cid"]
          "Installed component ID to compile this component as"
          (fmap prettyShow . configCID)
          (\v flags -> flags{configCID = fmap mkComponentId v})
          (reqArgFlag "CID")
       , option
          ""
          ["extra-lib-dirs"]
          "A list of directories to search for external libraries"
          configExtraLibDirs
          (\v flags -> flags{configExtraLibDirs = v})
          (reqArg' "PATH" (\x -> [makeSymbolicPath x]) (fmap getSymbolicPath))
       , option
          ""
          ["extra-lib-dirs-static"]
          "A list of directories to search for external libraries when linking fully static executables"
          configExtraLibDirsStatic
          (\v flags -> flags{configExtraLibDirsStatic = v})
          (reqArg' "PATH" (\x -> [makeSymbolicPath x]) (fmap getSymbolicPath))
       , option
          ""
          ["extra-framework-dirs"]
          "A list of directories to search for external frameworks (OS X only)"
          configExtraFrameworkDirs
          (\v flags -> flags{configExtraFrameworkDirs = v})
          (reqArg' "PATH" (\x -> [makeSymbolicPath x]) (fmap getSymbolicPath))
       , option
          ""
          ["extra-prog-path"]
          "A list of directories to search for required programs (in addition to the normal search locations)"
          configProgramPathExtra
          (\v flags -> flags{configProgramPathExtra = v})
          (reqArg' "PATH" (\x -> toNubList [x]) fromNubList)
       , option
          ""
          ["constraint"]
          "A list of additional constraints on the dependencies."
          configConstraints
          (\v flags -> flags{configConstraints = v})
          ( reqArg
              "DEPENDENCY"
              (parsecToReadE (const "dependency expected") ((\x -> [x]) `fmap` parsec))
              (map prettyShow)
          )
       , option
          ""
          ["dependency"]
          "A list of exact dependencies. E.g., --dependency=\"void=void-0.5.8-177d5cdf20962d0581fe2e4932a6c309\""
          configDependencies
          (\v flags -> flags{configDependencies = v})
          ( reqArg
              "NAME[:COMPONENT_NAME]=CID"
              (parsecToReadE (const "dependency expected") ((\x -> [x]) `fmap` parsecGivenComponent))
              (map prettyGivenComponent)
          )
       , option
          ""
          ["promised-dependency"]
          "A list of promised dependencies. E.g., --promised-dependency=\"void-0.5.8=void-0.5.8-177d5cdf20962d0581fe2e4932a6c309\""
          configPromisedDependencies
          (\v flags -> flags{configPromisedDependencies = v})
          ( reqArg
              "NAME-VER[:COMPONENT_NAME]=CID"
              (parsecToReadE (const "dependency expected") ((\x -> [x]) `fmap` parsecPromisedComponent))
              (map prettyPromisedComponent)
          )
       , option
          ""
          ["instantiate-with"]
          "A mapping of signature names to concrete module instantiations."
          configInstantiateWith
          (\v flags -> flags{configInstantiateWith = v})
          ( reqArg
              "NAME=MOD"
              (parsecToReadE ("Cannot parse module substitution: " ++) (fmap (: []) parsecModSubstEntry))
              (map (Disp.renderStyle defaultStyle . dispModSubstEntry))
          )
       , option
          ""
          ["tests"]
          "dependency checking and compilation for test suites listed in the package description file."
          configTests
          (\v flags -> flags{configTests = v})
          (boolOpt [] [])
       , option
          ""
          ["coverage"]
          "build package with Haskell Program Coverage. (GHC only)"
          configCoverage
          (\v flags -> flags{configCoverage = v})
          (boolOpt [] [])
       , option
          ""
          ["library-coverage"]
          "build package with Haskell Program Coverage. (GHC only) (DEPRECATED)"
          configLibCoverage
          (\v flags -> flags{configLibCoverage = v})
          (boolOpt [] [])
       , option
          ""
          ["exact-configuration"]
          "All direct dependencies and flags are provided on the command line."
          configExactConfiguration
          (\v flags -> flags{configExactConfiguration = v})
          trueArg
       , option
          ""
          ["benchmarks"]
          "dependency checking and compilation for benchmarks listed in the package description file."
          configBenchmarks
          (\v flags -> flags{configBenchmarks = v})
          (boolOpt [] [])
       , option
          ""
          ["relocatable"]
          "building a package that is relocatable. (GHC only)"
          configRelocatable
          (\v flags -> flags{configRelocatable = v})
          (boolOpt [] [])
       , option
          ""
          ["response-files"]
          "Enable workaround for old versions of programs like \"ar\" that do not support @file arguments"
          configUseResponseFiles
          (\v flags -> flags{configUseResponseFiles = v})
          (boolOpt' ([], ["disable-response-files"]) ([], []))
       , option
          ""
          ["allow-depending-on-private-libs"]
          ( "Allow depending on private libraries. "
              ++ "If set, the library visibility check MUST be done externally."
          )
          configAllowDependingOnPrivateLibs
          (\v flags -> flags{configAllowDependingOnPrivateLibs = v})
          trueArg
       , option
          ""
          ["coverage-for"]
          "A list of unit-ids of libraries to include in the Haskell Program Coverage report."
          configCoverageFor
          ( \v flags ->
              flags
                { configCoverageFor =
                    mergeListFlag (configCoverageFor flags) v
                }
          )
          ( reqArg'
              "UNITID"
              (Flag . (: []) . fromString)
              (fmap prettyShow . fromFlagOrDefault [])
          )
       , option
          ""
          ["ignore-build-tools"]
          ( "Ignore build tool dependencies. "
              ++ "If set, declared build tools needn't be found for compilation to proceed."
          )
          configIgnoreBuildTools
          (\v flags -> flags{configIgnoreBuildTools = v})
          trueArg
       ]
  where
    liftInstallDirs =
      liftOption configInstallDirs (\v flags -> flags{configInstallDirs = v})

    reqPathTemplateArgFlag title _sf _lf d get set =
      reqArgFlag
        title
        _sf
        _lf
        d
        (fmap fromPathTemplate . get)
        (set . fmap toPathTemplate)

readPackageDbList :: String -> [Maybe PackageDB]
readPackageDbList str = [readPackageDb str]

showPackageDbList :: [Maybe PackageDB] -> [String]
showPackageDbList = map showPackageDb

-- | Show a PackageDB stack entry
--
-- @since 3.7.0.0
showPackageDb :: Maybe PackageDB -> String
showPackageDb Nothing = "clear"
showPackageDb (Just GlobalPackageDB) = "global"
showPackageDb (Just UserPackageDB) = "user"
showPackageDb (Just (SpecificPackageDB db)) = getSymbolicPath db

showProfDetailLevelFlag :: Flag ProfDetailLevel -> [String]
showProfDetailLevelFlag NoFlag = []
showProfDetailLevelFlag (Flag dl) = [showProfDetailLevel dl]

parsecPromisedComponent :: ParsecParser PromisedComponent
parsecPromisedComponent = do
  pn <- parsec
  ln <- P.option LMainLibName $ do
    _ <- P.char ':'
    ucn <- parsec
    return $
      if unUnqualComponentName ucn == unPackageName (pkgName pn)
        then LMainLibName
        else LSubLibName ucn
  _ <- P.char '='
  PromisedComponent pn ln <$> parsec

prettyPromisedComponent :: PromisedComponent -> String
prettyPromisedComponent (PromisedComponent pn cn cid) =
  prettyShow pn
    ++ case cn of
      LMainLibName -> ""
      LSubLibName n -> ":" ++ prettyShow n
    ++ "="
    ++ prettyShow cid

parsecGivenComponent :: ParsecParser GivenComponent
parsecGivenComponent = do
  pn <- parsec
  ln <- P.option LMainLibName $ do
    _ <- P.char ':'
    ucn <- parsec
    return $
      if unUnqualComponentName ucn == unPackageName pn
        then LMainLibName
        else LSubLibName ucn
  _ <- P.char '='
  GivenComponent pn ln <$> parsec

prettyGivenComponent :: GivenComponent -> String
prettyGivenComponent (GivenComponent pn cn cid) =
  prettyShow pn
    ++ case cn of
      LMainLibName -> ""
      LSubLibName n -> ":" ++ prettyShow n
    ++ "="
    ++ prettyShow cid

installDirsOptions :: [OptionField (InstallDirs (Flag PathTemplate))]
installDirsOptions =
  [ option
      ""
      ["prefix"]
      "Bake this prefix in preparation of installation"
      prefix
      (\v flags -> flags{prefix = v})
      installDirArg
  , option
      ""
      ["bindir"]
      "Installation directory for executables"
      bindir
      (\v flags -> flags{bindir = v})
      installDirArg
  , option
      ""
      ["libdir"]
      "Installation directory for libraries"
      libdir
      (\v flags -> flags{libdir = v})
      installDirArg
  , option
      ""
      ["libsubdir"]
      "Subdirectory of libdir in which libs are installed"
      libsubdir
      (\v flags -> flags{libsubdir = v})
      installDirArg
  , option
      ""
      ["dynlibdir"]
      "Installation directory for dynamic libraries"
      dynlibdir
      (\v flags -> flags{dynlibdir = v})
      installDirArg
  , option
      ""
      ["bytecodelibdir"]
      "Installation directory for bytecode libraries"
      bytecodelibdir
      (\v flags -> flags{bytecodelibdir = v})
      installDirArg
  , option
      ""
      ["libexecdir"]
      "Installation directory for program executables"
      libexecdir
      (\v flags -> flags{libexecdir = v})
      installDirArg
  , option
      ""
      ["libexecsubdir"]
      "Subdirectory of libexecdir in which private executables are installed"
      libexecsubdir
      (\v flags -> flags{libexecsubdir = v})
      installDirArg
  , option
      ""
      ["datadir"]
      "Installation directory for read-only data"
      datadir
      (\v flags -> flags{datadir = v})
      installDirArg
  , option
      ""
      ["datasubdir"]
      "Subdirectory of datadir in which data files are installed"
      datasubdir
      (\v flags -> flags{datasubdir = v})
      installDirArg
  , option
      ""
      ["docdir"]
      "Installation directory for documentation"
      docdir
      (\v flags -> flags{docdir = v})
      installDirArg
  , option
      ""
      ["htmldir"]
      "Installation directory for HTML documentation"
      htmldir
      (\v flags -> flags{htmldir = v})
      installDirArg
  , option
      ""
      ["haddockdir"]
      "Installation directory for haddock interfaces"
      haddockdir
      (\v flags -> flags{haddockdir = v})
      installDirArg
  , option
      ""
      ["sysconfdir"]
      "Installation directory for configuration files"
      sysconfdir
      (\v flags -> flags{sysconfdir = v})
      installDirArg
  ]
  where
    installDirArg _sf _lf d get set =
      reqArgFlag
        "DIR"
        _sf
        _lf
        d
        (fmap fromPathTemplate . get)
        (set . fmap toPathTemplate)

emptyConfigFlags :: ConfigFlags
emptyConfigFlags = mempty

-- | Arguments to pass to a @configure@ script, e.g. generated by
-- @autoconf@.
configureArgs :: Bool -> ConfigFlags -> [String]
configureArgs bcHack flags =
  configureCompilerArgs bcHack (CF.packageFlags flags)
    ++ optFlag' "prefix" prefix
    ++ optFlag' "bindir" bindir
    ++ optFlag' "libdir" libdir
    ++ optFlag' "libexecdir" libexecdir
    ++ optFlag' "datadir" datadir
    ++ optFlag' "sysconfdir" sysconfdir
    ++ configConfigureArgs flags
  where
    optFlag' name config_field =
      case config_field (configInstallDirs flags) of
        Flag p -> ["--" ++ name ++ "=" ++ fromPathTemplate p]
        NoFlag -> []

-- | Arguments to pass to a @configure@ script, e.g. generated by
-- @autoconf@, when configuring a package as a whole.
--
-- Unlike 'configureArgs', these arguments do not include installation
-- directories, as these can differ between the components of a package.
packageConfigureArgs :: PackageConfigFlags -> [String]
packageConfigureArgs pkgFlags =
  configureCompilerArgs False pkgFlags
    ++ CF.configConfigureArgs pkgFlags

-- | The arguments to a @configure@ script which specify the compiler.
configureCompilerArgs :: Bool -> PackageConfigFlags -> [String]
configureCompilerArgs bcHack pkgFlags =
  hc_flag
    ++ case CF.configHcPkg pkgFlags of
      Flag p -> ["--with-hc-pkg=" ++ p]
      NoFlag -> []
  where
    hc_flag = case (CF.configHcFlavor pkgFlags, CF.configHcPath pkgFlags) of
      (_, Flag hc_path) -> [hc_flag_name ++ hc_path]
      (Flag hc, NoFlag) -> [hc_flag_name ++ prettyShow hc]
      (NoFlag, NoFlag) -> []
    hc_flag_name
      -- TODO kill off this bc hack when defaultUserHooks is removed.
      | bcHack = "--with-hc="
      | otherwise = "--with-compiler="
