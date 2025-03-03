{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

-----------------------------------------------------------------------------

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
      ( ConfigCommonFlags
      , configVerbosity
      , configDistPref
      , configCabalFilePath
      , configWorkingDir
      , configTargets
      , ..
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
  , configureOptions
  , installDirsOptions
  ) where

import Distribution.Compat.Prelude hiding (get)
import Prelude ()

import qualified Distribution.Compat.CharParsing as P
import Distribution.Compat.Semigroup (Last' (..), Option' (..))
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

-- | Flags to @configure@ command.
--
-- IMPORTANT: every time a new flag is added, 'D.C.Setup.filterConfigureFlags'
-- should be updated.
-- IMPORTANT: every time a new flag is added, it should be added to the Eq instance
data ConfigFlags = ConfigFlags
  { configCommonFlags :: !CommonSetupFlags
  , -- FIXME: the configPrograms is only here to pass info through to configure
    -- because the type of configure is constrained by the UserHooks.
    -- when we change UserHooks next we should pass the initial
    -- ProgramDb directly and not via ConfigFlags
    configPrograms_ :: Option' (Last' ProgramDb)
  -- ^ All programs that
  --  @cabal@ may run
  , configProgramPaths :: [(String, FilePath)]
  -- ^ user specified programs paths
  , configProgramArgs :: [(String, [String])]
  -- ^ user specified programs args
  , configProgramPathExtra :: NubList FilePath
  -- ^ Extend the $PATH
  , configHcFlavor :: Flag CompilerFlavor
  -- ^ The \"flavor\" of the
  --  compiler, e.g. GHC.
  , configHcPath :: Flag FilePath
  -- ^ given compiler location
  , configHcPkg :: Flag FilePath
  -- ^ given hc-pkg location
  , configVanillaLib :: Flag Bool
  -- ^ Enable vanilla library
  , configProfLib :: Flag Bool
  -- ^ Enable profiling in the library
  , configSharedLib :: Flag Bool
  -- ^ Build shared library
  , configStaticLib :: Flag Bool
  -- ^ Build static library
  , configDynExe :: Flag Bool
  -- ^ Enable dynamic linking of the
  --  executables.
  , configFullyStaticExe :: Flag Bool
  -- ^ Enable fully static linking of the
  --  executables.
  , configProfExe :: Flag Bool
  -- ^ Enable profiling in the
  --  executables.
  , configProf :: Flag Bool
  -- ^ Enable profiling in the library
  --  and executables.
  , configProfShared :: Flag Bool
  -- ^ Enable shared profiling objects
  , configProfDetail :: Flag ProfDetailLevel
  -- ^ Profiling detail level
  --   in the library and executables.
  , configProfLibDetail :: Flag ProfDetailLevel
  -- ^ Profiling  detail level
  --  in the library
  , configConfigureArgs :: [String]
  -- ^ Extra arguments to @configure@
  , configOptimization :: Flag OptimisationLevel
  -- ^ Enable optimization.
  , configProgPrefix :: Flag PathTemplate
  -- ^ Installed executable prefix.
  , configProgSuffix :: Flag PathTemplate
  -- ^ Installed executable suffix.
  , configInstallDirs :: InstallDirs (Flag PathTemplate)
  -- ^ Installation
  --  paths
  , configScratchDir :: Flag FilePath
  , configExtraLibDirs :: [SymbolicPath Pkg (Dir Lib)]
  -- ^ path to search for extra libraries
  , configExtraLibDirsStatic :: [SymbolicPath Pkg (Dir Lib)]
  -- ^ path to search for extra
  --   libraries when linking
  --   fully static executables
  , configExtraFrameworkDirs :: [SymbolicPath Pkg (Dir Framework)]
  -- ^ path to search for extra
  -- frameworks (OS X only)
  , configExtraIncludeDirs :: [SymbolicPath Pkg (Dir Include)]
  -- ^ path to search for header files
  , configIPID :: Flag String
  -- ^ explicit IPID to be used
  , configCID :: Flag ComponentId
  -- ^ explicit CID to be used
  , configDeterministic :: Flag Bool
  -- ^ be as deterministic as possible
  -- (e.g., invariant over GHC, database,
  -- etc).  Used by the test suite
  , configUserInstall :: Flag Bool
  -- ^ The --user\/--global flag
  , configPackageDBs :: [Maybe PackageDB]
  -- ^ Which package DBs to use
  , configGHCiLib :: Flag Bool
  -- ^ Enable compiling library for GHCi
  , configSplitSections :: Flag Bool
  -- ^ Enable -split-sections with GHC
  , configSplitObjs :: Flag Bool
  -- ^ Enable -split-objs with GHC
  , configStripExes :: Flag Bool
  -- ^ Enable executable stripping
  , configStripLibs :: Flag Bool
  -- ^ Enable library stripping
  , configConstraints :: [PackageVersionConstraint]
  -- ^ Additional constraints for
  --  dependencies.
  , configDependencies :: [GivenComponent]
  -- ^ The packages depended on which already exist
  , configPromisedDependencies :: [PromisedComponent]
  -- ^ The packages depended on which doesn't yet exist (i.e. promised).
  --  Promising dependencies enables us to configure components in parallel,
  --  and avoids expensive builds if they are not necessary.
  --  For example, in multi-repl mode, we don't want to build dependencies that
  --  are loaded into the interactive session, since we have to build them again.
  , configInstantiateWith :: [(ModuleName, Module)]
  -- ^ The requested Backpack instantiation.  If empty, either this
  -- package does not use Backpack, or we just want to typecheck
  -- the indefinite package.
  , configConfigurationsFlags :: FlagAssignment
  , configTests :: Flag Bool
  -- ^ Enable test suite compilation
  , configBenchmarks :: Flag Bool
  -- ^ Enable benchmark compilation
  , configCoverage :: Flag Bool
  -- ^ Enable program coverage
  , configLibCoverage :: Flag Bool
  -- ^ Enable program coverage (deprecated)
  , configExactConfiguration :: Flag Bool
  -- ^ All direct dependencies and flags are provided on the command line by
  --  the user via the '--dependency' and '--flags' options.
  , configFlagError :: Flag String
  -- ^ Halt and show an error message indicating an error in flag assignment
  , configRelocatable :: Flag Bool
  -- ^ Enable relocatable package built
  , configDebugInfo :: Flag DebugInfoLevel
  -- ^ Emit debug info.
  , configDumpBuildInfo :: Flag DumpBuildInfo
  -- ^ Should we dump available build information on build?
  -- Dump build information to disk before attempting to build,
  -- tooling can parse these files and use them to compile the
  -- source files themselves.
  , configUseResponseFiles :: Flag Bool
  -- ^ Whether to use response files at all. They're used for such tools
  -- as haddock, or ld.
  , configAllowDependingOnPrivateLibs :: Flag Bool
  -- ^ Allow depending on private sublibraries. This is used by external
  -- tools (like cabal-install) so they can add multiple-public-libraries
  -- compatibility to older ghcs by checking visibility externally.
  , configCoverageFor :: Flag [UnitId]
  -- ^ The list of libraries to be included in the hpc coverage report for
  -- testsuites run with @--enable-coverage@. Notably, this list must exclude
  -- indefinite libraries and instantiations because HPC does not support
  -- backpack (Nov. 2023).
  , configIgnoreBuildTools :: Flag Bool
  -- ^ When this flag is set, all tools declared in `build-tool`s and
  -- `build-tool-depends` will be ignored. This allows a Cabal package with
  -- build-tool-dependencies to be built even if the tool is not found.
  }
  deriving (Generic, Read, Show)

pattern ConfigCommonFlags
  :: Flag Verbosity
  -> Flag (SymbolicPath Pkg (Dir Dist))
  -> Flag (SymbolicPath CWD (Dir Pkg))
  -> Flag (SymbolicPath Pkg File)
  -> [String]
  -> ConfigFlags
pattern ConfigCommonFlags
  { configVerbosity
  , configDistPref
  , configWorkingDir
  , configCabalFilePath
  , configTargets
  } <-
  ( configCommonFlags ->
      CommonSetupFlags
        { setupVerbosity = configVerbosity
        , setupDistPref = configDistPref
        , setupWorkingDir = configWorkingDir
        , setupCabalFilePath = configCabalFilePath
        , setupTargets = configTargets
        }
    )

instance Binary ConfigFlags
instance Structured ConfigFlags

-- | More convenient version of 'configPrograms'. Results in an
-- 'error' if internal invariant is violated.
configPrograms :: WithCallStack (ConfigFlags -> ProgramDb)
configPrograms =
  fromMaybe (error "FIXME: remove configPrograms")
    . fmap getLast'
    . getOption'
    . configPrograms_

instance Eq ConfigFlags where
  (==) a b =
    -- configPrograms skipped: not user specified, has no Eq instance
    equal configCommonFlags
      && equal configProgramPaths
      && equal configProgramArgs
      && equal configProgramPathExtra
      && equal configHcFlavor
      && equal configHcPath
      && equal configHcPkg
      && equal configVanillaLib
      && equal configProfLib
      && equal configSharedLib
      && equal configStaticLib
      && equal configDynExe
      && equal configFullyStaticExe
      && equal configProfExe
      && equal configProf
      && equal configProfDetail
      && equal configProfShared
      && equal configProfLibDetail
      && equal configConfigureArgs
      && equal configOptimization
      && equal configProgPrefix
      && equal configProgSuffix
      && equal configInstallDirs
      && equal configScratchDir
      && equal configExtraLibDirs
      && equal configExtraLibDirsStatic
      && equal configExtraIncludeDirs
      && equal configIPID
      && equal configDeterministic
      && equal configUserInstall
      && equal configPackageDBs
      && equal configGHCiLib
      && equal configSplitSections
      && equal configSplitObjs
      && equal configStripExes
      && equal configStripLibs
      && equal configConstraints
      && equal configDependencies
      && equal configPromisedDependencies
      && equal configConfigurationsFlags
      && equal configTests
      && equal configBenchmarks
      && equal configCoverage
      && equal configLibCoverage
      && equal configExactConfiguration
      && equal configFlagError
      && equal configRelocatable
      && equal configDebugInfo
      && equal configDumpBuildInfo
      && equal configUseResponseFiles
      && equal configAllowDependingOnPrivateLibs
      && equal configCoverageFor
      && equal configIgnoreBuildTools
    where
      equal f = on (==) f a b

{- FOURMOLU_DISABLE -}
defaultConfigFlags :: ProgramDb -> ConfigFlags
defaultConfigFlags progDb =
  emptyConfigFlags
    { configCommonFlags = defaultCommonSetupFlags
    , configPrograms_ = Option' (Just (Last' progDb))
    , configHcFlavor = maybe NoFlag Flag defaultCompilerFlavor
    , configVanillaLib = Flag True
    , configProfLib = NoFlag
    , configSharedLib = NoFlag
    , configStaticLib = NoFlag
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
            [ (Flag GHC, ("g", ["ghc"]), "compile with GHC")
            , (Flag GHCJS, ([], ["ghcjs"]), "compile with GHCJS")
            , (Flag UHC, ([], ["uhc"]), "compile with UHC")
            , -- "haskell-suite" compiler id string will be replaced
              -- by a more specific one during the configure stage

              ( Flag (HaskellSuite "haskell-suite")
              , ([], ["haskell-suite"])
              , "compile with a haskell-suite compiler"
              )
            ]
        )
    , option
        "w"
        ["with-compiler"]
        "give the path to a particular compiler"
        configHcPath
        (\v flags -> flags{configHcPath = v})
        (reqArgFlag "PATH")
    , option
        ""
        ["with-hc-pkg"]
        "give the path to the package tool"
        configHcPkg
        (\v flags -> flags{configHcPkg = v})
        (reqArgFlag "PATH")
    ]
    ++ map liftInstallDirs installDirsOptions
    ++ [ option
          ""
          ["program-prefix"]
          "prefix to be applied to installed executables"
          configProgPrefix
          (\v flags -> flags{configProgPrefix = v})
          (reqPathTemplateArgFlag "PREFIX")
       , option
          ""
          ["program-suffix"]
          "suffix to be applied to installed executables"
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
          [ optArgDef'
              "n"
              (show NoOptimisation, Flag . flagToOptimisationLevel)
              ( \f -> case f of
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
              ( \f -> case f of
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
          "enable workaround for old versions of programs like \"ar\" that do not support @file arguments"
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
  cid <- parsec
  return $ PromisedComponent pn ln cid

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
  cid <- parsec
  return $ GivenComponent pn ln cid

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
      "bake this prefix in preparation of installation"
      prefix
      (\v flags -> flags{prefix = v})
      installDirArg
  , option
      ""
      ["bindir"]
      "installation directory for executables"
      bindir
      (\v flags -> flags{bindir = v})
      installDirArg
  , option
      ""
      ["libdir"]
      "installation directory for libraries"
      libdir
      (\v flags -> flags{libdir = v})
      installDirArg
  , option
      ""
      ["libsubdir"]
      "subdirectory of libdir in which libs are installed"
      libsubdir
      (\v flags -> flags{libsubdir = v})
      installDirArg
  , option
      ""
      ["dynlibdir"]
      "installation directory for dynamic libraries"
      dynlibdir
      (\v flags -> flags{dynlibdir = v})
      installDirArg
  , option
      ""
      ["libexecdir"]
      "installation directory for program executables"
      libexecdir
      (\v flags -> flags{libexecdir = v})
      installDirArg
  , option
      ""
      ["libexecsubdir"]
      "subdirectory of libexecdir in which private executables are installed"
      libexecsubdir
      (\v flags -> flags{libexecsubdir = v})
      installDirArg
  , option
      ""
      ["datadir"]
      "installation directory for read-only data"
      datadir
      (\v flags -> flags{datadir = v})
      installDirArg
  , option
      ""
      ["datasubdir"]
      "subdirectory of datadir in which data files are installed"
      datasubdir
      (\v flags -> flags{datasubdir = v})
      installDirArg
  , option
      ""
      ["docdir"]
      "installation directory for documentation"
      docdir
      (\v flags -> flags{docdir = v})
      installDirArg
  , option
      ""
      ["htmldir"]
      "installation directory for HTML documentation"
      htmldir
      (\v flags -> flags{htmldir = v})
      installDirArg
  , option
      ""
      ["haddockdir"]
      "installation directory for haddock interfaces"
      haddockdir
      (\v flags -> flags{haddockdir = v})
      installDirArg
  , option
      ""
      ["sysconfdir"]
      "installation directory for configuration files"
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

instance Monoid ConfigFlags where
  mempty = gmempty
  mappend = (<>)

instance Semigroup ConfigFlags where
  (<>) = gmappend

-- | Arguments to pass to a @configure@ script, e.g. generated by
-- @autoconf@.
configureArgs :: Bool -> ConfigFlags -> [String]
configureArgs bcHack flags =
  hc_flag
    ++ optFlag "with-hc-pkg" configHcPkg
    ++ optFlag' "prefix" prefix
    ++ optFlag' "bindir" bindir
    ++ optFlag' "libdir" libdir
    ++ optFlag' "libexecdir" libexecdir
    ++ optFlag' "datadir" datadir
    ++ optFlag' "sysconfdir" sysconfdir
    ++ configConfigureArgs flags
  where
    hc_flag = case (configHcFlavor flags, configHcPath flags) of
      (_, Flag hc_path) -> [hc_flag_name ++ hc_path]
      (Flag hc, NoFlag) -> [hc_flag_name ++ prettyShow hc]
      (NoFlag, NoFlag) -> []
    hc_flag_name
      -- TODO kill off this bc hack when defaultUserHooks is removed.
      | bcHack = "--with-hc="
      | otherwise = "--with-compiler="
    optFlag name config_field = case config_field flags of
      Flag p -> ["--" ++ name ++ "=" ++ p]
      NoFlag -> []
    optFlag' name config_field =
      optFlag
        name
        ( fmap fromPathTemplate
            . config_field
            . configInstallDirs
        )
