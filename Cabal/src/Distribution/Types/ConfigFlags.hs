{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}

-- | The flags to the @configure@ command.
--
-- 'ConfigFlags' separates the flags which describe the package as a whole
-- ('PackageConfigFlags') from the flags which describe the component being
-- configured ('ComponentConfigFlags').
module Distribution.Types.ConfigFlags
  ( ConfigFlags (..)
  , PackageConfigFlags (..)
  , ComponentConfigFlags (..)
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Data.Semigroup (Last (..))
import Distribution.Compiler
import Distribution.ModuleName
import Distribution.PackageDescription
import Distribution.Simple.Compiler
import Distribution.Simple.Flag
import Distribution.Simple.InstallDirs
import Distribution.Simple.Program
import Distribution.Simple.Setup.Common
import Distribution.Types.ComponentId
import Distribution.Types.DumpBuildInfo
import Distribution.Types.GivenComponent
import Distribution.Types.Module
import Distribution.Types.PackageVersionConstraint
import Distribution.Types.UnitId
import Distribution.Utils.NubList
import Distribution.Utils.Path

-- | Flags to the @configure@ command.
data ConfigFlags = NewConfigFlags
  -- IMPORTANT: every time a new flag is added, you should update:
  --
  --  - the 'Eq ConfigFlags' instance (below),
  --  - the ConfigFlags pattern synonym in Distribution.Simple.Setup.Config,
  --  - filterConfigureFlags in cabal-install.

  { commonFlags :: !CommonSetupFlags
  , -- FIXME: the configPrograms is only here to pass info through to configure
    -- because the type of configure is constrained by the UserHooks.
    -- when we change UserHooks next we should pass the initial
    -- ProgramDb directly and not via ConfigFlags
    initialPrograms :: Maybe (Last ProgramDb)
  -- ^ All programs that @cabal@ may run
  , packageFlags :: !PackageConfigFlags
  , componentFlags :: !ComponentConfigFlags
  }
  deriving (Generic, Read, Show)
  deriving (Semigroup, Monoid) via Generically ConfigFlags

instance Binary ConfigFlags
instance Structured ConfigFlags

instance Eq ConfigFlags where
  a == b =
    -- initialPrograms skipped: not user specified, has no Eq instance
    on (==) commonFlags a b
      && on (==) packageFlags a b
      && on (==) componentFlags a b

-- | Configure flags which describe the package as a whole, and are therefore
-- the same for every component of the package.
data PackageConfigFlags = PackageConfigFlags
  { configProgramPaths :: [(String, FilePath)]
  -- ^ user specified programs paths
  , configProgramArgs :: [(String, [String])]
  -- ^ user specified programs args
  , configProgramPathExtra :: NubList FilePath
  -- ^ Extend the $PATH
  , configHcFlavor :: Flag CompilerFlavor
  -- ^ The \"flavor\" of the compiler, e.g. GHC.
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
  , configBytecodeLib :: Flag Bool
  -- ^ Build bytecode library
  , configDynExe :: Flag Bool
  -- ^ Enable dynamic linking of the
  --  executables.
  , configFullyStaticExe :: Flag Bool
  -- ^ Enable fully static linking of the executables.
  , configProfExe :: Flag Bool
  -- ^ Enable profiling in the executables.
  , configProf :: Flag Bool
  -- ^ Enable profiling in the library and executables.
  , configProfShared :: Flag Bool
  -- ^ Enable shared profiling objects
  , configProfDetail :: Flag ProfDetailLevel
  -- ^ Profiling detail level in the library and executables.
  , configProfLibDetail :: Flag ProfDetailLevel
  -- ^ Profiling detail level in the library
  , configConfigureArgs :: [String]
  -- ^ Extra arguments to @configure@
  , configOptimization :: Flag OptimisationLevel
  -- ^ Enable optimization.
  , configProgPrefix :: Flag PathTemplate
  -- ^ Installed executable prefix.
  , configProgSuffix :: Flag PathTemplate
  -- ^ Installed executable suffix.
  , configExtraLibDirs :: [SymbolicPath Pkg (Dir Lib)]
  -- ^ path to search for extra libraries
  , configExtraLibDirsStatic :: [SymbolicPath Pkg (Dir Lib)]
  -- ^ path to search for extra libraries when linking fully static executables
  , configExtraFrameworkDirs :: [SymbolicPath Pkg (Dir Framework)]
  -- ^ path to search for extra frameworks (OS X only)
  , configExtraIncludeDirs :: [SymbolicPath Pkg (Dir Include)]
  -- ^ path to search for header files
  , configDeterministic :: Flag Bool
  -- ^ be as deterministic as possible (e.g., invariant over GHC, database, etc).
  --
  -- Used by the test suite
  , configUserInstall :: Flag Bool
  -- ^ The --user\/--global flag
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
  , configConfigurationsFlags :: FlagAssignment
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
  -- ^ Whether to use response files.
  --
  -- They're used for such tools as haddock, or ld.
  , configAllowDependingOnPrivateLibs :: Flag Bool
  -- ^ Allow depending on private sublibraries.
  --
  -- Used by external tools (like cabal-install) so they can add
  -- multiple-public-libraries compatibility to older GHCs by checking
  -- visibility externally.
  , configIgnoreBuildTools :: Flag Bool
  -- ^ When this flag is set, all tools declared in `build-tool`s and
  -- `build-tool-depends` will be ignored. This allows a Cabal package with
  -- build-tool-dependencies to be built even if the tool is not found.
  }
  deriving (Eq, Generic, Read, Show)
  deriving (Semigroup, Monoid) via Generically PackageConfigFlags

instance Binary PackageConfigFlags
instance Structured PackageConfigFlags

-- | Configure flags which describe an individual component being configured.
data ComponentConfigFlags = ComponentConfigFlags
  { configTargets :: [String]
  -- ^ The targets of this invocation, given as positional arguments.
  , configTests :: Flag Bool
  -- ^ Enable test suite compilation
  , configBenchmarks :: Flag Bool
  -- ^ Enable benchmark compilation
  , configInstallDirs :: InstallDirs (Flag PathTemplate)
  -- ^ Installation
  --  paths
  , configIPID :: Flag String
  -- ^ explicit IPID to be used
  , configCID :: Flag ComponentId
  -- ^ explicit CID to be used
  , configConstraints :: [PackageVersionConstraint]
  -- ^ Additional constraints for
  --  dependencies.
  , configPackageDBs :: [Maybe PackageDB]
  -- ^ Which package DBs to use
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
  , configCoverageFor :: Flag [UnitId]
  -- ^ The list of libraries to be included in the hpc coverage report for
  -- testsuites run with @--enable-coverage@. Notably, this list must exclude
  -- indefinite libraries and instantiations because HPC does not support
  -- backpack (Nov. 2023).
  }
  deriving (Eq, Generic, Read, Show)
  deriving (Semigroup, Monoid) via Generically ComponentConfigFlags

instance Binary ComponentConfigFlags
instance Structured ComponentConfigFlags
