{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Setup
-- Copyright   :  Isaac Jones 2003-2004
--                Duncan Coutts 2007
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This is a big module, but not very complicated. The code is very regular
-- and repetitive. It defines the command line interface for all the Cabal
-- commands. For each command (like @configure@, @build@ etc) it defines a type
-- that holds all the flags, the default set of flags and a 'CommandUI' that
-- maps command line flags to and from the corresponding flags type.
--
-- All the flags types are instances of 'Monoid', see
-- <http://www.haskell.org/pipermail/cabal-devel/2007-December/001509.html>
-- for an explanation.
--
-- The types defined here get used in the front end and especially in
-- @cabal-install@ which has to do quite a bit of manipulating sets of command
-- line flags.
--
-- This is actually relatively nice, it works quite well. The main change it
-- needs is to unify it with the code for managing sets of fields that can be
-- read and written from files. This would allow us to save configure flags in
-- config files.

module Distribution.Simple.Setup (

  GlobalFlags(..),   emptyGlobalFlags,   defaultGlobalFlags,   globalCommand,
  ConfigFlags(..),   emptyConfigFlags,   defaultConfigFlags,   configureCommand,
  configAbsolutePaths, readPackageDbList, showPackageDbList,
  CopyFlags(..),     emptyCopyFlags,     defaultCopyFlags,     copyCommand,
  InstallFlags(..),  emptyInstallFlags,  defaultInstallFlags,  installCommand,
  HaddockFlags(..),  emptyHaddockFlags,  defaultHaddockFlags,  haddockCommand,
  HscolourFlags(..), emptyHscolourFlags, defaultHscolourFlags, hscolourCommand,
  BuildFlags(..),    emptyBuildFlags,    defaultBuildFlags,    buildCommand,
  buildVerbose,
  ReplFlags(..),                         defaultReplFlags,     replCommand,
  CleanFlags(..),    emptyCleanFlags,    defaultCleanFlags,    cleanCommand,
  RegisterFlags(..), emptyRegisterFlags, defaultRegisterFlags, registerCommand,
                                                               unregisterCommand,
  SDistFlags(..),    emptySDistFlags,    defaultSDistFlags,    sdistCommand,
  TestFlags(..),     emptyTestFlags,     defaultTestFlags,     testCommand,
  TestShowDetails(..),
  BenchmarkFlags(..), emptyBenchmarkFlags,
  defaultBenchmarkFlags, benchmarkCommand,
  CopyDest(..),
  configureArgs, configureOptions, configureCCompiler, configureLinker,
  buildOptions, haddockOptions, installDirsOptions,
  programConfigurationOptions, programConfigurationPaths',
  splitArgs,

  defaultDistPref, optionDistPref,

  Flag(..),
  toFlag,
  fromFlag,
  fromFlagOrDefault,
  flagToMaybe,
  flagToList,
  boolOpt, boolOpt', trueArg, falseArg, optionVerbosity, optionNumJobs ) where

import Distribution.Compiler ()
import Distribution.ReadE
import Distribution.Text
         ( Text(..), display )
import qualified Distribution.Compat.ReadP as Parse
import qualified Text.PrettyPrint as Disp
import Distribution.ModuleName
import Distribution.Package ( Dependency(..)
                            , PackageName
                            , ComponentId(..) )
import Distribution.PackageDescription
         ( FlagName(..), FlagAssignment )
import Distribution.Simple.Command hiding (boolOpt, boolOpt')
import qualified Distribution.Simple.Command as Command
import Distribution.Simple.Compiler
         ( CompilerFlavor(..), defaultCompilerFlavor, PackageDB(..)
         , DebugInfoLevel(..), flagToDebugInfoLevel
         , OptimisationLevel(..), flagToOptimisationLevel
         , ProfDetailLevel(..), flagToProfDetailLevel, showProfDetailLevel
         , absolutePackageDBPath )
import Distribution.Simple.Utils
         ( wrapText, wrapLine, lowercase, intercalate )
import Distribution.Simple.Program (Program(..), ProgramConfiguration,
                             requireProgram,
                             programInvocation, progInvokePath, progInvokeArgs,
                             knownPrograms,
                             addKnownProgram, emptyProgramConfiguration,
                             haddockProgram, ghcProgram, gccProgram, ldProgram)
import Distribution.Simple.InstallDirs
         ( InstallDirs(..), CopyDest(..),
           PathTemplate, toPathTemplate, fromPathTemplate )
import Distribution.Verbosity
import Distribution.Utils.NubList

import Control.Monad (liftM)
import Distribution.Compat.Binary (Binary)
import Distribution.Compat.Semigroup as Semi
import Data.List   ( sort )
import Data.Char   ( isSpace, isAlpha )
import GHC.Generics (Generic)

-- FIXME Not sure where this should live
defaultDistPref :: FilePath
defaultDistPref = "dist"

-- ------------------------------------------------------------
-- * Flag type
-- ------------------------------------------------------------

-- | All flags are monoids, they come in two flavours:
--
-- 1. list flags eg
--
-- > --ghc-option=foo --ghc-option=bar
--
-- gives us all the values ["foo", "bar"]
--
-- 2. singular value flags, eg:
--
-- > --enable-foo --disable-foo
--
-- gives us Just False
-- So this Flag type is for the latter singular kind of flag.
-- Its monoid instance gives us the behaviour where it starts out as
-- 'NoFlag' and later flags override earlier ones.
--
data Flag a = Flag a | NoFlag deriving (Eq, Generic, Show, Read)

instance Binary a => Binary (Flag a)

instance Functor Flag where
  fmap f (Flag x) = Flag (f x)
  fmap _ NoFlag  = NoFlag

instance Monoid (Flag a) where
  mempty = NoFlag
  mappend = (Semi.<>)

instance Semigroup (Flag a) where
  _ <> f@(Flag _) = f
  f <> NoFlag     = f

instance Bounded a => Bounded (Flag a) where
  minBound = toFlag minBound
  maxBound = toFlag maxBound

instance Enum a => Enum (Flag a) where
  fromEnum = fromEnum . fromFlag
  toEnum   = toFlag   . toEnum
  enumFrom (Flag a) = map toFlag . enumFrom $ a
  enumFrom _        = []
  enumFromThen (Flag a) (Flag b) = toFlag `map` enumFromThen a b
  enumFromThen _        _        = []
  enumFromTo   (Flag a) (Flag b) = toFlag `map` enumFromTo a b
  enumFromTo   _        _        = []
  enumFromThenTo (Flag a) (Flag b) (Flag c) = toFlag `map` enumFromThenTo a b c
  enumFromThenTo _        _        _        = []

toFlag :: a -> Flag a
toFlag = Flag

fromFlag :: Flag a -> a
fromFlag (Flag x) = x
fromFlag NoFlag   = error "fromFlag NoFlag. Use fromFlagOrDefault"

fromFlagOrDefault :: a -> Flag a -> a
fromFlagOrDefault _   (Flag x) = x
fromFlagOrDefault def NoFlag   = def

flagToMaybe :: Flag a -> Maybe a
flagToMaybe (Flag x) = Just x
flagToMaybe NoFlag   = Nothing

flagToList :: Flag a -> [a]
flagToList (Flag x) = [x]
flagToList NoFlag   = []

allFlags :: [Flag Bool] -> Flag Bool
allFlags flags = if all (\f -> fromFlagOrDefault False f) flags
                 then Flag True
                 else NoFlag

-- ------------------------------------------------------------
-- * Global flags
-- ------------------------------------------------------------

-- In fact since individual flags types are monoids and these are just sets of
-- flags then they are also monoids pointwise. This turns out to be really
-- useful. The mempty is the set of empty flags and mappend allows us to
-- override specific flags. For example we can start with default flags and
-- override with the ones we get from a file or the command line, or both.

-- | Flags that apply at the top level, not to any sub-command.
data GlobalFlags = GlobalFlags {
    globalVersion        :: Flag Bool,
    globalNumericVersion :: Flag Bool
  }

defaultGlobalFlags :: GlobalFlags
defaultGlobalFlags  = GlobalFlags {
    globalVersion        = Flag False,
    globalNumericVersion = Flag False
  }

globalCommand :: [Command action] -> CommandUI GlobalFlags
globalCommand commands = CommandUI
  { commandName         = ""
  , commandSynopsis     = ""
  , commandUsage        = \pname ->
         "This Setup program uses the Haskell Cabal Infrastructure.\n"
      ++ "See http://www.haskell.org/cabal/ for more information.\n"
      ++ "\n"
      ++ "Usage: " ++ pname ++ " [GLOBAL FLAGS] [COMMAND [FLAGS]]\n"
  , commandDescription = Just $ \pname ->
      let
        commands' = commands ++ [commandAddAction helpCommandUI undefined]
        cmdDescs = getNormalCommandDescriptions commands'
        maxlen    = maximum $ [length name | (name, _) <- cmdDescs]
        align str = str ++ replicate (maxlen - length str) ' '
      in
         "Commands:\n"
      ++ unlines [ "  " ++ align name ++ "    " ++ description
                 | (name, description) <- cmdDescs ]
      ++ "\n"
      ++ "For more information about a command use\n"
      ++ "  " ++ pname ++ " COMMAND --help\n\n"
      ++ "Typical steps for installing Cabal packages:\n"
      ++ concat [ "  " ++ pname ++ " " ++ x ++ "\n"
                | x <- ["configure", "build", "install"]]
  , commandNotes        = Nothing
  , commandDefaultFlags = defaultGlobalFlags
  , commandOptions      = \_ ->
      [option ['V'] ["version"]
         "Print version information"
         globalVersion (\v flags -> flags { globalVersion = v })
         trueArg
      ,option [] ["numeric-version"]
         "Print just the version number"
         globalNumericVersion (\v flags -> flags { globalNumericVersion = v })
         trueArg
      ]
  }

emptyGlobalFlags :: GlobalFlags
emptyGlobalFlags = mempty

instance Monoid GlobalFlags where
  mempty = GlobalFlags {
    globalVersion        = mempty,
    globalNumericVersion = mempty
  }
  mappend = (Semi.<>)

instance Semigroup GlobalFlags where
  a <> b = GlobalFlags {
    globalVersion        = combine globalVersion,
    globalNumericVersion = combine globalNumericVersion
  }
    where combine field = field a `mappend` field b

-- ------------------------------------------------------------
-- * Config flags
-- ------------------------------------------------------------

-- | Flags to @configure@ command.
--
-- IMPORTANT: every time a new flag is added, 'D.C.Setup.filterConfigureFlags'
-- should be updated.
data ConfigFlags = ConfigFlags {
    --FIXME: the configPrograms is only here to pass info through to configure
    -- because the type of configure is constrained by the UserHooks.
    -- when we change UserHooks next we should pass the initial
    -- ProgramConfiguration directly and not via ConfigFlags
    configPrograms      :: ProgramConfiguration, -- ^All programs that cabal may
                                                 -- run

    configProgramPaths  :: [(String, FilePath)], -- ^user specified programs paths
    configProgramArgs   :: [(String, [String])], -- ^user specified programs args
    configProgramPathExtra :: NubList FilePath,  -- ^Extend the $PATH
    configHcFlavor      :: Flag CompilerFlavor, -- ^The \"flavor\" of the
                                                -- compiler, such as GHC or
                                                -- JHC.
    configHcPath        :: Flag FilePath, -- ^given compiler location
    configHcPkg         :: Flag FilePath, -- ^given hc-pkg location
    configVanillaLib    :: Flag Bool,     -- ^Enable vanilla library
    configProfLib       :: Flag Bool,     -- ^Enable profiling in the library
    configSharedLib     :: Flag Bool,     -- ^Build shared library
    configDynExe        :: Flag Bool,     -- ^Enable dynamic linking of the
                                          -- executables.
    configProfExe       :: Flag Bool,     -- ^Enable profiling in the
                                          -- executables.
    configProf          :: Flag Bool,     -- ^Enable profiling in the library
                                          -- and executables.
    configProfDetail    :: Flag ProfDetailLevel, -- ^Profiling detail level
                                          --  in the library and executables.
    configProfLibDetail :: Flag ProfDetailLevel, -- ^Profiling  detail level
                                                 -- in the library
    configConfigureArgs :: [String],      -- ^Extra arguments to @configure@
    configOptimization  :: Flag OptimisationLevel,  -- ^Enable optimization.
    configProgPrefix    :: Flag PathTemplate, -- ^Installed executable prefix.
    configProgSuffix    :: Flag PathTemplate, -- ^Installed executable suffix.
    configInstallDirs   :: InstallDirs (Flag PathTemplate), -- ^Installation
                                                            -- paths
    configScratchDir    :: Flag FilePath,
    configExtraLibDirs  :: [FilePath],   -- ^ path to search for extra libraries
    configExtraIncludeDirs :: [FilePath],   -- ^ path to search for header files
    configIPID          :: Flag String, -- ^ explicit IPID to be used

    configDistPref :: Flag FilePath, -- ^"dist" prefix
    configVerbosity :: Flag Verbosity, -- ^verbosity level
    configUserInstall :: Flag Bool,    -- ^The --user\/--global flag
    configPackageDBs :: [Maybe PackageDB], -- ^Which package DBs to use
    configGHCiLib   :: Flag Bool,      -- ^Enable compiling library for GHCi
    configSplitObjs :: Flag Bool,      -- ^Enable -split-objs with GHC
    configStripExes :: Flag Bool,      -- ^Enable executable stripping
    configStripLibs :: Flag Bool,      -- ^Enable library stripping
    configConstraints :: [Dependency], -- ^Additional constraints for
                                       -- dependencies.
    configDependencies :: [(PackageName, ComponentId)],
    configInstantiateWith :: [(ModuleName, (ComponentId, ModuleName))],
      -- ^The packages depended on.
    configConfigurationsFlags :: FlagAssignment,
    configTests               :: Flag Bool, -- ^Enable test suite compilation
    configBenchmarks          :: Flag Bool, -- ^Enable benchmark compilation
    configCoverage :: Flag Bool, -- ^Enable program coverage
    configLibCoverage :: Flag Bool, -- ^Enable program coverage (deprecated)
    configExactConfiguration  :: Flag Bool,
      -- ^All direct dependencies and flags are provided on the command line by
      -- the user via the '--dependency' and '--flags' options.
    configFlagError :: Flag String,
      -- ^Halt and show an error message indicating an error in flag assignment
    configRelocatable :: Flag Bool, -- ^ Enable relocatable package built
    configDebugInfo :: Flag DebugInfoLevel  -- ^ Emit debug info.
  }
  deriving (Generic, Read, Show)

instance Binary ConfigFlags

configAbsolutePaths :: ConfigFlags -> IO ConfigFlags
configAbsolutePaths f =
  (\v -> f { configPackageDBs = v })
  `liftM` mapM (maybe (return Nothing) (liftM Just . absolutePackageDBPath))
  (configPackageDBs f)

defaultConfigFlags :: ProgramConfiguration -> ConfigFlags
defaultConfigFlags progConf = emptyConfigFlags {
    configPrograms     = progConf,
    configHcFlavor     = maybe NoFlag Flag defaultCompilerFlavor,
    configVanillaLib   = Flag True,
    configProfLib      = NoFlag,
    configSharedLib    = NoFlag,
    configDynExe       = Flag False,
    configProfExe      = NoFlag,
    configProf         = NoFlag,
    configProfDetail   = NoFlag,
    configProfLibDetail= NoFlag,
    configOptimization = Flag NormalOptimisation,
    configProgPrefix   = Flag (toPathTemplate ""),
    configProgSuffix   = Flag (toPathTemplate ""),
    configDistPref     = NoFlag,
    configVerbosity    = Flag normal,
    configUserInstall  = Flag False,           --TODO: reverse this
#if defined(mingw32_HOST_OS)
    -- See #1589.
    configGHCiLib      = Flag True,
#else
    configGHCiLib      = NoFlag,
#endif
    configSplitObjs    = Flag False, -- takes longer, so turn off by default
    configStripExes    = Flag True,
    configStripLibs    = Flag True,
    configTests        = Flag False,
    configBenchmarks   = Flag False,
    configCoverage     = Flag False,
    configLibCoverage  = NoFlag,
    configExactConfiguration = Flag False,
    configFlagError    = NoFlag,
    configRelocatable  = Flag False,
    configDebugInfo    = Flag NoDebugInfo
  }

configureCommand :: ProgramConfiguration -> CommandUI ConfigFlags
configureCommand progConf = CommandUI
  { commandName         = "configure"
  , commandSynopsis     = "Prepare to build the package."
  , commandDescription  = Just $ \_ -> wrapText $
         "Configure how the package is built by setting "
      ++ "package (and other) flags.\n"
      ++ "\n"
      ++ "The configuration affects several other commands, "
      ++ "including build, test, bench, run, repl.\n"
  , commandNotes        = Just $ \_pname -> programFlagsDescription progConf
  , commandUsage        = \pname ->
      "Usage: " ++ pname ++ " configure [FLAGS]\n"
  , commandDefaultFlags = defaultConfigFlags progConf
  , commandOptions      = \showOrParseArgs ->
         configureOptions showOrParseArgs
      ++ programConfigurationPaths   progConf showOrParseArgs
           configProgramPaths (\v fs -> fs { configProgramPaths = v })
      ++ programConfigurationOption progConf showOrParseArgs
           configProgramArgs (\v fs -> fs { configProgramArgs = v })
      ++ programConfigurationOptions progConf showOrParseArgs
           configProgramArgs (\v fs -> fs { configProgramArgs = v })
  }

configureOptions :: ShowOrParseArgs -> [OptionField ConfigFlags]
configureOptions showOrParseArgs =
      [optionVerbosity configVerbosity
       (\v flags -> flags { configVerbosity = v })
      ,optionDistPref
         configDistPref (\d flags -> flags { configDistPref = d })
         showOrParseArgs

      ,option [] ["compiler"] "compiler"
         configHcFlavor (\v flags -> flags { configHcFlavor = v })
         (choiceOpt [ (Flag GHC,   ("g", ["ghc"]),   "compile with GHC")
                    , (Flag GHCJS, ([] , ["ghcjs"]), "compile with GHCJS")
                    , (Flag JHC,   ([] , ["jhc"]),   "compile with JHC")
                    , (Flag LHC,   ([] , ["lhc"]),   "compile with LHC")
                    , (Flag UHC,   ([] , ["uhc"]),   "compile with UHC")
                    -- "haskell-suite" compiler id string will be replaced
                    -- by a more specific one during the configure stage
                    , (Flag (HaskellSuite "haskell-suite"), ([] , ["haskell-suite"]),
                        "compile with a haskell-suite compiler")])

      ,option "w" ["with-compiler"]
         "give the path to a particular compiler"
         configHcPath (\v flags -> flags { configHcPath = v })
         (reqArgFlag "PATH")

      ,option "" ["with-hc-pkg"]
         "give the path to the package tool"
         configHcPkg (\v flags -> flags { configHcPkg = v })
         (reqArgFlag "PATH")
      ]
   ++ map liftInstallDirs installDirsOptions
   ++ [option "" ["program-prefix"]
          "prefix to be applied to installed executables"
          configProgPrefix
          (\v flags -> flags { configProgPrefix = v })
          (reqPathTemplateArgFlag "PREFIX")

      ,option "" ["program-suffix"]
          "suffix to be applied to installed executables"
          configProgSuffix (\v flags -> flags { configProgSuffix = v } )
          (reqPathTemplateArgFlag "SUFFIX")

      ,option "" ["library-vanilla"]
         "Vanilla libraries"
         configVanillaLib (\v flags -> flags { configVanillaLib = v })
         (boolOpt [] [])

      ,option "p" ["library-profiling"]
         "Library profiling"
         configProfLib (\v flags -> flags { configProfLib = v })
         (boolOpt "p" [])

      ,option "" ["shared"]
         "Shared library"
         configSharedLib (\v flags -> flags { configSharedLib = v })
         (boolOpt [] [])

      ,option "" ["executable-dynamic"]
         "Executable dynamic linking"
         configDynExe (\v flags -> flags { configDynExe = v })
         (boolOpt [] [])

      ,option "" ["profiling"]
         "Executable and library profiling"
         configProf (\v flags -> flags { configProf = v })
         (boolOpt [] [])

      ,option "" ["executable-profiling"]
         "Executable profiling (DEPRECATED)"
         configProfExe (\v flags -> flags { configProfExe = v })
         (boolOpt [] [])

      ,option "" ["profiling-detail"]
         ("Profiling detail level for executable and library (default, " ++
          "none, exported-functions, toplevel-functions,  all-functions).")
         configProfDetail (\v flags -> flags { configProfDetail = v })
         (reqArg' "level" (Flag . flagToProfDetailLevel)
                          showProfDetailLevelFlag)

      ,option "" ["library-profiling-detail"]
         "Profiling detail level for libraries only."
         configProfLibDetail (\v flags -> flags { configProfLibDetail = v })
         (reqArg' "level" (Flag . flagToProfDetailLevel)
                          showProfDetailLevelFlag)

      ,multiOption "optimization"
         configOptimization (\v flags -> flags { configOptimization = v })
         [optArg' "n" (Flag . flagToOptimisationLevel)
                     (\f -> case f of
                              Flag NoOptimisation      -> []
                              Flag NormalOptimisation  -> [Nothing]
                              Flag MaximumOptimisation -> [Just "2"]
                              _                        -> [])
                 "O" ["enable-optimization","enable-optimisation"]
                 "Build with optimization (n is 0--2, default is 1)",
          noArg (Flag NoOptimisation) []
                ["disable-optimization","disable-optimisation"]
                "Build without optimization"
         ]

      ,multiOption "debug-info"
         configDebugInfo (\v flags -> flags { configDebugInfo = v })
         [optArg' "n" (Flag . flagToDebugInfoLevel)
                     (\f -> case f of
                              Flag NoDebugInfo      -> []
                              Flag MinimalDebugInfo -> [Just "1"]
                              Flag NormalDebugInfo  -> [Nothing]
                              Flag MaximalDebugInfo -> [Just "3"]
                              _                     -> [])
                 "" ["enable-debug-info"]
                 "Emit debug info (n is 0--3, default is 0)",
          noArg (Flag NoDebugInfo) []
                ["disable-debug-info"]
                "Don't emit debug info"
         ]

      ,option "" ["library-for-ghci"]
         "compile library for use with GHCi"
         configGHCiLib (\v flags -> flags { configGHCiLib = v })
         (boolOpt [] [])

      ,option "" ["split-objs"]
         "split library into smaller objects to reduce binary sizes (GHC 6.6+)"
         configSplitObjs (\v flags -> flags { configSplitObjs = v })
         (boolOpt [] [])

      ,option "" ["executable-stripping"]
         "strip executables upon installation to reduce binary sizes"
         configStripExes (\v flags -> flags { configStripExes = v })
         (boolOpt [] [])

      ,option "" ["library-stripping"]
         "strip libraries upon installation to reduce binary sizes"
         configStripLibs (\v flags -> flags { configStripLibs = v })
         (boolOpt [] [])

      ,option "" ["configure-option"]
         "Extra option for configure"
         configConfigureArgs (\v flags -> flags { configConfigureArgs = v })
         (reqArg' "OPT" (\x -> [x]) id)

      ,option "" ["user-install"]
         "doing a per-user installation"
         configUserInstall (\v flags -> flags { configUserInstall = v })
         (boolOpt' ([],["user"]) ([], ["global"]))

      ,option "" ["package-db"]
         (   "Append the given package database to the list of package"
          ++ " databases used (to satisfy dependencies and register into)."
          ++ " May be a specific file, 'global' or 'user'. The initial list"
          ++ " is ['global'], ['global', 'user'], or ['global', $sandbox],"
          ++ " depending on context. Use 'clear' to reset the list to empty."
          ++ " See the user guide for details.")
         configPackageDBs (\v flags -> flags { configPackageDBs = v })
         (reqArg' "DB" readPackageDbList showPackageDbList)

      ,option "f" ["flags"]
         "Force values for the given flags in Cabal conditionals in the .cabal file.  E.g., --flags=\"debug -usebytestrings\" forces the flag \"debug\" to true and \"usebytestrings\" to false."
         configConfigurationsFlags (\v flags -> flags { configConfigurationsFlags = v })
         (reqArg' "FLAGS" readFlagList showFlagList)

      ,option "" ["extra-include-dirs"]
         "A list of directories to search for header files"
         configExtraIncludeDirs (\v flags -> flags {configExtraIncludeDirs = v})
         (reqArg' "PATH" (\x -> [x]) id)

      ,option "" ["ipid"]
         "Installed package ID to compile this package as"
         configIPID (\v flags -> flags {configIPID = v})
         (reqArgFlag "IPID")

      ,option "" ["extra-lib-dirs"]
         "A list of directories to search for external libraries"
         configExtraLibDirs (\v flags -> flags {configExtraLibDirs = v})
         (reqArg' "PATH" (\x -> [x]) id)

      ,option "" ["extra-prog-path"]
         "A list of directories to search for required programs (in addition to the normal search locations)"
         configProgramPathExtra (\v flags -> flags {configProgramPathExtra = v})
         (reqArg' "PATH" (\x -> toNubList [x]) fromNubList)

      ,option "" ["constraint"]
         "A list of additional constraints on the dependencies."
         configConstraints (\v flags -> flags { configConstraints = v})
         (reqArg "DEPENDENCY"
                 (readP_to_E (const "dependency expected") ((\x -> [x]) `fmap` parse))
                 (map (\x -> display x)))

      ,option "" ["dependency"]
         "A list of exact dependencies. E.g., --dependency=\"void=void-0.5.8-177d5cdf20962d0581fe2e4932a6c309\""
         configDependencies (\v flags -> flags { configDependencies = v})
         (reqArg "NAME=ID"
                 (readP_to_E (const "dependency expected") ((\x -> [x]) `fmap` parseDependency))
                 (map (\x -> display (fst x) ++ "=" ++ display (snd x))))

      ,option "" ["instantiate-with"]
         "A mapping of signature names to concrete module instantiations. E.g., --instantiate-with=\"Map=Data.Map.Strict@containers-0.5.5.1-inplace\""
         configInstantiateWith (\v flags -> flags { configInstantiateWith = v })
         (reqArg "NAME=PKG:MOD"
                 (readP_to_E (const "signature mapping expected") ((\x -> [x]) `fmap` parseHoleMapEntry))
                 (map (\(n,(p,m)) -> display n ++ "=" ++ display m ++ "@" ++ display p)))

      ,option "" ["tests"]
         "dependency checking and compilation for test suites listed in the package description file."
         configTests (\v flags -> flags { configTests = v })
         (boolOpt [] [])

      ,option "" ["coverage"]
         "build package with Haskell Program Coverage. (GHC only)"
         configCoverage (\v flags -> flags { configCoverage = v })
         (boolOpt [] [])

      ,option "" ["library-coverage"]
         "build package with Haskell Program Coverage. (GHC only) (DEPRECATED)"
         configLibCoverage (\v flags -> flags { configLibCoverage = v })
         (boolOpt [] [])

      ,option "" ["exact-configuration"]
         "All direct dependencies and flags are provided on the command line."
         configExactConfiguration
         (\v flags -> flags { configExactConfiguration = v })
         trueArg

      ,option "" ["benchmarks"]
         "dependency checking and compilation for benchmarks listed in the package description file."
         configBenchmarks (\v flags -> flags { configBenchmarks = v })
         (boolOpt [] [])

      ,option "" ["relocatable"]
         "building a package that is relocatable. (GHC only)"
         configRelocatable (\v flags -> flags { configRelocatable = v})
         (boolOpt [] [])
      ]
  where
    readFlagList :: String -> FlagAssignment
    readFlagList = map tagWithValue . words
      where tagWithValue ('-':fname) = (FlagName (lowercase fname), False)
            tagWithValue fname       = (FlagName (lowercase fname), True)

    showFlagList :: FlagAssignment -> [String]
    showFlagList fs = [ if not set then '-':fname else fname
                      | (FlagName fname, set) <- fs]

    liftInstallDirs =
      liftOption configInstallDirs (\v flags -> flags { configInstallDirs = v })

    reqPathTemplateArgFlag title _sf _lf d get set =
      reqArgFlag title _sf _lf d
        (fmap fromPathTemplate . get) (set . fmap toPathTemplate)

readPackageDbList :: String -> [Maybe PackageDB]
readPackageDbList "clear"  = [Nothing]
readPackageDbList "global" = [Just GlobalPackageDB]
readPackageDbList "user"   = [Just UserPackageDB]
readPackageDbList other    = [Just (SpecificPackageDB other)]

showPackageDbList :: [Maybe PackageDB] -> [String]
showPackageDbList = map showPackageDb
  where
    showPackageDb Nothing                       = "clear"
    showPackageDb (Just GlobalPackageDB)        = "global"
    showPackageDb (Just UserPackageDB)          = "user"
    showPackageDb (Just (SpecificPackageDB db)) = db

showProfDetailLevelFlag :: Flag ProfDetailLevel -> [String]
showProfDetailLevelFlag NoFlag    = []
showProfDetailLevelFlag (Flag dl) = [showProfDetailLevel dl]

parseDependency :: Parse.ReadP r (PackageName, ComponentId)
parseDependency = do
  x <- parse
  _ <- Parse.char '='
  y <- parse
  return (x, y)

parseHoleMapEntry :: Parse.ReadP r (ModuleName, (ComponentId, ModuleName))
parseHoleMapEntry = do
  x <- parse
  _ <- Parse.char '='
  y <- parse
  _ <- Parse.char '@'
  z <- parse
  return (x, (z, y))

installDirsOptions :: [OptionField (InstallDirs (Flag PathTemplate))]
installDirsOptions =
  [ option "" ["prefix"]
      "bake this prefix in preparation of installation"
      prefix (\v flags -> flags { prefix = v })
      installDirArg

  , option "" ["bindir"]
      "installation directory for executables"
      bindir (\v flags -> flags { bindir = v })
      installDirArg

  , option "" ["libdir"]
      "installation directory for libraries"
      libdir (\v flags -> flags { libdir = v })
      installDirArg

  , option "" ["libsubdir"]
      "subdirectory of libdir in which libs are installed"
      libsubdir (\v flags -> flags { libsubdir = v })
      installDirArg

  , option "" ["libexecdir"]
      "installation directory for program executables"
      libexecdir (\v flags -> flags { libexecdir = v })
      installDirArg

  , option "" ["datadir"]
      "installation directory for read-only data"
      datadir (\v flags -> flags { datadir = v })
      installDirArg

  , option "" ["datasubdir"]
      "subdirectory of datadir in which data files are installed"
      datasubdir (\v flags -> flags { datasubdir = v })
      installDirArg

  , option "" ["docdir"]
      "installation directory for documentation"
      docdir (\v flags -> flags { docdir = v })
      installDirArg

  , option "" ["htmldir"]
      "installation directory for HTML documentation"
      htmldir (\v flags -> flags { htmldir = v })
      installDirArg

  , option "" ["haddockdir"]
      "installation directory for haddock interfaces"
      haddockdir (\v flags -> flags { haddockdir = v })
      installDirArg

  , option "" ["sysconfdir"]
      "installation directory for configuration files"
      sysconfdir (\v flags -> flags { sysconfdir = v })
      installDirArg
  ]
  where
    installDirArg _sf _lf d get set =
      reqArgFlag "DIR" _sf _lf d
        (fmap fromPathTemplate . get) (set . fmap toPathTemplate)

emptyConfigFlags :: ConfigFlags
emptyConfigFlags = mempty

instance Monoid ConfigFlags where
  mempty = ConfigFlags {
    configPrograms      = error "FIXME: remove configPrograms",
    configProgramPaths  = mempty,
    configProgramArgs   = mempty,
    configProgramPathExtra = mempty,
    configHcFlavor      = mempty,
    configHcPath        = mempty,
    configHcPkg         = mempty,
    configVanillaLib    = mempty,
    configProfLib       = mempty,
    configSharedLib     = mempty,
    configDynExe        = mempty,
    configProfExe       = mempty,
    configProf          = mempty,
    configProfDetail    = mempty,
    configProfLibDetail = mempty,
    configConfigureArgs = mempty,
    configOptimization  = mempty,
    configProgPrefix    = mempty,
    configProgSuffix    = mempty,
    configInstallDirs   = mempty,
    configScratchDir    = mempty,
    configDistPref      = mempty,
    configVerbosity     = mempty,
    configUserInstall   = mempty,
    configPackageDBs    = mempty,
    configGHCiLib       = mempty,
    configSplitObjs     = mempty,
    configStripExes     = mempty,
    configStripLibs     = mempty,
    configExtraLibDirs  = mempty,
    configConstraints   = mempty,
    configDependencies  = mempty,
    configInstantiateWith     = mempty,
    configExtraIncludeDirs    = mempty,
    configIPID          = mempty,
    configConfigurationsFlags = mempty,
    configTests               = mempty,
    configCoverage         = mempty,
    configLibCoverage   = mempty,
    configExactConfiguration  = mempty,
    configBenchmarks          = mempty,
    configFlagError     = mempty,
    configRelocatable   = mempty,
    configDebugInfo     = mempty
  }
  mappend = (Semi.<>)

instance Semigroup ConfigFlags where
  a <> b =  ConfigFlags {
    configPrograms      = configPrograms b,
    configProgramPaths  = combine configProgramPaths,
    configProgramArgs   = combine configProgramArgs,
    configProgramPathExtra = combine configProgramPathExtra,
    configHcFlavor      = combine configHcFlavor,
    configHcPath        = combine configHcPath,
    configHcPkg         = combine configHcPkg,
    configVanillaLib    = combine configVanillaLib,
    configProfLib       = combine configProfLib,
    configSharedLib     = combine configSharedLib,
    configDynExe        = combine configDynExe,
    configProfExe       = combine configProfExe,
    configProf          = combine configProf,
    configProfDetail    = combine configProfDetail,
    configProfLibDetail = combine configProfLibDetail,
    configConfigureArgs = combine configConfigureArgs,
    configOptimization  = combine configOptimization,
    configProgPrefix    = combine configProgPrefix,
    configProgSuffix    = combine configProgSuffix,
    configInstallDirs   = combine configInstallDirs,
    configScratchDir    = combine configScratchDir,
    configDistPref      = combine configDistPref,
    configVerbosity     = combine configVerbosity,
    configUserInstall   = combine configUserInstall,
    configPackageDBs    = combine configPackageDBs,
    configGHCiLib       = combine configGHCiLib,
    configSplitObjs     = combine configSplitObjs,
    configStripExes     = combine configStripExes,
    configStripLibs     = combine configStripLibs,
    configExtraLibDirs  = combine configExtraLibDirs,
    configConstraints   = combine configConstraints,
    configDependencies  = combine configDependencies,
    configInstantiateWith     = combine configInstantiateWith,
    configExtraIncludeDirs    = combine configExtraIncludeDirs,
    configIPID          = combine configIPID,
    configConfigurationsFlags = combine configConfigurationsFlags,
    configTests               = combine configTests,
    configCoverage         = combine configCoverage,
    configLibCoverage         = combine configLibCoverage,
    configExactConfiguration  = combine configExactConfiguration,
    configBenchmarks          = combine configBenchmarks,
    configFlagError     = combine configFlagError,
    configRelocatable   = combine configRelocatable,
    configDebugInfo     = combine configDebugInfo
  }
    where combine field = field a `mappend` field b

-- ------------------------------------------------------------
-- * Copy flags
-- ------------------------------------------------------------

-- | Flags to @copy@: (destdir, copy-prefix (backwards compat), verbosity)
data CopyFlags = CopyFlags {
    copyDest      :: Flag CopyDest,
    copyDistPref  :: Flag FilePath,
    copyVerbosity :: Flag Verbosity
  }
  deriving Show

defaultCopyFlags :: CopyFlags
defaultCopyFlags  = CopyFlags {
    copyDest      = Flag NoCopyDest,
    copyDistPref  = NoFlag,
    copyVerbosity = Flag normal
  }

copyCommand :: CommandUI CopyFlags
copyCommand = CommandUI
  { commandName         = "copy"
  , commandSynopsis     = "Copy the files into the install locations."
  , commandDescription  = Just $ \_ -> wrapText $
          "Does not call register, and allows a prefix at install time. "
       ++ "Without the --destdir flag, configure determines location.\n"
  , commandNotes        = Nothing
  , commandUsage        = \pname ->
      "Usage: " ++ pname ++ " copy [FLAGS]\n"
  , commandDefaultFlags = defaultCopyFlags
  , commandOptions      = \showOrParseArgs ->
      [optionVerbosity copyVerbosity (\v flags -> flags { copyVerbosity = v })

      ,optionDistPref
         copyDistPref (\d flags -> flags { copyDistPref = d })
         showOrParseArgs

      ,option "" ["destdir"]
         "directory to copy files to, prepended to installation directories"
         copyDest (\v flags -> flags { copyDest = v })
         (reqArg "DIR" (succeedReadE (Flag . CopyTo))
                       (\f -> case f of Flag (CopyTo p) -> [p]; _ -> []))
      ]
  }

emptyCopyFlags :: CopyFlags
emptyCopyFlags = mempty

instance Monoid CopyFlags where
  mempty = CopyFlags {
    copyDest      = mempty,
    copyDistPref  = mempty,
    copyVerbosity = mempty
  }
  mappend = (Semi.<>)

instance Semigroup CopyFlags where
  a <> b = CopyFlags {
    copyDest      = combine copyDest,
    copyDistPref  = combine copyDistPref,
    copyVerbosity = combine copyVerbosity
  }
    where combine field = field a `mappend` field b

-- ------------------------------------------------------------
-- * Install flags
-- ------------------------------------------------------------

-- | Flags to @install@: (package db, verbosity)
data InstallFlags = InstallFlags {
    installPackageDB :: Flag PackageDB,
    installDistPref  :: Flag FilePath,
    installUseWrapper :: Flag Bool,
    installInPlace    :: Flag Bool,
    installVerbosity :: Flag Verbosity
  }
  deriving Show

defaultInstallFlags :: InstallFlags
defaultInstallFlags  = InstallFlags {
    installPackageDB = NoFlag,
    installDistPref  = NoFlag,
    installUseWrapper = Flag False,
    installInPlace    = Flag False,
    installVerbosity = Flag normal
  }

installCommand :: CommandUI InstallFlags
installCommand = CommandUI
  { commandName         = "install"
  , commandSynopsis     =
      "Copy the files into the install locations. Run register."
  , commandDescription  = Just $ \_ -> wrapText $
         "Unlike the copy command, install calls the register command."
      ++ "If you want to install into a location that is not what was"
      ++ "specified in the configure step, use the copy command.\n"
  , commandNotes        = Nothing
  , commandUsage        = \pname ->
      "Usage: " ++ pname ++ " install [FLAGS]\n"
  , commandDefaultFlags = defaultInstallFlags
  , commandOptions      = \showOrParseArgs ->
      [optionVerbosity installVerbosity (\v flags -> flags { installVerbosity = v })
      ,optionDistPref
         installDistPref (\d flags -> flags { installDistPref = d })
         showOrParseArgs

      ,option "" ["inplace"]
         "install the package in the install subdirectory of the dist prefix, so it can be used without being installed"
         installInPlace (\v flags -> flags { installInPlace = v })
         trueArg

      ,option "" ["shell-wrappers"]
         "using shell script wrappers around executables"
         installUseWrapper (\v flags -> flags { installUseWrapper = v })
         (boolOpt [] [])

      ,option "" ["package-db"] ""
         installPackageDB (\v flags -> flags { installPackageDB = v })
         (choiceOpt [ (Flag UserPackageDB, ([],["user"]),
                      "upon configuration register this package in the user's local package database")
                    , (Flag GlobalPackageDB, ([],["global"]),
                      "(default) upon configuration register this package in the system-wide package database")])
      ]
  }

emptyInstallFlags :: InstallFlags
emptyInstallFlags = mempty

instance Monoid InstallFlags where
  mempty = InstallFlags{
    installPackageDB = mempty,
    installDistPref  = mempty,
    installUseWrapper = mempty,
    installInPlace    = mempty,
    installVerbosity = mempty
  }
  mappend = (Semi.<>)

instance Semigroup InstallFlags where
  a <> b = InstallFlags{
    installPackageDB = combine installPackageDB,
    installDistPref  = combine installDistPref,
    installUseWrapper = combine installUseWrapper,
    installInPlace    = combine installInPlace,
    installVerbosity = combine installVerbosity
  }
    where combine field = field a `mappend` field b

-- ------------------------------------------------------------
-- * SDist flags
-- ------------------------------------------------------------

-- | Flags to @sdist@: (snapshot, verbosity)
data SDistFlags = SDistFlags {
    sDistSnapshot    :: Flag Bool,
    sDistDirectory   :: Flag FilePath,
    sDistDistPref    :: Flag FilePath,
    sDistListSources :: Flag FilePath,
    sDistVerbosity   :: Flag Verbosity
  }
  deriving Show

defaultSDistFlags :: SDistFlags
defaultSDistFlags = SDistFlags {
    sDistSnapshot    = Flag False,
    sDistDirectory   = mempty,
    sDistDistPref    = NoFlag,
    sDistListSources = mempty,
    sDistVerbosity   = Flag normal
  }

sdistCommand :: CommandUI SDistFlags
sdistCommand = CommandUI
  { commandName         = "sdist"
  , commandSynopsis     =
      "Generate a source distribution file (.tar.gz)."
  , commandDescription  = Nothing
  , commandNotes        = Nothing
  , commandUsage        = \pname ->
      "Usage: " ++ pname ++ " sdist [FLAGS]\n"
  , commandDefaultFlags = defaultSDistFlags
  , commandOptions      = \showOrParseArgs ->
      [optionVerbosity sDistVerbosity (\v flags -> flags { sDistVerbosity = v })
      ,optionDistPref
         sDistDistPref (\d flags -> flags { sDistDistPref = d })
         showOrParseArgs

      ,option "" ["list-sources"]
         "Just write a list of the package's sources to a file"
         sDistListSources (\v flags -> flags { sDistListSources = v })
         (reqArgFlag "FILE")

      ,option "" ["snapshot"]
         "Produce a snapshot source distribution"
         sDistSnapshot (\v flags -> flags { sDistSnapshot = v })
         trueArg

      ,option "" ["output-directory"]
       ("Generate a source distribution in the given directory, "
        ++ "without creating a tarball")
         sDistDirectory (\v flags -> flags { sDistDirectory = v })
         (reqArgFlag "DIR")
      ]
  }

emptySDistFlags :: SDistFlags
emptySDistFlags = mempty

instance Monoid SDistFlags where
  mempty = SDistFlags {
    sDistSnapshot    = mempty,
    sDistDirectory   = mempty,
    sDistDistPref    = mempty,
    sDistListSources = mempty,
    sDistVerbosity   = mempty
  }
  mappend = (Semi.<>)

instance Semigroup SDistFlags where
  a <> b = SDistFlags {
    sDistSnapshot    = combine sDistSnapshot,
    sDistDirectory   = combine sDistDirectory,
    sDistDistPref    = combine sDistDistPref,
    sDistListSources = combine sDistListSources,
    sDistVerbosity   = combine sDistVerbosity
  }
    where combine field = field a `mappend` field b

-- ------------------------------------------------------------
-- * Register flags
-- ------------------------------------------------------------

-- | Flags to @register@ and @unregister@: (user package, gen-script,
-- in-place, verbosity)
data RegisterFlags = RegisterFlags {
    regPackageDB   :: Flag PackageDB,
    regGenScript   :: Flag Bool,
    regGenPkgConf  :: Flag (Maybe FilePath),
    regInPlace     :: Flag Bool,
    regDistPref    :: Flag FilePath,
    regPrintId     :: Flag Bool,
    regVerbosity   :: Flag Verbosity
  }
  deriving Show

defaultRegisterFlags :: RegisterFlags
defaultRegisterFlags = RegisterFlags {
    regPackageDB   = NoFlag,
    regGenScript   = Flag False,
    regGenPkgConf  = NoFlag,
    regInPlace     = Flag False,
    regDistPref    = NoFlag,
    regPrintId     = Flag False,
    regVerbosity   = Flag normal
  }

registerCommand :: CommandUI RegisterFlags
registerCommand = CommandUI
  { commandName         = "register"
  , commandSynopsis     =
      "Register this package with the compiler."
  , commandDescription  = Nothing
  , commandNotes        = Nothing
  , commandUsage        = \pname ->
      "Usage: " ++ pname ++ " register [FLAGS]\n"
  , commandDefaultFlags = defaultRegisterFlags
  , commandOptions      = \showOrParseArgs ->
      [optionVerbosity regVerbosity (\v flags -> flags { regVerbosity = v })
      ,optionDistPref
         regDistPref (\d flags -> flags { regDistPref = d })
         showOrParseArgs

      ,option "" ["packageDB"] ""
         regPackageDB (\v flags -> flags { regPackageDB = v })
         (choiceOpt [ (Flag UserPackageDB, ([],["user"]),
                                "upon registration, register this package in the user's local package database")
                    , (Flag GlobalPackageDB, ([],["global"]),
                                "(default)upon registration, register this package in the system-wide package database")])

      ,option "" ["inplace"]
         "register the package in the build location, so it can be used without being installed"
         regInPlace (\v flags -> flags { regInPlace = v })
         trueArg

      ,option "" ["gen-script"]
         "instead of registering, generate a script to register later"
         regGenScript (\v flags -> flags { regGenScript = v })
         trueArg

      ,option "" ["gen-pkg-config"]
         "instead of registering, generate a package registration file"
         regGenPkgConf (\v flags -> flags { regGenPkgConf  = v })
         (optArg' "PKG" Flag flagToList)

      ,option "" ["print-ipid"]
         "print the installed package ID calculated for this package"
         regPrintId (\v flags -> flags { regPrintId = v })
         trueArg
      ]
  }

unregisterCommand :: CommandUI RegisterFlags
unregisterCommand = CommandUI
  { commandName         = "unregister"
  , commandSynopsis     =
      "Unregister this package with the compiler."
  , commandDescription  = Nothing
  , commandNotes        = Nothing
  , commandUsage        = \pname ->
      "Usage: " ++ pname ++ " unregister [FLAGS]\n"
  , commandDefaultFlags = defaultRegisterFlags
  , commandOptions      = \showOrParseArgs ->
      [optionVerbosity regVerbosity (\v flags -> flags { regVerbosity = v })
      ,optionDistPref
         regDistPref (\d flags -> flags { regDistPref = d })
          showOrParseArgs

      ,option "" ["user"] ""
         regPackageDB (\v flags -> flags { regPackageDB = v })
         (choiceOpt [ (Flag UserPackageDB, ([],["user"]),
                              "unregister this package in the user's local package database")
                    , (Flag GlobalPackageDB, ([],["global"]),
                              "(default) unregister this package in the  system-wide package database")])

      ,option "" ["gen-script"]
         "Instead of performing the unregister command, generate a script to unregister later"
         regGenScript (\v flags -> flags { regGenScript = v })
         trueArg
      ]
  }

emptyRegisterFlags :: RegisterFlags
emptyRegisterFlags = mempty

instance Monoid RegisterFlags where
  mempty = RegisterFlags {
    regPackageDB   = mempty,
    regGenScript   = mempty,
    regGenPkgConf  = mempty,
    regInPlace     = mempty,
    regPrintId     = mempty,
    regDistPref    = mempty,
    regVerbosity   = mempty
  }
  mappend = (Semi.<>)

instance Semigroup RegisterFlags where
  a <> b = RegisterFlags {
    regPackageDB   = combine regPackageDB,
    regGenScript   = combine regGenScript,
    regGenPkgConf  = combine regGenPkgConf,
    regInPlace     = combine regInPlace,
    regPrintId     = combine regPrintId,
    regDistPref    = combine regDistPref,
    regVerbosity   = combine regVerbosity
  }
    where combine field = field a `mappend` field b

-- ------------------------------------------------------------
-- * HsColour flags
-- ------------------------------------------------------------

data HscolourFlags = HscolourFlags {
    hscolourCSS         :: Flag FilePath,
    hscolourExecutables :: Flag Bool,
    hscolourTestSuites  :: Flag Bool,
    hscolourBenchmarks  :: Flag Bool,
    hscolourDistPref    :: Flag FilePath,
    hscolourVerbosity   :: Flag Verbosity
  }
  deriving Show

emptyHscolourFlags :: HscolourFlags
emptyHscolourFlags = mempty

defaultHscolourFlags :: HscolourFlags
defaultHscolourFlags = HscolourFlags {
    hscolourCSS         = NoFlag,
    hscolourExecutables = Flag False,
    hscolourTestSuites  = Flag False,
    hscolourBenchmarks  = Flag False,
    hscolourDistPref    = NoFlag,
    hscolourVerbosity   = Flag normal
  }

instance Monoid HscolourFlags where
  mempty = HscolourFlags {
    hscolourCSS         = mempty,
    hscolourExecutables = mempty,
    hscolourTestSuites  = mempty,
    hscolourBenchmarks  = mempty,
    hscolourDistPref    = mempty,
    hscolourVerbosity   = mempty
  }
  mappend = (Semi.<>)

instance Semigroup HscolourFlags where
  a <> b = HscolourFlags {
    hscolourCSS         = combine hscolourCSS,
    hscolourExecutables = combine hscolourExecutables,
    hscolourTestSuites  = combine hscolourTestSuites,
    hscolourBenchmarks  = combine hscolourBenchmarks,
    hscolourDistPref    = combine hscolourDistPref,
    hscolourVerbosity   = combine hscolourVerbosity
  }
    where combine field = field a `mappend` field b

hscolourCommand :: CommandUI HscolourFlags
hscolourCommand = CommandUI
  { commandName         = "hscolour"
  , commandSynopsis     =
      "Generate HsColour colourised code, in HTML format."
  , commandDescription  = Just (\_ -> "Requires the hscolour program.\n")
  , commandNotes        = Nothing
  , commandUsage        = \pname ->
      "Usage: " ++ pname ++ " hscolour [FLAGS]\n"
  , commandDefaultFlags = defaultHscolourFlags
  , commandOptions      = \showOrParseArgs ->
      [optionVerbosity hscolourVerbosity
       (\v flags -> flags { hscolourVerbosity = v })
      ,optionDistPref
         hscolourDistPref (\d flags -> flags { hscolourDistPref = d })
         showOrParseArgs

      ,option "" ["executables"]
         "Run hscolour for Executables targets"
         hscolourExecutables (\v flags -> flags { hscolourExecutables = v })
         trueArg

      ,option "" ["tests"]
         "Run hscolour for Test Suite targets"
         hscolourTestSuites (\v flags -> flags { hscolourTestSuites = v })
         trueArg

      ,option "" ["benchmarks"]
         "Run hscolour for Benchmark targets"
         hscolourBenchmarks (\v flags -> flags { hscolourBenchmarks = v })
         trueArg

      ,option "" ["all"]
         "Run hscolour for all targets"
         (\f -> allFlags [ hscolourExecutables f
                         , hscolourTestSuites  f
                         , hscolourBenchmarks  f])
         (\v flags -> flags { hscolourExecutables = v
                            , hscolourTestSuites  = v
                            , hscolourBenchmarks  = v })
         trueArg

      ,option "" ["css"]
         "Use a cascading style sheet"
         hscolourCSS (\v flags -> flags { hscolourCSS = v })
         (reqArgFlag "PATH")
      ]
  }

-- ------------------------------------------------------------
-- * Haddock flags
-- ------------------------------------------------------------

data HaddockFlags = HaddockFlags {
    haddockProgramPaths :: [(String, FilePath)],
    haddockProgramArgs  :: [(String, [String])],
    haddockHoogle       :: Flag Bool,
    haddockHtml         :: Flag Bool,
    haddockHtmlLocation :: Flag String,
    haddockForHackage   :: Flag Bool,
    haddockExecutables  :: Flag Bool,
    haddockTestSuites   :: Flag Bool,
    haddockBenchmarks   :: Flag Bool,
    haddockInternal     :: Flag Bool,
    haddockCss          :: Flag FilePath,
    haddockHscolour     :: Flag Bool,
    haddockHscolourCss  :: Flag FilePath,
    haddockContents     :: Flag PathTemplate,
    haddockDistPref     :: Flag FilePath,
    haddockKeepTempFiles:: Flag Bool,
    haddockVerbosity    :: Flag Verbosity
  }
  deriving Show

defaultHaddockFlags :: HaddockFlags
defaultHaddockFlags  = HaddockFlags {
    haddockProgramPaths = mempty,
    haddockProgramArgs  = [],
    haddockHoogle       = Flag False,
    haddockHtml         = Flag False,
    haddockHtmlLocation = NoFlag,
    haddockForHackage   = Flag False,
    haddockExecutables  = Flag False,
    haddockTestSuites   = Flag False,
    haddockBenchmarks   = Flag False,
    haddockInternal     = Flag False,
    haddockCss          = NoFlag,
    haddockHscolour     = Flag False,
    haddockHscolourCss  = NoFlag,
    haddockContents     = NoFlag,
    haddockDistPref     = NoFlag,
    haddockKeepTempFiles= Flag False,
    haddockVerbosity    = Flag normal
  }

haddockCommand :: CommandUI HaddockFlags
haddockCommand = CommandUI
  { commandName         = "haddock"
  , commandSynopsis     = "Generate Haddock HTML documentation."
  , commandDescription  = Just $ \_ ->
      "Requires the program haddock, version 2.x.\n"
  , commandNotes        = Nothing
  , commandUsage        = \pname ->
      "Usage: " ++ pname ++ " haddock [FLAGS]\n"
  , commandDefaultFlags = defaultHaddockFlags
  , commandOptions      = \showOrParseArgs ->
         haddockOptions showOrParseArgs
      ++ programConfigurationPaths   progConf ParseArgs
             haddockProgramPaths (\v flags -> flags { haddockProgramPaths = v})
      ++ programConfigurationOption  progConf showOrParseArgs
             haddockProgramArgs (\v fs -> fs { haddockProgramArgs = v })
      ++ programConfigurationOptions progConf ParseArgs
             haddockProgramArgs  (\v flags -> flags { haddockProgramArgs = v})
  }
  where
    progConf = addKnownProgram haddockProgram
             $ addKnownProgram ghcProgram
             $ emptyProgramConfiguration

haddockOptions :: ShowOrParseArgs -> [OptionField HaddockFlags]
haddockOptions showOrParseArgs =
  [optionVerbosity haddockVerbosity
   (\v flags -> flags { haddockVerbosity = v })
  ,optionDistPref
   haddockDistPref (\d flags -> flags { haddockDistPref = d })
   showOrParseArgs

  ,option "" ["keep-temp-files"]
   "Keep temporary files"
   haddockKeepTempFiles (\b flags -> flags { haddockKeepTempFiles = b })
   trueArg

  ,option "" ["hoogle"]
   "Generate a hoogle database"
   haddockHoogle (\v flags -> flags { haddockHoogle = v })
   trueArg

  ,option "" ["html"]
   "Generate HTML documentation (the default)"
   haddockHtml (\v flags -> flags { haddockHtml = v })
   trueArg

  ,option "" ["html-location"]
   "Location of HTML documentation for pre-requisite packages"
   haddockHtmlLocation (\v flags -> flags { haddockHtmlLocation = v })
   (reqArgFlag "URL")

  ,option "" ["for-hackage"]
   "Collection of flags to generate documentation suitable for upload to hackage"
   haddockForHackage (\v flags -> flags { haddockForHackage = v })
   trueArg

  ,option "" ["executables"]
   "Run haddock for Executables targets"
   haddockExecutables (\v flags -> flags { haddockExecutables = v })
   trueArg

  ,option "" ["tests"]
   "Run haddock for Test Suite targets"
   haddockTestSuites (\v flags -> flags { haddockTestSuites = v })
   trueArg

  ,option "" ["benchmarks"]
   "Run haddock for Benchmark targets"
   haddockBenchmarks (\v flags -> flags { haddockBenchmarks = v })
   trueArg

  ,option "" ["all"]
   "Run haddock for all targets"
   (\f -> allFlags [ haddockExecutables f
                   , haddockTestSuites  f
                   , haddockBenchmarks  f])
         (\v flags -> flags { haddockExecutables = v
                            , haddockTestSuites  = v
                            , haddockBenchmarks  = v })
         trueArg

  ,option "" ["internal"]
   "Run haddock for internal modules and include all symbols"
   haddockInternal (\v flags -> flags { haddockInternal = v })
   trueArg

  ,option "" ["css"]
   "Use PATH as the haddock stylesheet"
   haddockCss (\v flags -> flags { haddockCss = v })
   (reqArgFlag "PATH")

  ,option "" ["hyperlink-source","hyperlink-sources"]
   "Hyperlink the documentation to the source code (using HsColour)"
   haddockHscolour (\v flags -> flags { haddockHscolour = v })
   trueArg

  ,option "" ["hscolour-css"]
   "Use PATH as the HsColour stylesheet"
   haddockHscolourCss (\v flags -> flags { haddockHscolourCss = v })
   (reqArgFlag "PATH")

  ,option "" ["contents-location"]
   "Bake URL in as the location for the contents page"
   haddockContents (\v flags -> flags { haddockContents = v })
   (reqArg' "URL"
    (toFlag . toPathTemplate)
    (flagToList . fmap fromPathTemplate))
  ]

emptyHaddockFlags :: HaddockFlags
emptyHaddockFlags = mempty

instance Monoid HaddockFlags where
  mempty = HaddockFlags {
    haddockProgramPaths = mempty,
    haddockProgramArgs  = mempty,
    haddockHoogle       = mempty,
    haddockHtml         = mempty,
    haddockHtmlLocation = mempty,
    haddockForHackage   = mempty,
    haddockExecutables  = mempty,
    haddockTestSuites   = mempty,
    haddockBenchmarks   = mempty,
    haddockInternal     = mempty,
    haddockCss          = mempty,
    haddockHscolour     = mempty,
    haddockHscolourCss  = mempty,
    haddockContents     = mempty,
    haddockDistPref     = mempty,
    haddockKeepTempFiles= mempty,
    haddockVerbosity    = mempty
  }
  mappend = (Semi.<>)

instance Semigroup HaddockFlags where
  a <> b = HaddockFlags {
    haddockProgramPaths = combine haddockProgramPaths,
    haddockProgramArgs  = combine haddockProgramArgs,
    haddockHoogle       = combine haddockHoogle,
    haddockHtml         = combine haddockHtml,
    haddockHtmlLocation = combine haddockHtmlLocation,
    haddockForHackage   = combine haddockForHackage,
    haddockExecutables  = combine haddockExecutables,
    haddockTestSuites   = combine haddockTestSuites,
    haddockBenchmarks   = combine haddockBenchmarks,
    haddockInternal     = combine haddockInternal,
    haddockCss          = combine haddockCss,
    haddockHscolour     = combine haddockHscolour,
    haddockHscolourCss  = combine haddockHscolourCss,
    haddockContents     = combine haddockContents,
    haddockDistPref     = combine haddockDistPref,
    haddockKeepTempFiles= combine haddockKeepTempFiles,
    haddockVerbosity    = combine haddockVerbosity
  }
    where combine field = field a `mappend` field b

-- ------------------------------------------------------------
-- * Clean flags
-- ------------------------------------------------------------

data CleanFlags = CleanFlags {
    cleanSaveConf  :: Flag Bool,
    cleanDistPref  :: Flag FilePath,
    cleanVerbosity :: Flag Verbosity
  }
  deriving Show

defaultCleanFlags :: CleanFlags
defaultCleanFlags  = CleanFlags {
    cleanSaveConf  = Flag False,
    cleanDistPref  = NoFlag,
    cleanVerbosity = Flag normal
  }

cleanCommand :: CommandUI CleanFlags
cleanCommand = CommandUI
  { commandName         = "clean"
  , commandSynopsis     = "Clean up after a build."
  , commandDescription  = Just $ \_ ->
      "Removes .hi, .o, preprocessed sources, etc.\n"
  , commandNotes        = Nothing
  , commandUsage        = \pname ->
      "Usage: " ++ pname ++ " clean [FLAGS]\n"
  , commandDefaultFlags = defaultCleanFlags
  , commandOptions      = \showOrParseArgs ->
      [optionVerbosity cleanVerbosity (\v flags -> flags { cleanVerbosity = v })
      ,optionDistPref
         cleanDistPref (\d flags -> flags { cleanDistPref = d })
         showOrParseArgs

      ,option "s" ["save-configure"]
         "Do not remove the configuration file (dist/setup-config) during cleaning.  Saves need to reconfigure."
         cleanSaveConf (\v flags -> flags { cleanSaveConf = v })
         trueArg
      ]
  }

emptyCleanFlags :: CleanFlags
emptyCleanFlags = mempty

instance Monoid CleanFlags where
  mempty = CleanFlags {
    cleanSaveConf  = mempty,
    cleanDistPref  = mempty,
    cleanVerbosity = mempty
  }
  mappend = (Semi.<>)

instance Semigroup CleanFlags where
  a <> b = CleanFlags {
    cleanSaveConf  = combine cleanSaveConf,
    cleanDistPref  = combine cleanDistPref,
    cleanVerbosity = combine cleanVerbosity
  }
    where combine field = field a `mappend` field b

-- ------------------------------------------------------------
-- * Build flags
-- ------------------------------------------------------------

data BuildFlags = BuildFlags {
    buildProgramPaths :: [(String, FilePath)],
    buildProgramArgs :: [(String, [String])],
    buildDistPref    :: Flag FilePath,
    buildVerbosity   :: Flag Verbosity,
    buildNumJobs     :: Flag (Maybe Int),
    -- TODO: this one should not be here, it's just that the silly
    -- UserHooks stop us from passing extra info in other ways
    buildArgs :: [String]
  }
  deriving Show

{-# DEPRECATED buildVerbose "Use buildVerbosity instead" #-}
buildVerbose :: BuildFlags -> Verbosity
buildVerbose = fromFlagOrDefault normal . buildVerbosity

defaultBuildFlags :: BuildFlags
defaultBuildFlags  = BuildFlags {
    buildProgramPaths = mempty,
    buildProgramArgs = [],
    buildDistPref    = mempty,
    buildVerbosity   = Flag normal,
    buildNumJobs     = mempty,
    buildArgs        = []
  }

buildCommand :: ProgramConfiguration -> CommandUI BuildFlags
buildCommand progConf = CommandUI
  { commandName         = "build"
  , commandSynopsis     = "Compile all/specific components."
  , commandDescription  = Just $ \_ -> wrapText $
         "Components encompass executables, tests, and benchmarks.\n"
      ++ "\n"
      ++ "Affected by configuration options, see `configure`.\n"
  , commandNotes        = Just $ \pname ->
       "Examples:\n"
        ++ "  " ++ pname ++ " build           "
        ++ "    All the components in the package\n"
        ++ "  " ++ pname ++ " build foo       "
        ++ "    A component (i.e. lib, exe, test suite)\n\n"
        ++ programFlagsDescription progConf
--TODO: re-enable once we have support for module/file targets
--        ++ "  " ++ pname ++ " build Foo.Bar   "
--        ++ "    A module\n"
--        ++ "  " ++ pname ++ " build Foo/Bar.hs"
--        ++ "    A file\n\n"
--        ++ "If a target is ambiguous it can be qualified with the component "
--        ++ "name, e.g.\n"
--        ++ "  " ++ pname ++ " build foo:Foo.Bar\n"
--        ++ "  " ++ pname ++ " build testsuite1:Foo/Bar.hs\n"
  , commandUsage        = usageAlternatives "build" $
      [ "[FLAGS]"
      , "COMPONENTS [FLAGS]"
      ]
  , commandDefaultFlags = defaultBuildFlags
  , commandOptions      = \showOrParseArgs ->
      [ optionVerbosity
        buildVerbosity (\v flags -> flags { buildVerbosity = v })

      , optionDistPref
        buildDistPref (\d flags -> flags { buildDistPref = d }) showOrParseArgs
      ]
      ++ buildOptions progConf showOrParseArgs
  }

buildOptions :: ProgramConfiguration -> ShowOrParseArgs
                -> [OptionField BuildFlags]
buildOptions progConf showOrParseArgs =
  [ optionNumJobs
      buildNumJobs (\v flags -> flags { buildNumJobs = v })
  ]

  ++ programConfigurationPaths progConf showOrParseArgs
       buildProgramPaths (\v flags -> flags { buildProgramPaths = v})

  ++ programConfigurationOption progConf showOrParseArgs
       buildProgramArgs (\v fs -> fs { buildProgramArgs = v })

  ++ programConfigurationOptions progConf showOrParseArgs
       buildProgramArgs (\v flags -> flags { buildProgramArgs = v})

emptyBuildFlags :: BuildFlags
emptyBuildFlags = mempty

instance Monoid BuildFlags where
  mempty = BuildFlags {
    buildProgramPaths = mempty,
    buildProgramArgs = mempty,
    buildVerbosity   = mempty,
    buildDistPref    = mempty,
    buildNumJobs     = mempty,
    buildArgs        = mempty
  }
  mappend = (Semi.<>)

instance Semigroup BuildFlags where
  a <> b = BuildFlags {
    buildProgramPaths = combine buildProgramPaths,
    buildProgramArgs = combine buildProgramArgs,
    buildVerbosity   = combine buildVerbosity,
    buildDistPref    = combine buildDistPref,
    buildNumJobs     = combine buildNumJobs,
    buildArgs        = combine buildArgs
  }
    where combine field = field a `mappend` field b

-- ------------------------------------------------------------
-- * REPL Flags
-- ------------------------------------------------------------

data ReplFlags = ReplFlags {
    replProgramPaths :: [(String, FilePath)],
    replProgramArgs :: [(String, [String])],
    replDistPref    :: Flag FilePath,
    replVerbosity   :: Flag Verbosity,
    replReload      :: Flag Bool
  }
  deriving Show

defaultReplFlags :: ReplFlags
defaultReplFlags  = ReplFlags {
    replProgramPaths = mempty,
    replProgramArgs = [],
    replDistPref    = NoFlag,
    replVerbosity   = Flag normal,
    replReload      = Flag False
  }

instance Monoid ReplFlags where
  mempty = ReplFlags {
    replProgramPaths = mempty,
    replProgramArgs = mempty,
    replVerbosity   = mempty,
    replDistPref    = mempty,
    replReload      = mempty
  }
  mappend = (Semi.<>)

instance Semigroup ReplFlags where
  a <> b = ReplFlags {
    replProgramPaths = combine replProgramPaths,
    replProgramArgs = combine replProgramArgs,
    replVerbosity   = combine replVerbosity,
    replDistPref    = combine replDistPref,
    replReload      = combine replReload
  }
    where combine field = field a `mappend` field b

replCommand :: ProgramConfiguration -> CommandUI ReplFlags
replCommand progConf = CommandUI
  { commandName         = "repl"
  , commandSynopsis     =
      "Open an interpreter session for the given component."
  , commandDescription  = Just $ \pname -> wrapText $
         "If the current directory contains no package, ignores COMPONENT "
      ++ "parameters and opens an interactive interpreter session; if a "
      ++ "sandbox is present, its package database will be used.\n"
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
      ++ "implemented yet. For certain scenarios, `" ++ pname
      ++ " exec -- ghci :l Foo` may be used instead. Note that `exec` will "
      ++ "not (re)configure and you will have to specify the location of "
      ++ "other modules, if required.\n"

  , commandNotes        = Just $ \pname ->
         "Examples:\n"
      ++ "  " ++ pname ++ " repl           "
      ++ "    The first component in the package\n"
      ++ "  " ++ pname ++ " repl foo       "
      ++ "    A named component (i.e. lib, exe, test suite)\n"
      ++ "  " ++ pname ++ " repl --ghc-options=\"-lstdc++\""
      ++ "  Specifying flags for interpreter\n"
--TODO: re-enable once we have support for module/file targets
--        ++ "  " ++ pname ++ " repl Foo.Bar   "
--        ++ "    A module\n"
--        ++ "  " ++ pname ++ " repl Foo/Bar.hs"
--        ++ "    A file\n\n"
--        ++ "If a target is ambiguous it can be qualified with the component "
--        ++ "name, e.g.\n"
--        ++ "  " ++ pname ++ " repl foo:Foo.Bar\n"
--        ++ "  " ++ pname ++ " repl testsuite1:Foo/Bar.hs\n"
  , commandUsage =  \pname -> "Usage: " ++ pname ++ " repl [COMPONENT] [FLAGS]\n"
  , commandDefaultFlags = defaultReplFlags
  , commandOptions = \showOrParseArgs ->
      optionVerbosity replVerbosity (\v flags -> flags { replVerbosity = v })
      : optionDistPref
          replDistPref (\d flags -> flags { replDistPref = d })
          showOrParseArgs

      : programConfigurationPaths   progConf showOrParseArgs
          replProgramPaths (\v flags -> flags { replProgramPaths = v})

     ++ programConfigurationOption progConf showOrParseArgs
          replProgramArgs (\v flags -> flags { replProgramArgs = v})

     ++ programConfigurationOptions progConf showOrParseArgs
          replProgramArgs (\v flags -> flags { replProgramArgs = v})

     ++ case showOrParseArgs of
          ParseArgs ->
            [ option "" ["reload"]
              "Used from within an interpreter to update files."
              replReload (\v flags -> flags { replReload = v })
              trueArg
            ]
          _ -> []
  }

-- ------------------------------------------------------------
-- * Test flags
-- ------------------------------------------------------------

data TestShowDetails = Never | Failures | Always | Streaming | Direct
    deriving (Eq, Ord, Enum, Bounded, Show)

knownTestShowDetails :: [TestShowDetails]
knownTestShowDetails = [minBound..maxBound]

instance Text TestShowDetails where
    disp  = Disp.text . lowercase . show

    parse = maybe Parse.pfail return . classify =<< ident
      where
        ident        = Parse.munch1 (\c -> isAlpha c || c == '_' || c == '-')
        classify str = lookup (lowercase str) enumMap
        enumMap     :: [(String, TestShowDetails)]
        enumMap      = [ (display x, x)
                       | x <- knownTestShowDetails ]

--TODO: do we need this instance?
instance Monoid TestShowDetails where
    mempty = Never
    mappend = (Semi.<>)

instance Semigroup TestShowDetails where
    a <> b = if a < b then b else a

data TestFlags = TestFlags {
    testDistPref    :: Flag FilePath,
    testVerbosity   :: Flag Verbosity,
    testHumanLog    :: Flag PathTemplate,
    testMachineLog  :: Flag PathTemplate,
    testShowDetails :: Flag TestShowDetails,
    testKeepTix     :: Flag Bool,
    -- TODO: think about if/how options are passed to test exes
    testOptions     :: [PathTemplate]
  }

defaultTestFlags :: TestFlags
defaultTestFlags  = TestFlags {
    testDistPref    = NoFlag,
    testVerbosity   = Flag normal,
    testHumanLog    = toFlag $ toPathTemplate $ "$pkgid-$test-suite.log",
    testMachineLog  = toFlag $ toPathTemplate $ "$pkgid.log",
    testShowDetails = toFlag Failures,
    testKeepTix     = toFlag False,
    testOptions     = []
  }

testCommand :: CommandUI TestFlags
testCommand = CommandUI
  { commandName         = "test"
  , commandSynopsis     =
      "Run all/specific tests in the test suite."
  , commandDescription  = Just $ \pname -> wrapText $
         "If necessary (re)configures with `--enable-tests` flag and builds"
      ++ " the test suite.\n"
      ++ "\n"
      ++ "Remember that the tests' dependencies must be installed if there"
      ++ " are additional ones; e.g. with `" ++ pname
      ++ " install --only-dependencies --enable-tests`.\n"
      ++ "\n"
      ++ "By defining UserHooks in a custom Setup.hs, the package can"
      ++ " define actions to be executed before and after running tests.\n"
  , commandNotes        = Nothing
  , commandUsage        = usageAlternatives "test"
      [ "[FLAGS]"
      , "TESTCOMPONENTS [FLAGS]"
      ]
  , commandDefaultFlags = defaultTestFlags
  , commandOptions = \showOrParseArgs ->
      [ optionVerbosity testVerbosity (\v flags -> flags { testVerbosity = v })
      , optionDistPref
            testDistPref (\d flags -> flags { testDistPref = d })
            showOrParseArgs
      , option [] ["log"]
            ("Log all test suite results to file (name template can use "
            ++ "$pkgid, $compiler, $os, $arch, $test-suite, $result)")
            testHumanLog (\v flags -> flags { testHumanLog = v })
            (reqArg' "TEMPLATE"
                (toFlag . toPathTemplate)
                (flagToList . fmap fromPathTemplate))
      , option [] ["machine-log"]
            ("Produce a machine-readable log file (name template can use "
            ++ "$pkgid, $compiler, $os, $arch, $result)")
            testMachineLog (\v flags -> flags { testMachineLog = v })
            (reqArg' "TEMPLATE"
                (toFlag . toPathTemplate)
                (flagToList . fmap fromPathTemplate))
      , option [] ["show-details"]
            ("'always': always show results of individual test cases. "
             ++ "'never': never show results of individual test cases. "
             ++ "'failures': show results of failing test cases. "
             ++ "'streaming': show results of test cases in real time."
             ++ "'direct': send results of test cases in real time; no log file.")
            testShowDetails (\v flags -> flags { testShowDetails = v })
            (reqArg "FILTER"
                (readP_to_E (\_ -> "--show-details flag expects one of "
                              ++ intercalate ", "
                                   (map display knownTestShowDetails))
                            (fmap toFlag parse))
                (flagToList . fmap display))
      , option [] ["keep-tix-files"]
            "keep .tix files for HPC between test runs"
            testKeepTix (\v flags -> flags { testKeepTix = v})
            trueArg
      , option [] ["test-options"]
            ("give extra options to test executables "
             ++ "(name templates can use $pkgid, $compiler, "
             ++ "$os, $arch, $test-suite)")
            testOptions (\v flags -> flags { testOptions = v })
            (reqArg' "TEMPLATES" (map toPathTemplate . splitArgs)
                (const []))
      , option [] ["test-option"]
            ("give extra option to test executables "
             ++ "(no need to quote options containing spaces, "
             ++ "name template can use $pkgid, $compiler, "
             ++ "$os, $arch, $test-suite)")
            testOptions (\v flags -> flags { testOptions = v })
            (reqArg' "TEMPLATE" (\x -> [toPathTemplate x])
                (map fromPathTemplate))
      ]
  }

emptyTestFlags :: TestFlags
emptyTestFlags  = mempty

instance Monoid TestFlags where
  mempty = TestFlags {
    testDistPref    = mempty,
    testVerbosity   = mempty,
    testHumanLog    = mempty,
    testMachineLog  = mempty,
    testShowDetails = mempty,
    testKeepTix     = mempty,
    testOptions     = mempty
  }
  mappend = (Semi.<>)

instance Semigroup TestFlags where
  a <> b = TestFlags {
    testDistPref    = combine testDistPref,
    testVerbosity   = combine testVerbosity,
    testHumanLog    = combine testHumanLog,
    testMachineLog  = combine testMachineLog,
    testShowDetails = combine testShowDetails,
    testKeepTix     = combine testKeepTix,
    testOptions     = combine testOptions
  }
    where combine field = field a `mappend` field b

-- ------------------------------------------------------------
-- * Benchmark flags
-- ------------------------------------------------------------

data BenchmarkFlags = BenchmarkFlags {
    benchmarkDistPref  :: Flag FilePath,
    benchmarkVerbosity :: Flag Verbosity,
    benchmarkOptions   :: [PathTemplate]
  }

defaultBenchmarkFlags :: BenchmarkFlags
defaultBenchmarkFlags  = BenchmarkFlags {
    benchmarkDistPref  = NoFlag,
    benchmarkVerbosity = Flag normal,
    benchmarkOptions   = []
  }

benchmarkCommand :: CommandUI BenchmarkFlags
benchmarkCommand = CommandUI
  { commandName         = "bench"
  , commandSynopsis     =
      "Run all/specific benchmarks."
  , commandDescription  = Just $ \pname -> wrapText $
         "If necessary (re)configures with `--enable-benchmarks` flag and"
      ++ " builds the benchmarks.\n"
      ++ "\n"
      ++ "Remember that the benchmarks' dependencies must be installed if"
      ++ " there are additional ones; e.g. with `" ++ pname
      ++ " install --only-dependencies --enable-benchmarks`.\n"
      ++ "\n"
      ++ "By defining UserHooks in a custom Setup.hs, the package can"
      ++ " define actions to be executed before and after running"
      ++ " benchmarks.\n"
  , commandNotes        = Nothing
  , commandUsage        = usageAlternatives "bench"
      [ "[FLAGS]"
      , "BENCHCOMPONENTS [FLAGS]"
      ]
  , commandDefaultFlags = defaultBenchmarkFlags
  , commandOptions = \showOrParseArgs ->
      [ optionVerbosity benchmarkVerbosity
        (\v flags -> flags { benchmarkVerbosity = v })
      , optionDistPref
            benchmarkDistPref (\d flags -> flags { benchmarkDistPref = d })
            showOrParseArgs
      , option [] ["benchmark-options"]
            ("give extra options to benchmark executables "
             ++ "(name templates can use $pkgid, $compiler, "
             ++ "$os, $arch, $benchmark)")
            benchmarkOptions (\v flags -> flags { benchmarkOptions = v })
            (reqArg' "TEMPLATES" (map toPathTemplate . splitArgs)
                (const []))
      , option [] ["benchmark-option"]
            ("give extra option to benchmark executables "
             ++ "(no need to quote options containing spaces, "
             ++ "name template can use $pkgid, $compiler, "
             ++ "$os, $arch, $benchmark)")
            benchmarkOptions (\v flags -> flags { benchmarkOptions = v })
            (reqArg' "TEMPLATE" (\x -> [toPathTemplate x])
                (map fromPathTemplate))
      ]
  }

emptyBenchmarkFlags :: BenchmarkFlags
emptyBenchmarkFlags = mempty

instance Monoid BenchmarkFlags where
  mempty = BenchmarkFlags {
    benchmarkDistPref  = mempty,
    benchmarkVerbosity = mempty,
    benchmarkOptions   = mempty
  }
  mappend = (Semi.<>)

instance Semigroup BenchmarkFlags where
  a <> b = BenchmarkFlags {
    benchmarkDistPref  = combine benchmarkDistPref,
    benchmarkVerbosity = combine benchmarkVerbosity,
    benchmarkOptions   = combine benchmarkOptions
  }
    where combine field = field a `mappend` field b

-- ------------------------------------------------------------
-- * Shared options utils
-- ------------------------------------------------------------

programFlagsDescription :: ProgramConfiguration -> String
programFlagsDescription progConf =
     "The flags --with-PROG and --PROG-option(s) can be used with"
  ++ " the following programs:"
  ++ (concatMap (\line -> "\n  " ++ unwords line) . wrapLine 77 . sort)
     [ programName prog | (prog, _) <- knownPrograms progConf ]
  ++ "\n"

-- | For each known program @PROG@ in 'progConf', produce a @with-PROG@
-- 'OptionField'.
programConfigurationPaths
  :: ProgramConfiguration
  -> ShowOrParseArgs
  -> (flags -> [(String, FilePath)])
  -> ([(String, FilePath)] -> (flags -> flags))
  -> [OptionField flags]
programConfigurationPaths progConf showOrParseArgs get set =
  programConfigurationPaths' ("with-" ++) progConf showOrParseArgs get set

-- | Like 'programConfigurationPaths', but allows to customise the option name.
programConfigurationPaths'
  :: (String -> String)
  -> ProgramConfiguration
  -> ShowOrParseArgs
  -> (flags -> [(String, FilePath)])
  -> ([(String, FilePath)] -> (flags -> flags))
  -> [OptionField flags]
programConfigurationPaths' mkName progConf showOrParseArgs get set =
  case showOrParseArgs of
    -- we don't want a verbose help text list so we just show a generic one:
    ShowArgs  -> [withProgramPath "PROG"]
    ParseArgs -> map (withProgramPath . programName . fst)
                 (knownPrograms progConf)
  where
    withProgramPath prog =
      option "" [mkName prog]
        ("give the path to " ++ prog)
        get set
        (reqArg' "PATH" (\path -> [(prog, path)])
          (\progPaths -> [ path | (prog', path) <- progPaths, prog==prog' ]))

-- | For each known program @PROG@ in 'progConf', produce a @PROG-option@
-- 'OptionField'.
programConfigurationOption
  :: ProgramConfiguration
  -> ShowOrParseArgs
  -> (flags -> [(String, [String])])
  -> ([(String, [String])] -> (flags -> flags))
  -> [OptionField flags]
programConfigurationOption progConf showOrParseArgs get set =
  case showOrParseArgs of
    -- we don't want a verbose help text list so we just show a generic one:
    ShowArgs  -> [programOption "PROG"]
    ParseArgs -> map (programOption  . programName . fst)
                 (knownPrograms progConf)
  where
    programOption prog =
      option "" [prog ++ "-option"]
        ("give an extra option to " ++ prog ++
         " (no need to quote options containing spaces)")
        get set
        (reqArg' "OPT" (\arg -> [(prog, [arg])])
           (\progArgs -> concat [ args
                                | (prog', args) <- progArgs, prog==prog' ]))

-- | For each known program @PROG@ in 'progConf', produce a @PROG-options@
-- 'OptionField'.
programConfigurationOptions
  :: ProgramConfiguration
  -> ShowOrParseArgs
  -> (flags -> [(String, [String])])
  -> ([(String, [String])] -> (flags -> flags))
  -> [OptionField flags]
programConfigurationOptions progConf showOrParseArgs get set =
  case showOrParseArgs of
    -- we don't want a verbose help text list so we just show a generic one:
    ShowArgs  -> [programOptions  "PROG"]
    ParseArgs -> map (programOptions . programName . fst)
                 (knownPrograms progConf)
  where
    programOptions prog =
      option "" [prog ++ "-options"]
        ("give extra options to " ++ prog)
        get set
        (reqArg' "OPTS" (\args -> [(prog, splitArgs args)]) (const []))

-- ------------------------------------------------------------
-- * GetOpt Utils
-- ------------------------------------------------------------

boolOpt :: SFlags -> SFlags
           -> MkOptDescr (a -> Flag Bool) (Flag Bool -> a -> a) a
boolOpt  = Command.boolOpt  flagToMaybe Flag

boolOpt' :: OptFlags -> OptFlags
            -> MkOptDescr (a -> Flag Bool) (Flag Bool -> a -> a) a
boolOpt' = Command.boolOpt' flagToMaybe Flag

trueArg, falseArg :: MkOptDescr (a -> Flag Bool) (Flag Bool -> a -> a) a
trueArg  sfT lfT = boolOpt' (sfT, lfT) ([], [])   sfT lfT
falseArg sfF lfF = boolOpt' ([],  [])  (sfF, lfF) sfF lfF

reqArgFlag :: ArgPlaceHolder -> SFlags -> LFlags -> Description ->
              (b -> Flag String) -> (Flag String -> b -> b) -> OptDescr b
reqArgFlag ad = reqArg ad (succeedReadE Flag) flagToList

optionDistPref :: (flags -> Flag FilePath)
               -> (Flag FilePath -> flags -> flags)
               -> ShowOrParseArgs
               -> OptionField flags
optionDistPref get set = \showOrParseArgs ->
  option "" (distPrefFlagName showOrParseArgs)
    (   "The directory where Cabal puts generated build files "
     ++ "(default " ++ defaultDistPref ++ ")")
    get set
    (reqArgFlag "DIR")
  where
    distPrefFlagName ShowArgs  = ["builddir"]
    distPrefFlagName ParseArgs = ["builddir", "distdir", "distpref"]

optionVerbosity :: (flags -> Flag Verbosity)
                -> (Flag Verbosity -> flags -> flags)
                -> OptionField flags
optionVerbosity get set =
  option "v" ["verbose"]
    "Control verbosity (n is 0--3, default verbosity level is 1)"
    get set
    (optArg "n" (fmap Flag flagToVerbosity)
                (Flag verbose) -- default Value if no n is given
                (fmap (Just . showForCabal) . flagToList))

optionNumJobs :: (flags -> Flag (Maybe Int))
              -> (Flag (Maybe Int) -> flags -> flags)
              -> OptionField flags
optionNumJobs get set =
  option "j" ["jobs"]
    "Run NUM jobs simultaneously (or '$ncpus' if no NUM is given)."
    get set
    (optArg "NUM" (fmap Flag numJobsParser)
                  (Flag Nothing)
                  (map (Just . maybe "$ncpus" show) . flagToList))
  where
    numJobsParser :: ReadE (Maybe Int)
    numJobsParser = ReadE $ \s ->
      case s of
        "$ncpus" -> Right Nothing
        _        -> case reads s of
          [(n, "")]
            | n < 1     -> Left "The number of jobs should be 1 or more."
            | n > 64    -> Left "You probably don't want that many jobs."
            | otherwise -> Right (Just n)
          _             -> Left "The jobs value should be a number or '$ncpus'"

-- ------------------------------------------------------------
-- * Other Utils
-- ------------------------------------------------------------

-- | Arguments to pass to a @configure@ script, e.g. generated by
-- @autoconf@.
configureArgs :: Bool -> ConfigFlags -> [String]
configureArgs bcHack flags
  = hc_flag
 ++ optFlag  "with-hc-pkg" configHcPkg
 ++ optFlag' "prefix"      prefix
 ++ optFlag' "bindir"      bindir
 ++ optFlag' "libdir"      libdir
 ++ optFlag' "libexecdir"  libexecdir
 ++ optFlag' "datadir"     datadir
 ++ optFlag' "sysconfdir"  sysconfdir
 ++ configConfigureArgs flags
  where
        hc_flag = case (configHcFlavor flags, configHcPath flags) of
                        (_, Flag hc_path) -> [hc_flag_name ++ hc_path]
                        (Flag hc, NoFlag) -> [hc_flag_name ++ display hc]
                        (NoFlag,NoFlag)   -> []
        hc_flag_name
            --TODO kill off thic bc hack when defaultUserHooks is removed.
            | bcHack    = "--with-hc="
            | otherwise = "--with-compiler="
        optFlag name config_field = case config_field flags of
                        Flag p -> ["--" ++ name ++ "=" ++ p]
                        NoFlag -> []
        optFlag' name config_field = optFlag name (fmap fromPathTemplate
                                                 . config_field
                                                 . configInstallDirs)

configureCCompiler :: Verbosity -> ProgramConfiguration
                      -> IO (FilePath, [String])
configureCCompiler verbosity lbi = configureProg verbosity lbi gccProgram

configureLinker :: Verbosity -> ProgramConfiguration -> IO (FilePath, [String])
configureLinker verbosity lbi = configureProg verbosity lbi ldProgram

configureProg :: Verbosity -> ProgramConfiguration -> Program
                 -> IO (FilePath, [String])
configureProg verbosity programConfig prog = do
    (p, _) <- requireProgram verbosity prog programConfig
    let pInv = programInvocation p []
    return (progInvokePath pInv, progInvokeArgs pInv)

-- | Helper function to split a string into a list of arguments.
-- It's supposed to handle quoted things sensibly, eg:
--
-- > splitArgs "--foo=\"C:\Program Files\Bar\" --baz"
-- >   = ["--foo=C:\Program Files\Bar", "--baz"]
--
splitArgs :: String -> [String]
splitArgs  = space []
  where
    space :: String -> String -> [String]
    space w []      = word w []
    space w ( c :s)
        | isSpace c = word w (space [] s)
    space w ('"':s) = string w s
    space w s       = nonstring w s

    string :: String -> String -> [String]
    string w []      = word w []
    string w ('"':s) = space w s
    string w ( c :s) = string (c:w) s

    nonstring :: String -> String -> [String]
    nonstring w  []      = word w []
    nonstring w  ('"':s) = string w s
    nonstring w  ( c :s) = space (c:w) s

    word [] s = s
    word w  s = reverse w : s

-- The test cases kinda have to be rewritten from the ground up... :/
--hunitTests :: [Test]
--hunitTests =
--    let m = [("ghc", GHC), ("nhc98", NHC), ("hugs", Hugs)]
--        (flags, commands', unkFlags, ers)
--               = getOpt Permute options ["configure", "foobar", "--prefix=/foo", "--ghc", "--nhc98", "--hugs", "--with-compiler=/comp", "--unknown1", "--unknown2", "--install-prefix=/foo", "--user", "--global"]
--       in  [TestLabel "very basic option parsing" $ TestList [
--                 "getOpt flags" ~: "failed" ~:
--                 [Prefix "/foo", GhcFlag, NhcFlag, HugsFlag,
--                  WithCompiler "/comp", InstPrefix "/foo", UserFlag, GlobalFlag]
--                 ~=? flags,
--                 "getOpt commands" ~: "failed" ~: ["configure", "foobar"] ~=? commands',
--                 "getOpt unknown opts" ~: "failed" ~:
--                      ["--unknown1", "--unknown2"] ~=? unkFlags,
--                 "getOpt errors" ~: "failed" ~: [] ~=? ers],
--
--               TestLabel "test location of various compilers" $ TestList
--               ["configure parsing for prefix and compiler flag" ~: "failed" ~:
--                    (Right (ConfigCmd (Just comp, Nothing, Just "/usr/local"), []))
--                   ~=? (parseArgs ["--prefix=/usr/local", "--"++name, "configure"])
--                   | (name, comp) <- m],
--
--               TestLabel "find the package tool" $ TestList
--               ["configure parsing for prefix comp flag, withcompiler" ~: "failed" ~:
--                    (Right (ConfigCmd (Just comp, Just "/foo/comp", Just "/usr/local"), []))
--                   ~=? (parseArgs ["--prefix=/usr/local", "--"++name,
--                                   "--with-compiler=/foo/comp", "configure"])
--                   | (name, comp) <- m],
--
--               TestLabel "simpler commands" $ TestList
--               [flag ~: "failed" ~: (Right (flagCmd, [])) ~=? (parseArgs [flag])
--                   | (flag, flagCmd) <- [("build", BuildCmd),
--                                         ("install", InstallCmd Nothing False),
--                                         ("sdist", SDistCmd),
--                                         ("register", RegisterCmd False)]
--                  ]
--               ]

{- Testing ideas:
   * IO to look for hugs and hugs-pkg (which hugs, etc)
   * quickCheck to test permutations of arguments
   * what other options can we over-ride with a command-line flag?
-}
