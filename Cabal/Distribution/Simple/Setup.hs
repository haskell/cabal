-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Setup
-- Copyright   :  Isaac Jones 2003-2004
--                Duncan Coutts 2007
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

{- All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Isaac Jones nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. -}

module Distribution.Simple.Setup (

  GlobalFlags(..),   emptyGlobalFlags,   defaultGlobalFlags,   globalCommand,
  ConfigFlags(..),   emptyConfigFlags,   defaultConfigFlags,   configureCommand,
  CopyFlags(..),     emptyCopyFlags,     defaultCopyFlags,     copyCommand,
  InstallFlags(..),  emptyInstallFlags,  defaultInstallFlags,  installCommand,
  HaddockFlags(..),  emptyHaddockFlags,  defaultHaddockFlags,  haddockCommand,
  HscolourFlags(..), emptyHscolourFlags, defaultHscolourFlags, hscolourCommand,
  BuildFlags(..),    emptyBuildFlags,    defaultBuildFlags,    buildCommand,
  buildVerbose,
  CleanFlags(..),    emptyCleanFlags,    defaultCleanFlags,    cleanCommand,
  RegisterFlags(..), emptyRegisterFlags, defaultRegisterFlags, registerCommand,
                                                               unregisterCommand,
  SDistFlags(..),    emptySDistFlags,    defaultSDistFlags,    sdistCommand,
  TestFlags(..),     emptyTestFlags,     defaultTestFlags,     testCommand,
  TestShowDetails(..),
  CopyDest(..),
  configureArgs, configureOptions, configureCCompiler, configureLinker,
  installDirsOptions,

  defaultDistPref,

  Flag(..),
  toFlag,
  fromFlag,
  fromFlagOrDefault,
  flagToMaybe,
  flagToList,
  boolOpt, boolOpt', trueArg, falseArg, optionVerbosity ) where

import Distribution.Compiler ()
import Distribution.ReadE
import Distribution.Text
         ( Text(..), display )
import qualified Distribution.Compat.ReadP as Parse
import qualified Text.PrettyPrint as Disp
import Distribution.Package ( Dependency(..) )
import Distribution.PackageDescription
         ( FlagName(..), FlagAssignment )
import Distribution.Simple.Command hiding (boolOpt, boolOpt')
import qualified Distribution.Simple.Command as Command
import Distribution.Simple.Compiler
         ( CompilerFlavor(..), defaultCompilerFlavor, PackageDB(..)
         , OptimisationLevel(..), flagToOptimisationLevel )
import Distribution.Simple.Utils
         ( wrapLine, lowercase, intercalate )
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

import Data.List   ( sort )
import Data.Char   ( isSpace, isAlpha )
import Data.Monoid ( Monoid(..) )

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
data Flag a = Flag a | NoFlag deriving (Show, Read, Eq)

instance Functor Flag where
  fmap f (Flag x) = Flag (f x)
  fmap _ NoFlag  = NoFlag

instance Monoid (Flag a) where
  mempty = NoFlag
  _ `mappend` f@(Flag _) = f
  f `mappend` NoFlag    = f

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

globalCommand :: CommandUI GlobalFlags
globalCommand = CommandUI {
    commandName         = "",
    commandSynopsis     = "",
    commandUsage        = \_ ->
         "This Setup program uses the Haskell Cabal Infrastructure.\n"
      ++ "See http://www.haskell.org/cabal/ for more information.\n",
    commandDescription  = Just $ \pname ->
         "For more information about a command use\n"
      ++ "  " ++ pname ++ " COMMAND --help\n\n"
      ++ "Typical steps for installing Cabal packages:\n"
      ++ concat [ "  " ++ pname ++ " " ++ x ++ "\n"
                | x <- ["configure", "build", "install"]],
    commandDefaultFlags = defaultGlobalFlags,
    commandOptions      = \_ ->
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
  mappend a b = GlobalFlags {
    globalVersion        = combine globalVersion,
    globalNumericVersion = combine globalNumericVersion
  }
    where combine field = field a `mappend` field b

-- ------------------------------------------------------------
-- * Config flags
-- ------------------------------------------------------------

-- | Flags to @configure@ command
data ConfigFlags = ConfigFlags {
    --FIXME: the configPrograms is only here to pass info through to configure
    -- because the type of configure is constrained by the UserHooks.
    -- when we change UserHooks next we should pass the initial
    -- ProgramConfiguration directly and not via ConfigFlags
    configPrograms      :: ProgramConfiguration, -- ^All programs that cabal may run

    configProgramPaths  :: [(String, FilePath)], -- ^user specifed programs paths
    configProgramArgs   :: [(String, [String])], -- ^user specifed programs args
    configHcFlavor      :: Flag CompilerFlavor, -- ^The \"flavor\" of the compiler, sugh as GHC or Hugs.
    configHcPath        :: Flag FilePath, -- ^given compiler location
    configHcPkg         :: Flag FilePath, -- ^given hc-pkg location
    configVanillaLib    :: Flag Bool,     -- ^Enable vanilla library
    configProfLib       :: Flag Bool,     -- ^Enable profiling in the library
    configSharedLib     :: Flag Bool,     -- ^Build shared library
    configDynExe        :: Flag Bool,     -- ^Enable dynamic linking of the executables.
    configProfExe       :: Flag Bool,     -- ^Enable profiling in the executables.
    configConfigureArgs :: [String],      -- ^Extra arguments to @configure@
    configOptimization  :: Flag OptimisationLevel,  -- ^Enable optimization.
    configProgPrefix    :: Flag PathTemplate, -- ^Installed executable prefix.
    configProgSuffix    :: Flag PathTemplate, -- ^Installed executable suffix.
    configInstallDirs   :: InstallDirs (Flag PathTemplate), -- ^Installation paths
    configScratchDir    :: Flag FilePath,
    configExtraLibDirs  :: [FilePath],   -- ^ path to search for extra libraries
    configExtraIncludeDirs :: [FilePath],   -- ^ path to search for header files

    configDistPref :: Flag FilePath, -- ^"dist" prefix
    configVerbosity :: Flag Verbosity, -- ^verbosity level
    configUserInstall :: Flag Bool,    -- ^The --user\/--global flag
    configPackageDB :: Flag PackageDB, -- ^Which package DB to use
    configGHCiLib   :: Flag Bool,      -- ^Enable compiling library for GHCi
    configSplitObjs :: Flag Bool,      -- ^Enable -split-objs with GHC
    configStripExes :: Flag Bool,      -- ^Enable executable stripping
    configConstraints :: [Dependency], -- ^Additional constraints for
                                       -- dependencies
    configConfigurationsFlags :: FlagAssignment,
    configTests :: Flag Bool,     -- ^Enable test suite compilation
    configBenchmarks :: Flag Bool,     -- ^Enable benchmark compilation
    configLibCoverage :: Flag Bool    -- ^ Enable test suite program coverage
  }
  deriving (Read,Show)

defaultConfigFlags :: ProgramConfiguration -> ConfigFlags
defaultConfigFlags progConf = emptyConfigFlags {
    configPrograms     = progConf,
    configHcFlavor     = maybe NoFlag Flag defaultCompilerFlavor,
    configVanillaLib   = Flag True,
    configProfLib      = Flag False,
    configSharedLib    = Flag False,
    configDynExe       = Flag False,
    configProfExe      = Flag False,
    configOptimization = Flag NormalOptimisation,
    configProgPrefix   = Flag (toPathTemplate ""),
    configProgSuffix   = Flag (toPathTemplate ""),
    configDistPref     = Flag defaultDistPref,
    configVerbosity    = Flag normal,
    configUserInstall  = Flag False,           --TODO: reverse this
    configGHCiLib      = Flag True,
    configSplitObjs    = Flag False, -- takes longer, so turn off by default
    configStripExes    = Flag True,
    configTests  = Flag False,
    configBenchmarks   = Flag False,
    configLibCoverage = Flag False
  }

configureCommand :: ProgramConfiguration -> CommandUI ConfigFlags
configureCommand progConf = makeCommand name shortDesc longDesc defaultFlags options
  where
    name       = "configure"
    shortDesc  = "Prepare to build the package."
    longDesc   = Just (\_ -> programFlagsDescription progConf)
    defaultFlags = defaultConfigFlags progConf
    options showOrParseArgs =
         configureOptions showOrParseArgs
      ++ programConfigurationPaths   progConf showOrParseArgs
           configProgramPaths (\v fs -> fs { configProgramPaths = v })
      ++ programConfigurationOptions progConf showOrParseArgs
           configProgramArgs (\v fs -> fs { configProgramArgs = v })


configureOptions :: ShowOrParseArgs -> [OptionField ConfigFlags]
configureOptions showOrParseArgs =
      [optionVerbosity configVerbosity (\v flags -> flags { configVerbosity = v })
      ,optionDistPref
         configDistPref (\d flags -> flags { configDistPref = d })
         showOrParseArgs

      ,option [] ["compiler"] "compiler"
         configHcFlavor (\v flags -> flags { configHcFlavor = v })
         (choiceOpt [ (Flag GHC, ("g", ["ghc"]), "compile with GHC")
                    , (Flag NHC, ([] , ["nhc98"]), "compile with NHC")
                    , (Flag JHC, ([] , ["jhc"]), "compile with JHC")
                    , (Flag LHC, ([] , ["lhc"]), "compile with LHC")
                    , (Flag Hugs,([] , ["hugs"]), "compile with Hugs")
                    , (Flag UHC, ([] , ["uhc"]), "compile with UHC")])

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
   ++ [option "b" ["scratchdir"]
         "directory to receive the built package (hugs-only)"
         configScratchDir (\v flags -> flags { configScratchDir = v })
         (reqArgFlag "DIR")
      --TODO: eliminate scratchdir flag

      ,option "" ["program-prefix"]
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

      ,option "" ["executable-profiling"]
         "Executable profiling"
         configProfExe (\v flags -> flags { configProfExe = v })
         (boolOpt [] [])

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

      ,option "" ["configure-option"]
         "Extra option for configure"
         configConfigureArgs (\v flags -> flags { configConfigureArgs = v })
         (reqArg' "OPT" (\x -> [x]) id)

      ,option "" ["user-install"]
         "doing a per-user installation"
         configUserInstall (\v flags -> flags { configUserInstall = v })
         (boolOpt' ([],["user"]) ([], ["global"]))

      ,option "" ["package-db"]
         "Use a specific package database (to satisfy dependencies and register in)"
         configPackageDB (\v flags -> flags { configPackageDB = v })
         (reqArg' "PATH" (Flag . SpecificPackageDB)
                        (\f -> case f of
                                 Flag (SpecificPackageDB db) -> [db]
                                 _ -> []))

      ,option "f" ["flags"]
         "Force values for the given flags in Cabal conditionals in the .cabal file.  E.g., --flags=\"debug -usebytestrings\" forces the flag \"debug\" to true and \"usebytestrings\" to false."
         configConfigurationsFlags (\v flags -> flags { configConfigurationsFlags = v })
         (reqArg' "FLAGS" readFlagList showFlagList)

      ,option "" ["extra-include-dirs"]
         "A list of directories to search for header files"
         configExtraIncludeDirs (\v flags -> flags {configExtraIncludeDirs = v})
         (reqArg' "PATH" (\x -> [x]) id)

      ,option "" ["extra-lib-dirs"]
         "A list of directories to search for external libraries"
         configExtraLibDirs (\v flags -> flags {configExtraLibDirs = v})
         (reqArg' "PATH" (\x -> [x]) id)
      ,option "" ["constraint"]
         "A list of additional constraints on the dependencies."
         configConstraints (\v flags -> flags { configConstraints = v})
         (reqArg "DEPENDENCY"
                 (readP_to_E (const "dependency expected") ((\x -> [x]) `fmap` parse))
                 (map (\x -> display x)))
      ,option "" ["tests"]
         "dependency checking and compilation for test suites listed in the package description file."
         configTests (\v flags -> flags { configTests = v })
         (boolOpt [] [])
      ,option "" ["library-coverage"]
         "build library and test suites with Haskell Program Coverage enabled. (GHC only)"
         configLibCoverage (\v flags -> flags { configLibCoverage = v })
         (boolOpt [] [])
      ,option "" ["benchmarks"]
         "dependency checking and compilation for benchmarkss listed in the package description file."
         configBenchmarks (\v flags -> flags { configBenchmarks = v })
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
    configHcFlavor      = mempty,
    configHcPath        = mempty,
    configHcPkg         = mempty,
    configVanillaLib    = mempty,
    configProfLib       = mempty,
    configSharedLib     = mempty,
    configDynExe        = mempty,
    configProfExe       = mempty,
    configConfigureArgs = mempty,
    configOptimization  = mempty,
    configProgPrefix    = mempty,
    configProgSuffix    = mempty,
    configInstallDirs   = mempty,
    configScratchDir    = mempty,
    configDistPref      = mempty,
    configVerbosity     = mempty,
    configUserInstall   = mempty,
    configPackageDB     = mempty,
    configGHCiLib       = mempty,
    configSplitObjs     = mempty,
    configStripExes     = mempty,
    configExtraLibDirs  = mempty,
    configConstraints   = mempty,
    configExtraIncludeDirs    = mempty,
    configConfigurationsFlags = mempty,
    configTests   = mempty,
    configLibCoverage = mempty,
    configBenchmarks    = mempty
  }
  mappend a b =  ConfigFlags {
    configPrograms      = configPrograms b,
    configProgramPaths  = combine configProgramPaths,
    configProgramArgs   = combine configProgramArgs,
    configHcFlavor      = combine configHcFlavor,
    configHcPath        = combine configHcPath,
    configHcPkg         = combine configHcPkg,
    configVanillaLib    = combine configVanillaLib,
    configProfLib       = combine configProfLib,
    configSharedLib     = combine configSharedLib,
    configDynExe        = combine configDynExe,
    configProfExe       = combine configProfExe,
    configConfigureArgs = combine configConfigureArgs,
    configOptimization  = combine configOptimization,
    configProgPrefix    = combine configProgPrefix,
    configProgSuffix    = combine configProgSuffix,
    configInstallDirs   = combine configInstallDirs,
    configScratchDir    = combine configScratchDir,
    configDistPref      = combine configDistPref,
    configVerbosity     = combine configVerbosity,
    configUserInstall   = combine configUserInstall,
    configPackageDB     = combine configPackageDB,
    configGHCiLib       = combine configGHCiLib,
    configSplitObjs     = combine configSplitObjs,
    configStripExes     = combine configStripExes,
    configExtraLibDirs  = combine configExtraLibDirs,
    configConstraints   = combine configConstraints,
    configExtraIncludeDirs    = combine configExtraIncludeDirs,
    configConfigurationsFlags = combine configConfigurationsFlags,
    configTests = combine configTests,
    configLibCoverage = combine configLibCoverage,
    configBenchmarks    = combine configBenchmarks
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
    copyDistPref  = Flag defaultDistPref,
    copyVerbosity = Flag normal
  }

copyCommand :: CommandUI CopyFlags
copyCommand = makeCommand name shortDesc longDesc defaultCopyFlags options
  where
    name       = "copy"
    shortDesc  = "Copy the files into the install locations."
    longDesc   = Just $ \_ ->
          "Does not call register, and allows a prefix at install time\n"
       ++ "Without the --destdir flag, configure determines location.\n"
    options showOrParseArgs =
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

emptyCopyFlags :: CopyFlags
emptyCopyFlags = mempty

instance Monoid CopyFlags where
  mempty = CopyFlags {
    copyDest      = mempty,
    copyDistPref  = mempty,
    copyVerbosity = mempty
  }
  mappend a b = CopyFlags {
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
    installDistPref  = Flag defaultDistPref,
    installUseWrapper = Flag False,
    installInPlace    = Flag False,
    installVerbosity = Flag normal
  }

installCommand :: CommandUI InstallFlags
installCommand = makeCommand name shortDesc longDesc defaultInstallFlags options
  where
    name       = "install"
    shortDesc  = "Copy the files into the install locations. Run register."
    longDesc   = Just $ \_ ->
         "Unlike the copy command, install calls the register command.\n"
      ++ "If you want to install into a location that is not what was\n"
      ++ "specified in the configure step, use the copy command.\n"
    options showOrParseArgs =
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
  mappend a b = InstallFlags{
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
    sDistSnapshot  :: Flag Bool,
    sDistDirectory :: Flag FilePath,
    sDistDistPref  :: Flag FilePath,
    sDistVerbosity :: Flag Verbosity
  }
  deriving Show

defaultSDistFlags :: SDistFlags
defaultSDistFlags = SDistFlags {
    sDistSnapshot  = Flag False,
    sDistDirectory = mempty,
    sDistDistPref  = Flag defaultDistPref,
    sDistVerbosity = Flag normal
  }

sdistCommand :: CommandUI SDistFlags
sdistCommand = makeCommand name shortDesc longDesc defaultSDistFlags options
  where
    name       = "sdist"
    shortDesc  = "Generate a source distribution file (.tar.gz)."
    longDesc   = Nothing
    options showOrParseArgs =
      [optionVerbosity sDistVerbosity (\v flags -> flags { sDistVerbosity = v })
      ,optionDistPref
         sDistDistPref (\d flags -> flags { sDistDistPref = d })
         showOrParseArgs

      ,option "" ["snapshot"]
         "Produce a snapshot source distribution"
         sDistSnapshot (\v flags -> flags { sDistSnapshot = v })
         trueArg

      ,option "" ["output-directory"]
         "Generate a source distribution in the given directory"
         sDistDirectory (\v flags -> flags { sDistDirectory = v })
         (reqArgFlag "DIR")
      ]

emptySDistFlags :: SDistFlags
emptySDistFlags = mempty

instance Monoid SDistFlags where
  mempty = SDistFlags {
    sDistSnapshot  = mempty,
    sDistDirectory = mempty,
    sDistDistPref  = mempty,
    sDistVerbosity = mempty
  }
  mappend a b = SDistFlags {
    sDistSnapshot  = combine sDistSnapshot,
    sDistDirectory = combine sDistDirectory,
    sDistDistPref  = combine sDistDistPref,
    sDistVerbosity = combine sDistVerbosity
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
    regVerbosity   :: Flag Verbosity
  }
  deriving Show

defaultRegisterFlags :: RegisterFlags
defaultRegisterFlags = RegisterFlags {
    regPackageDB   = NoFlag,
    regGenScript   = Flag False,
    regGenPkgConf  = NoFlag,
    regInPlace     = Flag False,
    regDistPref    = Flag defaultDistPref,
    regVerbosity   = Flag normal
  }

registerCommand :: CommandUI RegisterFlags
registerCommand = makeCommand name shortDesc longDesc defaultRegisterFlags options
  where
    name       = "register"
    shortDesc  = "Register this package with the compiler."
    longDesc   = Nothing
    options showOrParseArgs =
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
      ]

unregisterCommand :: CommandUI RegisterFlags
unregisterCommand = makeCommand name shortDesc longDesc defaultRegisterFlags options
  where
    name       = "unregister"
    shortDesc  = "Unregister this package with the compiler."
    longDesc   = Nothing
    options showOrParseArgs =
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

emptyRegisterFlags :: RegisterFlags
emptyRegisterFlags = mempty

instance Monoid RegisterFlags where
  mempty = RegisterFlags {
    regPackageDB   = mempty,
    regGenScript   = mempty,
    regGenPkgConf  = mempty,
    regInPlace     = mempty,
    regDistPref    = mempty,
    regVerbosity   = mempty
  }
  mappend a b = RegisterFlags {
    regPackageDB   = combine regPackageDB,
    regGenScript   = combine regGenScript,
    regGenPkgConf  = combine regGenPkgConf,
    regInPlace     = combine regInPlace,
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
    hscolourDistPref    = Flag defaultDistPref,
    hscolourVerbosity   = Flag normal
  }

instance Monoid HscolourFlags where
  mempty = HscolourFlags {
    hscolourCSS         = mempty,
    hscolourExecutables = mempty,
    hscolourDistPref    = mempty,
    hscolourVerbosity   = mempty
  }
  mappend a b = HscolourFlags {
    hscolourCSS         = combine hscolourCSS,
    hscolourExecutables = combine hscolourExecutables,
    hscolourDistPref    = combine hscolourDistPref,
    hscolourVerbosity   = combine hscolourVerbosity
  }
    where combine field = field a `mappend` field b

hscolourCommand :: CommandUI HscolourFlags
hscolourCommand = makeCommand name shortDesc longDesc defaultHscolourFlags options
  where
    name       = "hscolour"
    shortDesc  = "Generate HsColour colourised code, in HTML format."
    longDesc   = Just (\_ -> "Requires hscolour.\n")
    options showOrParseArgs =
      [optionVerbosity hscolourVerbosity (\v flags -> flags { hscolourVerbosity = v })
      ,optionDistPref
         hscolourDistPref (\d flags -> flags { hscolourDistPref = d })
         showOrParseArgs

      ,option "" ["executables"]
         "Run hscolour for Executables targets"
         hscolourExecutables (\v flags -> flags { hscolourExecutables = v })
         trueArg

      ,option "" ["css"]
         "Use a cascading style sheet"
         hscolourCSS (\v flags -> flags { hscolourCSS = v })
         (reqArgFlag "PATH")
      ]

-- ------------------------------------------------------------
-- * Haddock flags
-- ------------------------------------------------------------

data HaddockFlags = HaddockFlags {
    haddockProgramPaths :: [(String, FilePath)],
    haddockProgramArgs  :: [(String, [String])],
    haddockHoogle       :: Flag Bool,
    haddockHtml         :: Flag Bool,
    haddockHtmlLocation :: Flag String,
    haddockExecutables  :: Flag Bool,
    haddockInternal     :: Flag Bool,
    haddockCss          :: Flag FilePath,
    haddockHscolour     :: Flag Bool,
    haddockHscolourCss  :: Flag FilePath,
    haddockDistPref     :: Flag FilePath,
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
    haddockExecutables  = Flag False,
    haddockInternal     = Flag False,
    haddockCss          = NoFlag,
    haddockHscolour     = Flag False,
    haddockHscolourCss  = NoFlag,
    haddockDistPref     = Flag defaultDistPref,
    haddockVerbosity    = Flag normal
  }

haddockCommand :: CommandUI HaddockFlags
haddockCommand = makeCommand name shortDesc longDesc defaultHaddockFlags options
  where
    name       = "haddock"
    shortDesc  = "Generate Haddock HTML documentation."
    longDesc   = Just $ \_ -> "Requires the program haddock, either version 0.x or 2.x.\n"
    options showOrParseArgs =
      [optionVerbosity haddockVerbosity (\v flags -> flags { haddockVerbosity = v })
      ,optionDistPref
         haddockDistPref (\d flags -> flags { haddockDistPref = d })
         showOrParseArgs

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

      ,option "" ["executables"]
         "Run haddock for Executables targets"
         haddockExecutables (\v flags -> flags { haddockExecutables = v })
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
      ]
      ++ programConfigurationPaths   progConf ParseArgs
             haddockProgramPaths (\v flags -> flags { haddockProgramPaths = v})
      ++ programConfigurationOptions progConf ParseArgs
             haddockProgramArgs  (\v flags -> flags { haddockProgramArgs = v})
    progConf = addKnownProgram haddockProgram
             $ addKnownProgram ghcProgram
             $ emptyProgramConfiguration

emptyHaddockFlags :: HaddockFlags
emptyHaddockFlags = mempty

instance Monoid HaddockFlags where
  mempty = HaddockFlags {
    haddockProgramPaths = mempty,
    haddockProgramArgs  = mempty,
    haddockHoogle       = mempty,
    haddockHtml         = mempty,
    haddockHtmlLocation = mempty,
    haddockExecutables  = mempty,
    haddockInternal     = mempty,
    haddockCss          = mempty,
    haddockHscolour     = mempty,
    haddockHscolourCss  = mempty,
    haddockDistPref     = mempty,
    haddockVerbosity    = mempty
  }
  mappend a b = HaddockFlags {
    haddockProgramPaths = combine haddockProgramPaths,
    haddockProgramArgs  = combine haddockProgramArgs,
    haddockHoogle       = combine haddockHoogle,
    haddockHtml         = combine haddockHoogle,
    haddockHtmlLocation = combine haddockHtmlLocation,
    haddockExecutables  = combine haddockExecutables,
    haddockInternal     = combine haddockInternal,
    haddockCss          = combine haddockCss,
    haddockHscolour     = combine haddockHscolour,
    haddockHscolourCss  = combine haddockHscolourCss,
    haddockDistPref     = combine haddockDistPref,
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
    cleanDistPref  = Flag defaultDistPref,
    cleanVerbosity = Flag normal
  }

cleanCommand :: CommandUI CleanFlags
cleanCommand = makeCommand name shortDesc longDesc defaultCleanFlags options
  where
    name       = "clean"
    shortDesc  = "Clean up after a build."
    longDesc   = Just (\_ -> "Removes .hi, .o, preprocessed sources, etc.\n")
    options showOrParseArgs =
      [optionVerbosity cleanVerbosity (\v flags -> flags { cleanVerbosity = v })
      ,optionDistPref
         cleanDistPref (\d flags -> flags { cleanDistPref = d })
         showOrParseArgs

      ,option "s" ["save-configure"]
         "Do not remove the configuration file (dist/setup-config) during cleaning.  Saves need to reconfigure."
         cleanSaveConf (\v flags -> flags { cleanSaveConf = v })
         trueArg
      ]

emptyCleanFlags :: CleanFlags
emptyCleanFlags = mempty

instance Monoid CleanFlags where
  mempty = CleanFlags {
    cleanSaveConf  = mempty,
    cleanDistPref  = mempty,
    cleanVerbosity = mempty
  }
  mappend a b = CleanFlags {
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
    buildVerbosity   :: Flag Verbosity
  }
  deriving Show

{-# DEPRECATED buildVerbose "Use buildVerbosity instead" #-}
buildVerbose :: BuildFlags -> Verbosity
buildVerbose = fromFlagOrDefault normal . buildVerbosity

defaultBuildFlags :: BuildFlags
defaultBuildFlags  = BuildFlags {
    buildProgramPaths = mempty,
    buildProgramArgs = [],
    buildDistPref    = Flag defaultDistPref,
    buildVerbosity   = Flag normal
  }

buildCommand :: ProgramConfiguration -> CommandUI BuildFlags
buildCommand progConf = makeCommand name shortDesc longDesc defaultBuildFlags options
  where
    name       = "build"
    shortDesc  = "Make this package ready for installation."
    longDesc   = Nothing
    options showOrParseArgs =
      optionVerbosity buildVerbosity (\v flags -> flags { buildVerbosity = v })
      : optionDistPref
          buildDistPref (\d flags -> flags { buildDistPref = d })
          showOrParseArgs

      : programConfigurationPaths   progConf showOrParseArgs
          buildProgramPaths (\v flags -> flags { buildProgramPaths = v})

     ++ programConfigurationOptions progConf showOrParseArgs
          buildProgramArgs (\v flags -> flags { buildProgramArgs = v})

emptyBuildFlags :: BuildFlags
emptyBuildFlags = mempty

instance Monoid BuildFlags where
  mempty = BuildFlags {
    buildProgramPaths = mempty,
    buildProgramArgs = mempty,
    buildVerbosity   = mempty,
    buildDistPref    = mempty
  }
  mappend a b = BuildFlags {
    buildProgramPaths = combine buildProgramPaths,
    buildProgramArgs = combine buildProgramArgs,
    buildVerbosity   = combine buildVerbosity,
    buildDistPref    = combine buildDistPref
  }
    where combine field = field a `mappend` field b

-- ------------------------------------------------------------
-- * Test flags
-- ------------------------------------------------------------

data TestShowDetails = Never | Failures | Always
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
    mappend a b = if a < b then b else a

data TestFlags = TestFlags {
    testDistPref  :: Flag FilePath,
    testVerbosity :: Flag Verbosity,
    testHumanLog :: Flag PathTemplate,
    testMachineLog :: Flag PathTemplate,
    testShowDetails :: Flag TestShowDetails,
    testKeepTix :: Flag Bool,
    --TODO: eliminate the test list and pass it directly as positional args to the testHook
    testList :: Flag [String],
    -- TODO: think about if/how options are passed to test exes
    testOptions :: [PathTemplate]
  }

defaultTestFlags :: TestFlags
defaultTestFlags  = TestFlags {
    testDistPref  = Flag defaultDistPref,
    testVerbosity = Flag normal,
    testHumanLog = toFlag $ toPathTemplate $ "$pkgid-$test-suite.log",
    testMachineLog = toFlag $ toPathTemplate $ "$pkgid.log",
    testShowDetails = toFlag Failures,
    testKeepTix = toFlag False,
    testList = Flag [],
    testOptions = []
  }

testCommand :: CommandUI TestFlags
testCommand = makeCommand name shortDesc longDesc defaultTestFlags options
  where
    name       = "test"
    shortDesc  = "Run the test suite, if any (configure with UserHooks)."
    longDesc   = Nothing
    options showOrParseArgs =
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
             ++ "'failures': show results of failing test cases.")
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

emptyTestFlags :: TestFlags
emptyTestFlags  = mempty

instance Monoid TestFlags where
  mempty = TestFlags {
    testDistPref  = mempty,
    testVerbosity = mempty,
    testHumanLog = mempty,
    testMachineLog = mempty,
    testShowDetails = mempty,
    testKeepTix = mempty,
    testList = mempty,
    testOptions = mempty
  }
  mappend a b = TestFlags {
    testDistPref  = combine testDistPref,
    testVerbosity = combine testVerbosity,
    testHumanLog = combine testHumanLog,
    testMachineLog = combine testMachineLog,
    testShowDetails = combine testShowDetails,
    testKeepTix = combine testKeepTix,
    testList = combine testList,
    testOptions = combine testOptions
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

programConfigurationPaths
  :: ProgramConfiguration
  -> ShowOrParseArgs
  -> (flags -> [(String, FilePath)])
  -> ([(String, FilePath)] -> (flags -> flags))
  -> [OptionField flags]
programConfigurationPaths progConf showOrParseArgs get set =
  case showOrParseArgs of
    -- we don't want a verbose help text list so we just show a generic one:
    ShowArgs  -> [withProgramPath "PROG"]
    ParseArgs -> map (withProgramPath . programName . fst) (knownPrograms progConf)
  where
    withProgramPath prog =
      option "" ["with-" ++ prog]
        ("give the path to " ++ prog)
        get set
        (reqArg' "PATH" (\path -> [(prog, path)])
          (\progPaths -> [ path | (prog', path) <- progPaths, prog==prog' ]))

programConfigurationOptions
  :: ProgramConfiguration
  -> ShowOrParseArgs
  -> (flags -> [(String, [String])])
  -> ([(String, [String])] -> (flags -> flags))
  -> [OptionField flags]
programConfigurationOptions progConf showOrParseArgs get set =
  case showOrParseArgs of
    -- we don't want a verbose help text list so we just show a generic one:
    ShowArgs  -> [programOptions  "PROG", programOption   "PROG"]
    ParseArgs -> map (programOptions . programName . fst) (knownPrograms progConf)
              ++ map (programOption  . programName . fst) (knownPrograms progConf)
  where
    programOptions prog =
      option "" [prog ++ "-options"]
        ("give extra options to " ++ prog)
        get set
        (reqArg' "OPTS" (\args -> [(prog, splitArgs args)]) (const []))

    programOption prog =
      option "" [prog ++ "-option"]
        ("give an extra option to " ++ prog ++
         " (no need to quote options containing spaces)")
        get set
        (reqArg' "OPT" (\arg -> [(prog, [arg])])
           (\progArgs -> concat [ args | (prog', args) <- progArgs, prog==prog' ]))


-- ------------------------------------------------------------
-- * GetOpt Utils
-- ------------------------------------------------------------

boolOpt :: SFlags -> SFlags -> MkOptDescr (a -> Flag Bool) (Flag Bool -> a -> a) a
boolOpt  = Command.boolOpt  flagToMaybe Flag

boolOpt' :: OptFlags -> OptFlags -> MkOptDescr (a -> Flag Bool) (Flag Bool -> a -> a) a
boolOpt' = Command.boolOpt' flagToMaybe Flag

trueArg, falseArg :: SFlags -> LFlags -> Description -> (b -> Flag Bool) ->
                     (Flag Bool -> (b -> b)) -> OptDescr b
trueArg  = noArg (Flag True)
falseArg = noArg (Flag False)

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

configureCCompiler :: Verbosity -> ProgramConfiguration -> IO (FilePath, [String])
configureCCompiler verbosity lbi = configureProg verbosity lbi gccProgram

configureLinker :: Verbosity -> ProgramConfiguration -> IO (FilePath, [String])
configureLinker verbosity lbi = configureProg verbosity lbi ldProgram

configureProg :: Verbosity -> ProgramConfiguration -> Program -> IO (FilePath, [String])
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
