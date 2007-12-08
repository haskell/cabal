{-# OPTIONS -cpp #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Setup
-- Copyright   :  Isaac Jones 2003-2004
-- 
-- Maintainer  :  Isaac Jones <ijones@syntaxpolice.org>
-- Stability   :  alpha
-- Portability :  portable
--
-- Explanation: Data types and parser for the standard command-line
-- setup.  Will also return commands it doesn't know about.

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

  module Distribution.Simple.Compiler,

  GlobalFlags(..),   emptyGlobalFlags,   globalCommand,
  ConfigFlags(..),   emptyConfigFlags,   configureCommand,
  CopyFlags(..),     emptyCopyFlags,     copyCommand,
  InstallFlags(..),  emptyInstallFlags,  installCommand,
  HaddockFlags(..),  emptyHaddockFlags,  haddockCommand,
  HscolourFlags(..), emptyHscolourFlags, hscolourCommand,
  BuildFlags(..),    emptyBuildFlags,    buildCommand,
  CleanFlags(..),    emptyCleanFlags,    cleanCommand,
  PFEFlags(..),      emptyPFEFlags,      programaticaCommand,
  MakefileFlags(..), emptyMakefileFlags, makefileCommand,
  RegisterFlags(..), emptyRegisterFlags, registerCommand, unregisterCommand,
  SDistFlags(..),    emptySDistFlags,    sdistCommand,
                                         testCommand,
  CopyDest(..),
  configureArgs,

#ifdef DEBUG
                           hunitTests,
#endif
                           ) where


-- Misc:
#ifdef DEBUG
import Test.HUnit (Test(..))
#endif

import Distribution.Simple.Command
import Distribution.Simple.Compiler (CompilerFlavor(..), Compiler(..),
                                     defaultCompilerFlavor, PackageDB(..))
import Distribution.Simple.Utils (wrapText)
import Distribution.Simple.Program (Program(..), ProgramConfiguration,
                             knownPrograms, userSpecifyPath, userSpecifyArgs)
import Distribution.Simple.InstallDirs (CopyDest(..), InstallDirs(..))
import Data.List (sort)
import Data.Char( toLower, isSpace )
import Distribution.GetOpt as GetOpt
import Distribution.Verbosity

-- ------------------------------------------------------------
-- * Flag-related types
-- ------------------------------------------------------------

-- | Flags that apply at the top level, not to any sub-command.
data GlobalFlags = GlobalFlags { globalVersion :: Bool
                               , globalNumericVersion :: Bool }

emptyGlobalFlags :: GlobalFlags
emptyGlobalFlags = GlobalFlags { globalVersion = False
                               , globalNumericVersion = False }

-- | Flags to @configure@ command
data ConfigFlags = ConfigFlags {
        configPrograms :: ProgramConfiguration, -- ^All programs that cabal may run
        configHcFlavor :: Maybe CompilerFlavor, -- ^The \"flavor\" of the compiler, sugh as GHC or Hugs.
        configHcPath   :: Maybe FilePath, -- ^given compiler location
        configHcPkg    :: Maybe FilePath, -- ^given hc-pkg location
        configVanillaLib  :: Bool,        -- ^Enable vanilla library
        configProfLib  :: Bool,           -- ^Enable profiling in the library
        configSharedLib  :: Bool,         -- ^Build shared library
        configProfExe  :: Bool,           -- ^Enable profiling in the executables.
        configConfigureArgs :: [String],  -- ^Extra arguments to @configure@
        configOptimization :: Bool,       -- ^Enable optimization.
        configPrefix   :: Maybe FilePath,
		-- ^installation prefix
	configBinDir   :: Maybe FilePath, 
		-- ^installation dir for binaries,
	configLibDir   :: Maybe FilePath, 
		-- ^installation dir for object code libraries, 
	configLibSubDir :: Maybe FilePath,
		-- ^subdirectory of libdir in which libs are installed
	configLibExecDir :: Maybe FilePath,
		-- ^installation dir for program executables,
	configDataDir  :: Maybe FilePath,
		-- ^installation dir for read-only arch-independent data,
	configDataSubDir :: Maybe FilePath,
		-- ^subdirectory of datadir in which data files are installed
	configDocDir   :: Maybe FilePath,
		-- ^installation dir for documentation
	configHtmlDir   :: Maybe FilePath,
		-- ^installation dir for HTML documentation
	configHaddockDir :: Maybe FilePath,
		-- ^installation dir for haddock interfaces
        configScratchDir :: Maybe FilePath,

        configVerbose  :: Verbosity,      -- ^verbosity level
	configPackageDB:: PackageDB,	  -- ^ the --user flag?
	configGHCiLib  :: Bool,           -- ^Enable compiling library for GHCi
	configSplitObjs :: Bool,	  -- ^Enable -split-objs with GHC
        configConfigurationsFlags :: [(String, Bool)]
    }
    deriving Show

-- |The default configuration of a package, before running configure,
-- most things are \"Nothing\", zero, etc.
emptyConfigFlags :: ProgramConfiguration -> ConfigFlags
emptyConfigFlags progConf = ConfigFlags {
        configPrograms = progConf,
        configHcFlavor = defaultCompilerFlavor,
        configHcPath   = Nothing,
        configHcPkg    = Nothing,
        configVanillaLib  = True,
        configProfLib  = False,
        configSharedLib  = False,
        configProfExe  = False,
        configConfigureArgs = [],
        configOptimization = True,
        configPrefix   = Nothing,
	configBinDir   = Nothing,
	configLibDir   = Nothing,
	configLibSubDir = Nothing,
	configLibExecDir = Nothing,
	configDataDir  = Nothing,
	configDataSubDir = Nothing,
	configDocDir = Nothing,
	configHtmlDir = Nothing,
	configHaddockDir = Nothing,
        configScratchDir = Nothing,
        configVerbose  = normal,
	configPackageDB = GlobalPackageDB,
	configGHCiLib  = True,
	configSplitObjs = False, -- takes longer, so turn off by default
        configConfigurationsFlags = []
    }

-- | Flags to @copy@: (destdir, copy-prefix (backwards compat), verbosity)
data CopyFlags = CopyFlags {copyDest :: CopyDest
                           ,copyVerbose :: Verbosity}
    deriving Show

emptyCopyFlags :: CopyFlags
emptyCopyFlags = CopyFlags{ copyDest = NoCopyDest,
                            copyVerbose = normal }

-- | Flags to @install@: (package db, verbosity)
data InstallFlags = InstallFlags {installPackageDB :: Maybe PackageDB
                                 ,installVerbose :: Verbosity}
    deriving Show

emptyInstallFlags :: InstallFlags
emptyInstallFlags = InstallFlags{ installPackageDB=Nothing,
                                  installVerbose = normal }

-- | Flags to @sdist@: (snapshot, verbosity)
data SDistFlags = SDistFlags {sDistSnapshot :: Bool
                             ,sDistVerbose :: Verbosity}
    deriving Show

emptySDistFlags :: SDistFlags
emptySDistFlags = SDistFlags {sDistSnapshot = False
                             ,sDistVerbose = normal}

-- | Flags to @register@ and @unregister@: (user package, gen-script,
-- in-place, verbosity)
data RegisterFlags = RegisterFlags { regPackageDB :: Maybe PackageDB
                                   , regGenScript :: Bool
                                   , regGenPkgConf :: Bool
                                   , regPkgConfFile :: Maybe FilePath
                                   , regInPlace :: Bool
                                   , regVerbose :: Verbosity }
    deriving Show


emptyRegisterFlags :: RegisterFlags
emptyRegisterFlags = RegisterFlags { regPackageDB = Nothing,
                                     regGenScript = False,
                                     regGenPkgConf = False,
                                     regPkgConfFile = Nothing,
                                     regInPlace = False,
                                     regVerbose = normal }

data HscolourFlags = HscolourFlags {hscolourCSS :: Maybe FilePath
                                   ,hscolourExecutables :: Bool
                                   ,hscolourVerbose :: Verbosity}
    deriving Show

emptyHscolourFlags :: HscolourFlags
emptyHscolourFlags = HscolourFlags {hscolourCSS = Nothing
                                   ,hscolourExecutables = False
                                   ,hscolourVerbose = normal}

data HaddockFlags = HaddockFlags {haddockHoogle :: Bool
                                 ,haddockHtmlLocation :: Maybe String
                                 ,haddockExecutables :: Bool
                                 ,haddockCss :: Maybe FilePath
                                 ,haddockHscolour :: Bool
                                 ,haddockHscolourCss :: Maybe FilePath
                                 ,haddockVerbose :: Verbosity}
    deriving Show

emptyHaddockFlags :: HaddockFlags
emptyHaddockFlags = HaddockFlags {haddockHoogle = False
                                 ,haddockHtmlLocation = Nothing
                                 ,haddockExecutables = False
                                 ,haddockCss = Nothing
                                 ,haddockHscolour = False
                                 ,haddockHscolourCss = Nothing
                                 ,haddockVerbose = normal}

data CleanFlags   = CleanFlags   {cleanSaveConf  :: Bool
                                 ,cleanVerbose   :: Verbosity}
    deriving Show
emptyCleanFlags :: CleanFlags
emptyCleanFlags = CleanFlags {cleanSaveConf = False, cleanVerbose = normal}

data BuildFlags   = BuildFlags   {buildVerbose   :: Verbosity,
                                  buildPrograms  :: ProgramConfiguration}
    deriving Show

emptyBuildFlags :: ProgramConfiguration -> BuildFlags
emptyBuildFlags progs = BuildFlags {buildVerbose  = normal,
                                    buildPrograms = progs}

data MakefileFlags = MakefileFlags {makefileVerbose :: Verbosity,
                                    makefileFile :: Maybe FilePath}
    deriving Show
emptyMakefileFlags :: MakefileFlags
emptyMakefileFlags = MakefileFlags {makefileVerbose = normal,
                                    makefileFile = Nothing}

data PFEFlags     = PFEFlags     {pfeVerbose     :: Verbosity}
    deriving Show

emptyPFEFlags :: PFEFlags
emptyPFEFlags = PFEFlags { pfeVerbose = normal }

-- ------------------------------------------------------------
-- * Commands
-- ------------------------------------------------------------

globalCommand :: CommandUI GlobalFlags
globalCommand = makeCommand name shortDesc longDesc emptyFlags options
  where
    name       = ""
    shortDesc  = ""
    longDesc   = Just $ \pname ->
         "Typical steps for installing Cabal packages:\n"
      ++ unlines [ "  " ++ pname ++ " " ++ x
                 | x <- ["configure", "build", "install"]]
      ++ "\nFor more information about a command, try '"
          ++ pname ++ " COMMAND --help'."
      ++ "\nThis Setup program uses the Haskell Cabal Infrastructure."
      ++ "\nSee http://www.haskell.org/cabal/ for more information.\n"
    emptyFlags = emptyGlobalFlags
    options _  =
      [option ['V'] ["version"]
         "Print version information"
         (noArg $ \flags -> flags { globalVersion = True })
      ,option [] ["numeric-version"]
         "Print just the version number"
         (noArg $ \flags -> flags { globalNumericVersion = True })
      ]

configureCommand :: ProgramConfiguration -> CommandUI ConfigFlags
configureCommand progConf = makeCommand name shortDesc longDesc emptyFlags options
  where
    name       = "configure"
    shortDesc  = "Prepare to build the package."
    longDesc   = Just (\_ -> programFlagsDescription progConf)
    emptyFlags = emptyConfigFlags progConf
    options showOrParseArgs =
      [optionVerbose
         (\v flags -> flags { configVerbose = v })

      ,option "g" ["ghc"]
         "compile with GHC"
         (noArg $ \flags -> flags { configHcFlavor = Just GHC })

      ,option "" ["nhc98"]
         "compile with NHC"
         (noArg $ \flags -> flags { configHcFlavor = Just NHC })

      ,option "" ["jhc"]
         "compile with JHC"
         (noArg $ \flags -> flags { configHcFlavor = Just JHC})

      ,option "" ["hugs"]
         "compile with hugs"
         (noArg $ \flags -> flags { configHcFlavor = Just Hugs })

      ,option "w" ["with-compiler"]
         "give the path to a particular compiler"
         (reqArg "PATH" $ \path flags -> flags { configHcPath = Just path })

      ,option "" ["with-hc-pkg"]
         "give the path to the package tool"
         (reqArg "PATH" $ \path flags -> flags { configHcPkg = Just path })

      ,option "" ["prefix"]
         "bake this prefix in preparation of installation"
         (reqArg "DIR" $ \path flags -> flags { configPrefix = Just path })

      ,option "" ["bindir"]
         "installation directory for executables"
         (reqArg "DIR" $ \path flags -> flags { configBinDir = Just path })

      ,option "" ["libdir"]
         "installation directory for libraries"
         (reqArg "DIR" $ \path flags -> flags { configLibDir = Just path })

      ,option "" ["libsubdir"]
	 "subdirectory of libdir in which libs are installed"
         (reqArg "DIR" $ \path flags -> flags { configLibSubDir = Just path })

      ,option "" ["libexecdir"]
	 "installation directory for program executables"
         (reqArg "DIR" $ \path flags -> flags { configLibExecDir = Just path })

      ,option "" ["datadir"]
	 "installation directory for read-only data"
         (reqArg "DIR" $ \path flags -> flags { configDataDir = Just path })

      ,option "" ["datasubdir"]
	 "subdirectory of datadir in which data files are installed"
         (reqArg "DIR" $ \path flags -> flags { configDataSubDir = Just path })

      ,option "" ["docdir"]
	 "installation directory for documentation"
         (reqArg "DIR" $ \path flags -> flags { configDocDir = Just path })

      ,option "" ["htmldir"]
	 "installation directory for HTML documentation"
         (reqArg "DIR" $ \path flags -> flags { configHtmlDir = Just path })

      ,option "" ["haddockdir"]
	 "installation directory for haddock interfaces"
         (reqArg "DIR" $ \path flags -> flags { configHaddockDir = Just path })

      ,option "b" ["scratchdir"]
         "directory to receive the built package [dist/scratch]"
          (reqArg "DIR" $ \path flags -> flags { configScratchDir = Just path })

      ,option "" ["enable-library-vanilla"]
         "Enable vanilla libraries"
          (noArg $ \flags -> flags { configVanillaLib  = True })

      ,option "" ["disable-library-vanilla"]
         "Disable vanilla libraries"
          (noArg $ \flags -> flags { configVanillaLib  = False,
                                     configGHCiLib = False })

      ,option "p" ["enable-library-profiling"]
         "Enable library profiling"
         (noArg $ \flags -> flags { configProfLib = True })

      ,option "" ["disable-library-profiling"]
         "Disable library profiling"
         (noArg $ \flags -> flags { configProfLib = False })

      ,option "" ["enable-shared"]
         "Enable shared library"
         (noArg $ \flags -> flags { configSharedLib = True })

      ,option "" ["disable-shared"]
         "Disable shared library"
         (noArg $ \flags -> flags { configSharedLib = False })

      ,option "" ["enable-executable-profiling"]
         "Enable executable profiling"
         (noArg $ \flags -> flags { configProfExe = True })

      ,option "" ["disable-executable-profiling"]
         "Disable executable profiling"
         (noArg $ \flags -> flags { configProfExe = False })

      ,option "O" ["enable-optimization"]
         "Build with optimization"
         (noArg $ \flags -> flags { configOptimization = True })

      ,option "" ["disable-optimization"]
         "Build without optimization"
         (noArg $ \flags -> flags { configOptimization = False })

      ,option "" ["enable-library-for-ghci"]
         "compile library for use with GHCi"
         (noArg $ \flags -> flags { configGHCiLib  = True })

      ,option "" ["disable-library-for-ghci"]
         "do not compile libraries for GHCi"
         (noArg $ \flags -> flags { configGHCiLib  = False })

      ,option "" ["enable-split-objs"]
         "split library into smaller objects to reduce binary sizes (GHC 6.6+)"
         (noArg $ \flags -> flags { configSplitObjs = True })

      ,option "" ["disable-split-objs"]
         "split library into smaller objects to reduce binary sizes (GHC 6.6+)"
         (noArg $ \flags -> flags { configSplitObjs = False })

      ,option "" ["configure-option"]
         "Extra option for configure"
         (reqArg "OPT" $ \opt flags -> flags { configConfigureArgs =
                                         opt : configConfigureArgs flags })

      ,option "" ["user"]
         "allow dependencies to be satisfied from the user package database. also implies install --user"
         (noArg $ \flags -> flags { configPackageDB = UserPackageDB })

      ,option "" ["global"]
         "(default) dependencies must be satisfied from the global package database"
         (noArg $ \flags -> flags { configPackageDB = GlobalPackageDB })

      ,option "f" ["flags"]
         "Force values for the given flags in Cabal conditionals in the .cabal file.  E.g., --flags=\"debug -usebytestrings\" forces the flag \"debug\" to true and \"usebytestrings\" to false."
         (reqArg "FLAGS" $ \fs flags -> flags { configConfigurationsFlags =
                                  flagList fs ++ configConfigurationsFlags flags })
      ]
      ++ programConfigurationPaths   progConf showOrParseArgs liftUpdatePrograms
      ++ programConfigurationOptions progConf showOrParseArgs liftUpdatePrograms

    flagList :: String -> [(String, Bool)]
    flagList = map tagWithValue . words
      where tagWithValue ('-':fname) = (map toLower fname, False)
            tagWithValue fname       = (map toLower fname, True)

    liftUpdatePrograms update flags = flags {
        configPrograms = update (configPrograms flags)
      }

programFlagsDescription :: ProgramConfiguration -> String
programFlagsDescription progConf =
     "The flags --with-PROG and --PROG-option(s) can be used with"
  ++ " the following programs:"
  ++ (concatMap ("\n  "++) . wrapText 77 . sort)
     [ programName prog | (prog, _) <- knownPrograms progConf ]
  ++ "\n"

programConfigurationPaths
  :: ProgramConfiguration
  -> ShowOrParseArgs
  -> ((ProgramConfiguration -> ProgramConfiguration) -> (flags -> flags))
  -> [GetOpt.OptDescr (flags -> flags)]
programConfigurationPaths progConf showOrParseArgs lift =
  case showOrParseArgs of
    -- we don't want a verbose help text list so we just show a generic one:
    ShowArgs  -> [withProgramPath "PROG"]
    ParseArgs -> map (withProgramPath . programName . fst) (knownPrograms progConf)
  where
    withProgramPath prog =
      option "" ["with-" ++ prog]
        ("give the path to " ++ prog)
        (reqArg "PATH" $ \path -> lift (userSpecifyPath prog path))

programConfigurationOptions
  :: ProgramConfiguration
  -> ShowOrParseArgs
  -> ((ProgramConfiguration -> ProgramConfiguration) -> (flags -> flags))
  -> [GetOpt.OptDescr (flags -> flags)]
programConfigurationOptions progConf showOrParseArgs lift =
  case showOrParseArgs of
    -- we don't want a verbose help text list so we just show a generic one:
    ShowArgs  -> [programOptions  "PROG", programOption   "PROG"]
    ParseArgs -> map (programOptions . programName . fst) (knownPrograms progConf)
              ++ map (programOption  . programName . fst) (knownPrograms progConf)
  where
    programOptions prog =
      option "" [prog ++ "-options"]
        ("give extra options to " ++ prog)
        (reqArg "OPTS" $ \args -> lift (userSpecifyArgs prog (splitArgs args)))

    programOption prog =
      option "" [prog ++ "-option"]
        ("give an extra option to " ++ prog ++
         " (no need to quote options containing spaces)")
        (reqArg "OPT" $ \arg -> lift (userSpecifyArgs prog [arg]))

buildCommand :: ProgramConfiguration -> CommandUI BuildFlags
buildCommand progConf = makeCommand name shortDesc longDesc emptyFlags options
  where
    name       = "build"
    shortDesc  = "Make this package ready for installation."
    longDesc   = Nothing
    emptyFlags = emptyBuildFlags progConf
    options showOrParseArgs =
      optionVerbose
        (\v flags -> flags { buildVerbose = v })

      : programConfigurationOptions progConf showOrParseArgs
        (\update flags -> flags { buildPrograms = update (buildPrograms flags) })

makefileCommand  :: CommandUI MakefileFlags
makefileCommand = makeCommand name shortDesc longDesc emptyFlags options
  where
    name       = "makefile"
    shortDesc  = "Generate a makefile (only for GHC libraries)."
    longDesc   = Nothing
    emptyFlags = emptyMakefileFlags
    options _  =
      [optionVerbose
         (\v flags -> flags { makefileVerbose = v })

      ,option "f" ["file"]
         "Filename to use (default: Makefile)."
         (reqArg "PATH" $ \f flags -> flags { makefileFile = Just f })
      ]

hscolourCommand  :: CommandUI HscolourFlags
hscolourCommand = makeCommand name shortDesc longDesc emptyFlags options
  where
    name       = "hscolour"
    shortDesc  = "Generate HsColour colourised code, in HTML format."
    longDesc   = Just (\_ -> "Requires hscolour.")
    emptyFlags = emptyHscolourFlags
    options _  =
      [optionVerbose
         (\v flags -> flags { hscolourVerbose = v })

      ,option "" ["executables"]
         "Run hscolour for Executables targets"
         (noArg $ \flags -> flags { hscolourExecutables = True })

      ,option "" ["css"]
         "Use a cascading style sheet"
         (reqArg "PATH" $ \c flags -> flags { hscolourCSS = Just c })
      ]

haddockCommand  :: CommandUI HaddockFlags
haddockCommand = makeCommand name shortDesc longDesc emptyFlags options
  where
    name       = "haddock"
    shortDesc  = "Generate Haddock HTML documentation."
    longDesc   = Just (\_ -> "Requires cpphs and haddock.\n")
    emptyFlags = emptyHaddockFlags
    options _  =
      [optionVerbose
         (\v flags -> flags { haddockVerbose = v })

      ,option "" ["hoogle"]
         "Generate a hoogle database"
         (noArg $ \flags -> flags { haddockHoogle = True })

      ,option "" ["html-location"]
         "Location of HTML documentation for pre-requisite packages"
         (reqArg "URL" $ \s flags -> flags { haddockHtmlLocation=Just s })

      ,option "" ["executables"]
         "Run haddock for Executables targets"
         (noArg $ \flags -> flags { haddockExecutables = True })

      ,option "" ["css"]
         "Use PATH as the haddock stylesheet"
         (reqArg "PATH" $ \path flags -> flags { haddockCss = Just path })

      ,option "" ["hyperlink-source"]
         "Hyperlink the documentation to the source code (using HsColour)"
         (noArg $ \flags -> flags { haddockHscolour = True })

      ,option "" ["hscolour-css"]
         "Use PATH as the HsColour stylesheet"
         (reqArg "PATH" $ \path flags -> flags { haddockHscolourCss = Just path })
      ]

programaticaCommand  :: CommandUI PFEFlags
programaticaCommand = makeCommand name shortDesc longDesc emptyFlags options
  where
    name       = "pfe"
    shortDesc  = "Generate Programatica Project."
    longDesc   = Nothing
    emptyFlags = emptyPFEFlags
    options _  =
      [optionVerbose
         (\v flags -> flags { pfeVerbose = v })
      ]

cleanCommand  :: CommandUI CleanFlags
cleanCommand = makeCommand name shortDesc longDesc emptyFlags options
  where
    name       = "clean"
    shortDesc  = "Clean up after a build."
    longDesc   = Just (\_ -> "Removes .hi, .o, preprocessed sources, etc.\n")
    emptyFlags = emptyCleanFlags
    options _  =
      [optionVerbose
         (\v flags -> flags { cleanVerbose = v })

      ,option "s" ["save-configure"]
         "Do not remove the configuration file (dist/setup-config) during cleaning.  Saves need to reconfigure."
         (noArg $ (\flags -> flags { cleanSaveConf = True }))
      ]

installCommand  :: CommandUI InstallFlags
installCommand = makeCommand name shortDesc longDesc emptyFlags options
  where
    name       = "install"
    shortDesc  = "Copy the files into the install locations. Run register."
    longDesc   = Just $ \_ ->
         "Unlike the copy command, install calls the register command.\n"
      ++ "If you want to install into a location that is not what was\n"
      ++ "specified in the configure step, use the copy command.\n"
    emptyFlags = emptyInstallFlags
    options _  =
      [optionVerbose
         (\v flags -> flags { installVerbose = v })

      ,option "" ["user"]
         "upon registration, register this package in the user's local package database"
         (noArg $ \flags -> flags { installPackageDB = Just UserPackageDB })

      ,option "" ["global"]
         "(default; override with configure) upon registration, register this package in the system-wide package database"
          (noArg $ \flags -> flags { installPackageDB = Just GlobalPackageDB })
      ]

copyCommand  :: CommandUI CopyFlags
copyCommand = makeCommand name shortDesc longDesc emptyFlags options
  where
    name       = "copy"
    shortDesc  = "Copy the files into the install locations."
    longDesc   = Just $ \_ ->
          "Does not call register, and allows a prefix at install time\n"
       ++ "Without the --destdir flag, configure determines location.\n"
    emptyFlags = emptyCopyFlags
    options _  =
      [optionVerbose
         (\v flags -> flags { copyVerbose = v })

      ,option "" ["destdir"]
         "directory to copy files to, prepended to installation directories"
         (reqArg "DIR" $ \path flags -> flags { copyDest = CopyTo path })

      ,option "" ["copy-prefix"]
         "[DEPRECATED, directory to copy files to instead of prefix]"
         (reqArg "DIR" $ \path flags -> flags { copyDest = CopyPrefix path })
      ]

sdistCommand  :: CommandUI SDistFlags
sdistCommand = makeCommand name shortDesc longDesc emptyFlags options
  where
    name       = "sdist"
    shortDesc  = "Generate a source distribution file (.tar.gz)."
    longDesc   = Nothing
    emptyFlags = emptySDistFlags
    options _  =
      [optionVerbose
         (\v flags -> flags { sDistVerbose = v })

      ,option "" ["snapshot"]
         "Produce a snapshot source distribution"
         (noArg $ \flags -> flags { sDistSnapshot = True })
      ]

testCommand  :: CommandUI ()
testCommand = makeCommand name shortDesc longDesc emptyFlags options
  where
    name       = "test"
    shortDesc  = "Run the test suite, if any (configure with UserHooks)."
    longDesc   = Nothing
    emptyFlags = ()
    options _  = []

registerCommand  :: CommandUI RegisterFlags
registerCommand = makeCommand name shortDesc longDesc emptyFlags options
  where
    name       = "register"
    shortDesc  = "Register this package with the compiler."
    longDesc   = Nothing
    emptyFlags = emptyRegisterFlags
    options _  =
      [optionVerbose
         (\v flags -> flags { regVerbose = v })

      ,option "" ["user"]
         "upon registration, register this package in the user's local package database"
          (noArg $ \flags -> flags { regPackageDB = Just UserPackageDB })

      ,option "" ["global"]
         "(default) upon registration, register this package in the system-wide package database"
          (noArg $ \flags -> flags { regPackageDB = Just GlobalPackageDB })

      ,option "" ["inplace"]
         "register the package in the build location, so it can be used without being installed"
         (noArg $ \flags -> flags { regInPlace = True })

      ,option "" ["gen-script"]
         "instead of registering, generate a script to register later"
         (noArg $ \flags -> flags { regGenScript = True })

      ,option "" ["gen-pkg-config"]
         "instead of registering, generate a package registration file"
         (optArg "PKG" $ \f flags -> flags { regGenPkgConf  = True,
                                             regPkgConfFile = f })
      ]

unregisterCommand  :: CommandUI RegisterFlags
unregisterCommand = makeCommand name shortDesc longDesc emptyFlags options
  where
    name       = "unregister"
    shortDesc  = "Unregister this package with the compiler."
    longDesc   = Nothing
    emptyFlags = emptyRegisterFlags
    options _  =
      [optionVerbose
         (\v flags -> flags { regVerbose = v })

      ,option "" ["user"]
         "unregister this package in the user's local package database"
         (noArg $ \flags -> flags { regPackageDB = Just UserPackageDB })

      ,option "" ["global"]
         "(default) unregister this package in the system-wide package database"
         (noArg $ \flags -> flags { regPackageDB = Just GlobalPackageDB })

      ,option "" ["gen-script"]
         "Instead of performing the unregister command, generate a script to unregister later"
         (noArg $ \flags -> flags { regGenScript = True })
      ]

-- ------------------------------------------------------------
-- * GetOpt Utils
-- ------------------------------------------------------------

option :: [Char] -> [String] -> String -> GetOpt.ArgDescr a -> GetOpt.OptDescr a
option short long arg desc = GetOpt.Option short long desc arg

noArg :: a -> GetOpt.ArgDescr a
noArg  = GetOpt.NoArg

reqArg :: String -> (String -> a) -> GetOpt.ArgDescr a
reqArg = flip GetOpt.ReqArg

optArg :: String -> (Maybe String -> a) -> GetOpt.ArgDescr a
optArg = flip GetOpt.OptArg

optionVerbose :: (Verbosity -> flags -> flags) -> GetOpt.OptDescr (flags -> flags)
optionVerbose lift =
  option "v" ["verbose"]
    "Control verbosity (n is 0--3, default verbosity level is 1)"
    (optArg "n" (lift . flagToVerbosity))

-- ------------------------------------------------------------
-- * Other Utils
-- ------------------------------------------------------------

-- | Arguments to pass to a @configure@ script, e.g. generated by
-- @autoconf@.
configureArgs :: ConfigFlags -> [String]
configureArgs flags
  = hc_flag ++
        optFlag "with-hc-pkg" configHcPkg ++
        optFlag "prefix" configPrefix ++
        optFlag "bindir" configBinDir ++
        optFlag "libdir" configLibDir ++
        optFlag "libexecdir" configLibExecDir ++
        optFlag "datadir" configDataDir ++
        reverse (configConfigureArgs flags)
  where
        hc_flag = case (configHcFlavor flags, configHcPath flags) of
                        (_, Just hc_path)  -> ["--with-hc=" ++ hc_path]
                        (Just hc, Nothing) -> ["--with-hc=" ++ showHC hc]
                        (Nothing,Nothing)  -> []
        optFlag name config_field = case config_field flags of
                        Just p -> ["--" ++ name ++ "=" ++ p]
                        Nothing -> []

        showHC GHC = "ghc"
        showHC NHC = "nhc98"
        showHC JHC = "jhc"
        showHC Hugs = "hugs"
        showHC c    = "unknown compiler: " ++ (show c)

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

#ifdef DEBUG
hunitTests :: [Test]
hunitTests = []
-- The test cases kinda have to be rewritten from the ground up... :/
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
#endif


{- Testing ideas:
   * IO to look for hugs and hugs-pkg (which hugs, etc)
   * quickCheck to test permutations of arguments
   * what other options can we over-ride with a command-line flag?
-}

