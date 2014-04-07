{-# LANGUAGE DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.PackageDescription
-- Copyright   :  Isaac Jones 2003-2005
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This defines the data structure for the @.cabal@ file format. There are
-- several parts to this structure. It has top level info and then 'Library',
-- 'Executable', 'TestSuite', and 'Benchmark' sections each of which have
-- associated 'BuildInfo' data that's used to build the library, exe, test, or
-- benchmark.  To further complicate things there is both a 'PackageDescription'
-- and a 'GenericPackageDescription'. This distinction relates to cabal
-- configurations. When we initially read a @.cabal@ file we get a
-- 'GenericPackageDescription' which has all the conditional sections.
-- Before actually building a package we have to decide
-- on each conditional. Once we've done that we get a 'PackageDescription'.
-- It was done this way initially to avoid breaking too much stuff when the
-- feature was introduced. It could probably do with being rationalised at some
-- point to make it simpler.

module Distribution.PackageDescription (
        -- * Package descriptions
        PackageDescription(..),
        emptyPackageDescription,
        specVersion,
        descCabalVersion,
        BuildType(..),
        knownBuildTypes,

        -- ** Libraries
        Library(..),
        emptyLibrary,
        withLib,
        hasLibs,
        libModules,

        -- ** Executables
        Executable(..),
        emptyExecutable,
        withExe,
        hasExes,
        exeModules,

        -- * Tests
        TestSuite(..),
        TestSuiteInterface(..),
        TestType(..),
        testType,
        knownTestTypes,
        emptyTestSuite,
        hasTests,
        withTest,
        testModules,
        enabledTests,

        -- * Benchmarks
        Benchmark(..),
        BenchmarkInterface(..),
        BenchmarkType(..),
        benchmarkType,
        knownBenchmarkTypes,
        emptyBenchmark,
        hasBenchmarks,
        withBenchmark,
        benchmarkModules,
        enabledBenchmarks,

        -- * Build information
        BuildInfo(..),
        emptyBuildInfo,
        allBuildInfo,
        allLanguages,
        allExtensions,
        usedExtensions,
        hcOptions,

        -- ** Supplementary build information
        HookedBuildInfo,
        emptyHookedBuildInfo,
        updatePackageDescription,

        -- * package configuration
        GenericPackageDescription(..),
        Flag(..), FlagName(..), FlagAssignment,
        CondTree(..), ConfVar(..), Condition(..),

        -- * Source repositories
        SourceRepo(..),
        RepoKind(..),
        RepoType(..),
        knownRepoTypes,
  ) where

import Data.Data   (Data)
import Data.List   (nub, intercalate)
import Data.Maybe  (fromMaybe, maybeToList)
import Data.Monoid (Monoid(mempty, mappend))
import Data.Typeable ( Typeable )
import Control.Monad (MonadPlus(mplus))
import Text.PrettyPrint as Disp
import qualified Distribution.Compat.ReadP as Parse
import qualified Data.Char as Char (isAlphaNum, isDigit, toLower)

import Distribution.Package
         ( PackageName(PackageName), PackageIdentifier(PackageIdentifier)
         , Dependency, Package(..) )
import Distribution.ModuleName ( ModuleName )
import Distribution.Version
         ( Version(Version), VersionRange, anyVersion, orLaterVersion
         , asVersionIntervals, LowerBound(..) )
import Distribution.License  (License(AllRightsReserved))
import Distribution.Compiler (CompilerFlavor)
import Distribution.System   (OS, Arch)
import Distribution.Text
         ( Text(..), display )
import Language.Haskell.Extension
         ( Language, Extension )

-- -----------------------------------------------------------------------------
-- The PackageDescription type

-- | This data type is the internal representation of the file @pkg.cabal@.
-- It contains two kinds of information about the package: information
-- which is needed for all packages, such as the package name and version, and
-- information which is needed for the simple build system only, such as
-- the compiler options and library name.
--
data PackageDescription
    =  PackageDescription {
        -- the following are required by all packages:
        package        :: PackageIdentifier,
        license        :: License,
        licenseFiles   :: [FilePath],
        copyright      :: String,
        maintainer     :: String,
        author         :: String,
        stability      :: String,
        testedWith     :: [(CompilerFlavor,VersionRange)],
        homepage       :: String,
        pkgUrl         :: String,
        bugReports     :: String,
        sourceRepos    :: [SourceRepo],
        synopsis       :: String, -- ^A one-line summary of this package
        description    :: String, -- ^A more verbose description of this package
        category       :: String,
        customFieldsPD :: [(String,String)], -- ^Custom fields starting
                                             -- with x-, stored in a
                                             -- simple assoc-list.
        buildDepends   :: [Dependency],
        -- | The version of the Cabal spec that this package description uses.
        -- For historical reasons this is specified with a version range but
        -- only ranges of the form @>= v@ make sense. We are in the process of
        -- transitioning to specifying just a single version, not a range.
        specVersionRaw :: Either Version VersionRange,
        buildType      :: Maybe BuildType,
        -- components
        library        :: Maybe Library,
        executables    :: [Executable],
        testSuites     :: [TestSuite],
        benchmarks     :: [Benchmark],
        dataFiles      :: [FilePath],
        dataDir        :: FilePath,
        extraSrcFiles  :: [FilePath],
        extraTmpFiles  :: [FilePath],
        extraDocFiles  :: [FilePath]
    }
    deriving (Show, Read, Eq, Typeable, Data)

instance Package PackageDescription where
  packageId = package

-- | The version of the Cabal spec that this package should be interpreted
-- against.
--
-- Historically we used a version range but we are switching to using a single
-- version. Currently we accept either. This function converts into a single
-- version by ignoring upper bounds in the version range.
--
specVersion :: PackageDescription -> Version
specVersion pkg = case specVersionRaw pkg of
  Left  version      -> version
  Right versionRange -> case asVersionIntervals versionRange of
                          []                            -> Version [0] []
                          ((LowerBound version _, _):_) -> version

-- | The range of versions of the Cabal tools that this package is intended to
-- work with.
--
-- This function is deprecated and should not be used for new purposes, only to
-- support old packages that rely on the old interpretation.
--
descCabalVersion :: PackageDescription -> VersionRange
descCabalVersion pkg = case specVersionRaw pkg of
  Left  version      -> orLaterVersion version
  Right versionRange -> versionRange
{-# DEPRECATED descCabalVersion "Use specVersion instead" #-}

emptyPackageDescription :: PackageDescription
emptyPackageDescription
    =  PackageDescription {
                      package      = PackageIdentifier (PackageName "")
                                                       (Version [] []),
                      license      = AllRightsReserved,
                      licenseFiles = [],
                      specVersionRaw = Right anyVersion,
                      buildType    = Nothing,
                      copyright    = "",
                      maintainer   = "",
                      author       = "",
                      stability    = "",
                      testedWith   = [],
                      buildDepends = [],
                      homepage     = "",
                      pkgUrl       = "",
                      bugReports   = "",
                      sourceRepos  = [],
                      synopsis     = "",
                      description  = "",
                      category     = "",
                      customFieldsPD = [],
                      library      = Nothing,
                      executables  = [],
                      testSuites   = [],
                      benchmarks   = [],
                      dataFiles    = [],
                      dataDir      = "",
                      extraSrcFiles = [],
                      extraTmpFiles = [],
                      extraDocFiles = []
                     }

-- | The type of build system used by this package.
data BuildType
  = Simple      -- ^ calls @Distribution.Simple.defaultMain@
  | Configure   -- ^ calls @Distribution.Simple.defaultMainWithHooks defaultUserHooks@,
                -- which invokes @configure@ to generate additional build
                -- information used by later phases.
  | Make        -- ^ calls @Distribution.Make.defaultMain@
  | Custom      -- ^ uses user-supplied @Setup.hs@ or @Setup.lhs@ (default)
  | UnknownBuildType String
                -- ^ a package that uses an unknown build type cannot actually
                --   be built. Doing it this way rather than just giving a
                --   parse error means we get better error messages and allows
                --   you to inspect the rest of the package description.
                deriving (Show, Read, Eq, Typeable, Data)

knownBuildTypes :: [BuildType]
knownBuildTypes = [Simple, Configure, Make, Custom]

instance Text BuildType where
  disp (UnknownBuildType other) = Disp.text other
  disp other                    = Disp.text (show other)

  parse = do
    name <- Parse.munch1 Char.isAlphaNum
    return $ case name of
      "Simple"    -> Simple
      "Configure" -> Configure
      "Custom"    -> Custom
      "Make"      -> Make
      _           -> UnknownBuildType name

-- ---------------------------------------------------------------------------
-- The Library type

data Library = Library {
        exposedModules    :: [ModuleName],
        libExposed        :: Bool, -- ^ Is the lib to be exposed by default?
        libBuildInfo      :: BuildInfo
    }
    deriving (Show, Eq, Read, Typeable, Data)

instance Monoid Library where
  mempty = Library {
    exposedModules = mempty,
    libExposed     = True,
    libBuildInfo   = mempty
  }
  mappend a b = Library {
    exposedModules = combine exposedModules,
    libExposed     = libExposed a && libExposed b, -- so False propagates
    libBuildInfo   = combine libBuildInfo
  }
    where combine field = field a `mappend` field b

emptyLibrary :: Library
emptyLibrary = mempty

-- |does this package have any libraries?
hasLibs :: PackageDescription -> Bool
hasLibs p = maybe False (buildable . libBuildInfo) (library p)

-- |'Maybe' version of 'hasLibs'
maybeHasLibs :: PackageDescription -> Maybe Library
maybeHasLibs p =
   library p >>= \lib -> if buildable (libBuildInfo lib)
                           then Just lib
                           else Nothing

-- |If the package description has a library section, call the given
--  function with the library build info as argument.
withLib :: PackageDescription -> (Library -> IO ()) -> IO ()
withLib pkg_descr f =
   maybe (return ()) f (maybeHasLibs pkg_descr)

-- | Get all the module names from the library (exposed and internal modules)
libModules :: Library -> [ModuleName]
libModules lib = exposedModules lib
              ++ otherModules (libBuildInfo lib)

-- ---------------------------------------------------------------------------
-- The Executable type

data Executable = Executable {
        exeName    :: String,
        modulePath :: FilePath,
        buildInfo  :: BuildInfo
    }
    deriving (Show, Read, Eq, Typeable, Data)

instance Monoid Executable where
  mempty = Executable {
    exeName    = mempty,
    modulePath = mempty,
    buildInfo  = mempty
  }
  mappend a b = Executable{
    exeName    = combine' exeName,
    modulePath = combine modulePath,
    buildInfo  = combine buildInfo
  }
    where combine field = field a `mappend` field b
          combine' field = case (field a, field b) of
                      ("","") -> ""
                      ("", x) -> x
                      (x, "") -> x
                      (x, y) -> error $ "Ambiguous values for executable field: '"
                                  ++ x ++ "' and '" ++ y ++ "'"

emptyExecutable :: Executable
emptyExecutable = mempty

-- |does this package have any executables?
hasExes :: PackageDescription -> Bool
hasExes p = any (buildable . buildInfo) (executables p)

-- | Perform the action on each buildable 'Executable' in the package
-- description.
withExe :: PackageDescription -> (Executable -> IO ()) -> IO ()
withExe pkg_descr f =
  sequence_ [f exe | exe <- executables pkg_descr, buildable (buildInfo exe)]

-- | Get all the module names from an exe
exeModules :: Executable -> [ModuleName]
exeModules exe = otherModules (buildInfo exe)

-- ---------------------------------------------------------------------------
-- The TestSuite type

-- | A \"test-suite\" stanza in a cabal file.
--
data TestSuite = TestSuite {
        testName      :: String,
        testInterface :: TestSuiteInterface,
        testBuildInfo :: BuildInfo,
        testEnabled   :: Bool
        -- TODO: By having a 'testEnabled' field in the PackageDescription, we
        -- are mixing build status information (i.e., arguments to 'configure')
        -- with static package description information. This is undesirable, but
        -- a better solution is waiting on the next overhaul to the
        -- GenericPackageDescription -> PackageDescription resolution process.
    }
    deriving (Show, Read, Eq, Typeable, Data)

-- | The test suite interfaces that are currently defined. Each test suite must
-- specify which interface it supports.
--
-- More interfaces may be defined in future, either new revisions or totally
-- new interfaces.
--
data TestSuiteInterface =

     -- | Test interface \"exitcode-stdio-1.0\". The test-suite takes the form
     -- of an executable. It returns a zero exit code for success, non-zero for
     -- failure. The stdout and stderr channels may be logged. It takes no
     -- command line parameters and nothing on stdin.
     --
     TestSuiteExeV10 Version FilePath

     -- | Test interface \"detailed-0.9\". The test-suite takes the form of a
     -- library containing a designated module that exports \"tests :: [Test]\".
     --
   | TestSuiteLibV09 Version ModuleName

     -- | A test suite that does not conform to one of the above interfaces for
     -- the given reason (e.g. unknown test type).
     --
   | TestSuiteUnsupported TestType
   deriving (Eq, Read, Show, Typeable, Data)

instance Monoid TestSuite where
    mempty = TestSuite {
        testName      = mempty,
        testInterface = mempty,
        testBuildInfo = mempty,
        testEnabled   = False
    }

    mappend a b = TestSuite {
        testName      = combine' testName,
        testInterface = combine  testInterface,
        testBuildInfo = combine  testBuildInfo,
        testEnabled   = testEnabled a || testEnabled b
    }
        where combine   field = field a `mappend` field b
              combine' f = case (f a, f b) of
                        ("", x) -> x
                        (x, "") -> x
                        (x, y) -> error "Ambiguous values for test field: '"
                            ++ x ++ "' and '" ++ y ++ "'"

instance Monoid TestSuiteInterface where
    mempty  =  TestSuiteUnsupported (TestTypeUnknown mempty (Version [] []))
    mappend a (TestSuiteUnsupported _) = a
    mappend _ b                        = b

emptyTestSuite :: TestSuite
emptyTestSuite = mempty

-- | Does this package have any test suites?
hasTests :: PackageDescription -> Bool
hasTests = any (buildable . testBuildInfo) . testSuites

-- | Get all the enabled test suites from a package.
enabledTests :: PackageDescription -> [TestSuite]
enabledTests = filter testEnabled . testSuites

-- | Perform an action on each buildable 'TestSuite' in a package.
withTest :: PackageDescription -> (TestSuite -> IO ()) -> IO ()
withTest pkg_descr f =
    mapM_ f $ filter (buildable . testBuildInfo) $ enabledTests pkg_descr

-- | Get all the module names from a test suite.
testModules :: TestSuite -> [ModuleName]
testModules test = (case testInterface test of
                     TestSuiteLibV09 _ m -> [m]
                     _                   -> [])
                ++ otherModules (testBuildInfo test)

-- | The \"test-type\" field in the test suite stanza.
--
data TestType = TestTypeExe Version     -- ^ \"type: exitcode-stdio-x.y\"
              | TestTypeLib Version     -- ^ \"type: detailed-x.y\"
              | TestTypeUnknown String Version -- ^ Some unknown test type e.g. \"type: foo\"
    deriving (Show, Read, Eq, Typeable, Data)

knownTestTypes :: [TestType]
knownTestTypes = [ TestTypeExe (Version [1,0] [])
                 , TestTypeLib (Version [0,9] []) ]

stdParse :: Text ver => (ver -> String -> res) -> Parse.ReadP r res
stdParse f = do
  cs   <- Parse.sepBy1 component (Parse.char '-')
  _    <- Parse.char '-'
  ver  <- parse
  let name = intercalate "-" cs
  return $! f ver (lowercase name)
  where
    component = do
      cs <- Parse.munch1 Char.isAlphaNum
      if all Char.isDigit cs then Parse.pfail else return cs
      -- each component must contain an alphabetic character, to avoid
      -- ambiguity in identifiers like foo-1 (the 1 is the version number).

instance Text TestType where
  disp (TestTypeExe ver)          = text "exitcode-stdio-" <> disp ver
  disp (TestTypeLib ver)          = text "detailed-"       <> disp ver
  disp (TestTypeUnknown name ver) = text name <> char '-' <> disp ver

  parse = stdParse $ \ver name -> case name of
    "exitcode-stdio" -> TestTypeExe ver
    "detailed"       -> TestTypeLib ver
    _                -> TestTypeUnknown name ver


testType :: TestSuite -> TestType
testType test = case testInterface test of
  TestSuiteExeV10 ver _         -> TestTypeExe ver
  TestSuiteLibV09 ver _         -> TestTypeLib ver
  TestSuiteUnsupported testtype -> testtype

-- ---------------------------------------------------------------------------
-- The Benchmark type

-- | A \"benchmark\" stanza in a cabal file.
--
data Benchmark = Benchmark {
        benchmarkName      :: String,
        benchmarkInterface :: BenchmarkInterface,
        benchmarkBuildInfo :: BuildInfo,
        benchmarkEnabled   :: Bool
        -- TODO: See TODO for 'testEnabled'.
    }
    deriving (Show, Read, Eq, Typeable, Data)

-- | The benchmark interfaces that are currently defined. Each
-- benchmark must specify which interface it supports.
--
-- More interfaces may be defined in future, either new revisions or
-- totally new interfaces.
--
data BenchmarkInterface =

     -- | Benchmark interface \"exitcode-stdio-1.0\". The benchmark
     -- takes the form of an executable. It returns a zero exit code
     -- for success, non-zero for failure. The stdout and stderr
     -- channels may be logged. It takes no command line parameters
     -- and nothing on stdin.
     --
     BenchmarkExeV10 Version FilePath

     -- | A benchmark that does not conform to one of the above
     -- interfaces for the given reason (e.g. unknown benchmark type).
     --
   | BenchmarkUnsupported BenchmarkType
   deriving (Eq, Read, Show, Typeable, Data)

instance Monoid Benchmark where
    mempty = Benchmark {
        benchmarkName      = mempty,
        benchmarkInterface = mempty,
        benchmarkBuildInfo = mempty,
        benchmarkEnabled   = False
    }

    mappend a b = Benchmark {
        benchmarkName      = combine' benchmarkName,
        benchmarkInterface = combine  benchmarkInterface,
        benchmarkBuildInfo = combine  benchmarkBuildInfo,
        benchmarkEnabled   = benchmarkEnabled a || benchmarkEnabled b
    }
        where combine   field = field a `mappend` field b
              combine' f = case (f a, f b) of
                        ("", x) -> x
                        (x, "") -> x
                        (x, y) -> error "Ambiguous values for benchmark field: '"
                            ++ x ++ "' and '" ++ y ++ "'"

instance Monoid BenchmarkInterface where
    mempty  =  BenchmarkUnsupported (BenchmarkTypeUnknown mempty (Version [] []))
    mappend a (BenchmarkUnsupported _) = a
    mappend _ b                        = b

emptyBenchmark :: Benchmark
emptyBenchmark = mempty

-- | Does this package have any benchmarks?
hasBenchmarks :: PackageDescription -> Bool
hasBenchmarks = any (buildable . benchmarkBuildInfo) . benchmarks

-- | Get all the enabled benchmarks from a package.
enabledBenchmarks :: PackageDescription -> [Benchmark]
enabledBenchmarks = filter benchmarkEnabled . benchmarks

-- | Perform an action on each buildable 'Benchmark' in a package.
withBenchmark :: PackageDescription -> (Benchmark -> IO ()) -> IO ()
withBenchmark pkg_descr f =
    mapM_ f $ filter (buildable . benchmarkBuildInfo) $ enabledBenchmarks pkg_descr

-- | Get all the module names from a benchmark.
benchmarkModules :: Benchmark -> [ModuleName]
benchmarkModules benchmark = otherModules (benchmarkBuildInfo benchmark)

-- | The \"benchmark-type\" field in the benchmark stanza.
--
data BenchmarkType = BenchmarkTypeExe Version
                     -- ^ \"type: exitcode-stdio-x.y\"
                   | BenchmarkTypeUnknown String Version
                     -- ^ Some unknown benchmark type e.g. \"type: foo\"
    deriving (Show, Read, Eq, Typeable, Data)

knownBenchmarkTypes :: [BenchmarkType]
knownBenchmarkTypes = [ BenchmarkTypeExe (Version [1,0] []) ]

instance Text BenchmarkType where
  disp (BenchmarkTypeExe ver)          = text "exitcode-stdio-" <> disp ver
  disp (BenchmarkTypeUnknown name ver) = text name <> char '-' <> disp ver

  parse = stdParse $ \ver name -> case name of
    "exitcode-stdio" -> BenchmarkTypeExe ver
    _                -> BenchmarkTypeUnknown name ver


benchmarkType :: Benchmark -> BenchmarkType
benchmarkType benchmark = case benchmarkInterface benchmark of
  BenchmarkExeV10 ver _              -> BenchmarkTypeExe ver
  BenchmarkUnsupported benchmarktype -> benchmarktype

-- ---------------------------------------------------------------------------
-- The BuildInfo type

-- Consider refactoring into executable and library versions.
data BuildInfo = BuildInfo {
        buildable         :: Bool,      -- ^ component is buildable here
        buildTools        :: [Dependency], -- ^ tools needed to build this bit
        cppOptions        :: [String],  -- ^ options for pre-processing Haskell code
        ccOptions         :: [String],  -- ^ options for C compiler
        ldOptions         :: [String],  -- ^ options for linker
        pkgconfigDepends  :: [Dependency], -- ^ pkg-config packages that are used
        frameworks        :: [String], -- ^support frameworks for Mac OS X
        cSources          :: [FilePath],
        hsSourceDirs      :: [FilePath], -- ^ where to look for the haskell module hierarchy
        otherModules      :: [ModuleName], -- ^ non-exposed or non-main modules

        defaultLanguage   :: Maybe Language,-- ^ language used when not explicitly specified
        otherLanguages    :: [Language],    -- ^ other languages used within the package
        defaultExtensions :: [Extension],   -- ^ language extensions used by all modules
        otherExtensions   :: [Extension],   -- ^ other language extensions used within the package
        oldExtensions     :: [Extension],   -- ^ the old extensions field, treated same as 'defaultExtensions'

        extraLibs         :: [String], -- ^ what libraries to link with when compiling a program that uses your package
        extraLibDirs      :: [String],
        includeDirs       :: [FilePath], -- ^directories to find .h files
        includes          :: [FilePath], -- ^ The .h files to be found in includeDirs
        installIncludes   :: [FilePath], -- ^ .h files to install with the package
        options           :: [(CompilerFlavor,[String])],
        ghcProfOptions    :: [String],
        ghcSharedOptions  :: [String],
        customFieldsBI    :: [(String,String)], -- ^Custom fields starting
                                                -- with x-, stored in a
                                                -- simple assoc-list.
        targetBuildDepends :: [Dependency] -- ^ Dependencies specific to a library or executable target
    }
    deriving (Show,Read,Eq,Typeable,Data)

instance Monoid BuildInfo where
  mempty = BuildInfo {
    buildable         = True,
    buildTools        = [],
    cppOptions        = [],
    ccOptions         = [],
    ldOptions         = [],
    pkgconfigDepends  = [],
    frameworks        = [],
    cSources          = [],
    hsSourceDirs      = [],
    otherModules      = [],
    defaultLanguage   = Nothing,
    otherLanguages    = [],
    defaultExtensions = [],
    otherExtensions   = [],
    oldExtensions     = [],
    extraLibs         = [],
    extraLibDirs      = [],
    includeDirs       = [],
    includes          = [],
    installIncludes   = [],
    options           = [],
    ghcProfOptions    = [],
    ghcSharedOptions  = [],
    customFieldsBI    = [],
    targetBuildDepends = []
  }
  mappend a b = BuildInfo {
    buildable         = buildable a && buildable b,
    buildTools        = combine    buildTools,
    cppOptions        = combine    cppOptions,
    ccOptions         = combine    ccOptions,
    ldOptions         = combine    ldOptions,
    pkgconfigDepends  = combine    pkgconfigDepends,
    frameworks        = combineNub frameworks,
    cSources          = combineNub cSources,
    hsSourceDirs      = combineNub hsSourceDirs,
    otherModules      = combineNub otherModules,
    defaultLanguage   = combineMby defaultLanguage,
    otherLanguages    = combineNub otherLanguages,
    defaultExtensions = combineNub defaultExtensions,
    otherExtensions   = combineNub otherExtensions,
    oldExtensions     = combineNub oldExtensions,
    extraLibs         = combine    extraLibs,
    extraLibDirs      = combineNub extraLibDirs,
    includeDirs       = combineNub includeDirs,
    includes          = combineNub includes,
    installIncludes   = combineNub installIncludes,
    options           = combine    options,
    ghcProfOptions    = combine    ghcProfOptions,
    ghcSharedOptions  = combine    ghcSharedOptions,
    customFieldsBI    = combine    customFieldsBI,
    targetBuildDepends = combineNub targetBuildDepends
  }
    where
      combine    field = field a `mappend` field b
      combineNub field = nub (combine field)
      combineMby field = field b `mplus` field a

emptyBuildInfo :: BuildInfo
emptyBuildInfo = mempty

-- | The 'BuildInfo' for the library (if there is one and it's buildable), and
-- all buildable executables, test suites and benchmarks.  Useful for gathering
-- dependencies.
allBuildInfo :: PackageDescription -> [BuildInfo]
allBuildInfo pkg_descr = [ bi | Just lib <- [library pkg_descr]
                              , let bi = libBuildInfo lib
                              , buildable bi ]
                      ++ [ bi | exe <- executables pkg_descr
                              , let bi = buildInfo exe
                              , buildable bi ]
                      ++ [ bi | tst <- testSuites pkg_descr
                              , let bi = testBuildInfo tst
                              , buildable bi
                              , testEnabled tst ]
                      ++ [ bi | tst <- benchmarks pkg_descr
                              , let bi = benchmarkBuildInfo tst
                              , buildable bi
                              , benchmarkEnabled tst ]
  --FIXME: many of the places where this is used, we actually want to look at
  --       unbuildable bits too, probably need separate functions

-- | The 'Language's used by this component
--
allLanguages :: BuildInfo -> [Language]
allLanguages bi = maybeToList (defaultLanguage bi)
               ++ otherLanguages bi

-- | The 'Extension's that are used somewhere by this component
--
allExtensions :: BuildInfo -> [Extension]
allExtensions bi = usedExtensions bi
                ++ otherExtensions bi

-- | The 'Extensions' that are used by all modules in this component
--
usedExtensions :: BuildInfo -> [Extension]
usedExtensions bi = oldExtensions bi
                 ++ defaultExtensions bi

type HookedBuildInfo = (Maybe BuildInfo, [(String, BuildInfo)])

emptyHookedBuildInfo :: HookedBuildInfo
emptyHookedBuildInfo = (Nothing, [])

-- |Select options for a particular Haskell compiler.
hcOptions :: CompilerFlavor -> BuildInfo -> [String]
hcOptions hc bi = [ opt | (hc',opts) <- options bi
                        , hc' == hc
                        , opt <- opts ]

-- ------------------------------------------------------------
-- * Source repos
-- ------------------------------------------------------------

-- | Information about the source revision control system for a package.
--
-- When specifying a repo it is useful to know the meaning or intention of the
-- information as doing so enables automation. There are two obvious common
-- purposes: one is to find the repo for the latest development version, the
-- other is to find the repo for this specific release. The 'ReopKind'
-- specifies which one we mean (or another custom one).
--
-- A package can specify one or the other kind or both. Most will specify just
-- a head repo but some may want to specify a repo to reconstruct the sources
-- for this package release.
--
-- The required information is the 'RepoType' which tells us if it's using
-- 'Darcs', 'Git' for example. The 'repoLocation' and other details are
-- interpreted according to the repo type.
--
data SourceRepo = SourceRepo {
  -- | The kind of repo. This field is required.
  repoKind     :: RepoKind,

  -- | The type of the source repository system for this repo, eg 'Darcs' or
  -- 'Git'. This field is required.
  repoType     :: Maybe RepoType,

  -- | The location of the repository. For most 'RepoType's this is a URL.
  -- This field is required.
  repoLocation :: Maybe String,

  -- | 'CVS' can put multiple \"modules\" on one server and requires a
  -- module name in addition to the location to identify a particular repo.
  -- Logically this is part of the location but unfortunately has to be
  -- specified separately. This field is required for the 'CVS' 'RepoType' and
  -- should not be given otherwise.
  repoModule   :: Maybe String,

  -- | The name or identifier of the branch, if any. Many source control
  -- systems have the notion of multiple branches in a repo that exist in the
  -- same location. For example 'Git' and 'CVS' use this while systems like
  -- 'Darcs' use different locations for different branches. This field is
  -- optional but should be used if necessary to identify the sources,
  -- especially for the 'RepoThis' repo kind.
  repoBranch   :: Maybe String,

  -- | The tag identify a particular state of the repository. This should be
  -- given for the 'RepoThis' repo kind and not for 'RepoHead' kind.
  --
  repoTag      :: Maybe String,

  -- | Some repositories contain multiple projects in different subdirectories
  -- This field specifies the subdirectory where this packages sources can be
  -- found, eg the subdirectory containing the @.cabal@ file. It is interpreted
  -- relative to the root of the repository. This field is optional. If not
  -- given the default is \".\" ie no subdirectory.
  repoSubdir   :: Maybe FilePath
}
  deriving (Eq, Read, Show, Typeable, Data)

-- | What this repo info is for, what it represents.
--
data RepoKind =
    -- | The repository for the \"head\" or development version of the project.
    -- This repo is where we should track the latest development activity or
    -- the usual repo people should get to contribute patches.
    RepoHead

    -- | The repository containing the sources for this exact package version
    -- or release. For this kind of repo a tag should be given to give enough
    -- information to re-create the exact sources.
  | RepoThis

  | RepoKindUnknown String
  deriving (Eq, Ord, Read, Show, Typeable, Data)

-- | An enumeration of common source control systems. The fields used in the
-- 'SourceRepo' depend on the type of repo. The tools and methods used to
-- obtain and track the repo depend on the repo type.
--
data RepoType = Darcs | Git | SVN | CVS
              | Mercurial | GnuArch | Bazaar | Monotone
              | OtherRepoType String
  deriving (Eq, Ord, Read, Show, Typeable, Data)

knownRepoTypes :: [RepoType]
knownRepoTypes = [Darcs, Git, SVN, CVS
                 ,Mercurial, GnuArch, Bazaar, Monotone]

repoTypeAliases :: RepoType -> [String]
repoTypeAliases Bazaar    = ["bzr"]
repoTypeAliases Mercurial = ["hg"]
repoTypeAliases GnuArch   = ["arch"]
repoTypeAliases _         = []

instance Text RepoKind where
  disp RepoHead                = Disp.text "head"
  disp RepoThis                = Disp.text "this"
  disp (RepoKindUnknown other) = Disp.text other

  parse = do
    name <- ident
    return $ case lowercase name of
      "head" -> RepoHead
      "this" -> RepoThis
      _      -> RepoKindUnknown name

instance Text RepoType where
  disp (OtherRepoType other) = Disp.text other
  disp other                 = Disp.text (lowercase (show other))
  parse = fmap classifyRepoType ident

classifyRepoType :: String -> RepoType
classifyRepoType s =
  fromMaybe (OtherRepoType s) $ lookup (lowercase s) repoTypeMap
  where
    repoTypeMap = [ (name, repoType')
                  | repoType' <- knownRepoTypes
                  , name <- display repoType' : repoTypeAliases repoType' ]

ident :: Parse.ReadP r String
ident = Parse.munch1 (\c -> Char.isAlphaNum c || c == '_' || c == '-')

lowercase :: String -> String
lowercase = map Char.toLower

-- ------------------------------------------------------------
-- * Utils
-- ------------------------------------------------------------

updatePackageDescription :: HookedBuildInfo -> PackageDescription -> PackageDescription
updatePackageDescription (mb_lib_bi, exe_bi) p
    = p{ executables = updateExecutables exe_bi    (executables p)
       , library     = updateLibrary     mb_lib_bi (library     p)
       }
    where
      updateLibrary :: Maybe BuildInfo -> Maybe Library -> Maybe Library
      updateLibrary (Just bi) (Just lib) = Just (lib{libBuildInfo = bi `mappend` libBuildInfo lib})
      updateLibrary Nothing   mb_lib     = mb_lib
      updateLibrary (Just _)  Nothing    = Nothing

      updateExecutables :: [(String, BuildInfo)] -- ^[(exeName, new buildinfo)]
                        -> [Executable]          -- ^list of executables to update
                        -> [Executable]          -- ^list with exeNames updated
      updateExecutables exe_bi' executables' = foldr updateExecutable executables' exe_bi'

      updateExecutable :: (String, BuildInfo) -- ^(exeName, new buildinfo)
                       -> [Executable]        -- ^list of executables to update
                       -> [Executable]        -- ^libst with exeName updated
      updateExecutable _                 []         = []
      updateExecutable exe_bi'@(name,bi) (exe:exes)
        | exeName exe == name = exe{buildInfo = bi `mappend` buildInfo exe} : exes
        | otherwise           = exe : updateExecutable exe_bi' exes

-- ---------------------------------------------------------------------------
-- The GenericPackageDescription type

data GenericPackageDescription =
    GenericPackageDescription {
        packageDescription :: PackageDescription,
        genPackageFlags       :: [Flag],
        condLibrary        :: Maybe (CondTree ConfVar [Dependency] Library),
        condExecutables    :: [(String, CondTree ConfVar [Dependency] Executable)],
        condTestSuites     :: [(String, CondTree ConfVar [Dependency] TestSuite)],
        condBenchmarks     :: [(String, CondTree ConfVar [Dependency] Benchmark)]
      }
    deriving (Show, Eq, Typeable, Data)

instance Package GenericPackageDescription where
  packageId = packageId . packageDescription

--TODO: make PackageDescription an instance of Text.

-- | A flag can represent a feature to be included, or a way of linking
--   a target against its dependencies, or in fact whatever you can think of.
data Flag = MkFlag
    { flagName        :: FlagName
    , flagDescription :: String
    , flagDefault     :: Bool
    , flagManual      :: Bool
    }
    deriving (Show, Eq, Typeable, Data)

-- | A 'FlagName' is the name of a user-defined configuration flag
newtype FlagName = FlagName String
    deriving (Eq, Ord, Show, Read, Typeable, Data)

-- | A 'FlagAssignment' is a total or partial mapping of 'FlagName's to
-- 'Bool' flag values. It represents the flags chosen by the user or
-- discovered during configuration. For example @--flags=foo --flags=-bar@
-- becomes @[("foo", True), ("bar", False)]@
--
type FlagAssignment = [(FlagName, Bool)]

-- | A @ConfVar@ represents the variable type used.
data ConfVar = OS OS
             | Arch Arch
             | Flag FlagName
             | Impl CompilerFlavor VersionRange
    deriving (Eq, Show, Typeable, Data)

--instance Text ConfVar where
--    disp (OS os) = "os(" ++ display os ++ ")"
--    disp (Arch arch) = "arch(" ++ display arch ++ ")"
--    disp (Flag (ConfFlag f)) = "flag(" ++ f ++ ")"
--    disp (Impl c v) = "impl(" ++ display c
--                       ++ " " ++ display v ++ ")"

-- | A boolean expression parameterized over the variable type used.
data Condition c = Var c
                 | Lit Bool
                 | CNot (Condition c)
                 | COr (Condition c) (Condition c)
                 | CAnd (Condition c) (Condition c)
    deriving (Show, Eq, Typeable, Data)

--instance Text c => Text (Condition c) where
--  disp (Var x) = text (show x)
--  disp (Lit b) = text (show b)
--  disp (CNot c) = char '!' <> parens (ppCond c)
--  disp (COr c1 c2) = parens $ sep [ppCond c1, text "||" <+> ppCond c2]
--  disp (CAnd c1 c2) = parens $ sep [ppCond c1, text "&&" <+> ppCond c2]

data CondTree v c a = CondNode
    { condTreeData        :: a
    , condTreeConstraints :: c
    , condTreeComponents  :: [( Condition v
                              , CondTree v c a
                              , Maybe (CondTree v c a))]
    }
    deriving (Show, Eq, Typeable, Data)

--instance (Text v, Text c) => Text (CondTree v c a) where
--  disp (CondNode _dat cs ifs) =
--    (text "build-depends: " <+>
--      disp cs)
--    $+$
--    (vcat $ map ppIf ifs)
--  where
--    ppIf (c,thenTree,mElseTree) =
--        ((text "if" <+> ppCond c <> colon) $$
--          nest 2 (ppCondTree thenTree disp))
--        $+$ (maybe empty (\t -> text "else: " $$ nest 2 (ppCondTree t disp))
--                   mElseTree)
