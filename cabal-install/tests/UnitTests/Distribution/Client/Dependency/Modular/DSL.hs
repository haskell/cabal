{-# LANGUAGE RecordWildCards #-}
-- | DSL for testing the modular solver
module UnitTests.Distribution.Client.Dependency.Modular.DSL (
    ExampleDependency(..)
  , Dependencies(..)
  , ExTest(..)
  , ExPreference(..)
  , ExampleDb
  , ExampleVersionRange
  , ExamplePkgVersion
  , exAv
  , exInst
  , exFlag
  , exResolve
  , extractInstallPlan
  , withSetupDeps
  , withTest
  , withTests
  ) where

-- base
import Data.Either (partitionEithers)
import Data.Maybe (catMaybes)
import Data.List (nub)
import Data.Monoid
import Data.Version
import qualified Data.Map as Map

-- Cabal
import qualified Distribution.Compiler             as C
import qualified Distribution.InstalledPackageInfo as C
import qualified Distribution.Package              as C
  hiding (HasUnitId(..))
import qualified Distribution.PackageDescription   as C
import qualified Distribution.Simple.PackageIndex  as C.PackageIndex
import qualified Distribution.System               as C
import qualified Distribution.Version              as C
import Language.Haskell.Extension (Extension(..), Language)

-- cabal-install
import Distribution.Client.ComponentDeps (ComponentDeps)
import Distribution.Client.Dependency
import Distribution.Client.Dependency.Types
import Distribution.Client.Types
import qualified Distribution.Client.InstallPlan   as CI.InstallPlan
import qualified Distribution.Client.PackageIndex  as CI.PackageIndex
import qualified Distribution.Client.PkgConfigDb   as PC
import qualified Distribution.Client.ComponentDeps as CD

{-------------------------------------------------------------------------------
  Example package database DSL

  In order to be able to set simple examples up quickly, we define a very
  simple version of the package database here explicitly designed for use in
  tests.

  The design of `ExampleDb` takes the perspective of the solver, not the
  perspective of the package DB. This makes it easier to set up tests for
  various parts of the solver, but makes the mapping somewhat awkward,  because
  it means we first map from "solver perspective" `ExampleDb` to the package
  database format, and then the modular solver internally in `IndexConversion`
  maps this back to the solver specific data structures.

  IMPLEMENTATION NOTES
  --------------------

  TODO: Perhaps these should be made comments of the corresponding data type
  definitions. For now these are just my own conclusions and may be wrong.

  * The difference between `GenericPackageDescription` and `PackageDescription`
    is that `PackageDescription` describes a particular _configuration_ of a
    package (for instance, see documentation for `checkPackage`). A
    `GenericPackageDescription` can be turned into a `PackageDescription` in
    two ways:

      a. `finalizePackageDescription` does the proper translation, by taking
         into account the platform, available dependencies, etc. and picks a
         flag assignment (or gives an error if no flag assignment can be found)
      b. `flattenPackageDescription` ignores flag assignment and just joins all
         components together.

    The slightly odd thing is that a `GenericPackageDescription` contains a
    `PackageDescription` as a field; both of the above functions do the same
    thing: they take the embedded `PackageDescription` as a basis for the result
    value, but override `library`, `executables`, `testSuites`, `benchmarks`
    and `buildDepends`.
  * The `condTreeComponents` fields of a `CondTree` is a list of triples
    `(condition, then-branch, else-branch)`, where the `else-branch` is
    optional.
-------------------------------------------------------------------------------}

type ExamplePkgName    = String
type ExamplePkgVersion = Int
type ExamplePkgHash    = String  -- for example "installed" packages
type ExampleFlagName   = String
type ExampleTestName   = String
type ExampleVersionRange = C.VersionRange
data Dependencies = NotBuildable | Buildable [ExampleDependency]

data ExampleDependency =
    -- | Simple dependency on any version
    ExAny ExamplePkgName

    -- | Simple dependency on a fixed version
  | ExFix ExamplePkgName ExamplePkgVersion

    -- | Dependencies indexed by a flag
  | ExFlag ExampleFlagName Dependencies Dependencies

    -- | Dependency on a language extension
  | ExExt Extension

    -- | Dependency on a language version
  | ExLang Language

    -- | Dependency on a pkg-config package
  | ExPkg (ExamplePkgName, ExamplePkgVersion)

data ExTest = ExTest ExampleTestName [ExampleDependency]

exFlag :: ExampleFlagName -> [ExampleDependency] -> [ExampleDependency]
       -> ExampleDependency
exFlag n t e = ExFlag n (Buildable t) (Buildable e)

data ExPreference = ExPref String ExampleVersionRange

data ExampleAvailable = ExAv {
    exAvName    :: ExamplePkgName
  , exAvVersion :: ExamplePkgVersion
  , exAvDeps    :: ComponentDeps [ExampleDependency]
  }

exAv :: ExamplePkgName -> ExamplePkgVersion -> [ExampleDependency]
     -> ExampleAvailable
exAv n v ds = ExAv { exAvName = n, exAvVersion = v
                   , exAvDeps = CD.fromLibraryDeps ds }

withSetupDeps :: ExampleAvailable -> [ExampleDependency] -> ExampleAvailable
withSetupDeps ex setupDeps = ex {
      exAvDeps = exAvDeps ex <> CD.fromSetupDeps setupDeps
    }

withTest :: ExampleAvailable -> ExTest -> ExampleAvailable
withTest ex test = withTests ex [test]

withTests :: ExampleAvailable -> [ExTest] -> ExampleAvailable
withTests ex tests =
  let testCDs = CD.fromList [(CD.ComponentTest name, deps)
                            | ExTest name deps <- tests]
  in ex { exAvDeps = exAvDeps ex <> testCDs }

data ExampleInstalled = ExInst {
    exInstName         :: ExamplePkgName
  , exInstVersion      :: ExamplePkgVersion
  , exInstHash         :: ExamplePkgHash
  , exInstBuildAgainst :: [ExampleInstalled]
  }

exInst :: ExamplePkgName -> ExamplePkgVersion -> ExamplePkgHash
       -> [ExampleInstalled] -> ExampleInstalled
exInst = ExInst

type ExampleDb = [Either ExampleInstalled ExampleAvailable]

type DependencyTree a = C.CondTree C.ConfVar [C.Dependency] a

exDbPkgs :: ExampleDb -> [ExamplePkgName]
exDbPkgs = map (either exInstName exAvName)

exAvSrcPkg :: ExampleAvailable -> SourcePackage
exAvSrcPkg ex =
    let (libraryDeps, exts, mlang, pcpkgs) = splitTopLevel (CD.libraryDeps (exAvDeps ex))
        testSuites = [(name, deps) | (CD.ComponentTest name, deps) <- CD.toList (exAvDeps ex)]
    in SourcePackage {
           packageInfoId        = exAvPkgId ex
         , packageSource        = LocalTarballPackage "<<path>>"
         , packageDescrOverride = Nothing
         , packageDescription   = C.GenericPackageDescription {
               C.packageDescription = C.emptyPackageDescription {
                   C.package        = exAvPkgId ex
                 , C.library        = error "not yet configured: library"
                 , C.executables    = error "not yet configured: executables"
                 , C.testSuites     = error "not yet configured: testSuites"
                 , C.benchmarks     = error "not yet configured: benchmarks"
                 , C.buildDepends   = error "not yet configured: buildDepends"
                 , C.setupBuildInfo = Just C.SetupBuildInfo {
                       C.setupDepends = mkSetupDeps (CD.setupDeps (exAvDeps ex)),
                       C.defaultSetupDepends = False
                     }
                 }
             , C.genPackageFlags = nub $ concatMap extractFlags $
                                   CD.libraryDeps (exAvDeps ex) ++ concatMap snd testSuites
             , C.condLibrary     = Just $ mkCondTree (extsLib exts <> langLib mlang <> pcpkgLib pcpkgs)
                                                     disableLib
                                                     (Buildable libraryDeps)
             , C.condExecutables = []
             , C.condTestSuites  =
                 let mkTree = mkCondTree mempty disableTest . Buildable
                 in map (\(t, deps) -> (t, mkTree deps)) testSuites
             , C.condBenchmarks  = []
             }
         }
  where
    -- Split the set of dependencies into the set of dependencies of the library,
    -- the dependencies of the test suites and extensions.
    splitTopLevel :: [ExampleDependency]
                  -> ( [ExampleDependency]
                     , [Extension]
                     , Maybe Language
                     , [(ExamplePkgName, ExamplePkgVersion)] -- pkg-config
                     )
    splitTopLevel [] =
        ([], [], Nothing, [])
    splitTopLevel (ExExt ext:deps) =
      let (other, exts, lang, pcpkgs) = splitTopLevel deps
      in (other, ext:exts, lang, pcpkgs)
    splitTopLevel (ExLang lang:deps) =
        case splitTopLevel deps of
            (other, exts, Nothing, pcpkgs) -> (other, exts, Just lang, pcpkgs)
            _ -> error "Only 1 Language dependency is supported"
    splitTopLevel (ExPkg pkg:deps) =
      let (other, exts, lang, pcpkgs) = splitTopLevel deps
      in (other, exts, lang, pkg:pcpkgs)
    splitTopLevel (dep:deps) =
      let (other, exts, lang, pcpkgs) = splitTopLevel deps
      in (dep:other, exts, lang, pcpkgs)

    -- Extract the total set of flags used
    extractFlags :: ExampleDependency -> [C.Flag]
    extractFlags (ExAny _)      = []
    extractFlags (ExFix _ _)    = []
    extractFlags (ExFlag f a b) = C.MkFlag {
                                      C.flagName        = C.FlagName f
                                    , C.flagDescription = ""
                                    , C.flagDefault     = True
                                    , C.flagManual      = False
                                    }
                                : concatMap extractFlags (deps a ++ deps b)
      where
        deps :: Dependencies -> [ExampleDependency]
        deps NotBuildable = []
        deps (Buildable ds) = ds
    extractFlags (ExExt _)      = []
    extractFlags (ExLang _)     = []
    extractFlags (ExPkg _)      = []

    mkCondTree :: Monoid a => a -> (a -> a) -> Dependencies -> DependencyTree a
    mkCondTree x dontBuild NotBuildable =
      C.CondNode {
             C.condTreeData        = dontBuild x
           , C.condTreeConstraints = []
           , C.condTreeComponents  = []
           }
    mkCondTree x dontBuild (Buildable deps) =
      let (directDeps, flaggedDeps) = splitDeps deps
      in C.CondNode {
             C.condTreeData        = x -- Necessary for language extensions
           , C.condTreeConstraints = map mkDirect directDeps
           , C.condTreeComponents  = map (mkFlagged dontBuild) flaggedDeps
           }

    mkDirect :: (ExamplePkgName, Maybe ExamplePkgVersion) -> C.Dependency
    mkDirect (dep, Nothing) = C.Dependency (C.PackageName dep) C.anyVersion
    mkDirect (dep, Just n)  = C.Dependency (C.PackageName dep) (C.thisVersion v)
      where
        v = Version [n, 0, 0] []

    mkFlagged :: Monoid a
              => (a -> a)
              -> (ExampleFlagName, Dependencies, Dependencies)
              -> (C.Condition C.ConfVar
                 , DependencyTree a, Maybe (DependencyTree a))
    mkFlagged dontBuild (f, a, b) = ( C.Var (C.Flag (C.FlagName f))
                                    , mkCondTree mempty dontBuild a
                                    , Just (mkCondTree mempty dontBuild b)
                                    )

    -- Split a set of dependencies into direct dependencies and flagged
    -- dependencies. A direct dependency is a tuple of the name of package and
    -- maybe its version (no version means any version) meant to be converted
    -- to a 'C.Dependency' with 'mkDirect' for example. A flagged dependency is
    -- the set of dependencies guarded by a flag.
    --
    -- TODO: Take care of flagged language extensions and language flavours.
    splitDeps :: [ExampleDependency]
              -> ( [(ExamplePkgName, Maybe Int)]
                 , [(ExampleFlagName, Dependencies, Dependencies)]
                 )
    splitDeps [] =
      ([], [])
    splitDeps (ExAny p:deps) =
      let (directDeps, flaggedDeps) = splitDeps deps
      in ((p, Nothing):directDeps, flaggedDeps)
    splitDeps (ExFix p v:deps) =
      let (directDeps, flaggedDeps) = splitDeps deps
      in ((p, Just v):directDeps, flaggedDeps)
    splitDeps (ExFlag f a b:deps) =
      let (directDeps, flaggedDeps) = splitDeps deps
      in (directDeps, (f, a, b):flaggedDeps)
    splitDeps (_:deps) = splitDeps deps

    -- Currently we only support simple setup dependencies
    mkSetupDeps :: [ExampleDependency] -> [C.Dependency]
    mkSetupDeps deps =
      let (directDeps, []) = splitDeps deps in map mkDirect directDeps

    -- A 'C.Library' with just the given extensions in its 'BuildInfo'
    extsLib :: [Extension] -> C.Library
    extsLib es = mempty { C.libBuildInfo = mempty { C.otherExtensions = es } }

    -- A 'C.Library' with just the given extensions in its 'BuildInfo'
    langLib :: Maybe Language -> C.Library
    langLib (Just lang) = mempty { C.libBuildInfo = mempty { C.defaultLanguage = Just lang } }
    langLib _ = mempty

    disableLib :: C.Library -> C.Library
    disableLib lib =
        lib { C.libBuildInfo = (C.libBuildInfo lib) { C.buildable = False }}

    disableTest :: C.TestSuite -> C.TestSuite
    disableTest test =
        test { C.testBuildInfo = (C.testBuildInfo test) { C.buildable = False }}

    -- A 'C.Library' with just the given pkgconfig-depends in its 'BuildInfo'
    pcpkgLib :: [(ExamplePkgName, ExamplePkgVersion)] -> C.Library
    pcpkgLib ds = mempty { C.libBuildInfo = mempty { C.pkgconfigDepends = [mkDirect (n, (Just v)) | (n,v) <- ds] } }

exAvPkgId :: ExampleAvailable -> C.PackageIdentifier
exAvPkgId ex = C.PackageIdentifier {
      pkgName    = C.PackageName (exAvName ex)
    , pkgVersion = Version [exAvVersion ex, 0, 0] []
    }

exInstInfo :: ExampleInstalled -> C.InstalledPackageInfo
exInstInfo ex = C.emptyInstalledPackageInfo {
      C.installedUnitId    = C.mkUnitId (exInstHash ex)
    , C.sourcePackageId    = exInstPkgId ex
    , C.depends            = map (C.mkUnitId . exInstHash)
                                 (exInstBuildAgainst ex)
    }

exInstPkgId :: ExampleInstalled -> C.PackageIdentifier
exInstPkgId ex = C.PackageIdentifier {
      pkgName    = C.PackageName (exInstName ex)
    , pkgVersion = Version [exInstVersion ex, 0, 0] []
    }

exAvIdx :: [ExampleAvailable] -> CI.PackageIndex.PackageIndex SourcePackage
exAvIdx = CI.PackageIndex.fromList . map exAvSrcPkg

exInstIdx :: [ExampleInstalled] -> C.PackageIndex.InstalledPackageIndex
exInstIdx = C.PackageIndex.fromList . map exInstInfo

exResolve :: ExampleDb
          -- List of extensions supported by the compiler, or Nothing if unknown.
          -> Maybe [Extension]
          -- List of languages supported by the compiler, or Nothing if unknown.
          -> Maybe [Language]
          -> PC.PkgConfigDb
          -> [ExamplePkgName]
          -> Bool
          -> [ExPreference]
          -> ([String], Either String CI.InstallPlan.InstallPlan)
exResolve db exts langs pkgConfigDb targets indepGoals prefs = runProgress $
    resolveDependencies C.buildPlatform
                        compiler pkgConfigDb
                        Modular
                        params
  where
    defaultCompiler = C.unknownCompilerInfo C.buildCompilerId C.NoAbiTag
    compiler = defaultCompiler { C.compilerInfoExtensions = exts
                               , C.compilerInfoLanguages  = langs
                               }
    (inst, avai) = partitionEithers db
    instIdx      = exInstIdx inst
    avaiIdx      = SourcePackageDb {
                       packageIndex       = exAvIdx avai
                     , packagePreferences = Map.empty
                     }
    enableTests  = fmap (\p -> PackageConstraintStanzas
                              (C.PackageName p) [TestStanzas])
                       (exDbPkgs db)
    targets'     = fmap (\p -> NamedPackage (C.PackageName p) []) targets
    params       =   addPreferences (fmap toPref prefs)
                   $ addConstraints (fmap toLpc enableTests)
                   $ (standardInstallPolicy instIdx avaiIdx targets') {
                     depResolverIndependentGoals = indepGoals
                     }
    toLpc     pc = LabeledPackageConstraint pc ConstraintSourceUnknown
    toPref (ExPref n v) = PackageVersionPreference (C.PackageName n) v

extractInstallPlan :: CI.InstallPlan.InstallPlan
                   -> [(ExamplePkgName, ExamplePkgVersion)]
extractInstallPlan = catMaybes . map confPkg . CI.InstallPlan.toList
  where
    confPkg :: CI.InstallPlan.PlanPackage -> Maybe (String, Int)
    confPkg (CI.InstallPlan.Configured pkg) = Just $ srcPkg pkg
    confPkg _                               = Nothing

    srcPkg :: ConfiguredPackage -> (String, Int)
    srcPkg (ConfiguredPackage pkg _flags _stanzas _deps) =
      let C.PackageIdentifier (C.PackageName p) (Version (n:_) _) =
            packageInfoId pkg
      in (p, n)

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | Run Progress computation
--
-- Like `runLog`, but for the more general `Progress` type.
runProgress :: Progress step e a -> ([step], Either e a)
runProgress = go
  where
    go (Step s p) = let (ss, result) = go p in (s:ss, result)
    go (Fail e)   = ([], Left e)
    go (Done a)   = ([], Right a)
