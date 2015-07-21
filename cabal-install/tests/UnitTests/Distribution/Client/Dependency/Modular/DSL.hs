{-# LANGUAGE RecordWildCards #-}
-- | DSL for testing the modular solver
module UnitTests.Distribution.Client.Dependency.Modular.DSL (
    ExampleDependency(..)
  , ExampleDb
  , exAv
  , exInst
  , exResolve
  , extractInstallPlan
  , withSetupDeps
  ) where

-- base
import Data.Either (partitionEithers)
import Data.Maybe (catMaybes)
import Data.Monoid
import Data.Version
import qualified Data.Map as Map

-- Cabal
import qualified Distribution.Compiler             as C
import qualified Distribution.InstalledPackageInfo as C
import qualified Distribution.Package              as C
  hiding (HasInstalledPackageId(..))
import qualified Distribution.PackageDescription   as C
import qualified Distribution.Simple.PackageIndex  as C.PackageIndex
import qualified Distribution.System               as C
import qualified Distribution.Version              as C

-- cabal-install
import Distribution.Client.ComponentDeps (ComponentDeps)
import Distribution.Client.Dependency
import Distribution.Client.Dependency.Types
import Distribution.Client.Types
import qualified Distribution.Client.InstallPlan   as CI.InstallPlan
import qualified Distribution.Client.PackageIndex  as CI.PackageIndex
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
    `GenericPackageDescription` can be returned into a `PackageDescription` in
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

data ExampleDependency =
    -- | Simple dependency on any version
    ExAny ExamplePkgName

    -- | Simple dependency on a fixed version
  | ExFix ExamplePkgName ExamplePkgVersion

    -- | Dependencies indexed by a flag
  | ExFlag ExampleFlagName [ExampleDependency] [ExampleDependency]

    -- | Dependency if tests are enabled
  | ExTest ExampleTestName [ExampleDependency]

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
    let (libraryDeps, testSuites) = splitTopLevel (CD.libraryDeps (exAvDeps ex))
    in SourcePackage {
           packageInfoId        = exAvPkgId ex
         , packageSource        = LocalTarballPackage "<<path>>"
         , packageDescrOverride = Nothing
         , packageDescription   = C.GenericPackageDescription{
               C.packageDescription = C.emptyPackageDescription {
                   C.package        = exAvPkgId ex
                 , C.library        = error "not yet configured: library"
                 , C.executables    = error "not yet configured: executables"
                 , C.testSuites     = error "not yet configured: testSuites"
                 , C.benchmarks     = error "not yet configured: benchmarks"
                 , C.buildDepends   = error "not yet configured: buildDepends"
                 , C.setupBuildInfo = Just C.SetupBuildInfo {
                       C.setupDepends = mkSetupDeps (CD.setupDeps (exAvDeps ex))
                     }
                 }
             , C.genPackageFlags = concatMap extractFlags
                                   (CD.libraryDeps (exAvDeps ex))
             , C.condLibrary     = Just $ mkCondTree libraryDeps
             , C.condExecutables = []
             , C.condTestSuites  = map (\(t, deps) -> (t, mkCondTree deps))
                                   testSuites
             , C.condBenchmarks  = []
             }
         }
  where
    splitTopLevel :: [ExampleDependency]
                  -> ( [ExampleDependency]
                     , [(ExampleTestName, [ExampleDependency])]
                     )
    splitTopLevel []                = ([], [])
    splitTopLevel (ExTest t a:deps) =
      let (other, testSuites) = splitTopLevel deps
      in (other, (t, a):testSuites)
    splitTopLevel (dep:deps)        =
      let (other, testSuites) = splitTopLevel deps
      in (dep:other, testSuites)

    extractFlags :: ExampleDependency -> [C.Flag]
    extractFlags (ExAny _)      = []
    extractFlags (ExFix _ _)    = []
    extractFlags (ExFlag f a b) = C.MkFlag {
                                      C.flagName        = C.FlagName f
                                    , C.flagDescription = ""
                                    , C.flagDefault     = False
                                    , C.flagManual      = False
                                    }
                                : concatMap extractFlags (a ++ b)
    extractFlags (ExTest _ a)   = concatMap extractFlags a

    mkCondTree :: Monoid a => [ExampleDependency] -> DependencyTree a
    mkCondTree deps =
      let (directDeps, flaggedDeps) = splitDeps deps
      in C.CondNode {
             C.condTreeData        = mempty -- irrelevant to the solver
           , C.condTreeConstraints = map mkDirect  directDeps
           , C.condTreeComponents  = map mkFlagged flaggedDeps
           }

    mkDirect :: (ExamplePkgName, Maybe ExamplePkgVersion) -> C.Dependency
    mkDirect (dep, Nothing) = C.Dependency (C.PackageName dep) C.anyVersion
    mkDirect (dep, Just n)  = C.Dependency (C.PackageName dep) (C.thisVersion v)
      where
        v = Version [n, 0, 0] []

    mkFlagged :: Monoid a
              => (ExampleFlagName, [ExampleDependency], [ExampleDependency])
              -> (C.Condition C.ConfVar
                 , DependencyTree a, Maybe (DependencyTree a))
    mkFlagged (f, a, b) = ( C.Var (C.Flag (C.FlagName f))
                          , mkCondTree a
                          , Just (mkCondTree b)
                          )

    splitDeps :: [ExampleDependency]
              -> ( [(ExamplePkgName, Maybe Int)]
                 , [(ExampleFlagName, [ExampleDependency], [ExampleDependency])]
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
    splitDeps (ExTest _ _:_) =
      error "Unexpected nested test"

    -- Currently we only support simple setup dependencies
    mkSetupDeps :: [ExampleDependency] -> [C.Dependency]
    mkSetupDeps deps =
      let (directDeps, []) = splitDeps deps in map mkDirect directDeps

exAvPkgId :: ExampleAvailable -> C.PackageIdentifier
exAvPkgId ex = C.PackageIdentifier {
      pkgName    = C.PackageName (exAvName ex)
    , pkgVersion = Version [exAvVersion ex, 0, 0] []
    }

exInstInfo :: ExampleInstalled -> C.InstalledPackageInfo
exInstInfo ex = C.emptyInstalledPackageInfo {
      C.installedPackageId = C.InstalledPackageId (exInstHash ex)
    , C.sourcePackageId    = exInstPkgId ex
    , C.packageKey         = exInstKey ex
    , C.depends            = map (C.InstalledPackageId . exInstHash)
                                 (exInstBuildAgainst ex)
    }

exInstPkgId :: ExampleInstalled -> C.PackageIdentifier
exInstPkgId ex = C.PackageIdentifier {
      pkgName    = C.PackageName (exInstName ex)
    , pkgVersion = Version [exInstVersion ex, 0, 0] []
    }

exInstLibName :: ExampleInstalled -> C.LibraryName
exInstLibName ex = C.packageKeyLibraryName (exInstPkgId ex) (exInstKey ex)

exInstKey :: ExampleInstalled -> C.PackageKey
exInstKey ex =
    C.mkPackageKey True
                   (exInstPkgId ex)
                   (map exInstLibName (exInstBuildAgainst ex))

exAvIdx :: [ExampleAvailable] -> CI.PackageIndex.PackageIndex SourcePackage
exAvIdx = CI.PackageIndex.fromList . map exAvSrcPkg

exInstIdx :: [ExampleInstalled] -> C.PackageIndex.InstalledPackageIndex
exInstIdx = C.PackageIndex.fromList . map exInstInfo

exResolve :: ExampleDb
          -> [ExamplePkgName]
          -> Bool
          -> ([String], Either String CI.InstallPlan.InstallPlan)
exResolve db targets indepGoals = runProgress $
    resolveDependencies C.buildPlatform
                        (C.unknownCompilerInfo C.buildCompilerId C.NoAbiTag)
                        Modular
                        params
  where
    (inst, avai) = partitionEithers db
    instIdx      = exInstIdx inst
    avaiIdx      = SourcePackageDb {
                       packageIndex       = exAvIdx avai
                     , packagePreferences = Map.empty
                     }
    enableTests  = map (\p -> PackageConstraintStanzas
                              (C.PackageName p) [TestStanzas])
                       (exDbPkgs db)
    targets'     = map (\p -> NamedPackage (C.PackageName p) []) targets
    params       = addConstraints (map toLpc enableTests)
                 $ (standardInstallPolicy instIdx avaiIdx targets') {
                       depResolverIndependentGoals = indepGoals
                     }
    toLpc     pc = LabeledPackageConstraint pc ConstraintSourceUnknown

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
