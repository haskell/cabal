-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Dependency
-- Copyright   :  (c) David Himmelstrup 2005,
--                    Bjorn Bringert 2007
--                    Duncan Coutts 2008
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Top level interface to dependency resolution.
-----------------------------------------------------------------------------
module Distribution.Client.Dependency (
    -- * The main package dependency resolver
    chooseSolver,
    resolveDependencies,
    Progress(..),
    foldProgress,

    -- * Alternate, simple resolver that does not do dependencies recursively
    resolveWithoutDependencies,

    -- * Constructing resolver policies
    DepResolverParams(..),
    PackageConstraint(..),
    PackagesPreferenceDefault(..),
    PackagePreference(..),
    InstalledPreference(..),

    -- ** Standard policy
    standardInstallPolicy,
    PackageSpecifier(..),

    -- ** Sandbox policy
    applySandboxInstallPolicy,

    -- ** Extra policy options
    dontUpgradeBasePackage,
    hideBrokenInstalledPackages,
    upgradeDependencies,
    reinstallTargets,

    -- ** Policy utils
    addConstraints,
    addPreferences,
    setPreferenceDefault,
    setReorderGoals,
    setIndependentGoals,
    setAvoidReinstalls,
    setShadowPkgs,
    setMaxBackjumps,
    addSourcePackages,
    hideInstalledPackagesSpecificByInstalledPackageId,
    hideInstalledPackagesSpecificBySourcePackageId,
    hideInstalledPackagesAllVersions,
    removeUpperBounds
  ) where

import Distribution.Client.Dependency.TopDown
         ( topDownResolver )
import Distribution.Client.Dependency.Modular
         ( modularResolver, SolverConfig(..) )
import qualified Distribution.Client.PackageIndex as PackageIndex
import qualified Distribution.Simple.PackageIndex as InstalledPackageIndex
import qualified Distribution.Client.InstallPlan as InstallPlan
import Distribution.Client.InstallPlan (InstallPlan)
import Distribution.Client.Types
         ( SourcePackageDb(SourcePackageDb)
         , SourcePackage(..) )
import Distribution.Client.Dependency.Types
         ( PreSolver(..), Solver(..), DependencyResolver, PackageConstraint(..)
         , AllowNewer(..), PackagePreferences(..), InstalledPreference(..)
         , PackagesPreferenceDefault(..)
         , Progress(..), foldProgress )
import Distribution.Client.Sandbox.Types
         ( SandboxPackageInfo(..) )
import Distribution.Client.Targets
import qualified Distribution.InstalledPackageInfo as Installed
import Distribution.Package
         ( PackageName(..), PackageId, Package(..), packageName, packageVersion
         , InstalledPackageId, Dependency(Dependency))
import qualified Distribution.PackageDescription as PD
         ( PackageDescription(..), GenericPackageDescription(..)
         , Library(..), Executable(..), TestSuite(..), Benchmark(..), CondTree)
import Distribution.PackageDescription (BuildInfo(targetBuildDepends))
import Distribution.PackageDescription.Configuration (mapCondTree)
import Distribution.Version
         ( Version(..), VersionRange, anyVersion, thisVersion, withinRange
         , removeUpperBound, simplifyVersionRange )
import Distribution.Compiler
         ( CompilerId(..), CompilerFlavor(..) )
import Distribution.System
         ( Platform )
import Distribution.Simple.Utils
         ( comparing, warn, info )
import Distribution.Text
         ( display )
import Distribution.Verbosity
         ( Verbosity )

import Data.List (maximumBy, foldl')
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set (Set)

-- ------------------------------------------------------------
-- * High level planner policy
-- ------------------------------------------------------------

-- | The set of parameters to the dependency resolver. These parameters are
-- relatively low level but many kinds of high level policies can be
-- implemented in terms of adjustments to the parameters.
--
data DepResolverParams = DepResolverParams {
       depResolverTargets           :: [PackageName],
       depResolverConstraints       :: [PackageConstraint],
       depResolverPreferences       :: [PackagePreference],
       depResolverPreferenceDefault :: PackagesPreferenceDefault,
       depResolverInstalledPkgIndex :: InstalledPackageIndex.PackageIndex,
       depResolverSourcePkgIndex    :: PackageIndex.PackageIndex SourcePackage,
       depResolverReorderGoals      :: Bool,
       depResolverIndependentGoals  :: Bool,
       depResolverAvoidReinstalls   :: Bool,
       depResolverShadowPkgs        :: Bool,
       depResolverMaxBackjumps      :: Maybe Int
     }


-- | A package selection preference for a particular package.
--
-- Preferences are soft constraints that the dependency resolver should try to
-- respect where possible. It is not specified if preferences on some packages
-- are more important than others.
--
data PackagePreference =

     -- | A suggested constraint on the version number.
     PackageVersionPreference   PackageName VersionRange

     -- | If we prefer versions of packages that are already installed.
   | PackageInstalledPreference PackageName InstalledPreference

basicDepResolverParams :: InstalledPackageIndex.PackageIndex
                       -> PackageIndex.PackageIndex SourcePackage
                       -> DepResolverParams
basicDepResolverParams installedPkgIndex sourcePkgIndex =
    DepResolverParams {
       depResolverTargets           = [],
       depResolverConstraints       = [],
       depResolverPreferences       = [],
       depResolverPreferenceDefault = PreferLatestForSelected,
       depResolverInstalledPkgIndex = installedPkgIndex,
       depResolverSourcePkgIndex    = sourcePkgIndex,
       depResolverReorderGoals      = False,
       depResolverIndependentGoals  = False,
       depResolverAvoidReinstalls   = False,
       depResolverShadowPkgs        = False,
       depResolverMaxBackjumps      = Nothing
     }

addTargets :: [PackageName]
           -> DepResolverParams -> DepResolverParams
addTargets extraTargets params =
    params {
      depResolverTargets = extraTargets ++ depResolverTargets params
    }

addConstraints :: [PackageConstraint]
               -> DepResolverParams -> DepResolverParams
addConstraints extraConstraints params =
    params {
      depResolverConstraints = extraConstraints
                            ++ depResolverConstraints params
    }

addPreferences :: [PackagePreference]
               -> DepResolverParams -> DepResolverParams
addPreferences extraPreferences params =
    params {
      depResolverPreferences = extraPreferences
                            ++ depResolverPreferences params
    }

setPreferenceDefault :: PackagesPreferenceDefault
                     -> DepResolverParams -> DepResolverParams
setPreferenceDefault preferenceDefault params =
    params {
      depResolverPreferenceDefault = preferenceDefault
    }

setReorderGoals :: Bool -> DepResolverParams -> DepResolverParams
setReorderGoals b params =
    params {
      depResolverReorderGoals = b
    }

setIndependentGoals :: Bool -> DepResolverParams -> DepResolverParams
setIndependentGoals b params =
    params {
      depResolverIndependentGoals = b
    }

setAvoidReinstalls :: Bool -> DepResolverParams -> DepResolverParams
setAvoidReinstalls b params =
    params {
      depResolverAvoidReinstalls = b
    }

setShadowPkgs :: Bool -> DepResolverParams -> DepResolverParams
setShadowPkgs b params =
    params {
      depResolverShadowPkgs = b
    }

setMaxBackjumps :: Maybe Int -> DepResolverParams -> DepResolverParams
setMaxBackjumps n params =
    params {
      depResolverMaxBackjumps = n
    }

dontUpgradeBasePackage :: DepResolverParams -> DepResolverParams
dontUpgradeBasePackage params =
    addConstraints extraConstraints params
  where
    extraConstraints =
      [ PackageConstraintInstalled pkgname
      | all (/=PackageName "base") (depResolverTargets params)
      , pkgname <-  [ PackageName "base", PackageName "ghc-prim" ]
      , isInstalled pkgname ]
    -- TODO: the top down resolver chokes on the base constraints
    -- below when there are no targets and thus no dep on base.
    -- Need to refactor contraints separate from needing packages.
    isInstalled = not . null
                . InstalledPackageIndex.lookupPackageName
                                 (depResolverInstalledPkgIndex params)

addSourcePackages :: [SourcePackage]
                  -> DepResolverParams -> DepResolverParams
addSourcePackages pkgs params =
    params {
      depResolverSourcePkgIndex =
        foldl (flip PackageIndex.insert)
              (depResolverSourcePkgIndex params) pkgs
    }

hideInstalledPackagesSpecificByInstalledPackageId :: [InstalledPackageId]
                                                     -> DepResolverParams
                                                     -> DepResolverParams
hideInstalledPackagesSpecificByInstalledPackageId pkgids params =
    --TODO: this should work using exclude constraints instead
    params {
      depResolverInstalledPkgIndex =
        foldl' (flip InstalledPackageIndex.deleteInstalledPackageId)
               (depResolverInstalledPkgIndex params) pkgids
    }

hideInstalledPackagesSpecificBySourcePackageId :: [PackageId]
                                                  -> DepResolverParams
                                                  -> DepResolverParams
hideInstalledPackagesSpecificBySourcePackageId pkgids params =
    --TODO: this should work using exclude constraints instead
    params {
      depResolverInstalledPkgIndex =
        foldl' (flip InstalledPackageIndex.deleteSourcePackageId)
               (depResolverInstalledPkgIndex params) pkgids
    }

hideInstalledPackagesAllVersions :: [PackageName]
                                 -> DepResolverParams -> DepResolverParams
hideInstalledPackagesAllVersions pkgnames params =
    --TODO: this should work using exclude constraints instead
    params {
      depResolverInstalledPkgIndex =
        foldl' (flip InstalledPackageIndex.deletePackageName)
               (depResolverInstalledPkgIndex params) pkgnames
    }


hideBrokenInstalledPackages :: DepResolverParams -> DepResolverParams
hideBrokenInstalledPackages params =
    hideInstalledPackagesSpecificByInstalledPackageId pkgids params
  where
    pkgids = map Installed.installedPackageId
           . InstalledPackageIndex.reverseDependencyClosure
                            (depResolverInstalledPkgIndex params)
           . map (Installed.installedPackageId . fst)
           . InstalledPackageIndex.brokenPackages
           $ depResolverInstalledPkgIndex params

-- | Remove upper bounds in dependencies using the policy specified by the
-- 'AllowNewer' argument (all/some/none).
removeUpperBounds :: AllowNewer -> DepResolverParams -> DepResolverParams
removeUpperBounds allowNewer params =
    params {
      -- NB: It's important to apply 'removeUpperBounds' after
      -- 'addSourcePackages'. Otherwise, the packages inserted by
      -- 'addSourcePackages' won't have upper bounds in dependencies relaxed.

      depResolverSourcePkgIndex = sourcePkgIndex'
    }
  where
    sourcePkgIndex  = depResolverSourcePkgIndex params
    sourcePkgIndex' = case allowNewer of
      AllowNewerNone      -> sourcePkgIndex
      AllowNewerAll       -> fmap relaxAllPackageDeps         sourcePkgIndex
      AllowNewerSome pkgs -> fmap (relaxSomePackageDeps pkgs) sourcePkgIndex

    relaxAllPackageDeps :: SourcePackage -> SourcePackage
    relaxAllPackageDeps = onAllBuildDepends doRelax
      where
        doRelax (Dependency pkgName verRange) =
          Dependency pkgName (removeUpperBound verRange)

    relaxSomePackageDeps :: [PackageName] -> SourcePackage -> SourcePackage
    relaxSomePackageDeps pkgNames = onAllBuildDepends doRelax
      where
        doRelax d@(Dependency pkgName verRange)
          | pkgName `elem` pkgNames = Dependency pkgName
                                      (removeUpperBound verRange)
          | otherwise               = d

    -- Walk a 'GenericPackageDescription' and apply 'f' to all 'build-depends'
    -- fields.
    onAllBuildDepends :: (Dependency -> Dependency)
                      -> SourcePackage -> SourcePackage
    onAllBuildDepends f srcPkg = srcPkg'
      where
        gpd        = packageDescription srcPkg
        pd         = PD.packageDescription gpd
        condLib    = PD.condLibrary        gpd
        condExes   = PD.condExecutables    gpd
        condTests  = PD.condTestSuites     gpd
        condBenchs = PD.condBenchmarks     gpd

        f' = onBuildInfo f
        onBuildInfo g bi = bi
          { targetBuildDepends = map g (targetBuildDepends bi) }

        onLibrary    lib  = lib { PD.libBuildInfo  = f' $ PD.libBuildInfo  lib }
        onExecutable exe  = exe { PD.buildInfo     = f' $ PD.buildInfo     exe }
        onTestSuite  tst  = tst { PD.testBuildInfo = f' $ PD.testBuildInfo tst }
        onBenchmark  bmk  = bmk { PD.benchmarkBuildInfo =
                                     f' $ PD.benchmarkBuildInfo bmk }

        srcPkg' = srcPkg { packageDescription = gpd' }
        gpd'    = gpd {
          PD.packageDescription = pd',
          PD.condLibrary        = condLib',
          PD.condExecutables    = condExes',
          PD.condTestSuites     = condTests',
          PD.condBenchmarks     = condBenchs'
          }
        pd' = pd {
          PD.buildDepends = map  f            (PD.buildDepends pd),
          PD.library      = fmap onLibrary    (PD.library pd),
          PD.executables  = map  onExecutable (PD.executables pd),
          PD.testSuites   = map  onTestSuite  (PD.testSuites pd),
          PD.benchmarks   = map  onBenchmark  (PD.benchmarks pd)
          }
        condLib'    = fmap (onCondTree onLibrary)             condLib
        condExes'   = map  (mapSnd $ onCondTree onExecutable) condExes
        condTests'  = map  (mapSnd $ onCondTree onTestSuite)  condTests
        condBenchs' = map  (mapSnd $ onCondTree onBenchmark)  condBenchs

        mapSnd :: (a -> b) -> (c,a) -> (c,b)
        mapSnd = fmap

        onCondTree :: (a -> b) -> PD.CondTree v [Dependency] a
                   -> PD.CondTree v [Dependency] b
        onCondTree g = mapCondTree g (map f) id


upgradeDependencies :: DepResolverParams -> DepResolverParams
upgradeDependencies = setPreferenceDefault PreferAllLatest


reinstallTargets :: DepResolverParams -> DepResolverParams
reinstallTargets params =
    hideInstalledPackagesAllVersions (depResolverTargets params) params


standardInstallPolicy :: InstalledPackageIndex.PackageIndex
                      -> SourcePackageDb
                      -> [PackageSpecifier SourcePackage]
                      -> DepResolverParams
standardInstallPolicy
    installedPkgIndex (SourcePackageDb sourcePkgIndex sourcePkgPrefs)
    pkgSpecifiers

  = addPreferences
      [ PackageVersionPreference name ver
      | (name, ver) <- Map.toList sourcePkgPrefs ]

  . addConstraints
      (concatMap pkgSpecifierConstraints pkgSpecifiers)

  . addTargets
      (map pkgSpecifierTarget pkgSpecifiers)

  . hideInstalledPackagesSpecificBySourcePackageId
      [ packageId pkg | SpecificSourcePackage pkg <- pkgSpecifiers ]

  . addSourcePackages
      [ pkg  | SpecificSourcePackage pkg <- pkgSpecifiers ]

  $ basicDepResolverParams
      installedPkgIndex sourcePkgIndex

applySandboxInstallPolicy :: SandboxPackageInfo
                             -> DepResolverParams
                             -> DepResolverParams
applySandboxInstallPolicy
  (SandboxPackageInfo modifiedDeps otherDeps allSandboxPkgs _allDeps)
  params

  = addPreferences [ PackageInstalledPreference n PreferInstalled
                   | n <- installedNotModified ]

  . addTargets installedNotModified

  . addPreferences
      [ PackageVersionPreference (packageName pkg)
        (thisVersion (packageVersion pkg)) | pkg <- otherDeps ]

  . addConstraints
      [ PackageConstraintVersion (packageName pkg)
        (thisVersion (packageVersion pkg)) | pkg <- modifiedDeps ]

  . addTargets [ packageName pkg | pkg <- modifiedDeps ]

  . hideInstalledPackagesSpecificBySourcePackageId
      [ packageId pkg | pkg <- modifiedDeps ]

  -- We don't need to add source packages for add-source deps to the
  -- 'installedPkgIndex' since 'getSourcePackages' did that for us.

  $ params

  where
    installedPkgIds =
      map fst . InstalledPackageIndex.allPackagesBySourcePackageId
      $ allSandboxPkgs
    modifiedPkgIds       = map packageId modifiedDeps
    installedNotModified = [ packageName pkg | pkg <- installedPkgIds,
                             pkg `notElem` modifiedPkgIds ]

-- ------------------------------------------------------------
-- * Interface to the standard resolver
-- ------------------------------------------------------------

chooseSolver :: Verbosity -> PreSolver -> CompilerId -> IO Solver
chooseSolver _         AlwaysTopDown _                = return TopDown
chooseSolver _         AlwaysModular _                = return Modular
chooseSolver verbosity Choose        (CompilerId f v) = do
  let chosenSolver | f == GHC && v <= Version [7] [] = TopDown
                   | otherwise                       = Modular
      msg TopDown = warn verbosity "Falling back to topdown solver for GHC < 7."
      msg Modular = info verbosity "Choosing modular solver."
  msg chosenSolver
  return chosenSolver

runSolver :: Solver -> SolverConfig -> DependencyResolver
runSolver TopDown = const topDownResolver -- TODO: warn about unsuported options
runSolver Modular = modularResolver

-- | Run the dependency solver.
--
-- Since this is potentially an expensive operation, the result is wrapped in a
-- a 'Progress' structure that can be unfolded to provide progress information,
-- logging messages and the final result or an error.
--
resolveDependencies :: Platform
                    -> CompilerId
                    -> Solver
                    -> DepResolverParams
                    -> Progress String String InstallPlan

    --TODO: is this needed here? see dontUpgradeBasePackage
resolveDependencies platform comp _solver params
  | null (depResolverTargets params)
  = return (mkInstallPlan platform comp [])

resolveDependencies platform comp  solver params =

    fmap (mkInstallPlan platform comp)
  $ runSolver solver (SolverConfig reorderGoals indGoals noReinstalls
                      shadowing maxBkjumps)
                     platform comp installedPkgIndex sourcePkgIndex
                     preferences constraints targets
  where
    DepResolverParams
      targets constraints
      prefs defpref
      installedPkgIndex
      sourcePkgIndex
      reorderGoals
      indGoals
      noReinstalls
      shadowing
      maxBkjumps      = dontUpgradeBasePackage
                      -- TODO:
                      -- The modular solver can properly deal with broken
                      -- packages and won't select them. So the
                      -- 'hideBrokenInstalledPackages' function should be moved
                      -- into a module that is specific to the Topdown solver.
                      . (if solver /= Modular then hideBrokenInstalledPackages
                                              else id)
                      $ params

    preferences = interpretPackagesPreference
                    (Set.fromList targets) defpref prefs

-- | Make an install plan from the output of the dep resolver.
-- It checks that the plan is valid, or it's an error in the dep resolver.
--
mkInstallPlan :: Platform
              -> CompilerId
              -> [InstallPlan.PlanPackage] -> InstallPlan
mkInstallPlan platform comp pkgIndex =
  case InstallPlan.new platform comp (PackageIndex.fromList pkgIndex) of
    Right plan     -> plan
    Left  problems -> error $ unlines $
        "internal error: could not construct a valid install plan."
      : "The proposed (invalid) plan contained the following problems:"
      : map InstallPlan.showPlanProblem problems


-- | Give an interpretation to the global 'PackagesPreference' as
--  specific per-package 'PackageVersionPreference'.
--
interpretPackagesPreference :: Set PackageName
                            -> PackagesPreferenceDefault
                            -> [PackagePreference]
                            -> (PackageName -> PackagePreferences)
interpretPackagesPreference selected defaultPref prefs =
  \pkgname -> PackagePreferences (versionPref pkgname) (installPref pkgname)

  where
    versionPref pkgname =
      fromMaybe anyVersion (Map.lookup pkgname versionPrefs)
    versionPrefs = Map.fromList
      [ (pkgname, pref)
      | PackageVersionPreference pkgname pref <- prefs ]

    installPref pkgname =
      fromMaybe (installPrefDefault pkgname) (Map.lookup pkgname installPrefs)
    installPrefs = Map.fromList
      [ (pkgname, pref)
      | PackageInstalledPreference pkgname pref <- prefs ]
    installPrefDefault = case defaultPref of
      PreferAllLatest         -> \_       -> PreferLatest
      PreferAllInstalled      -> \_       -> PreferInstalled
      PreferLatestForSelected -> \pkgname ->
        -- When you say cabal install foo, what you really mean is, prefer the
        -- latest version of foo, but the installed version of everything else
        if pkgname `Set.member` selected then PreferLatest
                                         else PreferInstalled

-- ------------------------------------------------------------
-- * Simple resolver that ignores dependencies
-- ------------------------------------------------------------

-- | A simplistic method of resolving a list of target package names to
-- available packages.
--
-- Specifically, it does not consider package dependencies at all. Unlike
-- 'resolveDependencies', no attempt is made to ensure that the selected
-- packages have dependencies that are satisfiable or consistent with
-- each other.
--
-- It is suitable for tasks such as selecting packages to download for user
-- inspection. It is not suitable for selecting packages to install.
--
-- Note: if no installed package index is available, it is ok to pass 'mempty'.
-- It simply means preferences for installed packages will be ignored.
--
resolveWithoutDependencies :: DepResolverParams
                           -> Either [ResolveNoDepsError] [SourcePackage]
resolveWithoutDependencies (DepResolverParams targets constraints
                              prefs defpref installedPkgIndex sourcePkgIndex
                              _reorderGoals _indGoals _avoidReinstalls
                              _shadowing _maxBjumps) =
    collectEithers (map selectPackage targets)
  where
    selectPackage :: PackageName -> Either ResolveNoDepsError SourcePackage
    selectPackage pkgname
      | null choices = Left  $! ResolveUnsatisfiable pkgname requiredVersions
      | otherwise    = Right $! maximumBy bestByPrefs choices

      where
        -- Constraints
        requiredVersions = packageConstraints pkgname
        pkgDependency    = Dependency pkgname requiredVersions
        choices          = PackageIndex.lookupDependency sourcePkgIndex
                                                         pkgDependency

        -- Preferences
        PackagePreferences preferredVersions preferInstalled
          = packagePreferences pkgname

        bestByPrefs   = comparing $ \pkg ->
                          (installPref pkg, versionPref pkg, packageVersion pkg)
        installPref   = case preferInstalled of
          PreferLatest    -> const False
          PreferInstalled -> not . null
                           . InstalledPackageIndex.lookupSourcePackageId
                                                     installedPkgIndex
                           . packageId
        versionPref   pkg = packageVersion pkg `withinRange` preferredVersions

    packageConstraints :: PackageName -> VersionRange
    packageConstraints pkgname =
      Map.findWithDefault anyVersion pkgname packageVersionConstraintMap
    packageVersionConstraintMap =
      Map.fromList [ (name, range)
                   | PackageConstraintVersion name range <- constraints ]

    packagePreferences :: PackageName -> PackagePreferences
    packagePreferences = interpretPackagesPreference
                           (Set.fromList targets) defpref prefs


collectEithers :: [Either a b] -> Either [a] [b]
collectEithers = collect . partitionEithers
  where
    collect ([], xs) = Right xs
    collect (errs,_) = Left errs
    partitionEithers :: [Either a b] -> ([a],[b])
    partitionEithers = foldr (either left right) ([],[])
     where
       left  a (l, r) = (a:l, r)
       right a (l, r) = (l, a:r)

-- | Errors for 'resolveWithoutDependencies'.
--
data ResolveNoDepsError =

     -- | A package name which cannot be resolved to a specific package.
     -- Also gives the constraint on the version and whether there was
     -- a constraint on the package being installed.
     ResolveUnsatisfiable PackageName VersionRange

instance Show ResolveNoDepsError where
  show (ResolveUnsatisfiable name ver) =
       "There is no available version of " ++ display name
    ++ " that satisfies " ++ display (simplifyVersionRange ver)
