{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Freeze
-- Copyright   :  (c) David Himmelstrup 2005
--                    Duncan Coutts 2011
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- The cabal freeze command
-----------------------------------------------------------------------------
module Distribution.Client.Freeze (
    freeze, getFreezePkgs
  ) where

import Distribution.Client.Config ( SavedConfig(..) )
import Distribution.Client.Types
import Distribution.Client.Targets
import Distribution.Client.Dependency
import Distribution.Client.Dependency.Types
         ( ConstraintSource(..), LabeledPackageConstraint(..) )
import Distribution.Client.IndexUtils as IndexUtils
         ( getSourcePackages, getInstalledPackages )
import Distribution.Client.InstallPlan
         ( InstallPlan, PlanPackage )
import qualified Distribution.Client.InstallPlan as InstallPlan
import Distribution.Client.PkgConfigDb
         ( PkgConfigDb, readPkgConfigDb )
import Distribution.Client.Setup
         ( GlobalFlags(..), FreezeFlags(..), ConfigExFlags(..)
         , RepoContext(..) )
import Distribution.Client.Sandbox.PackageEnvironment
         ( loadUserConfig, pkgEnvSavedConfig, showPackageEnvironment,
           userPackageEnvironmentFile )
import Distribution.Client.Sandbox.Types
         ( SandboxPackageInfo(..) )

import Distribution.Package
         ( Package, packageId, packageName, packageVersion )
import Distribution.Simple.Compiler
         ( Compiler, compilerInfo, PackageDBStack )
import Distribution.Simple.PackageIndex (InstalledPackageIndex)
import Distribution.Simple.Program
         ( ProgramConfiguration )
import Distribution.Simple.Setup
         ( fromFlag, fromFlagOrDefault, flagToMaybe )
import Distribution.Simple.Utils
         ( die, notice, debug, writeFileAtomic )
import Distribution.System
         ( Platform )
import Distribution.Text
         ( display )
import Distribution.Verbosity
         ( Verbosity )

import Control.Monad
         ( when )
import qualified Data.ByteString.Lazy.Char8 as BS.Char8
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid
         ( mempty )
#endif
import Data.Version
         ( showVersion )
import Distribution.Version
         ( thisVersion )

-- ------------------------------------------------------------
-- * The freeze command
-- ------------------------------------------------------------

-- | Freeze all of the dependencies by writing a constraints section
-- constraining each dependency to an exact version.
--
freeze :: Verbosity
      -> PackageDBStack
      -> RepoContext
      -> Compiler
      -> Platform
      -> ProgramConfiguration
      -> Maybe SandboxPackageInfo
      -> GlobalFlags
      -> FreezeFlags
      -> IO ()
freeze verbosity packageDBs repoCtxt comp platform conf mSandboxPkgInfo
      globalFlags freezeFlags = do

    pkgs  <- getFreezePkgs
               verbosity packageDBs repoCtxt comp platform conf mSandboxPkgInfo
               globalFlags freezeFlags

    if null pkgs
      then notice verbosity $ "No packages to be frozen. "
                           ++ "As this package has no dependencies."
      else if dryRun
             then notice verbosity $ unlines $
                     "The following packages would be frozen:"
                   : formatPkgs pkgs

             else freezePackages verbosity globalFlags pkgs

  where
    dryRun = fromFlag (freezeDryRun freezeFlags)

-- | Get the list of packages whose versions would be frozen by the @freeze@
-- command.
getFreezePkgs :: Verbosity
              -> PackageDBStack
              -> RepoContext
              -> Compiler
              -> Platform
              -> ProgramConfiguration
              -> Maybe SandboxPackageInfo
              -> GlobalFlags
              -> FreezeFlags
              -> IO [PlanPackage]
getFreezePkgs verbosity packageDBs repoCtxt comp platform conf mSandboxPkgInfo
      globalFlags freezeFlags = do

    installedPkgIndex <- getInstalledPackages verbosity comp packageDBs conf
    sourcePkgDb       <- getSourcePackages    verbosity repoCtxt
    pkgConfigDb       <- readPkgConfigDb      verbosity conf

    pkgSpecifiers <- resolveUserTargets verbosity repoCtxt
                       (fromFlag $ globalWorldFile globalFlags)
                       (packageIndex sourcePkgDb)
                       [UserTargetLocalDir "."]

    sanityCheck pkgSpecifiers
    planPackages
               verbosity comp platform mSandboxPkgInfo freezeFlags
               installedPkgIndex sourcePkgDb pkgConfigDb pkgSpecifiers
  where
    sanityCheck pkgSpecifiers = do
      when (not . null $ [n | n@(NamedPackage _ _) <- pkgSpecifiers]) $
        die $ "internal error: 'resolveUserTargets' returned "
           ++ "unexpected named package specifiers!"
      when (length pkgSpecifiers /= 1) $
        die $ "internal error: 'resolveUserTargets' returned "
           ++ "unexpected source package specifiers!"

planPackages :: Verbosity
             -> Compiler
             -> Platform
             -> Maybe SandboxPackageInfo
             -> FreezeFlags
             -> InstalledPackageIndex
             -> SourcePackageDb
             -> PkgConfigDb
             -> [PackageSpecifier SourcePackage]
             -> IO [PlanPackage]
planPackages verbosity comp platform mSandboxPkgInfo freezeFlags
             installedPkgIndex sourcePkgDb pkgConfigDb pkgSpecifiers = do

  solver <- chooseSolver verbosity
            (fromFlag (freezeSolver freezeFlags)) (compilerInfo comp)
  notice verbosity "Resolving dependencies..."

  installPlan <- foldProgress logMsg die return $
                   resolveDependencies
                     platform (compilerInfo comp) pkgConfigDb
                     solver
                     resolverParams

  return $ pruneInstallPlan installPlan pkgSpecifiers

  where
    resolverParams =

        setMaxBackjumps (if maxBackjumps < 0 then Nothing
                                             else Just maxBackjumps)

      . setIndependentGoals independentGoals

      . setReorderGoals reorderGoals

      . setShadowPkgs shadowPkgs

      . setStrongFlags strongFlags

      . addConstraints
          [ let pkg = pkgSpecifierTarget pkgSpecifier
                pc = PackageConstraintStanzas pkg stanzas
            in LabeledPackageConstraint pc ConstraintSourceFreeze
          | pkgSpecifier <- pkgSpecifiers ]

      . maybe id applySandboxInstallPolicy mSandboxPkgInfo

      $ standardInstallPolicy installedPkgIndex sourcePkgDb pkgSpecifiers

    logMsg message rest = debug verbosity message >> rest

    stanzas = [ TestStanzas | testsEnabled ]
           ++ [ BenchStanzas | benchmarksEnabled ]
    testsEnabled      = fromFlagOrDefault False $ freezeTests freezeFlags
    benchmarksEnabled = fromFlagOrDefault False $ freezeBenchmarks freezeFlags

    reorderGoals     = fromFlag (freezeReorderGoals     freezeFlags)
    independentGoals = fromFlag (freezeIndependentGoals freezeFlags)
    shadowPkgs       = fromFlag (freezeShadowPkgs       freezeFlags)
    strongFlags      = fromFlag (freezeStrongFlags      freezeFlags)
    maxBackjumps     = fromFlag (freezeMaxBackjumps     freezeFlags)


-- | Remove all unneeded packages from an install plan.
--
-- A package is unneeded if it is either
--
-- 1) the package that we are freezing, or
--
-- 2) not a dependency (directly or transitively) of the package we are
--    freezing.  This is useful for removing previously installed packages
--    which are no longer required from the install plan.
pruneInstallPlan :: InstallPlan
                 -> [PackageSpecifier SourcePackage]
                 -> [PlanPackage]
pruneInstallPlan installPlan pkgSpecifiers =
    removeSelf pkgIds $
    InstallPlan.dependencyClosure installPlan (map fakeUnitId pkgIds)
  where
    pkgIds = [ packageId pkg
             | SpecificSourcePackage pkg <- pkgSpecifiers ]
    removeSelf [thisPkg] = filter (\pp -> packageId pp /= packageId thisPkg)
    removeSelf _  = error $ "internal error: 'pruneInstallPlan' given "
                         ++ "unexpected package specifiers!"


freezePackages :: Package pkg => Verbosity -> GlobalFlags -> [pkg] -> IO ()
freezePackages verbosity globalFlags pkgs = do

    pkgEnv <- fmap (createPkgEnv . addFrozenConstraints) $
                   loadUserConfig verbosity ""  (flagToMaybe . globalConstraintsFile $ globalFlags)
    writeFileAtomic userPackageEnvironmentFile $ showPkgEnv pkgEnv
  where
    addFrozenConstraints config =
        config {
            savedConfigureExFlags = (savedConfigureExFlags config) {
                configExConstraints = map constraint pkgs
            }
        }
    constraint pkg =
        (pkgIdToConstraint $ packageId pkg, ConstraintSourceUserConfig userPackageEnvironmentFile)
      where
        pkgIdToConstraint pkgId =
            UserConstraintVersion (packageName pkgId)
                                  (thisVersion $ packageVersion pkgId)
    createPkgEnv config = mempty { pkgEnvSavedConfig = config }
    showPkgEnv = BS.Char8.pack . showPackageEnvironment


formatPkgs :: Package pkg => [pkg] -> [String]
formatPkgs = map $ showPkg . packageId
  where
    showPkg pid = name pid ++ " == " ++ version pid
    name = display . packageName
    version = showVersion . packageVersion
