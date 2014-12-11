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
    freeze,
  ) where

import Distribution.Client.Config ( SavedConfig(..) )
import Distribution.Client.Types
import Distribution.Client.Targets
import Distribution.Client.Dependency
import Distribution.Client.IndexUtils as IndexUtils
         ( getSourcePackages, getInstalledPackages )
import Distribution.Client.InstallPlan
         ( PlanPackage )
import qualified Distribution.Client.InstallPlan as InstallPlan
import Distribution.Client.Setup
         ( GlobalFlags(..), FreezeFlags(..), ConfigExFlags(..) )
import Distribution.Client.Sandbox.PackageEnvironment
         ( loadUserConfig, pkgEnvSavedConfig, showPackageEnvironment,
           userPackageEnvironmentFile )
import Distribution.Client.Sandbox.Types
         ( SandboxPackageInfo(..) )

import Distribution.Package
         ( Package, PackageIdentifier, packageId, packageName, packageVersion )
import Distribution.Simple.Compiler
         ( Compiler, compilerInfo, PackageDBStack )
import Distribution.Simple.PackageIndex (InstalledPackageIndex)
import qualified Distribution.Client.PackageIndex as PackageIndex
import Distribution.Simple.Program
         ( ProgramConfiguration )
import Distribution.Simple.Setup
         ( fromFlag, fromFlagOrDefault )
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
import Data.Monoid
         ( mempty )
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
      -> [Repo]
      -> Compiler
      -> Platform
      -> ProgramConfiguration
      -> Maybe SandboxPackageInfo
      -> GlobalFlags
      -> FreezeFlags
      -> IO ()
freeze verbosity packageDBs repos comp platform conf mSandboxPkgInfo
      globalFlags freezeFlags = do

    installedPkgIndex <- getInstalledPackages verbosity comp packageDBs conf
    sourcePkgDb       <- getSourcePackages    verbosity repos

    pkgSpecifiers <- resolveUserTargets verbosity
                       (fromFlag $ globalWorldFile globalFlags)
                       (packageIndex sourcePkgDb)
                       [UserTargetLocalDir "."]

    sanityCheck pkgSpecifiers
    pkgs  <- planPackages
               verbosity comp platform mSandboxPkgInfo freezeFlags
               installedPkgIndex sourcePkgDb pkgSpecifiers

    if null pkgs
      then notice verbosity $ "No packages to be frozen. "
                           ++ "As this package has no dependencies."
      else if dryRun
             then notice verbosity $ unlines $
                     "The following packages would be frozen:"
                   : formatPkgs pkgs

             else freezePackages verbosity pkgs

  where
    dryRun = fromFlag (freezeDryRun freezeFlags)

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
             -> [PackageSpecifier SourcePackage]
             -> IO [PlanPackage]
planPackages verbosity comp platform mSandboxPkgInfo freezeFlags
             installedPkgIndex sourcePkgDb pkgSpecifiers = do

  solver <- chooseSolver verbosity
            (fromFlag (freezeSolver freezeFlags)) (compilerInfo comp)
  notice verbosity "Resolving dependencies..."

  installPlan <- foldProgress logMsg die return $
                   resolveDependencies
                     platform (compilerInfo comp)
                     solver
                     resolverParams

  return $ either id
                  (error "planPackages: installPlan contains broken packages")
                  (pruneInstallPlan installPlan pkgSpecifiers)

  where
    resolverParams =

        setMaxBackjumps (if maxBackjumps < 0 then Nothing
                                             else Just maxBackjumps)

      . setIndependentGoals independentGoals

      . setReorderGoals reorderGoals

      . setShadowPkgs shadowPkgs

      . setStrongFlags strongFlags

      . addConstraints
          [ PackageConstraintStanzas (pkgSpecifierTarget pkgSpecifier) stanzas
          | pkgSpecifier <- pkgSpecifiers ]

      . maybe id applySandboxInstallPolicy mSandboxPkgInfo

      $ standardInstallPolicy installedPkgIndex sourcePkgDb pkgSpecifiers

    logMsg message rest = debug verbosity message >> rest

    stanzas = concat
        [ if testsEnabled      then [TestStanzas]  else []
        , if benchmarksEnabled then [BenchStanzas] else []
        ]
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
pruneInstallPlan :: InstallPlan.InstallPlan
                 -> [PackageSpecifier SourcePackage]
                 -> Either [PlanPackage] [(PlanPackage, [PackageIdentifier])]
pruneInstallPlan installPlan pkgSpecifiers =
    mapLeft (removeSelf pkgIds . PackageIndex.allPackages) $
    PackageIndex.dependencyClosure pkgIdx pkgIds
  where
    pkgIdx = PackageIndex.fromList $ InstallPlan.toList installPlan
    pkgIds = [ packageId pkg | SpecificSourcePackage pkg <- pkgSpecifiers ]
    mapLeft f (Left v)  = Left $ f v
    mapLeft _ (Right v) = Right v
    removeSelf [thisPkg] = filter (\pp -> packageId pp /= thisPkg)
    removeSelf _ =
        error $ "internal error: 'pruneInstallPlan' given "
           ++ "unexpected package specifiers!"


freezePackages :: Package pkg => Verbosity -> [pkg] -> IO ()
freezePackages verbosity pkgs = do
    pkgEnv <- fmap (createPkgEnv . addFrozenConstraints) $
                   loadUserConfig verbosity ""
    writeFileAtomic userPackageEnvironmentFile $ showPkgEnv pkgEnv
  where
    addFrozenConstraints config =
        config {
            savedConfigureExFlags = (savedConfigureExFlags config) {
                configExConstraints = constraints pkgs
            }
        }
    constraints = map $ pkgIdToConstraint . packageId
      where
        pkgIdToConstraint pkg =
            UserConstraintVersion (packageName pkg)
                                  (thisVersion $ packageVersion pkg)
    createPkgEnv config = mempty { pkgEnvSavedConfig = config }
    showPkgEnv = BS.Char8.pack . showPackageEnvironment


formatPkgs :: Package pkg => [pkg] -> [String]
formatPkgs = map $ showPkg . packageId
  where
    showPkg pid = name pid ++ " == " ++ version pid
    name = display . packageName
    version = showVersion . packageVersion
