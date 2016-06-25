{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

-- | cabal-install CLI command: freeze
--
module Distribution.Client.CmdFreeze (
    freezeAction,
  ) where

import Distribution.Client.ProjectPlanning
         ( ElaboratedInstallPlan, rebuildInstallPlan )
import Distribution.Client.ProjectConfig
         ( ProjectConfig(..), ProjectConfigShared(..)
         , commandLineFlagsToProjectConfig, writeProjectLocalFreezeConfig
         , findProjectRoot )
import Distribution.Client.ProjectPlanning.Types
         ( ElaboratedConfiguredPackage(..) )
import Distribution.Client.Targets
         ( UserConstraint(..) )
import Distribution.Solver.Types.ConstraintSource
         ( ConstraintSource(..) )
import Distribution.Client.DistDirLayout
         ( defaultDistDirLayout, defaultCabalDirLayout )
import Distribution.Client.Config
         ( defaultCabalDir )
import qualified Distribution.Client.InstallPlan as InstallPlan


import Distribution.Package
         ( PackageName, packageName, packageVersion )
import Distribution.Version
         ( VersionRange, thisVersion, unionVersionRanges )
import Distribution.PackageDescription
         ( FlagAssignment )
import Distribution.Client.Setup
         ( GlobalFlags, ConfigFlags(..), ConfigExFlags, InstallFlags )
import Distribution.Simple.Setup
         ( HaddockFlags, fromFlagOrDefault )
import Distribution.Simple.Utils
         ( die, notice )
import Distribution.Verbosity
         ( normal )

import Data.Monoid as Monoid
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Monad (unless)
import System.FilePath


-- | To a first approximation, the @freeze@ command runs the first phase of
-- the @build@ command where we bring the install plan up to date, and then
-- based on the install plan we write out a @cabal.project.freeze@ config file.
--
-- For more details on how this works, see the module
-- "Distribution.Client.ProjectOrchestration"
--
freezeAction :: (ConfigFlags, ConfigExFlags, InstallFlags, HaddockFlags)
             -> [String] -> GlobalFlags -> IO ()
freezeAction (configFlags, configExFlags, installFlags, haddockFlags)
             extraArgs globalFlags = do

    unless (null extraArgs) $
      die $ "'freeze' doesn't take any extra arguments: "
         ++ unwords extraArgs

    cabalDir <- defaultCabalDir
    let cabalDirLayout = defaultCabalDirLayout cabalDir

    projectRootDir <- findProjectRoot
    let distDirLayout = defaultDistDirLayout projectRootDir

    let cliConfig = commandLineFlagsToProjectConfig
                      globalFlags configFlags configExFlags
                      installFlags haddockFlags


    (_, elaboratedPlan, _, _) <-
      rebuildInstallPlan verbosity
                         projectRootDir distDirLayout cabalDirLayout
                         cliConfig

    let freezeConfig = projectFreezeConfig elaboratedPlan
    writeProjectLocalFreezeConfig projectRootDir freezeConfig
    notice verbosity $
      "Wrote freeze file: " ++ projectRootDir </> "cabal.project.freeze"

  where
    verbosity = fromFlagOrDefault normal (configVerbosity configFlags)



-- | Given the install plan, produce a config value with constraints that
-- freezes the versions of packages used in the plan.
--
projectFreezeConfig :: ElaboratedInstallPlan -> ProjectConfig
projectFreezeConfig elaboratedPlan =
    Monoid.mempty {
      projectConfigShared = Monoid.mempty {
        projectConfigConstraints =
          concat (Map.elems (projectFreezeConstraints elaboratedPlan))
      }
    }

-- | Given the install plan, produce solver constraints that will ensure the
-- solver picks the same solution again in future in different environments.
--
projectFreezeConstraints :: ElaboratedInstallPlan
                         -> Map PackageName [(UserConstraint, ConstraintSource)]
projectFreezeConstraints plan =
    --
    -- TODO: [required eventually] this is currently an underapproximation
    -- since the constraints language is not expressive enough to specify the
    -- precise solution. See https://github.com/haskell/cabal/issues/3502.
    --
    -- For the moment we deal with multiple versions in the solution by using
    -- constraints that allow either version. Also, we do not include any
    -- constraints for packages that are local to the project (e.g. if the
    -- solution has two instances of Cabal, one from the local project and one
    -- pulled in as a setup deps then we exclude all constraints on Cabal, not
    -- just the constraint for the local instance since any constraint would
    -- apply to both instances).
    --
    Map.unionWith (++) versionConstraints flagConstraints
    `Map.difference` localPackages
  where
    versionConstraints :: Map PackageName [(UserConstraint, ConstraintSource)]
    versionConstraints =
      Map.mapWithKey
        (\p v -> [(UserConstraintVersion p v, ConstraintSourceFreeze)])
        versionRanges

    versionRanges :: Map PackageName VersionRange
    versionRanges =
      Map.fromListWith unionVersionRanges $
          [ (packageName pkg, thisVersion (packageVersion pkg))
          | InstallPlan.PreExisting pkg <- InstallPlan.toList plan
          ]
       ++ [ (packageName pkg, thisVersion (packageVersion pkg))
          | InstallPlan.Configured pkg <- InstallPlan.toList plan
          ]

    flagConstraints :: Map PackageName [(UserConstraint, ConstraintSource)]
    flagConstraints =
      Map.mapWithKey
        (\p f -> [(UserConstraintFlags p f, ConstraintSourceFreeze)])
        flagAssignments

    flagAssignments :: Map PackageName FlagAssignment
    flagAssignments =
      Map.fromList
        [ (pkgname, flags)
        | InstallPlan.Configured pkg <- InstallPlan.toList plan
        , let flags   = pkgFlagAssignment pkg
              pkgname = packageName pkg
        , not (null flags) ]

    localPackages :: Map PackageName ()
    localPackages =
      Map.fromList
        [ (packageName pkg, ())
        | InstallPlan.Configured pkg <- InstallPlan.toList plan
        , pkgLocalToProject pkg
        ]

