{-# LANGUAGE CPP, NamedFieldPuns, RecordWildCards #-}

-- | cabal-install CLI command: freeze
--
module Distribution.Client.CmdFreeze (
    freezeCommand,
    freezeAction,
  ) where

import Distribution.Client.ProjectPlanning
import Distribution.Client.ProjectConfig
         ( ProjectConfig(..), ProjectConfigShared(..)
         , commandLineFlagsToProjectConfig, writeProjectLocalFreezeConfig
         , findProjectRoot, getProjectFileName )
import Distribution.Client.Targets
         ( UserQualifier(..), UserConstraintScope(..), UserConstraint(..) )
import Distribution.Solver.Types.PackageConstraint
         ( PackageProperty(..) )
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
         ( VersionRange, thisVersion
         , unionVersionRanges, simplifyVersionRange )
import Distribution.PackageDescription
         ( FlagAssignment )
import Distribution.Client.Setup
         ( GlobalFlags, ConfigFlags(..), ConfigExFlags, InstallFlags )
import Distribution.Simple.Setup
         ( HaddockFlags, fromFlagOrDefault )
import Distribution.Simple.Utils
         ( die', notice )
import Distribution.Verbosity
         ( normal )

import Data.Monoid as Monoid
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Monad (unless)
import System.FilePath

import Distribution.Simple.Command
         ( CommandUI(..), usageAlternatives )
import Distribution.Simple.Utils
         ( wrapText )
import qualified Distribution.Client.Setup as Client


freezeCommand :: CommandUI (ConfigFlags, ConfigExFlags, InstallFlags, HaddockFlags)
freezeCommand = Client.installCommand {
  commandName         = "new-freeze",
  commandSynopsis     = "Freezes a Nix-local build project",
  commandUsage        = usageAlternatives "new-freeze" [ "[FLAGS]" ],
  commandDescription  = Just $ \_ -> wrapText $
        "Performs dependency solving on a Nix-local build project, and"
     ++ " then writes out the precise dependency configuration to cabal.project.freeze"
     ++ " (or $project_file.freeze if --project-file is specified)"
     ++ " so that the plan is always used in subsequent builds.",
  commandNotes        = Just $ \pname ->
        "Examples:\n"
     ++ "  " ++ pname ++ " new-freeze          "
     ++ "    Freeze the configuration of the current project\n"
   }

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
      die' verbosity $ "'freeze' doesn't take any extra arguments: "
         ++ unwords extraArgs

    cabalDir <- defaultCabalDir
    let cabalDirLayout = defaultCabalDirLayout cabalDir

    projectRootDir <- findProjectRoot installFlags
    let distDirLayout = defaultDistDirLayout configFlags projectRootDir

    let cliConfig = commandLineFlagsToProjectConfig
                      globalFlags configFlags configExFlags
                      installFlags haddockFlags


    (_, elaboratedPlan, _, _) <-
      rebuildInstallPlan verbosity installFlags
                         projectRootDir distDirLayout cabalDirLayout
                         cliConfig

    let freezeConfig = projectFreezeConfig elaboratedPlan
    writeProjectLocalFreezeConfig installFlags projectRootDir freezeConfig
    notice verbosity $
      "Wrote freeze file: " ++ projectRootDir </> getProjectFileName installFlags  <.> "freeze"

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
    -- /version/ constraints for packages that are local to the project (e.g.
    -- if the solution has two instances of Cabal, one from the local project
    -- and one pulled in as a setup deps then we exclude all constraints on
    -- Cabal, not just the constraint for the local instance since any
    -- constraint would apply to both instances). We do however keep flag
    -- constraints of local packages.
    --
    deleteLocalPackagesVersionConstraints
      (Map.unionWith (++) versionConstraints flagConstraints)
  where
    versionConstraints :: Map PackageName [(UserConstraint, ConstraintSource)]
    versionConstraints =
      Map.mapWithKey
        (\p v -> [(UserConstraint (UserQualified UserQualToplevel p) (PackagePropertyVersion v),
                   ConstraintSourceFreeze)])
        versionRanges

    versionRanges :: Map PackageName VersionRange
    versionRanges =
      Map.map simplifyVersionRange $
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
        (\p f -> [(UserConstraint (UserQualified UserQualToplevel p) (PackagePropertyFlags f),
                   ConstraintSourceFreeze)])
        flagAssignments

    flagAssignments :: Map PackageName FlagAssignment
    flagAssignments =
      Map.fromList
        [ (pkgname, flags)
        | InstallPlan.Configured elab <- InstallPlan.toList plan
        , let flags   = elabFlagAssignment elab
              pkgname = packageName elab
        , not (null flags) ]

    -- As described above, remove the version constraints on local packages,
    -- but leave any flag constraints.
    deleteLocalPackagesVersionConstraints
      :: Map PackageName [(UserConstraint, ConstraintSource)]
      -> Map PackageName [(UserConstraint, ConstraintSource)]
    deleteLocalPackagesVersionConstraints =
#if MIN_VERSION_containers(0,5,0)
      Map.mergeWithKey
        (\_pkgname () constraints ->
            case filter (not . isVersionConstraint . fst) constraints of
              []           -> Nothing
              constraints' -> Just constraints')
        (const Map.empty) id
        localPackages
#else
      Map.mapMaybeWithKey
        (\pkgname constraints ->
            if pkgname `Map.member` localPackages
              then case filter (not . isVersionConstraint . fst) constraints of
                     []           -> Nothing
                     constraints' -> Just constraints'
              else Just constraints)
#endif

    isVersionConstraint (UserConstraint _ (PackagePropertyVersion _)) = True
    isVersionConstraint _                                             = False

    localPackages :: Map PackageName ()
    localPackages =
      Map.fromList
        [ (packageName elab, ())
        | InstallPlan.Configured elab <- InstallPlan.toList plan
        , elabLocalToProject elab
        ]

