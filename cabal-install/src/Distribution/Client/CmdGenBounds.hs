{-# LANGUAGE NamedFieldPuns #-}

module Distribution.Client.CmdGenBounds
  ( genBounds
  , genBoundsCommand
  , genBoundsAction
  , GenBoundsFlags (..)
  , defaultGenBoundsFlags
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import qualified Data.Map as Map

import Control.Monad (mapM_)

import Distribution.Client.Errors

import Distribution.Client.ProjectPlanning hiding (pruneInstallPlanToTargets)
import Distribution.Client.ProjectPlanning.Types
import Distribution.Client.Types.ConfiguredId (confInstId)
import Distribution.Client.Utils hiding (pvpize)
import Distribution.InstalledPackageInfo (InstalledPackageInfo, installedComponentId)
import Distribution.Package
import Distribution.PackageDescription
import Distribution.Simple.Utils
import Distribution.Version

import Distribution.Client.Setup (GlobalFlags (..))

-- Project orchestration imports

import Distribution.Client.CmdErrorMessages
import Distribution.Client.GenBounds
import qualified Distribution.Client.InstallPlan as InstallPlan
import Distribution.Client.NixStyleOptions
import Distribution.Client.ProjectFlags
import Distribution.Client.ProjectOrchestration
import Distribution.Client.ScriptUtils
import Distribution.Client.TargetProblem
import Distribution.Simple.Command
import Distribution.Types.Component
import Distribution.Verbosity

-- | The data type for gen-bounds command flags
data GenBoundsFlags = GenBoundsFlags {}

-- | Default values for the gen-bounds flags
defaultGenBoundsFlags :: GenBoundsFlags
defaultGenBoundsFlags = GenBoundsFlags{}

-- | The @gen-bounds@ command definition
genBoundsCommand :: CommandUI (NixStyleFlags GenBoundsFlags)
genBoundsCommand =
  CommandUI
    { commandName = "v2-gen-bounds"
    , commandSynopsis = "Generate dependency bounds for packages in the project."
    , commandUsage = usageAlternatives "v2-gen-bounds" ["[TARGETS] [FLAGS]"]
    , commandDescription = Just $ \_ ->
        "Generate PVP-compliant dependency bounds for packages in the project."
    , commandNotes = Just $ \pname ->
        "Examples:\n"
          ++ "  "
          ++ pname
          ++ " v2-gen-bounds\n"
          ++ "    Generate bounds for the package in the current directory "
          ++ "or all packages in the project\n"
          ++ "  "
          ++ pname
          ++ " v2-gen-bounds pkgname\n"
          ++ "    Generate bounds for the package named pkgname in the project\n"
          ++ "  "
          ++ pname
          ++ " v2-gen-bounds ./pkgfoo\n"
          ++ "    Generate bounds for the package in the ./pkgfoo directory\n"
    , commandDefaultFlags = defaultNixStyleFlags defaultGenBoundsFlags
    , commandOptions =
        removeIgnoreProjectOption
          . nixStyleOptions (const [])
    }

-- | The action for the @gen-bounds@ command when used in a project context.
genBoundsAction :: NixStyleFlags GenBoundsFlags -> [String] -> GlobalFlags -> IO ()
genBoundsAction flags targetStrings globalFlags =
  withContextAndSelectors verbosity RejectNoTargets Nothing flags targetStrings globalFlags OtherCommand $ \targetCtx ctx targetSelectors -> do
    baseCtx <- case targetCtx of
      ProjectContext -> return ctx
      GlobalContext -> return ctx
      ScriptContext path _ ->
        dieWithException verbosity $
          GenBoundsDoesNotSupportScript path

    let ProjectBaseContext{distDirLayout, cabalDirLayout, projectConfig, localPackages} = baseCtx

    -- Step 1: Create the install plan for the project.
    (_, elaboratedPlan, _, _, _) <-
      rebuildInstallPlan
        verbosity
        distDirLayout
        cabalDirLayout
        projectConfig
        localPackages
        Nothing

    -- Step 2: Resolve the targets for the gen-bounds command.
    targets <-
      either (reportGenBoundsTargetProblems verbosity) return $
        resolveTargetsFromSolver
          selectPackageTargets
          selectComponentTarget
          elaboratedPlan
          Nothing
          targetSelectors

    -- Step 3: Prune the install plan to the targets.
    let elaboratedPlan' =
          pruneInstallPlanToTargets
            TargetActionBuild
            targets
            elaboratedPlan

    let
      -- Step 4a: Find the local packages from the install plan. These are the
      -- candidates for which we will generate bounds.
      localPkgs :: [ElaboratedConfiguredPackage]
      localPkgs = mapMaybe (InstallPlan.foldPlanPackage (const Nothing) (\p -> Just p)) (InstallPlan.toList elaboratedPlan')

      -- Step 4b: Extract which versions we chose for each package from the pruned install plan.
      pkgVersionMap :: Map.Map ComponentId PackageIdentifier
      pkgVersionMap = Map.fromList (map (InstallPlan.foldPlanPackage externalVersion localVersion) (InstallPlan.toList elaboratedPlan'))

      externalVersion :: InstalledPackageInfo -> (ComponentId, PackageIdentifier)
      externalVersion pkg = (installedComponentId pkg, packageId pkg)

      localVersion :: ElaboratedConfiguredPackage -> (ComponentId, PackageIdentifier)
      localVersion pkg = (elabComponentId pkg, packageId pkg)

    let genBoundsActionForPkg :: ElaboratedConfiguredPackage -> [GenBoundsResult]
        genBoundsActionForPkg pkg =
          -- Step 5: Match up the user specified targets with the local packages.
          case Map.lookup (installedUnitId pkg) targets of
            Nothing -> []
            Just tgts ->
              map (\(tgt, _) -> getBoundsForComponent tgt pkg pkgVersionMap) tgts

    -- Process each package to find the ones needing bounds
    let boundsActions = concatMap genBoundsActionForPkg localPkgs

    if (any isBoundsNeeded boundsActions)
      then do
        notice verbosity boundsNeededMsg
        mapM_ (renderBoundsResult verbosity) boundsActions
      else notice verbosity "All bounds up-to-date"
  where
    verbosity = cfgVerbosity normal flags

data GenBoundsResult = GenBoundsResult PackageIdentifier ComponentTarget (Maybe [PackageIdentifier])

isBoundsNeeded :: GenBoundsResult -> Bool
isBoundsNeeded (GenBoundsResult _ _ Nothing) = False
isBoundsNeeded _ = True

renderBoundsResult :: Verbosity -> GenBoundsResult -> IO ()
renderBoundsResult verbosity (GenBoundsResult pid tgt bounds) =
  case bounds of
    Nothing ->
      notice
        verbosity
        ("Congratulations, all dependencies for " ++ prettyShow (packageName pid) ++ ":" ++ showComponentTarget pid tgt ++ " have upper bounds!")
    Just pkgBounds -> do
      notice verbosity $
        "For component " ++ prettyShow (pkgName pid) ++ ":" ++ showComponentTarget pid tgt ++ ":"
      let padTo = maximum $ map (length . unPackageName . packageName) pkgBounds
      traverse_ (notice verbosity . (++ ",") . showBounds padTo) pkgBounds

-- | Process a single BuildInfo to identify and report missing upper bounds
getBoundsForComponent
  :: ComponentTarget
  -> ElaboratedConfiguredPackage
  -> Map.Map ComponentId PackageIdentifier
  -> GenBoundsResult
getBoundsForComponent tgt pkg pkgVersionMap =
  if null needBounds
    then boundsResult Nothing
    else -- All the things we depend on.

      let componentDeps = elabLibDependencies pkg
          -- Match these up to package names, this is a list of Package name to versions.
          -- Now just match that up with what the user wrote in the build-depends section.
          depsWithVersions = mapMaybe (\cid -> Map.lookup (confInstId $ fst cid) pkgVersionMap) componentDeps
          isNeeded = hasElem needBounds . packageName
       in boundsResult (Just (filter isNeeded depsWithVersions))
  where
    pd = elabPkgDescription pkg
    -- Extract the build-depends for the right part of the cabal file.
    bi = buildInfoForTarget pd tgt

    -- We need to generate bounds if
    -- \* the dependency does not have an upper bound
    -- \* the dependency is not the same package as the one we are processing
    boundFilter dep =
      (not (hasUpperBound (depVerRange dep)))
        && packageName pd /= depPkgName dep

    -- The dependencies that need bounds.
    needBounds = map depPkgName $ filter boundFilter $ targetBuildDepends bi

    boundsResult = GenBoundsResult (packageId pkg) tgt

buildInfoForTarget :: PackageDescription -> ComponentTarget -> BuildInfo
buildInfoForTarget pd (ComponentTarget cname _) = componentBuildInfo $ getComponent pd cname

-- | This defines what a 'TargetSelector' means for the @gen-bounds@ command.
-- Copy of selectPackageTargets from CmdBuild.hs
selectPackageTargets
  :: TargetSelector
  -> [AvailableTarget k]
  -> Either TargetProblem' [k]
selectPackageTargets targetSelector targets
  -- If there are any buildable targets then we select those
  | not (null targetsBuildable) =
      Right targetsBuildable
  -- If there are targets but none are buildable then we report those
  | not (null targets) =
      Left (TargetProblemNoneEnabled targetSelector targets')
  -- If there are no targets at all then we report that
  | otherwise =
      Left (TargetProblemNoTargets targetSelector)
  where
    targets' = forgetTargetsDetail targets
    targetsBuildable =
      selectBuildableTargetsWith
        (buildable targetSelector)
        targets

    -- When there's a target filter like "pkg:tests" then we do select tests,
    -- but if it's just a target like "pkg" then we don't build tests unless
    -- they are requested by default (i.e. by using --enable-tests)
    buildable (TargetPackage _ _ Nothing) TargetNotRequestedByDefault = False
    buildable (TargetAllPackages Nothing) TargetNotRequestedByDefault = False
    buildable _ _ = True

-- | For a 'TargetComponent' 'TargetSelector', check if the component can be
-- selected. Copy of selectComponentTarget from CmdBuild.hs
selectComponentTarget
  :: SubComponentTarget
  -> AvailableTarget k
  -> Either TargetProblem' k
selectComponentTarget = selectComponentTargetBasic

-- | Report target problems for gen-bounds command
reportGenBoundsTargetProblems :: Verbosity -> [TargetProblem'] -> IO a
reportGenBoundsTargetProblems verbosity problems =
  reportTargetProblems verbosity "gen-bounds" problems
