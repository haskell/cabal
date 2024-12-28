{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | cabal-install CLI command: repl
module Distribution.Client.CmdRepl
  ( -- * The @repl@ CLI and action
    replCommand
  , replAction
  , ReplFlags (..)

    -- * Internals exposed for testing
  , matchesMultipleProblem
  , selectPackageTargets
  , selectComponentTarget
  , MultiReplDecision (..)
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Compat.Lens
import qualified Distribution.Types.Lens as L

import Distribution.Client.CmdErrorMessages
  ( Plural (..)
  , componentKind
  , renderComponentKind
  , renderListCommaAnd
  , renderListSemiAnd
  , renderTargetProblem
  , renderTargetSelector
  , showTargetSelector
  , sortGroupOn
  , targetSelectorRefersToPkgs
  )
import Distribution.Client.DistDirLayout
  ( DistDirLayout (..)
  )
import Distribution.Client.Errors
import qualified Distribution.Client.InstallPlan as InstallPlan
import Distribution.Client.NixStyleOptions
  ( NixStyleFlags (..)
  , cfgVerbosity
  , defaultNixStyleFlags
  , nixStyleOptions
  )
import Distribution.Client.ProjectBuilding
  ( improveInstallPlanWithUpToDatePackages
  , rebuildTargetsDryRun
  )
import Distribution.Client.ProjectOrchestration
import Distribution.Client.ProjectPlanning
  ( ElaboratedInstallPlan
  , ElaboratedSharedConfig (..)
  )
import Distribution.Client.ProjectPlanning.Types
  ( elabOrderExeDependencies
  , showElaboratedInstallPlan
  )
import Distribution.Client.ScriptUtils
  ( AcceptNoTargets (..)
  , TargetContext (..)
  , fakeProjectSourcePackage
  , lSrcpkgDescription
  , updateContextAndWriteProjectFile
  , updateContextAndWriteProjectFile'
  , withContextAndSelectors
  )
import Distribution.Client.Setup
  ( ConfigFlags (..)
  , GlobalFlags
  )
import qualified Distribution.Client.Setup as Client
import Distribution.Client.TargetProblem
  ( TargetProblem (..)
  )
import Distribution.Client.Targets
  ( UserConstraint (..)
  , UserConstraintScope (..)
  )
import Distribution.Client.Types
  ( PackageSpecifier (..)
  , UnresolvedSourcePackage
  )
import Distribution.Compiler
  ( CompilerFlavor (GHC)
  )
import Distribution.Package
  ( Package (..)
  , UnitId
  , installedUnitId
  , mkPackageName
  , packageName
  )
import Distribution.Simple.Command
  ( CommandUI (..)
  , usageAlternatives
  )
import Distribution.Simple.Compiler
  ( Compiler
  , PackageDBX (..)
  , compilerCompatVersion
  )
import Distribution.Simple.Program.GHC
import Distribution.Simple.Setup
  ( Flag
  , ReplOptions (..)
  , commonSetupTempFileOptions
  )
import Distribution.Simple.Utils
  ( debugNoWrap
  , dieWithException
  , notice
  , withTempDirectoryEx
  , wrapText
  )
import Distribution.Solver.Types.ConstraintSource
  ( ConstraintSource (ConstraintSourceMultiRepl, ConstraintSourceWithRepl)
  )
import Distribution.Solver.Types.PackageConstraint
  ( PackageProperty (PackagePropertyVersion)
  )
import Distribution.Solver.Types.SourcePackage
  ( SourcePackage (..)
  )
import Distribution.Types.BuildInfo
  ( BuildInfo (..)
  , emptyBuildInfo
  )
import Distribution.Types.ComponentName
  ( componentNameString
  )
import Distribution.Types.CondTree
  ( CondTree (..)
  )
import Distribution.Types.Dependency
  ( Dependency (..)
  , mainLibSet
  )
import Distribution.Types.Library
  ( Library (..)
  , emptyLibrary
  )
import Distribution.Types.ParStrat
import Distribution.Types.Version
  ( Version
  , mkVersion
  )
import Distribution.Types.VersionRange
  ( anyVersion
  , orLaterVersion
  )
import Distribution.Utils.Generic
  ( safeHead
  )
import Distribution.Verbosity
  ( lessVerbose
  , normal
  )
import Language.Haskell.Extension
  ( Language (..)
  )

import Control.Monad (mapM)
import qualified Data.ByteString.Lazy as BS
import Data.List
  ( (\\)
  )
import qualified Data.Map as Map
import qualified Data.Set as Set
import Distribution.Client.ProjectConfig
  ( ProjectConfig (..)
  , ProjectConfigShared (..)
  )
import Distribution.Client.ReplFlags
  ( EnvFlags (envIncludeTransitive, envPackages)
  , ReplFlags (..)
  , defaultReplFlags
  , topReplOptions
  )
import Distribution.Compat.Binary (decode)
import Distribution.Simple.Flag (flagToMaybe, fromFlagOrDefault, pattern Flag)
import Distribution.Simple.Program.Builtin (ghcProgram)
import Distribution.Simple.Program.Db (requireProgram)
import Distribution.Simple.Program.Types
import Distribution.Types.PackageName.Magic (fakePackageId)
import System.Directory
  ( doesFileExist
  , getCurrentDirectory
  , listDirectory
  , makeAbsolute
  )
import System.FilePath
  ( searchPathSeparator
  , splitSearchPath
  , (</>)
  )
import Text.PrettyPrint hiding ((<>))

replCommand :: CommandUI (NixStyleFlags ReplFlags)
replCommand =
  Client.installCommand
    { commandName = "v2-repl"
    , commandSynopsis = "Open an interactive session for the given component."
    , commandUsage = usageAlternatives "v2-repl" ["[TARGET] [FLAGS]"]
    , commandDescription = Just $ \_ ->
        wrapText $
          "Open an interactive session for a component within the project. The "
            ++ "available targets are the same as for the 'v2-build' command: "
            ++ "individual components within packages in the project, including "
            ++ "libraries, executables, test-suites or benchmarks. Packages can "
            ++ "also be specified in which case the library component in the "
            ++ "package will be used, or the (first listed) executable in the "
            ++ "package if there is no library.\n\n"
            ++ "Dependencies are built or rebuilt as necessary. Additional "
            ++ "configuration flags can be specified on the command line and these "
            ++ "extend the project configuration from the 'cabal.project', "
            ++ "'cabal.project.local' and other files."
    , commandNotes = Just $ \pname ->
        "Examples, open an interactive session:\n"
          ++ "  "
          ++ pname
          ++ " v2-repl\n"
          ++ "    for the default component in the package in the current directory\n"
          ++ "  "
          ++ pname
          ++ " v2-repl pkgname\n"
          ++ "    for the default component in the package named 'pkgname'\n"
          ++ "  "
          ++ pname
          ++ " v2-repl ./pkgfoo\n"
          ++ "    for the default component in the package in the ./pkgfoo directory\n"
          ++ "  "
          ++ pname
          ++ " v2-repl cname\n"
          ++ "    for the component named 'cname'\n"
          ++ "  "
          ++ pname
          ++ " v2-repl pkgname:cname\n"
          ++ "    for the component 'cname' in the package 'pkgname'\n\n"
          ++ "  "
          ++ pname
          ++ " v2-repl --build-depends lens\n"
          ++ "    add the latest version of the library 'lens' to the default component "
          ++ "(or no componentif there is no project present)\n"
          ++ "  "
          ++ pname
          ++ " v2-repl --build-depends \"lens >= 4.15 && < 4.18\"\n"
          ++ "    add a version (constrained between 4.15 and 4.18) of the library 'lens' "
          ++ "to the default component (or no component if there is no project present)\n"
    , commandDefaultFlags = defaultNixStyleFlags defaultReplFlags
    , commandOptions = nixStyleOptions topReplOptions
    }

data MultiReplDecision = MultiReplDecision
  { compilerVersion :: Maybe Version
  , enabledByFlag :: Bool
  }
  deriving (Eq, Show)

useMultiRepl :: MultiReplDecision -> Bool
useMultiRepl MultiReplDecision{compilerVersion, enabledByFlag} =
  compilerVersion >= Just minMultipleHomeUnitsVersion && enabledByFlag

multiReplDecision :: ProjectConfigShared -> Compiler -> ReplFlags -> MultiReplDecision
multiReplDecision ctx compiler flags =
  MultiReplDecision
    -- Check if the compiler is new enough, need at least 9.4 to start a multi session
    (compilerCompatVersion GHC compiler)
    -- Then check the user actually asked for it, either via the project file, the global config or
    -- a repl specific option.
    (fromFlagOrDefault False (projectConfigMultiRepl ctx <> replUseMulti flags))

-- | An explicit target has been resolved to a context and selectors.
type ResolvedTarget = ((ProjectBaseContext, Bool), [TargetSelector])

-- | When an explicit target could not be resolved this is the retarget
-- suggestion and message.
type RetargetSuggestion = (String, String)

-- | Either an explicit target could be resolved or we need to retarget.
type TargetPick = Either RetargetSuggestion ResolvedTarget

-- | The @repl@ command is very much like @build@. It brings the install plan
-- up to date, selects that part of the plan needed by the given or implicit
-- repl target and then executes the plan.
--
-- Compared to @build@ the difference is that multiple targets are handled
-- specially and the target type is repl rather than build. The
-- general plan execution infrastructure handles both build and repl targets.
--
-- For more details on how this works, see the module
-- "Distribution.Client.ProjectOrchestration"
replAction :: NixStyleFlags ReplFlags -> [String] -> GlobalFlags -> IO ()
replAction flags@NixStyleFlags{extraFlags = ReplFlags{..}} targetStrings globalFlags = withCtx $
  \targetCtx ctx userTargetSelectors -> do
    when (buildSettingOnlyDeps (buildSettings ctx)) $
      dieWithException verbosity ReplCommandDoesn'tSupport
    let projectRoot = distProjectRootDirectory $ distDirLayout ctx

    targetPick :: TargetPick <- case targetCtx of
      ProjectContext -> do
        case userTargetSelectors of
          [] -> do
            -- If in the project context with no selectors then for a target we:
            -- 1. pick the only package in the project
            -- 2. pick 'all' if multiple packages and multi-repl is enabled
            --
            -- We could have picked 'all' even for a single package project with
            -- multi-repl but that is not as specific.
            let projectFile = projectConfigProjectFile . projectConfigShared $ projectConfig ctx
            let pkgs = projectPackages $ projectConfig ctx
            case pkgs of
              [pkg] -> return $ Left (pkg, retargetMsg ("package '" ++ pkg ++ "'"))
              _ ->
                if isMultiReplEnabled ctx
                  then return $ Left ("all", retargetMsg "'all'")
                  else
                    dieWithException verbosity $
                      RenderReplTargetProblem [render (reportProjectNoTarget projectFile pkgs)]
          _ -> return $ Right ((ctx, isMultiReplEnabled ctx), userTargetSelectors)
      -- In the global context, construct a fake package which can be used to start
      -- a repl with extra arguments if `-b` is given.
      GlobalContext -> do
        unless (null userTargetSelectors) $
          dieWithException verbosity $
            ReplTakesNoArguments targetStrings
        let
          sourcePackage =
            fakeProjectSourcePackage projectRoot
              & ( (lSrcpkgDescription . L.condLibrary)
                    ?~ (CondNode library [baseDep] [])
                )
          library = emptyLibrary{libBuildInfo = lBuildInfo}
          lBuildInfo =
            emptyBuildInfo
              { targetBuildDepends = [baseDep] ++ envPackages replEnvFlags
              , defaultLanguage = Just Haskell2010
              }
          baseDep = Dependency "base" anyVersion mainLibSet

        -- Write the fake package
        updatedCtx <- updateContextAndWriteProjectFile' ctx sourcePackage
        -- Specify the selector for this package
        let fakeSelector = [TargetPackage TargetExplicitNamed [fakePackageId] Nothing]
        return $ Right ((updatedCtx, isMultiReplEnabled updatedCtx), fakeSelector)

      -- For the script context, no special behaviour.
      ScriptContext scriptPath scriptExecutable -> do
        unless (length targetStrings == 1) $
          dieWithException verbosity $
            ReplTakesSingleArgument targetStrings
        existsScriptPath <- doesFileExist scriptPath
        unless existsScriptPath $
          dieWithException verbosity $
            ReplTakesSingleArgument targetStrings

        updatedCtx <- updateContextAndWriteProjectFile ctx scriptPath scriptExecutable
        return $ Right ((updatedCtx, isMultiReplEnabled updatedCtx), userTargetSelectors)

    either retargetRepl (targetedRepl flags ctx targetCtx) targetPick
  where
    retargetRepl :: RetargetSuggestion -> IO ()
    retargetRepl (newTarget, msg) = do
      notice verbosity msg
      replAction flags [newTarget] globalFlags

    withCtx =
      withContextAndSelectors
        verbosity
        (if null targetStrings then AcceptNoTargets else RejectNoTargets)
        (Just LibKind)
        flags
        targetStrings
        globalFlags
        ReplCommand

    verbosity = cfgVerbosity normal flags

    -- If multi-repl is used, we need a Cabal recent enough to handle it.  We
    -- need to do this before solving, but the compiler version is only known
    -- after solving (phaseConfigureCompiler), so instead of using
    -- multiReplDecision we just check the flag.
    isMultiReplEnabled :: ProjectBaseContext -> Bool
    isMultiReplEnabled ctx =
      fromFlagOrDefault False $
        projectConfigMultiRepl (projectConfigShared $ projectConfig ctx)
          <> replUseMulti

    retargetMsg :: String -> String
    retargetMsg newTarget = "No target specified, using " ++ newTarget ++ " as the target for the REPL."

-- | Bring up a REPL with the targets.  With resolved user selectors and
-- context, adjust the REPL behaviour for the target.
targetedRepl :: NixStyleFlags ReplFlags -> ProjectBaseContext -> TargetContext -> ResolvedTarget -> IO ()
targetedRepl
  flags@NixStyleFlags{extraFlags = replFlags@ReplFlags{..}, configFlags}
  ctx
  targetCtx
  ((baseCtx, multiReplEnabled), targetSelectors) = do
    let withReplEnabled =
          isJust $ flagToMaybe $ replWithRepl configureReplOptions

        addConstraintWhen cond constraint base_ctx =
          if cond
            then
              base_ctx
                & lProjectConfig . lProjectConfigShared . lProjectConfigConstraints
                  %~ (constraint :)
            else base_ctx

        -- This is the constraint setup.Cabal>=3.11. 3.11 is when Cabal options
        -- used for multi-repl were introduced.
        -- Idelly we'd apply this constraint only on the closure of repl targets,
        -- but that would require another solver run for marginal advantages that
        -- will further shrink as 3.11 is adopted.
        addMultiReplConstraint = addConstraintWhen multiReplEnabled $ requireCabal [3, 11] ConstraintSourceMultiRepl

        -- Similarly, if you use `--with-repl` then your version of `Cabal` needs to
        -- support the `--with-repl` flag.
        addWithReplConstraint = addConstraintWhen withReplEnabled $ requireCabal [3, 15] ConstraintSourceWithRepl

        baseCtx' = addMultiReplConstraint $ addWithReplConstraint baseCtx

    (originalComponent, baseCtx'') <-
      if null (envPackages replEnvFlags)
        then return (Nothing, baseCtx')
        else -- Unfortunately, the best way to do this is to let the normal solver
        -- help us resolve the targets, but that isn't ideal for performance,
        -- especially in the no-project case.
        withInstallPlan (lessVerbose verbosity) baseCtx' $ \elaboratedPlan sharedConfig -> do
          -- targets should be non-empty map, but there's no NonEmptyMap yet.
          targets <- validatedTargets' (projectConfigShared (projectConfig ctx)) (pkgConfigCompiler sharedConfig) elaboratedPlan targetSelectors

          let
            (unitId, _) = fromMaybe (error "panic: targets should be non-empty") $ safeHead $ Map.toList targets
            originalDeps = installedUnitId <$> InstallPlan.directDeps elaboratedPlan unitId
            oci = OriginalComponentInfo unitId originalDeps
            pkgId = fromMaybe (error $ "cannot find " ++ prettyShow unitId) $ packageId <$> InstallPlan.lookup elaboratedPlan unitId
            baseCtx'' = addDepsToProjectTarget (envPackages replEnvFlags) pkgId baseCtx'

          return (Just oci, baseCtx'')

    -- Now, we run the solver again with the added packages. While the graph
    -- won't actually reflect the addition of transitive dependencies,
    -- they're going to be available already and will be offered to the REPL
    -- and that's good enough.
    --
    -- In addition, to avoid a *third* trip through the solver, we are
    -- replicating the second half of 'runProjectPreBuildPhase' by hand
    -- here.
    (buildCtx, compiler, platform, replOpts', targets) <- withInstallPlan verbosity baseCtx'' $
      \elaboratedPlan elaboratedShared' -> do
        let ProjectBaseContext{..} = baseCtx''

        -- Recalculate with updated project.
        targets <- validatedTargets' (projectConfigShared projectConfig) (pkgConfigCompiler elaboratedShared') elaboratedPlan targetSelectors

        let
          elaboratedPlan' =
            -- Guard against pruning with empty targets and failing an assertion
            -- within pruneInstallPlanToTargets.
            if null targets
              then elaboratedPlan
              else
                pruneInstallPlanToTargets
                  TargetActionRepl
                  targets
                  elaboratedPlan
          includeTransitive = fromFlagOrDefault True (envIncludeTransitive replEnvFlags)

        pkgsBuildStatus <-
          rebuildTargetsDryRun
            distDirLayout
            elaboratedShared'
            elaboratedPlan'

        let elaboratedPlan'' =
              improveInstallPlanWithUpToDatePackages
                pkgsBuildStatus
                elaboratedPlan'
        debugNoWrap verbosity (showElaboratedInstallPlan elaboratedPlan'')

        let
          buildCtx =
            ProjectBuildContext
              { elaboratedPlanOriginal = elaboratedPlan
              , elaboratedPlanToExecute = elaboratedPlan''
              , elaboratedShared = elaboratedShared'
              , pkgsBuildStatus
              , targetsMap = targets
              }

          ElaboratedSharedConfig{pkgConfigCompiler = compiler, pkgConfigPlatform = platform} = elaboratedShared'

          repl_flags = case originalComponent of
            Just oci -> generateReplFlags includeTransitive elaboratedPlan' oci
            Nothing -> []

        return (buildCtx, compiler, platform, configureReplOptions & lReplOptionsFlags %~ (++ repl_flags), targets)

    -- Multi Repl implementation see: https://well-typed.com/blog/2023/03/cabal-multi-unit/ for
    -- a high-level overview about how everything fits together.
    if Set.size (distinctTargetComponents targets) > 1
      then withTempDirectoryEx verbosity tempFileOptions distDir "multi-out" $ \dir' -> do
        -- multi target repl
        dir <- makeAbsolute dir'
        -- Modify the replOptions so that the ./Setup repl command will write options
        -- into the multi-out directory.
        replOpts'' <- case targetCtx of
          ProjectContext -> return $ replOpts'{replOptionsFlagOutput = Flag dir}
          _ -> usingGhciScript compiler projectRoot replOpts'

        let buildCtx' = buildCtx & lElaboratedShared . lPkgConfigReplOptions .~ replOpts''
        printPlan verbosity baseCtx'' buildCtx'

        -- The project build phase will call `./Setup repl` but write the options
        -- out into a file without starting a repl.
        buildOutcomes <- runProjectBuildPhase verbosity baseCtx'' buildCtx'
        runProjectPostBuildPhase verbosity baseCtx'' buildCtx' buildOutcomes

        -- calculate PATH, we construct a PATH which is the union of all paths from
        -- the units which have been loaded. This is not quite right but usually works fine.
        path_files <- listDirectory (dir </> "paths")

        -- Note: decode is partial. Should we use Structured here?
        -- This might blow up with @build-type: Custom@ stuff.
        ghcProgs <- mapM (\f -> decode @ConfiguredProgram <$> BS.readFile (dir </> "paths" </> f)) path_files

        let all_paths = concatMap programOverrideEnv ghcProgs
        let sp = intercalate [searchPathSeparator] (map fst (sortBy (comparing @Int snd) $ Map.toList (combine_search_paths all_paths)))
        -- HACK: Just combine together all env overrides, placing the most common things last

        -- ghc program with overridden PATH
        (ghcProg, _) <- requireProgram verbosity ghcProgram (pkgConfigCompilerProgs (elaboratedShared buildCtx'))
        let ghcProg' = ghcProg{programOverrideEnv = [("PATH", Just sp)]}

        -- Find what the unit files are, and start a repl based on all the response
        -- files which have been created in the directory.
        -- unit files for components
        unit_files <- (filter (/= "paths")) <$> listDirectory dir

        -- Order the unit files so that the find target becomes the active unit
        let active_unit_fp :: Maybe FilePath
            active_unit_fp = do
              -- Get the first target selectors from the cli
              activeTarget <- safeHead targetSelectors
              -- Lookup the targets :: Map UnitId [(ComponentTarget, NonEmpty TargetSelector)]
              unitId <-
                Map.toList targets
                  -- Keep the UnitId matching the desired target selector
                  & find (\(_, xs) -> any (\(_, selectors) -> activeTarget `elem` selectors) xs)
                  & fmap fst
              -- Convert to filename (adapted from 'storePackageDirectory')
              pure (prettyShow unitId)
            unit_files_ordered :: [FilePath]
            unit_files_ordered =
              let (active_unit_files, other_units) = partition (\fp -> Just fp == active_unit_fp) unit_files
               in -- GHC considers the last unit passed to be the active one
                  other_units ++ active_unit_files

            convertParStrat :: ParStratX Int -> ParStratX String
            convertParStrat Serial = Serial
            convertParStrat (UseSem n) = NumJobs (Just n)
            convertParStrat (NumJobs mn) = NumJobs mn

        let ghc_opts =
              mempty
                { ghcOptMode = Flag GhcModeInteractive
                , ghcOptUnitFiles = map (dir </>) unit_files_ordered
                , ghcOptNumJobs = Flag (convertParStrat (buildSettingNumJobs (buildSettings ctx)))
                , ghcOptPackageDBs = [GlobalPackageDB]
                }

        -- run ghc --interactive with
        runReplProgram (flagToMaybe $ replWithRepl replOpts') tempFileOptions verbosity ghcProg' compiler platform Nothing ghc_opts
      else do
        -- single target repl
        replOpts'' <- case targetCtx of
          ProjectContext -> return replOpts'
          _ -> usingGhciScript compiler projectRoot replOpts'

        let buildCtx' = buildCtx & lElaboratedShared . lPkgConfigReplOptions .~ replOpts''
        printPlan verbosity baseCtx'' buildCtx'

        buildOutcomes <- runProjectBuildPhase verbosity baseCtx'' buildCtx'
        runProjectPostBuildPhase verbosity baseCtx'' buildCtx' buildOutcomes
    where
      projectRoot = distProjectRootDirectory $ distDirLayout ctx
      distDir = distDirectory $ distDirLayout ctx

      combine_search_paths paths =
        foldl' go Map.empty paths
        where
          go m ("PATH", Just s) = foldl' (\m' f -> Map.insertWith (+) f 1 m') m (splitSearchPath s)
          go m _ = m

      verbosity = cfgVerbosity normal flags
      tempFileOptions = commonSetupTempFileOptions $ configCommonFlags configFlags
      validatedTargets' = validatedTargets verbosity replFlags

-- | Create a constraint which requires a later version of Cabal.
-- This is used for commands which require a specific feature from the Cabal library
-- such as multi-repl or the --with-repl flag.
requireCabal :: [Int] -> ConstraintSource -> (UserConstraint, ConstraintSource)
requireCabal version source =
  ( UserConstraint
      (UserAnySetupQualifier (mkPackageName "Cabal"))
      (PackagePropertyVersion $ orLaterVersion $ mkVersion version)
  , source
  )

reportProjectNoTarget :: Flag FilePath -> [String] -> Doc
reportProjectNoTarget projectFile pkgs =
  case (null pkgs, projectName) of
    (True, Just project) ->
      text "There are no packages in"
        <+> (project <> char '.')
        <+> text "Please add a package to the project and"
        <+> pickComponent
    (True, Nothing) ->
      text "Please add a package to the project and" <+> pickComponent
    (False, Just project) ->
      text "Please"
        <+> pickComponent
        <+> text "The packages in"
        <+> project
        <+> (text "from which to select a component target are" <> colon)
        $+$ nest 1 (vcat [text "-" <+> text pkg | pkg <- sort pkgs])
    (False, Nothing) ->
      text "Please"
        <+> pickComponent
        <+> (text "The packages from which to select a component in 'cabal.project'" <> comma)
        <+> (text "the implicit default as if `--project-file=cabal.project` was added as a command option" <> comma)
        <+> (text "are" <> colon)
        $+$ nest 1 (vcat [text "-" <+> text pkg | pkg <- sort pkgs])
  where
    projectName = case projectFile of
      Flag "" -> Nothing
      Flag n -> Just $ quotes (text n)
      _ -> Nothing
    pickComponent = text "pick a single [package:][ctype:]component (or all) as target for the REPL command."

-- | Invariant: validatedTargets returns at least one target for the REPL.
validatedTargets
  :: Verbosity
  -> ReplFlags
  -> ProjectConfigShared
  -> Compiler
  -> ElaboratedInstallPlan
  -> [TargetSelector]
  -> IO TargetsMap
validatedTargets verbosity replFlags ctx compiler elaboratedPlan targetSelectors = do
  let multi_repl_enabled = multiReplDecision ctx compiler replFlags
  -- Interpret the targets on the command line as repl targets (as opposed to
  -- say build or haddock targets).
  targets <-
    either (reportTargetProblems verbosity) return $
      resolveTargetsFromSolver
        (selectPackageTargets multi_repl_enabled)
        selectComponentTarget
        elaboratedPlan
        Nothing
        targetSelectors

  -- Reject multiple targets, or at least targets in different components. It is
  -- ok to have two module/file targets in the same component, but not two that
  -- live in different components.
  when (Set.size (distinctTargetComponents targets) > 1 && not (useMultiRepl multi_repl_enabled)) $
    reportTargetProblems
      verbosity
      [multipleTargetsProblem multi_repl_enabled targets]

  return targets

-- | First version of GHC which supports multiple home packages
minMultipleHomeUnitsVersion :: Version
minMultipleHomeUnitsVersion = mkVersion [9, 4]

data OriginalComponentInfo = OriginalComponentInfo
  { ociUnitId :: UnitId
  , ociOriginalDeps :: [UnitId]
  }
  deriving (Show)

addDepsToProjectTarget
  :: [Dependency]
  -> PackageId
  -> ProjectBaseContext
  -> ProjectBaseContext
addDepsToProjectTarget deps pkgId ctx =
  (\p -> ctx{localPackages = p}) . fmap addDeps . localPackages $ ctx
  where
    addDeps
      :: PackageSpecifier UnresolvedSourcePackage
      -> PackageSpecifier UnresolvedSourcePackage
    addDeps (SpecificSourcePackage pkg)
      | packageId pkg /= pkgId = SpecificSourcePackage pkg
      | SourcePackage{..} <- pkg =
          SpecificSourcePackage $
            pkg
              { srcpkgDescription =
                  -- New dependencies are added to the original ones found in the
                  -- `targetBuildDepends` field.
                  -- `traverseBuildInfos` is used in order to update _all_ the
                  -- occurrences of the field `targetBuildDepends`. It ensures that
                  -- fields depending on the latter are also consistently updated.
                  srcpkgDescription
                    & (L.traverseBuildInfos . L.targetBuildDepends)
                      %~ (deps ++)
              }
    addDeps spec = spec

generateReplFlags :: Bool -> ElaboratedInstallPlan -> OriginalComponentInfo -> [String]
generateReplFlags includeTransitive elaboratedPlan OriginalComponentInfo{..} = flags
  where
    exeDeps :: [UnitId]
    exeDeps =
      foldMap
        (InstallPlan.foldPlanPackage (const []) elabOrderExeDependencies)
        (InstallPlan.dependencyClosure elaboratedPlan [ociUnitId])

    deps, deps', trans, trans' :: [UnitId]
    flags :: [String]
    deps = installedUnitId <$> InstallPlan.directDeps elaboratedPlan ociUnitId
    deps' = deps \\ ociOriginalDeps
    trans = installedUnitId <$> InstallPlan.dependencyClosure elaboratedPlan deps'
    trans' = trans \\ ociOriginalDeps
    flags =
      fmap (("-package-id " ++) . prettyShow) . (\\ exeDeps) $
        if includeTransitive then trans' else deps'

-- | Add repl options to ensure the repl actually starts in the current working directory.
--
-- In a global or script context, when we are using a fake package, @cabal repl@
-- starts in the fake package directory instead of the directory it was called from,
-- so we need to tell ghci to change back to the correct directory.
--
-- The @-ghci-script@ flag is path to the ghci script responsible for changing to the
-- correct directory. Only works on GHC >= 7.6, though. 🙁
usingGhciScript :: Compiler -> FilePath -> ReplOptions -> IO ReplOptions
usingGhciScript compiler projectRoot replOpts
  | compilerCompatVersion GHC compiler >= Just minGhciScriptVersion = do
      let ghciScriptPath = projectRoot </> "setcwd.ghci"
      cwd <- getCurrentDirectory
      writeFile ghciScriptPath (":cd " ++ cwd)
      return $ replOpts & lReplOptionsFlags %~ (("-ghci-script" ++ ghciScriptPath) :)
  | otherwise = return replOpts

-- | First version of GHC where GHCi supported the flag we need.
-- https://downloads.haskell.org/~ghc/7.6.1/docs/html/users_guide/release-7-6-1.html
minGhciScriptVersion :: Version
minGhciScriptVersion = mkVersion [7, 6]

-- | This defines what a 'TargetSelector' means for the @repl@ command.
-- It selects the 'AvailableTarget's that the 'TargetSelector' refers to,
-- or otherwise classifies the problem.
--
-- For repl we select:
--
-- * the library if there is only one and it's buildable; or
--
-- * the exe if there is only one and it's buildable; or
--
-- * any other buildable component.
--
-- Fail if there are no buildable lib\/exe components, or if there are
-- multiple libs or exes.
selectPackageTargets
  :: MultiReplDecision
  -> TargetSelector
  -> [AvailableTarget k]
  -> Either ReplTargetProblem [k]
selectPackageTargets multiple_targets_allowed =
  -- If explicitly enabled, then select the targets like we would for multi-repl but
  -- might still fail later because of compiler version.
  if enabledByFlag multiple_targets_allowed
    then selectPackageTargetsMulti
    else selectPackageTargetsSingle multiple_targets_allowed

selectPackageTargetsMulti
  :: TargetSelector
  -> [AvailableTarget k]
  -> Either ReplTargetProblem [k]
selectPackageTargetsMulti targetSelector targets
  | not (null targetsBuildable) =
      Right targetsBuildable
  -- If there are no targets at all then we report that
  | otherwise =
      Left (TargetProblemNoTargets targetSelector)
  where
    ( targetsBuildable
      , _
      ) =
        selectBuildableTargetsWith'
          (isRequested targetSelector)
          targets

    -- When there's a target filter like "pkg:tests" then we do select tests,
    -- but if it's just a target like "pkg" then we don't build tests unless
    -- they are requested by default (i.e. by using --enable-tests)
    isRequested (TargetAllPackages Nothing) TargetNotRequestedByDefault = False
    isRequested (TargetPackage _ _ Nothing) TargetNotRequestedByDefault = False
    isRequested _ _ = True

-- | Target selection behaviour which only select a single target.
-- This is used when the compiler version doesn't support multi-repl or the user
-- didn't request it.
selectPackageTargetsSingle
  :: MultiReplDecision
  -> TargetSelector
  -> [AvailableTarget k]
  -> Either ReplTargetProblem [k]
selectPackageTargetsSingle decision targetSelector targets
  -- If there is exactly one buildable library then we select that
  | [target] <- targetsLibsBuildable =
      Right [target]
  -- but fail if there are multiple buildable libraries.
  | not (null targetsLibsBuildable) =
      Left (matchesMultipleProblem decision targetSelector targetsLibsBuildable')
  -- If there is exactly one buildable executable then we select that
  | [target] <- targetsExesBuildable =
      Right [target]
  -- but fail if there are multiple buildable executables.
  | not (null targetsExesBuildable) =
      Left (matchesMultipleProblem decision targetSelector targetsExesBuildable')
  -- If there is exactly one other target then we select that
  | [target] <- targetsBuildable =
      Right [target]
  -- but fail if there are multiple such targets
  | not (null targetsBuildable) =
      Left (matchesMultipleProblem decision targetSelector targetsBuildable')
  -- If there are targets but none are buildable then we report those
  | not (null targets) =
      Left (TargetProblemNoneEnabled targetSelector targets')
  -- If there are no targets at all then we report that
  | otherwise =
      Left (TargetProblemNoTargets targetSelector)
  where
    targets' = forgetTargetsDetail targets
    ( targetsLibsBuildable
      , targetsLibsBuildable'
      ) =
        selectBuildableTargets'
          . filterTargetsKind LibKind
          $ targets
    ( targetsExesBuildable
      , targetsExesBuildable'
      ) =
        selectBuildableTargets'
          . filterTargetsKind ExeKind
          $ targets
    ( targetsBuildable
      , targetsBuildable'
      ) =
        selectBuildableTargetsWith'
          (isRequested targetSelector)
          targets

    -- When there's a target filter like "pkg:tests" then we do select tests,
    -- but if it's just a target like "pkg" then we don't build tests unless
    -- they are requested by default (i.e. by using --enable-tests)
    isRequested (TargetAllPackages Nothing) TargetNotRequestedByDefault = False
    isRequested (TargetPackage _ _ Nothing) TargetNotRequestedByDefault = False
    isRequested _ _ = True

-- | For a 'TargetComponent' 'TargetSelector', check if the component can be
-- selected.
--
-- For the @repl@ command we just need the basic checks on being buildable etc.
selectComponentTarget
  :: SubComponentTarget
  -> AvailableTarget k
  -> Either ReplTargetProblem k
selectComponentTarget = selectComponentTargetBasic

data ReplProblem
  = TargetProblemMatchesMultiple MultiReplDecision TargetSelector [AvailableTarget ()]
  | -- | Multiple 'TargetSelector's match multiple targets
    TargetProblemMultipleTargets MultiReplDecision TargetsMap
  deriving (Eq, Show)

-- | The various error conditions that can occur when matching a
-- 'TargetSelector' against 'AvailableTarget's for the @repl@ command.
type ReplTargetProblem = TargetProblem ReplProblem

matchesMultipleProblem
  :: MultiReplDecision
  -> TargetSelector
  -> [AvailableTarget ()]
  -> ReplTargetProblem
matchesMultipleProblem decision targetSelector targetsExesBuildable =
  CustomTargetProblem $ TargetProblemMatchesMultiple decision targetSelector targetsExesBuildable

multipleTargetsProblem
  :: MultiReplDecision
  -> TargetsMap
  -> ReplTargetProblem
multipleTargetsProblem decision = CustomTargetProblem . TargetProblemMultipleTargets decision

reportTargetProblems :: Verbosity -> [TargetProblem ReplProblem] -> IO a
reportTargetProblems verbosity =
  dieWithException verbosity . RenderReplTargetProblem . map renderReplTargetProblem

renderReplTargetProblem :: TargetProblem ReplProblem -> String
renderReplTargetProblem = renderTargetProblem "open a repl for" renderReplProblem

renderReplProblem :: ReplProblem -> String
renderReplProblem (TargetProblemMatchesMultiple decision targetSelector targets) =
  "Cannot open a repl for multiple components at once. The target '"
    ++ showTargetSelector targetSelector
    ++ "' refers to "
    ++ renderTargetSelector targetSelector
    ++ " which "
    ++ (if targetSelectorRefersToPkgs targetSelector then "includes " else "are ")
    ++ renderListSemiAnd
      [ "the "
        ++ renderComponentKind Plural ckind
        ++ " "
        ++ renderListCommaAnd
          [ maybe (prettyShow pkgname) prettyShow (componentNameString cname)
          | t <- ts
          , let cname = availableTargetComponentName t
                pkgname = packageName (availableTargetPackageId t)
          ]
      | (ckind, ts) <- sortGroupOn availableTargetComponentKind targets
      ]
    ++ ".\n\n"
    ++ explainMultiReplDecision decision
  where
    availableTargetComponentKind =
      componentKind
        . availableTargetComponentName
renderReplProblem (TargetProblemMultipleTargets multi_decision selectorMap) =
  "Cannot open a repl for multiple components at once. The targets "
    ++ renderListCommaAnd
      [ "'" ++ showTargetSelector ts ++ "'"
      | ts <- uniqueTargetSelectors selectorMap
      ]
    ++ " refer to different components."
    ++ ".\n\n"
    ++ explainMultiReplDecision multi_decision

explainMultiReplDecision :: MultiReplDecision -> [Char]
explainMultiReplDecision MultiReplDecision{compilerVersion, enabledByFlag} =
  case (compilerVersion >= Just minMultipleHomeUnitsVersion, enabledByFlag) of
    -- Compiler not new enough, and not requested anyway.
    (False, False) -> explanationSingleComponentLimitation compilerVersion
    -- Compiler too old, but was requested
    (False, True) -> "Multiple component session requested but compiler version is too old.\n" ++ explanationSingleComponentLimitation compilerVersion
    -- Compiler new enough, but not requested
    (True, False) -> explanationNeedToEnableFlag
    _ -> error "explainMultiReplDecision"

explanationNeedToEnableFlag :: String
explanationNeedToEnableFlag =
  "Your compiler supports a multiple component repl but support is not enabled.\n"
    ++ "The experimental multi repl can be enabled by\n"
    ++ "  * Globally: Setting multi-repl: True in your .cabal/config\n"
    ++ "  * Project Wide: Setting multi-repl: True in your cabal.project file\n"
    ++ "  * Per Invocation: By passing --enable-multi-repl when starting the repl"

explanationSingleComponentLimitation :: Maybe Version -> String
explanationSingleComponentLimitation version =
  "The reason for this limitation is that your version "
    ++ versionString
    ++ "of ghci does not "
    ++ "support loading multiple components as source. Load just one component "
    ++ "and when you make changes to a dependent component then quit and reload.\n"
    ++ prettyShow minMultipleHomeUnitsVersion
    ++ " is needed to support multiple component sessions."
  where
    versionString = case version of
      Nothing -> ""
      Just ver -> "(" ++ prettyShow ver ++ ") "

-- Lenses
lElaboratedShared :: Lens' ProjectBuildContext ElaboratedSharedConfig
lElaboratedShared f s = fmap (\x -> s{elaboratedShared = x}) (f (elaboratedShared s))
{-# INLINE lElaboratedShared #-}

lPkgConfigReplOptions :: Lens' ElaboratedSharedConfig ReplOptions
lPkgConfigReplOptions f s = fmap (\x -> s{pkgConfigReplOptions = x}) (f (pkgConfigReplOptions s))
{-# INLINE lPkgConfigReplOptions #-}

lReplOptionsFlags :: Lens' ReplOptions [String]
lReplOptionsFlags f s = fmap (\x -> s{replOptionsFlags = x}) (f (replOptionsFlags s))
{-# INLINE lReplOptionsFlags #-}

lProjectConfig :: Lens' ProjectBaseContext ProjectConfig
lProjectConfig f s = fmap (\x -> s{projectConfig = x}) (f (projectConfig s))
{-# INLINE lProjectConfig #-}

lProjectConfigShared :: Lens' ProjectConfig ProjectConfigShared
lProjectConfigShared f s = fmap (\x -> s{projectConfigShared = x}) (f (projectConfigShared s))
{-# INLINE lProjectConfigShared #-}

lProjectConfigConstraints :: Lens' ProjectConfigShared [(UserConstraint, ConstraintSource)]
lProjectConfigConstraints f s = fmap (\x -> s{projectConfigConstraints = x}) (f (projectConfigConstraints s))
{-# INLINE lProjectConfigConstraints #-}
