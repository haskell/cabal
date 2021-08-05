{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

-- | cabal-install CLI command: repl
--
module Distribution.Client.CmdRepl (
    -- * The @repl@ CLI and action
    replCommand,
    replAction,

    -- * Internals exposed for testing
    matchesMultipleProblem,
    selectPackageTargets,
    selectComponentTarget
  ) where

import Prelude ()
import Distribution.Client.Compat.Prelude

import Distribution.Compat.Lens
import qualified Distribution.Types.Lens as L

import Distribution.Client.NixStyleOptions
         ( NixStyleFlags (..), nixStyleOptions, defaultNixStyleFlags )
import Distribution.Client.CmdErrorMessages
         ( renderTargetSelector, showTargetSelector,
           renderTargetProblem,
           targetSelectorRefersToPkgs,
           renderComponentKind, renderListCommaAnd, renderListSemiAnd,
           componentKind, sortGroupOn, Plural(..) )
import Distribution.Client.TargetProblem
         ( TargetProblem(..) )
import qualified Distribution.Client.InstallPlan as InstallPlan
import Distribution.Client.ProjectBuilding
         ( rebuildTargetsDryRun, improveInstallPlanWithUpToDatePackages )
import Distribution.Client.ProjectConfig
         ( ProjectConfig(..), withProjectOrGlobalConfig
         , projectConfigConfigFile )
import Distribution.Client.ProjectFlags
         ( flagIgnoreProject )
import Distribution.Client.ProjectOrchestration
import Distribution.Client.ProjectPlanning
       ( ElaboratedSharedConfig(..), ElaboratedInstallPlan )
import Distribution.Client.ProjectPlanning.Types
       ( elabOrderExeDependencies )
import Distribution.Client.Setup
         ( GlobalFlags, ConfigFlags(..) )
import qualified Distribution.Client.Setup as Client
import Distribution.Client.Types
         ( PackageLocation(..), PackageSpecifier(..), UnresolvedSourcePackage )
import Distribution.Simple.Setup
         ( fromFlagOrDefault, replOptions
         , Flag(..), toFlag, falseArg )
import Distribution.Simple.Command
         ( CommandUI(..), liftOptionL, usageAlternatives, option
         , ShowOrParseArgs, OptionField, reqArg )
import Distribution.Compiler
         ( CompilerFlavor(GHC) )
import Distribution.Simple.Compiler
         ( compilerCompatVersion )
import Distribution.Package
         ( Package(..), packageName, UnitId, installedUnitId )
import Distribution.PackageDescription.PrettyPrint
import Distribution.Parsec
         ( parsecCommaList )
import Distribution.ReadE
         ( ReadE, parsecToReadE )
import qualified Distribution.SPDX.License as SPDX
import Distribution.Solver.Types.SourcePackage
         ( SourcePackage(..) )
import Distribution.Types.BuildInfo
         ( BuildInfo(..), emptyBuildInfo )
import Distribution.Types.ComponentName
         ( componentNameString )
import Distribution.Types.CondTree
         ( CondTree(..), traverseCondTreeC )
import Distribution.Types.Dependency
         ( Dependency(..), mainLibSet )
import Distribution.Types.GenericPackageDescription
         ( emptyGenericPackageDescription )
import Distribution.Types.PackageDescription
         ( PackageDescription(..), emptyPackageDescription )
import Distribution.Types.PackageName.Magic
         ( fakePackageId )
import Distribution.Types.Library
         ( Library(..), emptyLibrary )
import Distribution.Types.Version
         ( mkVersion )
import Distribution.Types.VersionRange
         ( anyVersion )
import Distribution.Utils.Generic
         ( safeHead )
import Distribution.Verbosity
         ( normal, lessVerbose )
import Distribution.Simple.Utils
         ( wrapText, die', debugNoWrap, ordNub, createTempDirectory, handleDoesNotExist )
import Language.Haskell.Extension
         ( Language(..) )
import Distribution.CabalSpecVersion
         ( CabalSpecVersion (..) )

import Data.List
         ( (\\) )
import qualified Data.Map as Map
import qualified Data.Set as Set
import System.Directory
         ( getCurrentDirectory, getTemporaryDirectory, removeDirectoryRecursive )
import System.FilePath
         ( (</>) )

type ReplFlags = [String]

data EnvFlags = EnvFlags
  { envPackages :: [Dependency]
  , envIncludeTransitive :: Flag Bool
  }

defaultEnvFlags :: EnvFlags
defaultEnvFlags = EnvFlags
  { envPackages = []
  , envIncludeTransitive = toFlag True
  }

envOptions :: ShowOrParseArgs -> [OptionField EnvFlags]
envOptions _ =
  [ option ['b'] ["build-depends"]
    "Include additional packages in the environment presented to GHCi."
    envPackages (\p flags -> flags { envPackages = p ++ envPackages flags })
    (reqArg "DEPENDENCIES" dependenciesReadE (fmap prettyShow :: [Dependency] -> [String]))
  , option [] ["no-transitive-deps"]
    "Don't automatically include transitive dependencies of requested packages."
    envIncludeTransitive (\p flags -> flags { envIncludeTransitive = p })
    falseArg
  ]
  where
    dependenciesReadE :: ReadE [Dependency]
    dependenciesReadE =
      parsecToReadE
        ("couldn't parse dependencies: " ++)
        (parsecCommaList parsec)

replCommand :: CommandUI (NixStyleFlags (ReplFlags, EnvFlags))
replCommand = Client.installCommand {
  commandName         = "v2-repl",
  commandSynopsis     = "Open an interactive session for the given component.",
  commandUsage        = usageAlternatives "v2-repl" [ "[TARGET] [FLAGS]" ],
  commandDescription  = Just $ \_ -> wrapText $
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
     ++ "'cabal.project.local' and other files.",
  commandNotes        = Just $ \pname ->
        "Examples, open an interactive session:\n"
     ++ "  " ++ pname ++ " v2-repl\n"
     ++ "    for the default component in the package in the current directory\n"
     ++ "  " ++ pname ++ " v2-repl pkgname\n"
     ++ "    for the default component in the package named 'pkgname'\n"
     ++ "  " ++ pname ++ " v2-repl ./pkgfoo\n"
     ++ "    for the default component in the package in the ./pkgfoo directory\n"
     ++ "  " ++ pname ++ " v2-repl cname\n"
     ++ "    for the component named 'cname'\n"
     ++ "  " ++ pname ++ " v2-repl pkgname:cname\n"
     ++ "    for the component 'cname' in the package 'pkgname'\n\n"
     ++ "  " ++ pname ++ " v2-repl --build-depends lens\n"
     ++ "    add the latest version of the library 'lens' to the default component "
        ++ "(or no componentif there is no project present)\n"
     ++ "  " ++ pname ++ " v2-repl --build-depends \"lens >= 4.15 && < 4.18\"\n"
     ++ "    add a version (constrained between 4.15 and 4.18) of the library 'lens' "
        ++ "to the default component (or no component if there is no project present)\n",

  commandDefaultFlags = defaultNixStyleFlags ([], defaultEnvFlags),
  commandOptions = nixStyleOptions $ \showOrParseArgs ->
    map (liftOptionL _1) (replOptions showOrParseArgs) ++
    map (liftOptionL _2) (envOptions showOrParseArgs)
  }

-- | The @repl@ command is very much like @build@. It brings the install plan
-- up to date, selects that part of the plan needed by the given or implicit
-- repl target and then executes the plan.
--
-- Compared to @build@ the difference is that only one target is allowed
-- (given or implicit) and the target type is repl rather than build. The
-- general plan execution infrastructure handles both build and repl targets.
--
-- For more details on how this works, see the module
-- "Distribution.Client.ProjectOrchestration"
--
replAction :: NixStyleFlags (ReplFlags, EnvFlags) -> [String] -> GlobalFlags -> IO ()
replAction flags@NixStyleFlags { extraFlags = (replFlags, envFlags), ..} targetStrings globalFlags = do
    let
      with           = withProject    cliConfig             verbosity targetStrings
      without config = withoutProject (config <> cliConfig) verbosity targetStrings

    (baseCtx, targetSelectors, finalizer, replType) <-
      withProjectOrGlobalConfig verbosity ignoreProject globalConfigFlag with without

    when (buildSettingOnlyDeps (buildSettings baseCtx)) $
      die' verbosity $ "The repl command does not support '--only-dependencies'. "
          ++ "You may wish to use 'build --only-dependencies' and then "
          ++ "use 'repl'."

    (originalComponent, baseCtx') <- if null (envPackages envFlags)
      then return (Nothing, baseCtx)
      else
        -- Unfortunately, the best way to do this is to let the normal solver
        -- help us resolve the targets, but that isn't ideal for performance,
        -- especially in the no-project case.
        withInstallPlan (lessVerbose verbosity) baseCtx $ \elaboratedPlan _ -> do
          -- targets should be non-empty map, but there's no NonEmptyMap yet.
          targets <- validatedTargets elaboratedPlan targetSelectors

          let
            (unitId, _) = fromMaybe (error "panic: targets should be non-empty") $ safeHead $ Map.toList targets
            originalDeps = installedUnitId <$> InstallPlan.directDeps elaboratedPlan unitId
            oci = OriginalComponentInfo unitId originalDeps
            pkgId = fromMaybe (error $ "cannot find " ++ prettyShow unitId) $ packageId <$> InstallPlan.lookup elaboratedPlan unitId
            baseCtx' = addDepsToProjectTarget (envPackages envFlags) pkgId baseCtx

          return (Just oci, baseCtx')

    -- Now, we run the solver again with the added packages. While the graph
    -- won't actually reflect the addition of transitive dependencies,
    -- they're going to be available already and will be offered to the REPL
    -- and that's good enough.
    --
    -- In addition, to avoid a *third* trip through the solver, we are
    -- replicating the second half of 'runProjectPreBuildPhase' by hand
    -- here.
    (buildCtx, replFlags'') <- withInstallPlan verbosity baseCtx' $
      \elaboratedPlan elaboratedShared' -> do
        let ProjectBaseContext{..} = baseCtx'

        -- Recalculate with updated project.
        targets <- validatedTargets elaboratedPlan targetSelectors

        let
          elaboratedPlan' = pruneInstallPlanToTargets
                              TargetActionRepl
                              targets
                              elaboratedPlan
          includeTransitive = fromFlagOrDefault True (envIncludeTransitive envFlags)

        pkgsBuildStatus <- rebuildTargetsDryRun distDirLayout elaboratedShared'
                                          elaboratedPlan'

        let elaboratedPlan'' = improveInstallPlanWithUpToDatePackages
                                pkgsBuildStatus elaboratedPlan'
        debugNoWrap verbosity (InstallPlan.showInstallPlan elaboratedPlan'')

        let
          buildCtx = ProjectBuildContext
            { elaboratedPlanOriginal = elaboratedPlan
            , elaboratedPlanToExecute = elaboratedPlan''
            , elaboratedShared = elaboratedShared'
            , pkgsBuildStatus
            , targetsMap = targets
            }

          ElaboratedSharedConfig { pkgConfigCompiler = compiler } = elaboratedShared'

          -- First version of GHC where GHCi supported the flag we need.
          -- https://downloads.haskell.org/~ghc/7.6.1/docs/html/users_guide/release-7-6-1.html
          minGhciScriptVersion = mkVersion [7, 6]

          replFlags' = case originalComponent of
            Just oci -> generateReplFlags includeTransitive elaboratedPlan' oci
            Nothing  -> []
          replFlags'' = case replType of
            GlobalRepl scriptPath
              | Just version <- compilerCompatVersion GHC compiler
              , version >= minGhciScriptVersion -> ("-ghci-script" ++ scriptPath) : replFlags'
            _                                   -> replFlags'

        return (buildCtx, replFlags'')

    let buildCtx' = buildCtx
          { elaboratedShared = (elaboratedShared buildCtx)
                { pkgConfigReplOptions = replFlags ++ replFlags'' }
          }
    printPlan verbosity baseCtx' buildCtx'

    buildOutcomes <- runProjectBuildPhase verbosity baseCtx' buildCtx'
    runProjectPostBuildPhase verbosity baseCtx' buildCtx' buildOutcomes
    finalizer
  where
    verbosity = fromFlagOrDefault normal (configVerbosity configFlags)
    ignoreProject = flagIgnoreProject projectFlags
    cliConfig = commandLineFlagsToProjectConfig globalFlags flags mempty -- ClientInstallFlags, not needed here
    globalConfigFlag = projectConfigConfigFile (projectConfigShared cliConfig)

    validatedTargets elaboratedPlan targetSelectors = do
      -- Interpret the targets on the command line as repl targets
      -- (as opposed to say build or haddock targets).
      targets <- either (reportTargetProblems verbosity) return
          $ resolveTargets
              selectPackageTargets
              selectComponentTarget
              elaboratedPlan
              Nothing
              targetSelectors

      -- Reject multiple targets, or at least targets in different
      -- components. It is ok to have two module/file targets in the
      -- same component, but not two that live in different components.
      when (Set.size (distinctTargetComponents targets) > 1) $
        reportTargetProblems verbosity
          [multipleTargetsProblem targets]

      return targets

data OriginalComponentInfo = OriginalComponentInfo
  { ociUnitId :: UnitId
  , ociOriginalDeps :: [UnitId]
  }
  deriving (Show)

-- | Tracks what type of GHCi instance we're creating.
data ReplType = ProjectRepl
              | GlobalRepl FilePath -- ^ The 'FilePath' argument is path to a GHCi
                                    --   script responsible for changing to the
                                    --   correct directory. Only works on GHC geq
                                    --   7.6, though. 🙁
              deriving (Show, Eq)

withProject :: ProjectConfig -> Verbosity -> [String]
            -> IO (ProjectBaseContext, [TargetSelector], IO (), ReplType)
withProject cliConfig verbosity targetStrings = do
  baseCtx <- establishProjectBaseContext verbosity cliConfig OtherCommand

  targetSelectors <- either (reportTargetSelectorProblems verbosity) return
                 =<< readTargetSelectors (localPackages baseCtx) (Just LibKind) targetStrings

  return (baseCtx, targetSelectors, return (), ProjectRepl)

withoutProject :: ProjectConfig -> Verbosity -> [String]
               -> IO (ProjectBaseContext, [TargetSelector], IO (), ReplType)
withoutProject config verbosity extraArgs = do
  unless (null extraArgs) $
    die' verbosity $ "'repl' doesn't take any extra arguments when outside a project: " ++ unwords extraArgs

  globalTmp <- getTemporaryDirectory
  tempDir <- createTempDirectory globalTmp "cabal-repl."

  -- We need to create a dummy package that lives in our dummy project.
  let
    sourcePackage = SourcePackage
      { srcpkgPackageId     = pkgId
      , srcpkgDescription   = genericPackageDescription
      , srcpkgSource        = LocalUnpackedPackage tempDir
      , srcpkgDescrOverride = Nothing
      }
    genericPackageDescription = emptyGenericPackageDescription
      & L.packageDescription .~ packageDescription
      & L.condLibrary        .~ Just (CondNode library [baseDep] [])
    packageDescription = emptyPackageDescription
      { package = pkgId
      , specVersion = CabalSpecV2_2
      , licenseRaw = Left SPDX.NONE
      }
    library = emptyLibrary { libBuildInfo = buildInfo }
    buildInfo = emptyBuildInfo
      { targetBuildDepends = [baseDep]
      , defaultLanguage = Just Haskell2010
      }
    baseDep = Dependency "base" anyVersion mainLibSet
    pkgId = fakePackageId

  writeGenericPackageDescription (tempDir </> "fake-package.cabal") genericPackageDescription

  let ghciScriptPath = tempDir </> "setcwd.ghci"
  cwd <- getCurrentDirectory
  writeFile ghciScriptPath (":cd " ++ cwd)

  distDirLayout <- establishDummyDistDirLayout verbosity config tempDir
  baseCtx <-
    establishDummyProjectBaseContext
      verbosity
      config
      distDirLayout
      [SpecificSourcePackage sourcePackage]
      OtherCommand

  let
    targetSelectors = [TargetPackage TargetExplicitNamed [pkgId] Nothing]
    finalizer = handleDoesNotExist () (removeDirectoryRecursive tempDir)

  return (baseCtx, targetSelectors, finalizer, GlobalRepl ghciScriptPath)

addDepsToProjectTarget :: [Dependency]
                       -> PackageId
                       -> ProjectBaseContext
                       -> ProjectBaseContext
addDepsToProjectTarget deps pkgId ctx =
    (\p -> ctx { localPackages = p }) . fmap addDeps . localPackages $ ctx
  where
    addDeps :: PackageSpecifier UnresolvedSourcePackage
            -> PackageSpecifier UnresolvedSourcePackage
    addDeps (SpecificSourcePackage pkg)
      | packageId pkg /= pkgId = SpecificSourcePackage pkg
      | SourcePackage{..} <- pkg =
        SpecificSourcePackage $ pkg { srcpkgDescription =
          srcpkgDescription & (\f -> L.allCondTrees $ traverseCondTreeC f)
                            %~ (deps ++)
        }
    addDeps spec = spec

generateReplFlags :: Bool -> ElaboratedInstallPlan -> OriginalComponentInfo -> ReplFlags
generateReplFlags includeTransitive elaboratedPlan OriginalComponentInfo{..} = flags
  where
    exeDeps :: [UnitId]
    exeDeps =
      foldMap
        (InstallPlan.foldPlanPackage (const []) elabOrderExeDependencies)
        (InstallPlan.dependencyClosure elaboratedPlan [ociUnitId])

    deps, deps', trans, trans' :: [UnitId]
    flags :: ReplFlags
    deps   = installedUnitId <$> InstallPlan.directDeps elaboratedPlan ociUnitId
    deps'  = deps \\ ociOriginalDeps
    trans  = installedUnitId <$> InstallPlan.dependencyClosure elaboratedPlan deps'
    trans' = trans \\ ociOriginalDeps
    flags  = fmap (("-package-id " ++) . prettyShow) . (\\ exeDeps)
      $ if includeTransitive then trans' else deps'

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
--
selectPackageTargets  :: TargetSelector
                      -> [AvailableTarget k] -> Either ReplTargetProblem [k]
selectPackageTargets targetSelector targets

    -- If there is exactly one buildable library then we select that
  | [target] <- targetsLibsBuildable
  = Right [target]

    -- but fail if there are multiple buildable libraries.
  | not (null targetsLibsBuildable)
  = Left (matchesMultipleProblem targetSelector targetsLibsBuildable')

    -- If there is exactly one buildable executable then we select that
  | [target] <- targetsExesBuildable
  = Right [target]

    -- but fail if there are multiple buildable executables.
  | not (null targetsExesBuildable)
  = Left (matchesMultipleProblem targetSelector targetsExesBuildable')

    -- If there is exactly one other target then we select that
  | [target] <- targetsBuildable
  = Right [target]

    -- but fail if there are multiple such targets
  | not (null targetsBuildable)
  = Left (matchesMultipleProblem targetSelector targetsBuildable')

    -- If there are targets but none are buildable then we report those
  | not (null targets)
  = Left (TargetProblemNoneEnabled targetSelector targets')

    -- If there are no targets at all then we report that
  | otherwise
  = Left (TargetProblemNoTargets targetSelector)
  where
    targets'                = forgetTargetsDetail targets
    (targetsLibsBuildable,
     targetsLibsBuildable') = selectBuildableTargets'
                            . filterTargetsKind LibKind
                            $ targets
    (targetsExesBuildable,
     targetsExesBuildable') = selectBuildableTargets'
                            . filterTargetsKind ExeKind
                            $ targets
    (targetsBuildable,
     targetsBuildable')     = selectBuildableTargetsWith'
                                (isRequested targetSelector) targets

    -- When there's a target filter like "pkg:tests" then we do select tests,
    -- but if it's just a target like "pkg" then we don't build tests unless
    -- they are requested by default (i.e. by using --enable-tests)
    isRequested (TargetAllPackages  Nothing) TargetNotRequestedByDefault = False
    isRequested (TargetPackage _ _  Nothing) TargetNotRequestedByDefault = False
    isRequested _ _ = True


-- | For a 'TargetComponent' 'TargetSelector', check if the component can be
-- selected.
--
-- For the @repl@ command we just need the basic checks on being buildable etc.
--
selectComponentTarget :: SubComponentTarget
                      -> AvailableTarget k -> Either ReplTargetProblem k
selectComponentTarget = selectComponentTargetBasic


data ReplProblem
  = TargetProblemMatchesMultiple TargetSelector [AvailableTarget ()]

    -- | Multiple 'TargetSelector's match multiple targets
  | TargetProblemMultipleTargets TargetsMap
  deriving (Eq, Show)

-- | The various error conditions that can occur when matching a
-- 'TargetSelector' against 'AvailableTarget's for the @repl@ command.
--
type ReplTargetProblem = TargetProblem ReplProblem

matchesMultipleProblem
  :: TargetSelector
  -> [AvailableTarget ()]
  -> ReplTargetProblem
matchesMultipleProblem targetSelector targetsExesBuildable =
  CustomTargetProblem $ TargetProblemMatchesMultiple targetSelector targetsExesBuildable

multipleTargetsProblem
  :: TargetsMap
  -> ReplTargetProblem
multipleTargetsProblem = CustomTargetProblem . TargetProblemMultipleTargets

reportTargetProblems :: Verbosity -> [TargetProblem ReplProblem] -> IO a
reportTargetProblems verbosity =
    die' verbosity . unlines . map renderReplTargetProblem

renderReplTargetProblem :: TargetProblem ReplProblem -> String
renderReplTargetProblem = renderTargetProblem "open a repl for" renderReplProblem

renderReplProblem :: ReplProblem -> String
renderReplProblem (TargetProblemMatchesMultiple targetSelector targets) =
    "Cannot open a repl for multiple components at once. The target '"
 ++ showTargetSelector targetSelector ++ "' refers to "
 ++ renderTargetSelector targetSelector ++ " which "
 ++ (if targetSelectorRefersToPkgs targetSelector then "includes " else "are ")
 ++ renderListSemiAnd
      [ "the " ++ renderComponentKind Plural ckind ++ " " ++
        renderListCommaAnd
          [ maybe (prettyShow pkgname) prettyShow (componentNameString cname)
          | t <- ts
          , let cname   = availableTargetComponentName t
                pkgname = packageName (availableTargetPackageId t)
          ]
      | (ckind, ts) <- sortGroupOn availableTargetComponentKind targets
      ]
 ++ ".\n\n" ++ explanationSingleComponentLimitation
  where
    availableTargetComponentKind = componentKind
                                 . availableTargetComponentName

renderReplProblem (TargetProblemMultipleTargets selectorMap) =
    "Cannot open a repl for multiple components at once. The targets "
 ++ renderListCommaAnd
      [ "'" ++ showTargetSelector ts ++ "'"
      | ts <- ordNub (concatMap snd (concat (Map.elems selectorMap))) ]
 ++ " refer to different components."
 ++ ".\n\n" ++ explanationSingleComponentLimitation

explanationSingleComponentLimitation :: String
explanationSingleComponentLimitation =
    "The reason for this limitation is that current versions of ghci do not "
 ++ "support loading multiple components as source. Load just one component "
 ++ "and when you make changes to a dependent component then quit and reload."
