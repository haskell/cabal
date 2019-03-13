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
    TargetProblem(..),
    selectPackageTargets,
    selectComponentTarget
  ) where

import Prelude ()
import Distribution.Client.Compat.Prelude

import Distribution.Compat.Lens
import qualified Distribution.Types.Lens as L

import Distribution.Client.CmdErrorMessages
import Distribution.Client.CmdInstall
         ( establishDummyProjectBaseContext )
import qualified Distribution.Client.InstallPlan as InstallPlan
import Distribution.Client.ProjectBuilding
         ( rebuildTargetsDryRun, improveInstallPlanWithUpToDatePackages )
import Distribution.Client.ProjectConfig
         ( ProjectConfig(..), withProjectOrGlobalConfig
         , projectConfigConfigFile, readGlobalConfig )
import Distribution.Client.ProjectOrchestration
import Distribution.Client.ProjectPlanning 
       ( ElaboratedSharedConfig(..), ElaboratedInstallPlan )
import Distribution.Client.ProjectPlanning.Types
       ( elabOrderExeDependencies )
import Distribution.Client.RebuildMonad
         ( runRebuild )
import Distribution.Client.Setup
         ( GlobalFlags, ConfigFlags(..), ConfigExFlags, InstallFlags )
import qualified Distribution.Client.Setup as Client
import Distribution.Client.Types
         ( PackageLocation(..), PackageSpecifier(..), UnresolvedSourcePackage )
import Distribution.Simple.Setup
         ( HaddockFlags, fromFlagOrDefault, replOptions
         , Flag(..), toFlag, trueArg, falseArg )
import Distribution.Simple.Command
         ( CommandUI(..), liftOption, usageAlternatives, option
         , ShowOrParseArgs, OptionField, reqArg )
import Distribution.Package
         ( Package(..), packageName, UnitId, installedUnitId )
import Distribution.PackageDescription.PrettyPrint
import Distribution.Parsec.Class
         ( Parsec(..) )
import Distribution.Pretty
         ( prettyShow )
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
         ( Dependency(..) )
import Distribution.Types.GenericPackageDescription
         ( emptyGenericPackageDescription )
import Distribution.Types.PackageDescription
         ( PackageDescription(..), emptyPackageDescription )
import Distribution.Types.Library
         ( Library(..), emptyLibrary )
import Distribution.Types.PackageId
         ( PackageIdentifier(..) )
import Distribution.Types.Version
         ( mkVersion, version0 )
import Distribution.Types.VersionRange
         ( anyVersion )
import Distribution.Text
         ( display )
import Distribution.Verbosity
         ( Verbosity, normal, lessVerbose )
import Distribution.Simple.Utils
         ( wrapText, die', debugNoWrap, ordNub, createTempDirectory, handleDoesNotExist )
import Language.Haskell.Extension
         ( Language(..) )

import Data.List
         ( (\\) )
import qualified Data.Map as Map
import qualified Data.Set as Set
import System.Directory
         ( getTemporaryDirectory, removeDirectoryRecursive )
import System.FilePath
         ( (</>) )

type ReplFlags = [String]

data EnvFlags = EnvFlags 
  { envPackages :: [Dependency]
  , envIncludeTransitive :: Flag Bool
  , envIgnoreProject :: Flag Bool
  }

defaultEnvFlags :: EnvFlags
defaultEnvFlags = EnvFlags
  { envPackages = []
  , envIncludeTransitive = toFlag True
  , envIgnoreProject = toFlag False
  }

envOptions :: ShowOrParseArgs -> [OptionField EnvFlags]
envOptions _ =
  [ option ['b'] ["build-depends"]
    "Include an additional package in the environment presented to GHCi."
    envPackages (\p flags -> flags { envPackages = p ++ envPackages flags })
    (reqArg "DEPENDENCY" dependencyReadE (fmap prettyShow :: [Dependency] -> [String]))
  , option [] ["no-transitive-deps"]
    "Don't automatically include transitive dependencies of requested packages."
    envIncludeTransitive (\p flags -> flags { envIncludeTransitive = p })
    falseArg
  , option ['z'] ["ignore-project"]
    "Only include explicitly specified packages (and 'base')."
    envIgnoreProject (\p flags -> flags { envIgnoreProject = p })
    trueArg
  ]
  where
    dependencyReadE :: ReadE [Dependency]
    dependencyReadE =
      fmap pure $
        parsecToReadE
          ("couldn't parse dependency: " ++)
          parsec

replCommand :: CommandUI (ConfigFlags, ConfigExFlags, InstallFlags, HaddockFlags, ReplFlags, EnvFlags)
replCommand = Client.installCommand {
  commandName         = "new-repl",
  commandSynopsis     = "Open an interactive session for the given component.",
  commandUsage        = usageAlternatives "new-repl" [ "[TARGET] [FLAGS]" ],
  commandDescription  = Just $ \_ -> wrapText $
        "Open an interactive session for a component within the project. The "
     ++ "available targets are the same as for the 'new-build' command: "
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
     ++ "  " ++ pname ++ " new-repl\n"
     ++ "    for the default component in the package in the current directory\n"
     ++ "  " ++ pname ++ " new-repl pkgname\n"
     ++ "    for the default component in the package named 'pkgname'\n"
     ++ "  " ++ pname ++ " new-repl ./pkgfoo\n"
     ++ "    for the default component in the package in the ./pkgfoo directory\n"
     ++ "  " ++ pname ++ " new-repl cname\n"
     ++ "    for the component named 'cname'\n"
     ++ "  " ++ pname ++ " new-repl pkgname:cname\n"
     ++ "    for the component 'cname' in the package 'pkgname'\n\n"
     ++ "  " ++ pname ++ " new-repl --build-depends lens\n"
     ++ "    add the latest version of the library 'lens' to the default component "
        ++ "(or no componentif there is no project present)\n"
     ++ "  " ++ pname ++ " new-repl --build-depends \"lens >= 4.15 && < 4.18\"\n"
     ++ "    add a version (constrained between 4.15 and 4.18) of the library 'lens' "
        ++ "to the default component (or no component if there is no project present)\n"

     ++ cmdCommonHelpTextNewBuildBeta,
  commandDefaultFlags = (configFlags,configExFlags,installFlags,haddockFlags,[],defaultEnvFlags),
  commandOptions = \showOrParseArgs ->
        map liftOriginal (commandOptions Client.installCommand showOrParseArgs)
        ++ map liftReplOpts (replOptions showOrParseArgs)
        ++ map liftEnvOpts  (envOptions  showOrParseArgs)
   }
  where
    (configFlags,configExFlags,installFlags,haddockFlags) = commandDefaultFlags Client.installCommand

    liftOriginal = liftOption projectOriginal updateOriginal
    liftReplOpts = liftOption projectReplOpts updateReplOpts
    liftEnvOpts  = liftOption projectEnvOpts  updateEnvOpts

    projectOriginal          (a,b,c,d,_,_) = (a,b,c,d)
    updateOriginal (a,b,c,d) (_,_,_,_,e,f) = (a,b,c,d,e,f)

    projectReplOpts  (_,_,_,_,e,_) = e
    updateReplOpts e (a,b,c,d,_,f) = (a,b,c,d,e,f)

    projectEnvOpts  (_,_,_,_,_,f) = f
    updateEnvOpts f (a,b,c,d,e,_) = (a,b,c,d,e,f)

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
replAction :: (ConfigFlags, ConfigExFlags, InstallFlags, HaddockFlags, ReplFlags, EnvFlags)
           -> [String] -> GlobalFlags -> IO ()
replAction (configFlags, configExFlags, installFlags, haddockFlags, replFlags, envFlags)
           targetStrings globalFlags = do
    let
      ignoreProject = fromFlagOrDefault False (envIgnoreProject envFlags)
      with           = withProject    cliConfig             verbosity targetStrings
      without config = withoutProject (config <> cliConfig) verbosity targetStrings
    
    (baseCtx, targetSelectors, finalizer) <- if ignoreProject
      then do
        globalConfig <- runRebuild "" $ readGlobalConfig verbosity globalConfigFlag
        without globalConfig
      else withProjectOrGlobalConfig verbosity globalConfigFlag with without

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
          targets <- validatedTargets elaboratedPlan targetSelectors
          
          let
            (unitId, _) = head $ Map.toList targets
            originalDeps = installedUnitId <$> InstallPlan.directDeps elaboratedPlan unitId
            oci = OriginalComponentInfo unitId originalDeps
            Just pkgId = packageId <$> InstallPlan.lookup elaboratedPlan unitId 
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
    (buildCtx, replFlags') <- withInstallPlan verbosity baseCtx' $ 
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
          replFlags' = case originalComponent of 
            Just oci -> generateReplFlags includeTransitive elaboratedPlan' oci
            Nothing  -> []
        
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
        return (buildCtx, replFlags')

    let buildCtx' = buildCtx
          { elaboratedShared = (elaboratedShared buildCtx)
                { pkgConfigReplOptions = replFlags ++ replFlags' }
          }
    printPlan verbosity baseCtx' buildCtx'

    buildOutcomes <- runProjectBuildPhase verbosity baseCtx' buildCtx'
    runProjectPostBuildPhase verbosity baseCtx' buildCtx' buildOutcomes
    finalizer
  where
    verbosity = fromFlagOrDefault normal (configVerbosity configFlags)
    cliConfig = commandLineFlagsToProjectConfig
                  globalFlags configFlags configExFlags
                  installFlags
                  mempty -- ClientInstallFlags, not needed here
                  haddockFlags
    globalConfigFlag = projectConfigConfigFile (projectConfigShared cliConfig)
    
    validatedTargets elaboratedPlan targetSelectors = do
      -- Interpret the targets on the command line as repl targets
      -- (as opposed to say build or haddock targets).
      targets <- either (reportTargetProblems verbosity) return
          $ resolveTargets
              selectPackageTargets
              selectComponentTarget
              TargetProblemCommon
              elaboratedPlan
              Nothing
              targetSelectors

      -- Reject multiple targets, or at least targets in different
      -- components. It is ok to have two module/file targets in the
      -- same component, but not two that live in different components.
      when (Set.size (distinctTargetComponents targets) > 1) $
        reportTargetProblems verbosity
          [TargetProblemMultipleTargets targets]

      return targets

data OriginalComponentInfo = OriginalComponentInfo
  { ociUnitId :: UnitId
  , ociOriginalDeps :: [UnitId]
  }
  deriving (Show)

withProject :: ProjectConfig -> Verbosity -> [String] -> IO (ProjectBaseContext, [TargetSelector], IO ())
withProject cliConfig verbosity targetStrings = do
  baseCtx <- establishProjectBaseContext verbosity cliConfig

  targetSelectors <- either (reportTargetSelectorProblems verbosity) return
                 =<< readTargetSelectors (localPackages baseCtx) (Just LibKind) targetStrings

  return (baseCtx, targetSelectors, return ())

withoutProject :: ProjectConfig -> Verbosity -> [String]  -> IO (ProjectBaseContext, [TargetSelector], IO ())
withoutProject config verbosity extraArgs = do
  unless (null extraArgs) $
    die' verbosity $ "'repl' doesn't take any extra arguments when outside a project: " ++ unwords extraArgs

  globalTmp <- getTemporaryDirectory
  tempDir <- createTempDirectory globalTmp "cabal-repl."

  -- We need to create a dummy package that lives in our dummy project.
  let
    sourcePackage = SourcePackage
      { packageInfoId        = pkgId
      , packageDescription   = genericPackageDescription
      , packageSource        = LocalUnpackedPackage tempDir
      , packageDescrOverride = Nothing
      }
    genericPackageDescription = emptyGenericPackageDescription 
      & L.packageDescription .~ packageDescription
      & L.condLibrary        .~ Just (CondNode library [baseDep] [])
    packageDescription = emptyPackageDescription
      { package = pkgId
      , specVersionRaw = Left (mkVersion [2, 2])
      , licenseRaw = Left SPDX.NONE
      }
    library = emptyLibrary { libBuildInfo = buildInfo }
    buildInfo = emptyBuildInfo
      { targetBuildDepends = [baseDep]
      , defaultLanguage = Just Haskell2010
      }
    baseDep = Dependency "base" anyVersion
    pkgId = PackageIdentifier "fake-package" version0

  writeGenericPackageDescription (tempDir </> "fake-package.cabal") genericPackageDescription
  
  baseCtx <- 
    establishDummyProjectBaseContext
      verbosity
      config
      tempDir
      [SpecificSourcePackage sourcePackage]

  let
    targetSelectors = [TargetPackage TargetExplicitNamed [pkgId] Nothing]
    finalizer = handleDoesNotExist () (removeDirectoryRecursive tempDir)

  return (baseCtx, targetSelectors, finalizer)

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
        SpecificSourcePackage $ pkg { packageDescription = 
          packageDescription & (\f -> L.allCondTrees $ traverseCondTreeC f)
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
                      -> [AvailableTarget k] -> Either TargetProblem [k]
selectPackageTargets targetSelector targets

    -- If there is exactly one buildable library then we select that
  | [target] <- targetsLibsBuildable
  = Right [target]

    -- but fail if there are multiple buildable libraries.
  | not (null targetsLibsBuildable)
  = Left (TargetProblemMatchesMultiple targetSelector targetsLibsBuildable')

    -- If there is exactly one buildable executable then we select that
  | [target] <- targetsExesBuildable
  = Right [target]

    -- but fail if there are multiple buildable executables.
  | not (null targetsExesBuildable)
  = Left (TargetProblemMatchesMultiple targetSelector targetsExesBuildable')

    -- If there is exactly one other target then we select that
  | [target] <- targetsBuildable
  = Right [target]

    -- but fail if there are multiple such targets
  | not (null targetsBuildable)
  = Left (TargetProblemMatchesMultiple targetSelector targetsBuildable')

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
                      -> AvailableTarget k -> Either TargetProblem k
selectComponentTarget subtarget =
    either (Left . TargetProblemCommon) Right
  . selectComponentTargetBasic subtarget


-- | The various error conditions that can occur when matching a
-- 'TargetSelector' against 'AvailableTarget's for the @repl@ command.
--
data TargetProblem =
     TargetProblemCommon       TargetProblemCommon

     -- | The 'TargetSelector' matches targets but none are buildable
   | TargetProblemNoneEnabled TargetSelector [AvailableTarget ()]

     -- | There are no targets at all
   | TargetProblemNoTargets   TargetSelector

     -- | A single 'TargetSelector' matches multiple targets
   | TargetProblemMatchesMultiple TargetSelector [AvailableTarget ()]

     -- | Multiple 'TargetSelector's match multiple targets
   | TargetProblemMultipleTargets TargetsMap
  deriving (Eq, Show)

reportTargetProblems :: Verbosity -> [TargetProblem] -> IO a
reportTargetProblems verbosity =
    die' verbosity . unlines . map renderTargetProblem

renderTargetProblem :: TargetProblem -> String
renderTargetProblem (TargetProblemCommon problem) =
    renderTargetProblemCommon "open a repl for" problem

renderTargetProblem (TargetProblemMatchesMultiple targetSelector targets) =
    "Cannot open a repl for multiple components at once. The target '"
 ++ showTargetSelector targetSelector ++ "' refers to "
 ++ renderTargetSelector targetSelector ++ " which "
 ++ (if targetSelectorRefersToPkgs targetSelector then "includes " else "are ")
 ++ renderListSemiAnd
      [ "the " ++ renderComponentKind Plural ckind ++ " " ++
        renderListCommaAnd
          [ maybe (display pkgname) display (componentNameString cname)
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

renderTargetProblem (TargetProblemMultipleTargets selectorMap) =
    "Cannot open a repl for multiple components at once. The targets "
 ++ renderListCommaAnd
      [ "'" ++ showTargetSelector ts ++ "'"
      | ts <- ordNub (concatMap snd (concat (Map.elems selectorMap))) ]
 ++ " refer to different components."
 ++ ".\n\n" ++ explanationSingleComponentLimitation

renderTargetProblem (TargetProblemNoneEnabled targetSelector targets) =
    renderTargetProblemNoneEnabled "open a repl for" targetSelector targets

renderTargetProblem (TargetProblemNoTargets targetSelector) =
    renderTargetProblemNoTargets "open a repl for" targetSelector


explanationSingleComponentLimitation :: String
explanationSingleComponentLimitation =
    "The reason for this limitation is that current versions of ghci do not "
 ++ "support loading multiple components as source. Load just one component "
 ++ "and when you make changes to a dependent component then quit and reload."

