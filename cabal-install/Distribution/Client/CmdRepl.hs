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

import Distribution.Client.ProjectPlanning 
       ( ElaboratedSharedConfig(..) )
import Distribution.Client.ProjectOrchestration
import Distribution.Client.CmdErrorMessages

import Distribution.Compat.Lens
import qualified Distribution.Types.Lens as L

import Distribution.Client.CmdInstall
         ( establishDummyProjectBaseContext )
import Distribution.Client.IndexUtils
         ( getSourcePackages )
import qualified Distribution.Client.InstallPlan as InstallPlan
import Distribution.Client.ProjectConfig
         ( ProjectConfig(..), BadPackageLocations(..), BadPackageLocation(..)
         , ProjectConfigProvenance(..), projectConfigWithBuilderRepoContext )
import Distribution.Client.Setup
         ( GlobalFlags, ConfigFlags(..), ConfigExFlags, InstallFlags )
import qualified Distribution.Client.Setup as Client
import Distribution.Client.TargetSelector
         ( TargetSelector(..), TargetImplicitCwd(..), ComponentKind(..) )
import Distribution.Client.Types
         ( PackageLocation(..), PackageSpecifier(..), UnresolvedSourcePackage
         , SourcePackageDb(..) )
import Distribution.Simple.Setup
         ( HaddockFlags, fromFlagOrDefault, replOptions
         , Flag(..), toFlag, trueArg, falseArg )
import Distribution.Simple.Command
         ( CommandUI(..), liftOption, usageAlternatives, option
         , ShowOrParseArgs, OptionField, reqArg )
import Distribution.Package
         ( Package(..), packageName )
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
import qualified Distribution.Solver.Types.PackageIndex as PackageIndex
import Distribution.Types.BuildInfo
         ( BuildInfo(..), emptyBuildInfo )
import Distribution.Types.ComponentName
         ( ComponentName(..), componentNameString )
import Distribution.Types.CondTree
         ( CondTree(..) )
import Distribution.Types.Dependency
         ( Dependency(..), thisPackageVersion )
import Distribution.Types.GenericPackageDescription
         ( emptyGenericPackageDescription )
import Distribution.Types.PackageDescription
         ( PackageDescription(..), emptyPackageDescription )
import Distribution.Types.Library
         ( Library(..), emptyLibrary )
import Distribution.Types.PackageId
         ( PackageIdentifier(..), PackageId )
import Distribution.Types.UnqualComponentName
         ( UnqualComponentName )
import Distribution.Types.Version
         ( mkVersion, version0, nullVersion )
import Distribution.Types.VersionRange
         ( anyVersion )
import Distribution.Text
         ( display )
import Distribution.Verbosity
         ( Verbosity, normal, lessVerbose )
import Distribution.Simple.Utils
         ( wrapText, die', ordNub, createTempDirectory, handleDoesNotExist )
import Language.Haskell.Extension
         ( Language(..) )

import Control.Exception
         ( catch, throwIO )
import Control.Monad 
         ( when, unless )
import Data.List
         ( sortOn )
import qualified Data.Map as Map
import Data.Ord
        ( Down(..) )
import qualified Data.Set as Set
import System.Directory
         ( getTemporaryDirectory, removeDirectoryRecursive )
import System.FilePath
         ( (</>) )

type ReplFlags = [String]

data EnvFlags = EnvFlags 
  { envPackages :: [PackageId]
  , envIncludeTransitive :: Flag Bool
  , envOnlySpecified :: Flag Bool
  }

defaultEnvFlags :: EnvFlags
defaultEnvFlags = EnvFlags
  { envPackages = []
  , envIncludeTransitive = toFlag True
  , envOnlySpecified = toFlag False
  }

envOptions :: ShowOrParseArgs -> [OptionField EnvFlags]
envOptions _ =
  [ option ['p'] ["package"]
    "Include an additional package in the environment presented to GHCi."
    envPackages (\p flags -> flags { envPackages = p ++ envPackages flags })
    (reqArg "PACKAGE" packageIdReadE (fmap prettyShow :: [PackageId] -> [String]))
  , option [] ["no-transitive-deps"]
    "Don't automatically include transitive dependencies of requested packages."
    envIncludeTransitive (\p flags -> flags { envIncludeTransitive = p })
    falseArg
  , option ['z'] ["only-specified"]
    "Only include explicitly specified packages (and 'base'). This implies '--no-transitive-deps'."
    envOnlySpecified (\p flags -> flags { envOnlySpecified = p, envIncludeTransitive = not <$> p})
    trueArg
  ]
  where
    packageIdReadE :: ReadE [PackageId]
    packageIdReadE =
      fmap pure $
        parsecToReadE
          ("couldn't parse package ID: " ++)
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
     ++ "  " ++ pname ++ " new-repl --package lens\n"
     ++ "    add the package 'lens' to the default component (or no component "
        ++ "if there is no package present)\n"

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
      onlySpecified = fromFlagOrDefault False (envOnlySpecified envFlags)
      with    = withProject    cliConfig verbosity targetStrings
      without = withoutProject cliConfig verbosity targetStrings
    
    (baseCtx, targetSelectors, finalizer) <- 
      if onlySpecified
        then 
          without
        else
          catch with
            $ \case
              (BadPackageLocations prov locs) 
                | prov == Set.singleton Implicit
                , let 
                  isGlobErr (BadLocGlobEmptyMatch _) = True
                  isGlobErr _ = False
                , any isGlobErr locs ->
                  without
              err -> throwIO err

    when (buildSettingOnlyDeps (buildSettings baseCtx)) $
      die' verbosity $ "The repl command does not support '--only-dependencies'. "
          ++ "You may wish to use 'build --only-dependencies' and then "
          ++ "use 'repl'."

    baseCtx' <- if null (envPackages envFlags)
      then return baseCtx
      else
        -- Unfortunately, the best way to do this is to let the normal solver
        -- help us resolve the targets, but that isn't ideal for performance,
        -- especially in the no-project case.
        withInstallPlan (lessVerbose verbosity) baseCtx $ \elaboratedPlan -> do
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
          
          let
            (unitId, ((ComponentTarget cname _, _):_)) = head $ Map.toList targets
            Just pkgId = packageId <$> InstallPlan.lookup elaboratedPlan unitId
            deps = pkgIdToDependency <$> envPackages envFlags

          return $ addDepsToProjectTarget deps pkgId cname baseCtx
          
    -- Now, we run the solver again with the added packages. While the graph 
    -- won't actually reflect the addition of transitive dependencies,
    -- they're going to be available already and will be offered to the REPL
    -- and that's good enough.
    buildCtx' <- runProjectPreBuildPhase verbosity baseCtx' $ \elaboratedPlan -> do
      -- Recalculate with updated project.
      targets <- either (reportTargetProblems verbosity) return
                $ resolveTargets
                    selectPackageTargets
                    selectComponentTarget
                    TargetProblemCommon
                    elaboratedPlan
                    Nothing
                    targetSelectors
      when (Set.size (distinctTargetComponents targets) > 1) $
        reportTargetProblems verbosity
          [TargetProblemMultipleTargets targets]
      let elaboratedPlan' = pruneInstallPlanToTargets
                              TargetActionRepl
                              targets
                              elaboratedPlan
      return (elaboratedPlan', targets)

    let buildCtx = buildCtx'
          { elaboratedShared = (elaboratedShared buildCtx')
                { pkgConfigReplOptions = replFlags }
          }
    printPlan verbosity baseCtx buildCtx

    buildOutcomes <- runProjectBuildPhase verbosity baseCtx buildCtx
    runProjectPostBuildPhase verbosity baseCtx buildCtx buildOutcomes
    finalizer
  where
    verbosity = fromFlagOrDefault normal (configVerbosity configFlags)
    cliConfig = commandLineFlagsToProjectConfig
                  globalFlags configFlags configExFlags
                  installFlags haddockFlags

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

  baseCtx <- establishDummyProjectBaseContext
              verbosity
              config
              tempDir
              []
  
  pkgDb <- projectConfigWithBuilderRepoContext 
            verbosity
            (buildSettings baseCtx)
            (getSourcePackages verbosity)

  -- We need to create a dummy package that lives in our dummy project.
  let
    (basePkg:_) = sortOn (Down . packageId) $ 
      PackageIndex.lookupPackageName (packageIndex pkgDb) "base"

    sourcePackage = SourcePackage
      { packageInfoId        = pkgId
      , packageDescription   = genericPackageDescription
      , packageSource        = LocalUnpackedPackage tempDir
      , packageDescrOverride = Nothing
      }
    genericPackageDescription = emptyGenericPackageDescription 
      & L.packageDescription .~ packageDescription
      & L.condLibrary        .~ Just (CondNode library [] [])
    packageDescription = emptyPackageDescription
      { package = pkgId
      , specVersionRaw = Left (mkVersion [2, 2])
      , library = Just library
      , licenseRaw = Left SPDX.NONE
      }
    library = emptyLibrary { libBuildInfo = buildInfo }
    buildInfo = emptyBuildInfo
      { targetBuildDepends = [pkgIdToDependency (packageId basePkg)]
      , defaultLanguage = Just Haskell2010
      }
    pkgId = PackageIdentifier "fake-package" version0

  putStrLn $ showGenericPackageDescription genericPackageDescription
  writeGenericPackageDescription (tempDir </> "fake-package.cabal") genericPackageDescription

  baseCtx' <- establishDummyProjectBaseContext
            verbosity
            config
            tempDir
            [SpecificSourcePackage sourcePackage]

  let
    targetSelectors = [TargetPackage TargetExplicitNamed [pkgId] Nothing]
    finalizer = return () --handleDoesNotExist () (removeDirectoryRecursive tempDir)

  return (baseCtx', targetSelectors, finalizer)

addDepsToProjectTarget :: [Dependency]
                       -> PackageId
                       -> ComponentName
                       -> ProjectBaseContext
                       -> ProjectBaseContext
addDepsToProjectTarget deps pkgId cname ctx = 
    (\p -> ctx { localPackages = p }) . fmap (fmap go) . localPackages $ ctx
  where
    go :: UnresolvedSourcePackage -> UnresolvedSourcePackage
    go pkg
      | packageId pkg /= pkgId = pkg
      | SourcePackage{..} <- pkg =
        pkg { packageDescription = 
          packageDescription & L.packageDescription . buildInfoL cname . L.targetBuildDepends %~ (deps ++) 
        }
    
pkgIdToDependency :: PackageId -> Dependency
pkgIdToDependency pkgId
  | PackageIdentifier{..} <- pkgId
  , pkgVersion == nullVersion = Dependency pkgName anyVersion
  | otherwise                 = thisPackageVersion pkgId

buildInfoL :: ComponentName -> Traversal' PackageDescription BuildInfo
buildInfoL cname = case cname of
    CLibName         -> L.library  . traversed . L.libBuildInfo
    CSubLibName name -> 
      buildInfoL' name L.subLibraries (L.libName . non "") L.libBuildInfo
    CFLibName   name -> 
      buildInfoL' name L.foreignLibs  L.foreignLibName     L.foreignLibBuildInfo
    CExeName    name -> 
      buildInfoL' name L.executables  L.exeName            L.exeBuildInfo
    CTestName   name -> 
      buildInfoL' name L.testSuites   L.testName           L.testBuildInfo
    CBenchName  name ->
      buildInfoL' name L.benchmarks   L.benchmarkName      L.benchmarkBuildInfo
  where
    buildInfoL' :: UnqualComponentName
                -> Traversal' PackageDescription [a]
                -> Traversal' a UnqualComponentName
                -> Traversal' a BuildInfo
                -> Traversal' PackageDescription BuildInfo
    buildInfoL' name pdL nameL biL =
        pdL
      . traversed
      . filtered ((== name) . view nameL)
      . biL

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

