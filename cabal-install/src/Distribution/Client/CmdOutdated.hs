{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      :  Distribution.Client.CmdOutdated
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Implementation of the 'outdated' command. Checks for outdated
-- dependencies in the package description file or freeze file.
module Distribution.Client.CmdOutdated
  ( outdatedCommand
  , outdatedAction
  , ListOutdatedSettings (..)
  , listOutdated
  , IgnoreMajorVersionBumps (..)
  , showResult
  )
where

import Distribution.Client.Compat.Prelude
import Prelude ()

import qualified Data.Set as Set
import Distribution.Client.Config
  ( SavedConfig
      ( savedConfigureExFlags
      )
  )
import qualified Distribution.Client.IndexUtils as IndexUtils
import Distribution.Client.ProjectConfig
import Distribution.Client.Sandbox.PackageEnvironment
  ( loadUserConfig
  )
import Distribution.Client.Setup
import Distribution.Client.Targets
  ( UserConstraint
  , userToPackageConstraint
  )
import Distribution.Client.Types.SourcePackageDb as SourcePackageDb
import qualified Distribution.Compat.CharParsing as P
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration
import Distribution.Solver.Types.PackageConstraint
  ( packageConstraintToDependency
  )
import Distribution.Version
  ( LowerBound (..)
  , UpperBound (..)
  , Version
  , VersionInterval (..)
  , VersionRange
  , asVersionIntervals
  , majorBoundVersion
  )

import qualified Data.Set as S
import Distribution.Client.NixStyleOptions
import System.Directory
  ( getCurrentDirectory
  )

import Distribution.Client.ProjectOrchestration

import Control.Monad
import Distribution.Client.ScriptUtils
import Distribution.Package
import Distribution.ReadE
import Distribution.Simple.Command
import Distribution.Simple.Flag
import Distribution.Simple.Setup hiding (GlobalFlags (..))
import Distribution.Simple.Utils
import Distribution.Types.PackageVersionConstraint
import Distribution.Verbosity

import qualified Data.Map.Strict as Map
import Distribution.Client.CmdErrorMessages
import Distribution.Client.ProjectPlanning.Types
import Distribution.Client.TargetProblem
import Distribution.Client.Types.PackageSpecifier
import Distribution.Solver.Types.ConstraintSource
import Distribution.Solver.Types.SourcePackage
import Distribution.Types.Component
import qualified Text.PrettyPrint as PP

import Distribution.Client.Errors

-------------------------------------------------------------------------------
-- Command
-------------------------------------------------------------------------------

outdatedCommand :: CommandUI (NixStyleFlags OutdatedFlags)
outdatedCommand =
  CommandUI
    { commandName = "v2-outdated"
    , commandSynopsis = "Check for outdated dependencies."
    , commandDescription = Just $ \_ ->
        wrapText $
          "Checks for outdated dependencies in the package description file "
            ++ "or freeze file"
    , commandNotes = Nothing
    , commandUsage = \pname ->
        "Usage: " ++ pname ++ " outdated [FLAGS] [PACKAGES]\n"
    , commandDefaultFlags = defaultNixStyleFlags defaultOutdatedFlags
    , commandOptions = nixStyleOptions $ \showOrParseArgs ->
        outdatedOptions showOrParseArgs
    }

-------------------------------------------------------------------------------
-- Flags
-------------------------------------------------------------------------------

data IgnoreMajorVersionBumps
  = IgnoreMajorVersionBumpsNone
  | IgnoreMajorVersionBumpsAll
  | IgnoreMajorVersionBumpsSome [PackageName]

instance Monoid IgnoreMajorVersionBumps where
  mempty = IgnoreMajorVersionBumpsNone
  mappend = (<>)

instance Semigroup IgnoreMajorVersionBumps where
  IgnoreMajorVersionBumpsNone <> r = r
  l@IgnoreMajorVersionBumpsAll <> _ = l
  l@(IgnoreMajorVersionBumpsSome _) <> IgnoreMajorVersionBumpsNone = l
  (IgnoreMajorVersionBumpsSome _) <> r@IgnoreMajorVersionBumpsAll = r
  (IgnoreMajorVersionBumpsSome a) <> (IgnoreMajorVersionBumpsSome b) =
    IgnoreMajorVersionBumpsSome (a ++ b)

data OutdatedFlags = OutdatedFlags
  { outdatedFreezeFile :: Flag Bool
  , outdatedNewFreezeFile :: Flag Bool
  , outdatedSimpleOutput :: Flag Bool
  , outdatedExitCode :: Flag Bool
  , outdatedQuiet :: Flag Bool
  , outdatedIgnore :: [PackageName]
  , outdatedMinor :: Maybe IgnoreMajorVersionBumps
  }

defaultOutdatedFlags :: OutdatedFlags
defaultOutdatedFlags =
  OutdatedFlags
    { outdatedFreezeFile = mempty
    , outdatedNewFreezeFile = mempty
    , outdatedSimpleOutput = mempty
    , outdatedExitCode = mempty
    , outdatedQuiet = mempty
    , outdatedIgnore = mempty
    , outdatedMinor = mempty
    }

outdatedOptions :: ShowOrParseArgs -> [OptionField OutdatedFlags]
outdatedOptions _showOrParseArgs =
  [ option
      []
      ["freeze-file", "v1-freeze-file"]
      "Act on the freeze file"
      outdatedFreezeFile
      (\v flags -> flags{outdatedFreezeFile = v})
      trueArg
  , option
      []
      ["project-context", "v2-freeze-file", "new-freeze-file"]
      "Check for outdated dependencies in the project context, for example, dependencies specified in cabal.project orcabal.project.freeze."
      outdatedNewFreezeFile
      (\v flags -> flags{outdatedNewFreezeFile = v})
      trueArg
  , option
      []
      ["simple-output"]
      "Only print names of outdated dependencies, one per line"
      outdatedSimpleOutput
      (\v flags -> flags{outdatedSimpleOutput = v})
      trueArg
  , option
      []
      ["exit-code"]
      "Exit with non-zero when there are outdated dependencies"
      outdatedExitCode
      (\v flags -> flags{outdatedExitCode = v})
      trueArg
  , option
      ['q']
      ["quiet"]
      "Don't print any output. Implies '--exit-code' and '-v0'"
      outdatedQuiet
      (\v flags -> flags{outdatedQuiet = v})
      trueArg
  , option
      []
      ["ignore"]
      "Packages to ignore"
      outdatedIgnore
      (\v flags -> flags{outdatedIgnore = v})
      (reqArg "PKGS" pkgNameListParser (map prettyShow))
  , option
      []
      ["minor"]
      "Ignore major version bumps for these packages"
      outdatedMinor
      (\v flags -> flags{outdatedMinor = v})
      ( optArg
          "PKGS"
          ignoreMajorVersionBumpsParser
          ("", Just IgnoreMajorVersionBumpsAll)
          ignoreMajorVersionBumpsPrinter
      )
  ]
  where
    ignoreMajorVersionBumpsPrinter
      :: Maybe IgnoreMajorVersionBumps
      -> [Maybe String]
    ignoreMajorVersionBumpsPrinter Nothing = []
    ignoreMajorVersionBumpsPrinter (Just IgnoreMajorVersionBumpsNone) = []
    ignoreMajorVersionBumpsPrinter (Just IgnoreMajorVersionBumpsAll) = [Nothing]
    ignoreMajorVersionBumpsPrinter (Just (IgnoreMajorVersionBumpsSome pkgs)) =
      map (Just . prettyShow) pkgs

    ignoreMajorVersionBumpsParser =
      (Just . IgnoreMajorVersionBumpsSome) `fmap` pkgNameListParser

    pkgNameListParser =
      parsecToReadE
        ("Couldn't parse the list of package names: " ++)
        (fmap toList (P.sepByNonEmpty parsec (P.char ',')))

-------------------------------------------------------------------------------
-- Action
-------------------------------------------------------------------------------

getSourcePackages :: Verbosity -> ProjectConfig -> IO SourcePackageDb
getSourcePackages verbosity projectConfig =
  projectConfigWithSolverRepoContext
    verbosity
    (projectConfigShared projectConfig)
    (projectConfigBuildOnly projectConfig)
    (\ctx -> IndexUtils.getSourcePackages verbosity ctx)

outdatedAction :: NixStyleFlags OutdatedFlags -> [String] -> GlobalFlags -> IO ()
outdatedAction flags targetStrings globalFlags =
  withContextAndSelectors
    verbosity
    AcceptNoTargets
    Nothing
    flags
    targetStrings
    globalFlags
    OtherCommand
    $ \_targetCtx ctx targetSelectors -> do
      deps <-
        if
            | freezeFile -> depsFromFreezeFile verbosity
            | newFreezeFile -> depsFromProjectContext verbosity (projectConfig ctx)
            | otherwise -> depsFromLocalPackages verbosity ctx targetSelectors

      debug verbosity $
        "Dependencies loaded: "
          ++ intercalate ", " (map prettyShow deps)

      sourcePkgDb <- getSourcePackages verbosity (projectConfig ctx)
      let outdatedDeps =
            listOutdated
              deps
              sourcePkgDb
              (ListOutdatedSettings ignorePred minorPred)
      when (not quiet) $
        showResult verbosity outdatedDeps simpleOutput
      when (exitCode && (not . null $ outdatedDeps)) exitFailure
  where
    OutdatedFlags{..} = extraFlags flags
    verbosity =
      mkVerbosity defaultVerbosityHandles $
        if quiet
          then silent
          else fromFlagOrDefault normal (setupVerbosity (configCommonFlags (configFlags flags)))
    freezeFile = fromFlagOrDefault False outdatedFreezeFile
    newFreezeFile = fromFlagOrDefault False outdatedNewFreezeFile
    simpleOutput = fromFlagOrDefault False outdatedSimpleOutput
    quiet = fromFlagOrDefault False outdatedQuiet
    exitCode = fromFlagOrDefault quiet outdatedExitCode
    ignorePred =
      let ignoreSet = S.fromList outdatedIgnore
       in \pkgname -> pkgname `S.member` ignoreSet
    minorPred = case outdatedMinor of
      Nothing -> const False
      Just IgnoreMajorVersionBumpsNone -> const False
      Just IgnoreMajorVersionBumpsAll -> const True
      Just (IgnoreMajorVersionBumpsSome pkgs) ->
        let minorSet = S.fromList pkgs
         in \pkgname -> pkgname `S.member` minorSet

reportOutdatedTargetProblem :: Verbosity -> [TargetProblem'] -> IO a
reportOutdatedTargetProblem verbosity problems =
  reportTargetProblems verbosity "check outdated dependencies for" problems

-- | Print either the list of all outdated dependencies, or a message
-- that there are none.
showResult :: Verbosity -> [OutdatedDependency] -> Bool -> IO ()
showResult verbosity outdatedDeps simpleOutput =
  if not . null $ outdatedDeps
    then do
      when (not simpleOutput) $
        notice verbosity "Outdated dependencies:"
      if simpleOutput
        then -- Simple output just prints package names, one per line
          outputSimple
        else -- Hierarchical output grouped by package and component
          outputStructured
    else notice verbosity "All dependencies are up to date."
  where
    -- Group dependencies by package and component
    groupByPackage :: [OutdatedDependency] -> [(String, [(String, [OutdatedDependency])])]
    groupByPackage deps =
      let
        -- First, create a list of (package, component, dependency) tuples
        pkgCompDeps =
          [ (extractPackageName src, extractComponentName src, dep)
          | dep@(OutdatedDependency _ _ src) <- deps
          ]
        -- Group by package
        pkgGroups =
          Map.fromListWith
            (Map.unionWith (++))
            [ (pkg, (Map.singleton comp [d]))
            | (pkg, comp, d) <- pkgCompDeps
            ]
       in
        Map.toList (Map.map Map.toList pkgGroups)

    -- Extract package name from the source
    extractPackageName :: OutdatedDependencySource -> String
    extractPackageName (ConfigSource _) = "project-config"
    extractPackageName (ComponentSource pkgId _) = prettyShow (pkgName pkgId)

    -- Extract component name from the source
    extractComponentName :: OutdatedDependencySource -> String
    extractComponentName (ConfigSource src) = showConstraintSource src
    extractComponentName (ComponentSource pkgId ctarget) = showComponentTarget pkgId ctarget

    getConstraintPackageName :: PackageVersionConstraint -> PackageName
    getConstraintPackageName (PackageVersionConstraint pn _) = pn

    outputSimple =
      let pns = sortNub $ map (getConstraintPackageName . outdatedDependency) outdatedDeps
       in for_ pns $ \pn ->
            noticeNoWrap verbosity (prettyShow pn)

    outputStructured =
      let
        -- Group by package name, then by component
        packageGroups = groupByPackage outdatedDeps
       in
        for_ packageGroups $ \(pkgName, componentGroups) -> do
          noticeNoWrap verbosity $ "* " ++ pkgName
          for_ componentGroups $ \(compName, deps) -> do
            noticeNoWrap verbosity $ "  - " ++ compName
            for_ (sortBy (comparing (getConstraintPackageName . outdatedDependency)) deps) $
              \(OutdatedDependency d v _) ->
                noticeNoWrap verbosity $ "    * " ++ prettyShow d ++ " (latest: " ++ prettyShow v ++ ")"

data OutdatedDependencyX v = OutdatedDependency
  { outdatedDependency :: PackageVersionConstraint
  , _outdatedVersion :: v
  , _outdatedSource :: OutdatedDependencySource
  }

instance Pretty (OutdatedDependencyX Version) where
  pretty (OutdatedDependency dep ver src) =
    pretty dep
      <+> PP.text "(latest:"
      <+> pretty ver
      <+> PP.text ","
      <+> PP.text "from:"
      <+> PP.text (prettyOutdatedDependencySource src)
      <+> PP.text ")"

instance Pretty (OutdatedDependencyX ()) where
  pretty (OutdatedDependency dep _ src) =
    pretty dep <+> PP.text "(from:" <+> PP.text (prettyOutdatedDependencySource src) `mappend` PP.text ")"

data OutdatedDependencySource = ConfigSource ConstraintSource | ComponentSource PackageId ComponentTarget

-- | Pretty print an 'OutdatedDependencySource'.
prettyOutdatedDependencySource :: OutdatedDependencySource -> String
prettyOutdatedDependencySource (ConfigSource src) = showConstraintSource src
prettyOutdatedDependencySource (ComponentSource pkgId ctarget) = prettyShow pkgId ++ ":" ++ showComponentTarget pkgId ctarget

type CandidateOutdatedDependency = OutdatedDependencyX ()

mkCandidateOutdatedDependency :: PackageVersionConstraint -> OutdatedDependencySource -> CandidateOutdatedDependency
mkCandidateOutdatedDependency dep src = OutdatedDependency dep () src

type OutdatedDependency = OutdatedDependencyX Version

-- | Convert a list of 'UserConstraint's to a 'Dependency' list.
userConstraintsToDependencies :: [(UserConstraint, ConstraintSource)] -> [CandidateOutdatedDependency]
userConstraintsToDependencies ucnstrs =
  mapMaybe (\(uc, src) -> fmap (`mkCandidateOutdatedDependency` ConfigSource src) (packageConstraintToDependency . userToPackageConstraint $ uc)) ucnstrs

-- | Read the list of dependencies from the freeze file.
depsFromFreezeFile :: Verbosity -> IO [CandidateOutdatedDependency]
depsFromFreezeFile verbosity = do
  cwd <- getCurrentDirectory
  userConfig <- loadUserConfig verbosity cwd Nothing
  let ucnstrs =
        configExConstraints . savedConfigureExFlags $
          userConfig
      deps = userConstraintsToDependencies ucnstrs
  debug verbosity "Reading the list of dependencies from the freeze file"
  return deps

-- | Read the list of dependencies from the cabal.project context.
-- This will get dependencies from
--  * cabal.project.freeze
--  * cabal.project.local
--  * cabal.project
-- files
depsFromProjectContext :: Verbosity -> ProjectConfig -> IO [CandidateOutdatedDependency]
depsFromProjectContext verbosity projectConfig = do
  let ucnstrs = projectConfigConstraints $ projectConfigShared projectConfig
      deps = userConstraintsToDependencies ucnstrs
      provenance = projectConfigProvenance projectConfig
  debug verbosity $
    "Reading the list of dependencies from the project files: "
      ++ intercalate ", " [prettyShow p | Explicit p <- Set.toList provenance]
  return deps

-- | Read the list of dependencies from the package description.
depsFromPkgDesc :: Verbosity -> PackageId -> GenericPackageDescription -> ComponentTarget -> IO [CandidateOutdatedDependency]
depsFromPkgDesc verbosity pkgId gpd t@(ComponentTarget cname _subtarget) = do
  let pd = flattenPackageDescription gpd
      bd = targetBuildDepends (componentBuildInfo (getComponent pd cname))
  debug
    verbosity
    "Reading the list of dependencies from the package description"
  return $ map toPVC bd
  where
    toPVC (Dependency pn vr _) = mkCandidateOutdatedDependency (PackageVersionConstraint pn vr) (ComponentSource pkgId t)

-- | Various knobs for customising the behaviour of 'listOutdated'.
data ListOutdatedSettings = ListOutdatedSettings
  { listOutdatedIgnorePred :: PackageName -> Bool
  -- ^ Should this package be ignored?
  , listOutdatedMinorPred :: PackageName -> Bool
  -- ^ Should major version bumps be ignored for this package?
  }

-- | Find all outdated dependencies.
listOutdated
  :: [CandidateOutdatedDependency]
  -> SourcePackageDb
  -> ListOutdatedSettings
  -> [OutdatedDependency]
listOutdated deps sourceDb (ListOutdatedSettings ignorePred minorPred) =
  mapMaybe isOutdated deps
  where
    isOutdated :: CandidateOutdatedDependency -> Maybe OutdatedDependency
    isOutdated (OutdatedDependency dep () src)
      | ignorePred pname = Nothing
      | otherwise =
          let this = map packageVersion $ SourcePackageDb.lookupDependency sourceDb pname vr
              latest = lookupLatest dep
           in (\v -> OutdatedDependency dep v src) `fmap` isOutdated' this latest
      where
        PackageVersionConstraint pname vr = simplifyPackageVersionConstraint dep

    isOutdated' :: [Version] -> [Version] -> Maybe Version
    isOutdated' [] _ = Nothing
    isOutdated' _ [] = Nothing
    isOutdated' this latest =
      let this' = maximum this
          latest' = maximum latest
       in if this' < latest' then Just latest' else Nothing

    lookupLatest :: PackageVersionConstraint -> [Version]
    lookupLatest (PackageVersionConstraint pname vr)
      | minorPred pname =
          map packageVersion $ SourcePackageDb.lookupDependency sourceDb pname (relaxMinor vr)
      | otherwise =
          map packageVersion $ SourcePackageDb.lookupPackageName sourceDb pname

    relaxMinor :: VersionRange -> VersionRange
    relaxMinor vr =
      let vis = asVersionIntervals vr
       in maybe vr relax (safeLast vis)
      where
        relax (VersionInterval (LowerBound v0 _) upper) =
          case upper of
            NoUpperBound -> vr
            UpperBound _v1 _ -> majorBoundVersion v0

-- | For the outdated command, when a whole package is specified we want
-- to select all buildable components.
selectPackageTargetsForOutdated
  :: TargetSelector
  -> [AvailableTarget k]
  -> Either (TargetProblem') [k]
selectPackageTargetsForOutdated targetSelector targets
  -- No targets available at all is an error
  | null targets = Left (TargetProblemNoTargets targetSelector)
  -- We select all buildable components for a package
  | otherwise = Right $ selectBuildableTargets targets

-- | For the outdated command, when a specific component is specified
-- we simply select that component.
selectComponentTargetForOutdated
  :: SubComponentTarget
  -> AvailableTarget k
  -> Either (TargetProblem') k
selectComponentTargetForOutdated subtarget target =
  selectComponentTargetBasic subtarget target

-- | Read the list of dependencies from local packages
depsFromLocalPackages :: Verbosity -> ProjectBaseContext -> [TargetSelector] -> IO [CandidateOutdatedDependency]
depsFromLocalPackages verbosity ctx targetSelectors = do
  when (null targetSelectors) $
    dieWithException verbosity TargetSelectorNoTargetsInCwdTrue
  targets <-
    either (reportOutdatedTargetProblem verbosity) return $
      resolveTargetsFromLocalPackages
        selectPackageTargetsForOutdated
        selectComponentTargetForOutdated
        (localPackages ctx)
        targetSelectors
  fmap concat <$> forM (localPackages ctx) $ \pkg -> case pkg of
    SpecificSourcePackage pkg' -> do
      -- Find the package in the resolved targets
      let pkgId = packageId pkg'
      let pkgTargets =
            case Map.lookup pkgId targets of
              Just componentTargets -> map fst componentTargets
              Nothing -> []
      -- If no specific components were targeted, use the whole package
      -- Get dependencies for each targeted component
      fmap concat <$> forM pkgTargets $ \target ->
        depsFromPkgDesc verbosity pkgId (srcpkgDescription pkg') target
    _ -> return []
