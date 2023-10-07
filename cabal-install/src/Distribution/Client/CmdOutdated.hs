{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

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
  )
where

import Distribution.Client.Compat.Prelude
import Distribution.Compat.Lens
  ( _1
  , _2
  )
import Prelude ()

import Distribution.Client.Config
  ( SavedConfig
      ( savedConfigureExFlags
      , savedConfigureFlags
      , savedGlobalFlags
      )
  )
import Distribution.Client.DistDirLayout
  ( DistDirLayout (distProjectFile, distProjectRootDirectory)
  , defaultDistDirLayout
  )
import Distribution.Client.IndexUtils as IndexUtils
import Distribution.Client.ProjectConfig
import Distribution.Client.ProjectConfig.Legacy
  ( instantiateProjectConfigSkeletonWithCompiler
  )
import Distribution.Client.ProjectFlags
  ( ProjectFlags (..)
  , defaultProjectFlags
  , projectFlagsOptions
  , removeIgnoreProjectOption
  )
import Distribution.Client.RebuildMonad
  ( runRebuild
  )
import Distribution.Client.Sandbox
  ( loadConfigOrSandboxConfig
  )
import Distribution.Client.Sandbox.PackageEnvironment
  ( loadUserConfig
  )
import Distribution.Client.Setup
import Distribution.Client.Targets
  ( UserConstraint
  , userToPackageConstraint
  )
import Distribution.Client.Types.SourcePackageDb as SourcePackageDb
import Distribution.Solver.Types.PackageConstraint
  ( packageConstraintToDependency
  )
import Distribution.Utils.Generic
  ( safeLast
  , wrapText
  )

import Distribution.Client.HttpUtils
import qualified Distribution.Compat.CharParsing as P
import Distribution.Package
  ( PackageName
  , packageVersion
  )
import Distribution.PackageDescription
  ( allBuildDepends
  )
import Distribution.PackageDescription.Configuration
  ( finalizePD
  )
import Distribution.ReadE
  ( parsecToReadE
  )
import Distribution.Simple.Command
  ( CommandUI (..)
  , OptionField
  , ShowOrParseArgs
  , liftOptionL
  , optArg
  , option
  , reqArg
  )
import Distribution.Simple.Compiler
  ( Compiler
  , compilerInfo
  )
import Distribution.Simple.Flag
  ( Flag (..)
  , flagToMaybe
  , fromFlagOrDefault
  , toFlag
  )
import Distribution.Simple.PackageDescription
  ( readGenericPackageDescription
  )
import Distribution.Simple.Setup
  ( optionVerbosity
  , trueArg
  )
import Distribution.Simple.Utils
  ( debug
  , dieWithException
  , notice
  , tryFindPackageDesc
  )
import Distribution.System
  ( Platform (..)
  )
import Distribution.Types.ComponentRequestedSpec
  ( ComponentRequestedSpec (..)
  )
import Distribution.Types.Dependency
  ( Dependency (..)
  )
import Distribution.Types.PackageVersionConstraint
  ( PackageVersionConstraint (..)
  , simplifyPackageVersionConstraint
  )
import Distribution.Utils.NubList
  ( fromNubList
  )
import Distribution.Verbosity
  ( normal
  , silent
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
import Distribution.Client.Errors
import System.Directory
  ( doesFileExist
  , getCurrentDirectory
  )

-------------------------------------------------------------------------------
-- Command
-------------------------------------------------------------------------------

outdatedCommand :: CommandUI (ProjectFlags, OutdatedFlags)
outdatedCommand =
  CommandUI
    { commandName = "outdated"
    , commandSynopsis = "Check for outdated dependencies."
    , commandDescription = Just $ \_ ->
        wrapText $
          "Checks for outdated dependencies in the package description file "
            ++ "or freeze file"
    , commandNotes = Nothing
    , commandUsage = \pname ->
        "Usage: " ++ pname ++ " outdated [FLAGS] [PACKAGES]\n"
    , commandDefaultFlags = (defaultProjectFlags, defaultOutdatedFlags)
    , commandOptions = \showOrParseArgs ->
        map
          (liftOptionL _1)
          (removeIgnoreProjectOption (projectFlagsOptions showOrParseArgs))
          ++ map (liftOptionL _2) (outdatedOptions showOrParseArgs)
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
  { outdatedVerbosity :: Flag Verbosity
  , outdatedFreezeFile :: Flag Bool
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
    { outdatedVerbosity = toFlag normal
    , outdatedFreezeFile = mempty
    , outdatedNewFreezeFile = mempty
    , outdatedSimpleOutput = mempty
    , outdatedExitCode = mempty
    , outdatedQuiet = mempty
    , outdatedIgnore = mempty
    , outdatedMinor = mempty
    }

outdatedOptions :: ShowOrParseArgs -> [OptionField OutdatedFlags]
outdatedOptions _showOrParseArgs =
  [ optionVerbosity
      outdatedVerbosity
      (\v flags -> flags{outdatedVerbosity = v})
  , option
      []
      ["freeze-file", "v1-freeze-file"]
      "Act on the freeze file"
      outdatedFreezeFile
      (\v flags -> flags{outdatedFreezeFile = v})
      trueArg
  , option
      []
      ["v2-freeze-file", "new-freeze-file"]
      "Act on the new-style freeze file (default: cabal.project.freeze)"
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

-- | Entry point for the 'outdated' command.
outdatedAction :: (ProjectFlags, OutdatedFlags) -> [String] -> GlobalFlags -> IO ()
outdatedAction (ProjectFlags{flagProjectDir, flagProjectFile}, OutdatedFlags{..}) _targetStrings globalFlags = do
  config <- loadConfigOrSandboxConfig verbosity globalFlags
  let globalFlags' = savedGlobalFlags config `mappend` globalFlags
      configFlags = savedConfigureFlags config
  withRepoContext verbosity globalFlags' $ \repoContext -> do
    when (not newFreezeFile && (isJust mprojectDir || isJust mprojectFile)) $
      dieWithException verbosity OutdatedAction

    sourcePkgDb <- IndexUtils.getSourcePackages verbosity repoContext
    (comp, platform, _progdb) <- configCompilerAux' configFlags
    deps <-
      if freezeFile
        then depsFromFreezeFile verbosity
        else
          if newFreezeFile
            then do
              httpTransport <-
                configureTransport
                  verbosity
                  (fromNubList . globalProgPathExtra $ globalFlags)
                  (flagToMaybe . globalHttpTransport $ globalFlags)
              depsFromNewFreezeFile verbosity httpTransport comp platform mprojectDir mprojectFile
            else do
              depsFromPkgDesc verbosity comp platform
    debug verbosity $
      "Dependencies loaded: "
        ++ intercalate ", " (map prettyShow deps)
    let outdatedDeps =
          listOutdated
            deps
            sourcePkgDb
            (ListOutdatedSettings ignorePred minorPred)
    when (not quiet) $
      showResult verbosity outdatedDeps simpleOutput
    if exitCode && (not . null $ outdatedDeps)
      then exitFailure
      else return ()
  where
    verbosity =
      if quiet
        then silent
        else fromFlagOrDefault normal outdatedVerbosity
    freezeFile = fromFlagOrDefault False outdatedFreezeFile
    newFreezeFile = fromFlagOrDefault False outdatedNewFreezeFile
    mprojectDir = flagToMaybe flagProjectDir
    mprojectFile = flagToMaybe flagProjectFile
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

-- | Print either the list of all outdated dependencies, or a message
-- that there are none.
showResult :: Verbosity -> [(PackageVersionConstraint, Version)] -> Bool -> IO ()
showResult verbosity outdatedDeps simpleOutput =
  if not . null $ outdatedDeps
    then do
      when (not simpleOutput) $
        notice verbosity "Outdated dependencies:"
      for_ outdatedDeps $ \(d@(PackageVersionConstraint pn _), v) ->
        let outdatedDep =
              if simpleOutput
                then prettyShow pn
                else prettyShow d ++ " (latest: " ++ prettyShow v ++ ")"
         in notice verbosity outdatedDep
    else notice verbosity "All dependencies are up to date."

-- | Convert a list of 'UserConstraint's to a 'Dependency' list.
userConstraintsToDependencies :: [UserConstraint] -> [PackageVersionConstraint]
userConstraintsToDependencies ucnstrs =
  mapMaybe (packageConstraintToDependency . userToPackageConstraint) ucnstrs

-- | Read the list of dependencies from the freeze file.
depsFromFreezeFile :: Verbosity -> IO [PackageVersionConstraint]
depsFromFreezeFile verbosity = do
  cwd <- getCurrentDirectory
  userConfig <- loadUserConfig verbosity cwd Nothing
  let ucnstrs =
        map fst . configExConstraints . savedConfigureExFlags $
          userConfig
      deps = userConstraintsToDependencies ucnstrs
  debug verbosity "Reading the list of dependencies from the freeze file"
  return deps

-- | Read the list of dependencies from the new-style freeze file.
depsFromNewFreezeFile :: Verbosity -> HttpTransport -> Compiler -> Platform -> Maybe FilePath -> Maybe FilePath -> IO [PackageVersionConstraint]
depsFromNewFreezeFile verbosity httpTransport compiler (Platform arch os) mprojectDir mprojectFile = do
  projectRoot <-
    either throwIO return
      =<< findProjectRoot verbosity mprojectDir mprojectFile
  let distDirLayout =
        defaultDistDirLayout
          projectRoot
          {- TODO: Support dist dir override -} Nothing
          Nothing
  projectConfig <- runRebuild (distProjectRootDirectory distDirLayout) $ do
    pcs <- readProjectLocalFreezeConfig verbosity httpTransport distDirLayout
    pure $ instantiateProjectConfigSkeletonWithCompiler os arch (compilerInfo compiler) mempty pcs
  let ucnstrs =
        map fst . projectConfigConstraints . projectConfigShared $
          projectConfig
      deps = userConstraintsToDependencies ucnstrs
      freezeFile = distProjectFile distDirLayout "freeze"
  freezeFileExists <- doesFileExist freezeFile

  unless freezeFileExists $
    dieWithException verbosity $
      FreezeFileExistsErr freezeFile

  debug verbosity $
    "Reading the list of dependencies from the new-style freeze file " ++ freezeFile
  return deps

-- | Read the list of dependencies from the package description.
depsFromPkgDesc :: Verbosity -> Compiler -> Platform -> IO [PackageVersionConstraint]
depsFromPkgDesc verbosity comp platform = do
  cwd <- getCurrentDirectory
  path <- tryFindPackageDesc verbosity cwd
  gpd <- readGenericPackageDescription verbosity path
  let cinfo = compilerInfo comp
      epd =
        finalizePD
          mempty
          (ComponentRequestedSpec True True)
          (const True)
          platform
          cinfo
          []
          gpd
  case epd of
    Left _ -> dieWithException verbosity FinalizePDFailed
    Right (pd, _) -> do
      let bd = allBuildDepends pd
      debug
        verbosity
        "Reading the list of dependencies from the package description"
      return $ map toPVC bd
  where
    toPVC (Dependency pn vr _) = PackageVersionConstraint pn vr

-- | Various knobs for customising the behaviour of 'listOutdated'.
data ListOutdatedSettings = ListOutdatedSettings
  { listOutdatedIgnorePred :: PackageName -> Bool
  -- ^ Should this package be ignored?
  , listOutdatedMinorPred :: PackageName -> Bool
  -- ^ Should major version bumps be ignored for this package?
  }

-- | Find all outdated dependencies.
listOutdated
  :: [PackageVersionConstraint]
  -> SourcePackageDb
  -> ListOutdatedSettings
  -> [(PackageVersionConstraint, Version)]
listOutdated deps sourceDb (ListOutdatedSettings ignorePred minorPred) =
  mapMaybe isOutdated $ map simplifyPackageVersionConstraint deps
  where
    isOutdated :: PackageVersionConstraint -> Maybe (PackageVersionConstraint, Version)
    isOutdated dep@(PackageVersionConstraint pname vr)
      | ignorePred pname = Nothing
      | otherwise =
          let this = map packageVersion $ SourcePackageDb.lookupDependency sourceDb pname vr
              latest = lookupLatest dep
           in (\v -> (dep, v)) `fmap` isOutdated' this latest

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
