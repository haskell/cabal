{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

-- | cabal-install CLI command: outdated
module Distribution.Client.CmdOutdated
  ( outdatedCommand
  , outdatedAction
  ) where

import qualified Data.Set as Set

import Distribution.Client.Compat.Prelude
import Distribution.Client.Config
  ( SavedConfig
      ( savedGlobalFlags
      )
  )
import Distribution.Client.Errors (CabalInstallException (OutdatedAction))
import qualified Distribution.Client.IndexUtils as IndexUtils
import Distribution.Client.NixStyleOptions
  ( NixStyleFlags (..)
  , defaultNixStyleFlags
  , nixStyleOptions
  )
import Distribution.Client.Outdated
  ( IgnoreMajorVersionBumps (..)
  , ListOutdatedSettings (..)
  )
import qualified Distribution.Client.Outdated as V1Outdated
import Distribution.Client.ProjectConfig
  ( ProjectConfig (..)
  , commandLineFlagsToProjectConfig
  )
import Distribution.Client.ProjectFlags
  ( ProjectFlags (..)
  )
import Distribution.Client.ProjectOrchestration
  ( CurrentCommand (..)
  , ProjectBaseContext (..)
  , establishProjectBaseContext
  )
import Distribution.Client.Sandbox
  ( loadConfigOrSandboxConfig
  )
import Distribution.Client.Setup
  ( ConfigFlags (..)
  , GlobalFlags (..)
  , configCompilerAux'
  , withRepoContext
  )
import Distribution.Client.Types.PackageLocation
  ( UnresolvedPkgLoc
  )
import Distribution.Client.Types.PackageSpecifier
  ( PackageSpecifier (..)
  )
import qualified Distribution.Compat.CharParsing as P
import Distribution.ReadE
  ( parsecToReadE
  )
import Distribution.Simple.Command
  ( CommandUI (..)
  , OptionField
  , ShowOrParseArgs
  , optArg
  , option
  , reqArg
  , usageAlternatives
  )
import Distribution.Simple.Flag
  ( Flag (..)
  , flagToMaybe
  , fromFlagOrDefault
  )
import Distribution.Simple.Setup
  ( trueArg
  )
import Distribution.Simple.Utils
  ( debug
  , dieWithException
  , wrapText
  )
import Distribution.Solver.Types.SourcePackage
  ( SourcePackage (..)
  )
import Distribution.Types.CondTree
  ( CondTree (..)
  , ignoreConditions
  )
import Distribution.Types.Dependency (Dependency (..))
import Distribution.Types.GenericPackageDescription
  ( GenericPackageDescription (..)
  )
import Distribution.Types.PackageName
  ( PackageName
  )
import Distribution.Types.PackageVersionConstraint
  ( PackageVersionConstraint (..)
  )
import Distribution.Types.UnqualComponentName (UnqualComponentName)
import Distribution.Verbosity
  ( normal
  , silent
  )
import Distribution.Version
  ( simplifyVersionRange
  )

outdatedCommand :: CommandUI (NixStyleFlags OutdatedFlags)
outdatedCommand =
  CommandUI
    { commandName = "v2-outdated"
    , commandSynopsis = "Check for outdated dependencies."
    , commandUsage = usageAlternatives "v2-outdated" ["[FLAGS]", "[PACKAGES]"]
    , commandDefaultFlags = defaultNixStyleFlags defaultOutdatedFlags
    , commandDescription = Just $ \_ ->
        wrapText $
          "Checks for outdated dependencies in the package description file "
            ++ "or freeze file"
    , commandNotes = Nothing
    , commandOptions = nixStyleOptions outdatedOptions
    }

-- | To a first approximation, the @outdated@ command runs the first phase of
-- the @build@ command where we bring the install plan up to date, and then
-- based on the install plan we write out a @cabal.project.outdated@ config file.
--
-- For more details on how this works, see the module
-- "Distribution.Client.ProjectOrchestration"
outdatedAction :: NixStyleFlags OutdatedFlags -> [String] -> GlobalFlags -> IO ()
outdatedAction flags _extraArgs globalFlags = do
  let mprojectDir = flagToMaybe . flagProjectDir $ projectFlags flags
      mprojectFile = flagToMaybe . flagProjectFile $ projectFlags flags

  config <- loadConfigOrSandboxConfig verbosity globalFlags
  let globalFlags' = savedGlobalFlags config `mappend` globalFlags

  (comp, platform, _progdb) <- configCompilerAux' $ configFlags flags

  withRepoContext verbosity globalFlags' $ \repoContext -> do
    when (not v2FreezeFile && (isJust mprojectDir || isJust mprojectFile)) $
      dieWithException verbosity OutdatedAction

    sourcePkgDb <- IndexUtils.getSourcePackages verbosity repoContext
    prjBaseCtxt <- establishProjectBaseContext verbosity cliConfig OtherCommand
    pkgVerConstraints <-
      if
          | v1FreezeFile -> V1Outdated.depsFromFreezeFile verbosity
          | v2FreezeFile ->
              V1Outdated.depsFromNewFreezeFile verbosity globalFlags comp platform mprojectDir mprojectFile
          | otherwise -> pure $ extractPackageVersionConstraints (localPackages prjBaseCtxt)

    debug verbosity $
      "Dependencies loaded: " ++ intercalate ", " (map prettyShow pkgVerConstraints)

    let outdatedDeps = V1Outdated.listOutdated pkgVerConstraints sourcePkgDb (ListOutdatedSettings ignorePred minorPred)

    when (not quiet) $
      V1Outdated.showResult verbosity outdatedDeps simpleOutput
    if exitCode && (not . null $ outdatedDeps)
      then exitFailure
      else pure ()
  where
    cliConfig :: ProjectConfig
    cliConfig =
      commandLineFlagsToProjectConfig
        globalFlags
        flags
        mempty -- ClientInstallFlags, not needed here
    outdatedFlags :: OutdatedFlags
    outdatedFlags = extraFlags flags

    v1FreezeFile, v2FreezeFile, simpleOutput, exitCode, quiet :: Bool
    v1FreezeFile = fromFlagOrDefault False $ outdatedFreezeFile outdatedFlags
    v2FreezeFile = fromFlagOrDefault False $ outdatedNewFreezeFile outdatedFlags
    simpleOutput = fromFlagOrDefault False $ outdatedSimpleOutput outdatedFlags
    exitCode = fromFlagOrDefault quiet $ outdatedExitCode outdatedFlags
    quiet = fromFlagOrDefault False $ outdatedQuiet outdatedFlags

    ignorePred :: PackageName -> Bool
    ignorePred =
      let ignoreSet = Set.fromList $ outdatedIgnore outdatedFlags
       in \pkgname -> pkgname `Set.member` ignoreSet

    minorPred :: PackageName -> Bool
    minorPred =
      case outdatedMinor outdatedFlags of
        Nothing -> const False
        Just IgnoreMajorVersionBumpsNone -> const False
        Just IgnoreMajorVersionBumpsAll -> const True
        Just (IgnoreMajorVersionBumpsSome pkgs) ->
          let minorSet = Set.fromList pkgs
           in \pkgname -> pkgname `Set.member` minorSet

    verbosity :: Verbosity
    verbosity =
      if quiet
        then silent
        else fromFlagOrDefault normal (configVerbosity $ configFlags flags)

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

extractPackageVersionConstraints :: [PackageSpecifier (SourcePackage UnresolvedPkgLoc)] -> [PackageVersionConstraint]
extractPackageVersionConstraints =
  map toPackageVersionConstraint . concatMap genericPackageDependencies . mapMaybe getGenericPackageDescription
  where
    getGenericPackageDescription :: PackageSpecifier (SourcePackage UnresolvedPkgLoc) -> Maybe GenericPackageDescription
    getGenericPackageDescription ps =
      case ps of
        NamedPackage{} -> Nothing
        SpecificSourcePackage x -> Just $ srcpkgDescription x

    toPackageVersionConstraint :: Dependency -> PackageVersionConstraint
    toPackageVersionConstraint (Dependency name versionRange _) =
      PackageVersionConstraint name (simplifyVersionRange versionRange)

genericPackageDependencies :: GenericPackageDescription -> [Dependency]
genericPackageDependencies gpd =
  concat
    [ maybe [] (snd . ignoreConditions) $ condLibrary gpd
    , concatMap extract $ condSubLibraries gpd
    , concatMap extract $ condForeignLibs gpd
    , concatMap extract $ condExecutables gpd
    , concatMap extract $ condTestSuites gpd
    , concatMap extract $ condBenchmarks gpd
    ]
  where
    extract :: forall a confVar. Semigroup a => (UnqualComponentName, CondTree confVar [Dependency] a) -> [Dependency]
    extract = snd . ignoreConditions . snd

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
