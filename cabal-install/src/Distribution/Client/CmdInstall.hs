{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | cabal-install CLI command: build
module Distribution.Client.CmdInstall
  ( -- * The @build@ CLI and action
    installCommand
  , installAction

    -- * Internals exposed for testing
  , selectPackageTargets
  , selectComponentTarget

    -- * Internals exposed for CmdRepl + CmdRun
  , establishDummyDistDirLayout
  , establishDummyProjectBaseContext
  ) where

import Distribution.Client.Compat.Prelude
import Distribution.Compat.Directory
  ( doesPathExist
  )
import Prelude ()

import Distribution.Client.CmdErrorMessages
import Distribution.Client.CmdSdist
import Distribution.Client.ProjectOrchestration
import Distribution.Client.TargetProblem
  ( TargetProblem (..)
  , TargetProblem'
  )

import Distribution.Client.CmdInstall.ClientInstallFlags
import Distribution.Client.CmdInstall.ClientInstallTargetSelector

import Distribution.Client.Config
  ( SavedConfig (..)
  , defaultInstallPath
  , loadConfig
  )
import Distribution.Client.DistDirLayout
  ( CabalDirLayout (..)
  , DistDirLayout (..)
  , StoreDirLayout (..)
  , cabalStoreDirLayout
  , mkCabalDirLayout
  )
import Distribution.Client.IndexUtils
  ( getInstalledPackages
  , getSourcePackages
  )
import qualified Distribution.Client.InstallPlan as InstallPlan
import Distribution.Client.InstallSymlink
  ( promptRun
  , symlinkBinary
  , trySymlink
  )
import Distribution.Client.NixStyleOptions
  ( NixStyleFlags (..)
  , defaultNixStyleFlags
  , nixStyleOptions
  )
import Distribution.Client.ProjectConfig
  ( ProjectPackageLocation (..)
  , fetchAndReadSourcePackages
  , projectConfigWithBuilderRepoContext
  , resolveBuildTimeSettings
  , withProjectOrGlobalConfig
  )
import Distribution.Client.ProjectConfig.Types
  ( MapMappend (..)
  , PackageConfig (..)
  , ProjectConfig (..)
  , ProjectConfigBuildOnly (..)
  , ProjectConfigShared (..)
  , getMapLast
  , getMapMappend
  , projectConfigBuildOnly
  , projectConfigConfigFile
  , projectConfigLogsDir
  , projectConfigStoreDir
  )
import Distribution.Client.ProjectFlags (ProjectFlags (..))
import Distribution.Client.ProjectPlanning
  ( storePackageInstallDirs'
  )
import Distribution.Client.ProjectPlanning.Types
  ( ElaboratedInstallPlan
  )
import Distribution.Client.RebuildMonad
  ( runRebuild
  )
import Distribution.Client.Setup
  ( ConfigFlags (..)
  , GlobalFlags (..)
  , InstallFlags (..)
  )
import Distribution.Client.Types
  ( PackageLocation (..)
  , PackageSpecifier (..)
  , SourcePackageDb (..)
  , UnresolvedSourcePackage
  )
import Distribution.Client.Types.OverwritePolicy
  ( OverwritePolicy (..)
  )
import Distribution.Package
  ( Package (..)
  , PackageName
  , mkPackageName
  , unPackageName
  )
import Distribution.Simple.BuildPaths
  ( exeExtension
  )
import Distribution.Simple.Command
  ( CommandUI (..)
  , optionName
  , usageAlternatives
  )
import Distribution.Simple.Compiler
  ( Compiler (..)
  , CompilerFlavor (..)
  , CompilerId (..)
  , PackageDB (..)
  , PackageDBStack
  )
import Distribution.Simple.Configure
  ( configCompilerEx
  )
import Distribution.Simple.Flag
  ( flagElim
  , flagToMaybe
  , fromFlagOrDefault
  )
import Distribution.Simple.GHC
  ( GhcEnvironmentFileEntry (..)
  , GhcImplInfo (..)
  , ParseErrorExc
  , getGhcAppDir
  , getImplInfo
  , ghcPlatformAndVersionString
  , readGhcEnvironmentFile
  , renderGhcEnvironmentFile
  )
import qualified Distribution.Simple.InstallDirs as InstallDirs
import qualified Distribution.Simple.PackageIndex as PI
import Distribution.Simple.Program.Db
  ( defaultProgramDb
  , modifyProgramSearchPath
  , userSpecifyArgss
  , userSpecifyPaths
  )
import Distribution.Simple.Program.Find
  ( ProgramSearchPathEntry (..)
  )
import Distribution.Simple.Setup
  ( Flag (..)
  , installDirsOptions
  )
import Distribution.Simple.Utils
  ( createDirectoryIfMissingVerbose
  , dieWithException
  , notice
  , ordNub
  , safeHead
  , warn
  , withTempDirectory
  , wrapText
  )
import Distribution.Solver.Types.PackageConstraint
  ( PackageProperty (..)
  )
import Distribution.Solver.Types.PackageIndex
  ( lookupPackageName
  , searchByName
  )
import Distribution.Solver.Types.SourcePackage
  ( SourcePackage (..)
  )
import Distribution.System
  ( OS (Windows)
  , Platform
  , buildOS
  )
import Distribution.Types.InstalledPackageInfo
  ( InstalledPackageInfo (..)
  )
import Distribution.Types.PackageId
  ( PackageIdentifier (..)
  )
import Distribution.Types.UnitId
  ( UnitId
  )
import Distribution.Types.UnqualComponentName
  ( UnqualComponentName
  , unUnqualComponentName
  )
import Distribution.Types.Version
  ( Version
  , nullVersion
  )
import Distribution.Types.VersionRange
  ( thisVersion
  )
import Distribution.Utils.Generic
  ( writeFileAtomic
  )
import Distribution.Verbosity
  ( lessVerbose
  , normal
  )

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import Data.Ord
  ( Down (..)
  )
import qualified Data.Set as S
import Distribution.Client.Errors
import Distribution.Utils.NubList
  ( fromNubList
  )
import Network.URI (URI)
import System.Directory
  ( copyFile
  , createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , getTemporaryDirectory
  , makeAbsolute
  , removeDirectory
  , removeFile
  )
import System.FilePath
  ( takeBaseName
  , takeDirectory
  , (<.>)
  , (</>)
  )

installCommand :: CommandUI (NixStyleFlags ClientInstallFlags)
installCommand =
  CommandUI
    { commandName = "v2-install"
    , commandSynopsis = "Install packages."
    , commandUsage =
        usageAlternatives
          "v2-install"
          ["[TARGETS] [FLAGS]"]
    , commandDescription = Just $ \_ ->
        wrapText $
          "Installs one or more packages. This is done by installing them "
            ++ "in the store and symlinking/copying the executables in the directory "
            ++ "specified by the --installdir flag (`~/.local/bin/` by default). "
            ++ "If you want the installed executables to be available globally, "
            ++ "make sure that the PATH environment variable contains that directory. "
            ++ "\n\n"
            ++ "If TARGET is a library and --lib (provisional) is used, "
            ++ "it will be added to the global environment. "
            ++ "When doing this, cabal will try to build a plan that includes all "
            ++ "the previously installed libraries. This is currently not implemented."
    , commandNotes = Just $ \pname ->
        "Examples:\n"
          ++ "  "
          ++ pname
          ++ " v2-install\n"
          ++ "    Install the package in the current directory\n"
          ++ "  "
          ++ pname
          ++ " v2-install pkgname\n"
          ++ "    Install the package named pkgname"
          ++ " (fetching it from hackage if necessary)\n"
          ++ "  "
          ++ pname
          ++ " v2-install ./pkgfoo\n"
          ++ "    Install the package in the ./pkgfoo directory\n"
    , commandOptions = \x -> filter notInstallDirOpt $ nixStyleOptions clientInstallOptions x
    , commandDefaultFlags = defaultNixStyleFlags defaultClientInstallFlags
    }
  where
    -- install doesn't take installDirs flags, since it always installs into the store in a fixed way.
    notInstallDirOpt x = not $ optionName x `elem` installDirOptNames
    installDirOptNames = map optionName installDirsOptions

-- | The @install@ command actually serves four different needs. It installs:
-- * exes:
--   For example a program from hackage. The behavior is similar to the old
--   install command, except that now conflicts between separate runs of the
--   command are impossible thanks to the store.
--   Exes are installed in the store like a normal dependency, then they are
--   symlinked/copied in the directory specified by --installdir.
--   To do this we need a dummy projectBaseContext containing the targets as
--   extra packages and using a temporary dist directory.
-- * libraries
--   Libraries install through a similar process, but using GHC environment
--   files instead of symlinks. This means that 'v2-install'ing libraries
--   only works on GHC >= 8.0.
--
-- For more details on how this works, see the module
-- "Distribution.Client.ProjectOrchestration"
installAction :: NixStyleFlags ClientInstallFlags -> [String] -> GlobalFlags -> IO ()
installAction flags@NixStyleFlags{extraFlags = clientInstallFlags', ..} targetStrings globalFlags = do
  -- Ensure there were no invalid configuration options specified.
  verifyPreconditionsOrDie verbosity configFlags'

  -- We cannot use establishDummyProjectBaseContext to get these flags, since
  -- it requires one of them as an argument. Normal establishProjectBaseContext
  -- does not, and this is why this is done only for the install command
  clientInstallFlags <- getClientInstallFlags verbosity globalFlags clientInstallFlags'

  let
    installLibs = fromFlagOrDefault False (cinstInstallLibs clientInstallFlags)
    targetFilter = if installLibs then Just LibKind else Just ExeKind
    targetStrings' = if null targetStrings then ["."] else targetStrings

    -- Note the logic here is rather goofy. Target selectors of the form "foo:bar" also parse as uris.
    -- However, we want install to also take uri arguments. Hence, we only parse uri arguments in the case where
    -- no project file is present (including an implicit one derived from being in a package directory)
    -- or where the --ignore-project flag is passed explicitly. In such a case we only parse colon-free target selectors
    -- as selectors, and otherwise parse things as URIs.

    -- However, in the special case where --ignore-project is passed with no selectors, we want to act as though this is
    -- a "normal" ignore project that actually builds and installs the selected package.

    withProject :: IO ([PackageSpecifier UnresolvedSourcePackage], [URI], [TargetSelector], ProjectConfig)
    withProject = do
      let reducedVerbosity = lessVerbose verbosity

      -- First, we need to learn about what's available to be installed.
      localBaseCtx <-
        establishProjectBaseContext reducedVerbosity cliConfig InstallCommand
      let localDistDirLayout = distDirLayout localBaseCtx
      pkgDb <-
        projectConfigWithBuilderRepoContext
          reducedVerbosity
          (buildSettings localBaseCtx)
          (getSourcePackages verbosity)

      let
        (targetStrings'', packageIds) =
          partitionEithers
            . flip fmap targetStrings'
            $ \str -> case simpleParsec str of
              Just (pkgId :: PackageId)
                | pkgVersion pkgId /= nullVersion -> Right pkgId
              _ -> Left str
        packageSpecifiers =
          flip fmap packageIds $ \case
            PackageIdentifier{..}
              | pkgVersion == nullVersion -> NamedPackage pkgName []
              | otherwise ->
                  NamedPackage
                    pkgName
                    [ PackagePropertyVersion
                        (thisVersion pkgVersion)
                    ]
        packageTargets =
          flip TargetPackageNamed targetFilter . pkgName <$> packageIds

      if null targetStrings'' -- if every selector is already resolved as a packageid, return without further parsing.
        then return (packageSpecifiers, [], packageTargets, projectConfig localBaseCtx)
        else do
          targetSelectors <-
            either (reportTargetSelectorProblems verbosity) return
              =<< readTargetSelectors
                (localPackages localBaseCtx)
                Nothing
                targetStrings''

          (specs, selectors) <-
            getSpecsAndTargetSelectors
              verbosity
              reducedVerbosity
              pkgDb
              targetSelectors
              localDistDirLayout
              localBaseCtx
              targetFilter

          return
            ( specs ++ packageSpecifiers
            , []
            , selectors ++ packageTargets
            , projectConfig localBaseCtx
            )

    withoutProject :: ProjectConfig -> IO ([PackageSpecifier UnresolvedSourcePackage], [URI], [TargetSelector], ProjectConfig)
    withoutProject _ | null targetStrings = withProject -- if there's no targets, we don't parse specially, but treat it as install in a standard cabal package dir
    withoutProject globalConfig = do
      tss <- traverse (parseWithoutProjectTargetSelector verbosity) targetStrings'
      let
        projectConfig = globalConfig <> cliConfig

        ProjectConfigBuildOnly
          { projectConfigLogsDir
          } = projectConfigBuildOnly projectConfig

        ProjectConfigShared
          { projectConfigStoreDir
          } = projectConfigShared projectConfig

        mlogsDir = flagToMaybe projectConfigLogsDir
        mstoreDir = flagToMaybe projectConfigStoreDir
      cabalDirLayout <- mkCabalDirLayout mstoreDir mlogsDir

      let
        buildSettings =
          resolveBuildTimeSettings
            verbosity
            cabalDirLayout
            projectConfig

      SourcePackageDb{packageIndex} <-
        projectConfigWithBuilderRepoContext
          verbosity
          buildSettings
          (getSourcePackages verbosity)

      for_ (concatMap woPackageNames tss) $ \name -> do
        when (null (lookupPackageName packageIndex name)) $ do
          let xs = searchByName packageIndex (unPackageName name)
          let emptyIf True _ = []
              emptyIf False zs = zs
              str2 =
                emptyIf
                  (null xs)
                  [ "Did you mean any of the following?\n"
                  , unlines (("- " ++) . unPackageName . fst <$> xs)
                  ]
          dieWithException verbosity $ WithoutProject (unPackageName name) str2

      let
        (uris, packageSpecifiers) = partitionEithers $ map woPackageSpecifiers tss
        packageTargets = map woPackageTargets tss

      return (packageSpecifiers, uris, packageTargets, projectConfig)

  (specs, uris, targetSelectors, config) <-
    withProjectOrGlobalConfig verbosity ignoreProject globalConfigFlag withProject withoutProject

  let
    ProjectConfig
      { projectConfigBuildOnly =
        ProjectConfigBuildOnly
          { projectConfigLogsDir
          }
      , projectConfigShared =
        ProjectConfigShared
          { projectConfigHcFlavor
          , projectConfigHcPath
          , projectConfigHcPkg
          , projectConfigStoreDir
          }
      , projectConfigLocalPackages =
        PackageConfig
          { packageConfigProgramPaths
          , packageConfigProgramArgs
          , packageConfigProgramPathExtra
          }
      } = config

    hcFlavor = flagToMaybe projectConfigHcFlavor
    hcPath = flagToMaybe projectConfigHcPath
    hcPkg = flagToMaybe projectConfigHcPkg

    -- ProgramDb with directly user specified paths
    preProgDb =
      userSpecifyPaths (Map.toList (getMapLast packageConfigProgramPaths))
        . userSpecifyArgss (Map.toList (getMapMappend packageConfigProgramArgs))
        . modifyProgramSearchPath
          ( ++
              [ ProgramSearchPathDir dir
              | dir <- fromNubList packageConfigProgramPathExtra
              ]
          )
        $ defaultProgramDb

  -- progDb is a program database with compiler tools configured properly
  ( compiler@Compiler
      { compilerId =
        compilerId@(CompilerId compilerFlavor compilerVersion)
      }
    , platform
    , progDb
    ) <-
    configCompilerEx hcFlavor hcPath hcPkg preProgDb verbosity

  let
    GhcImplInfo{supportsPkgEnvFiles} = getImplInfo compiler

  envFile <- getEnvFile clientInstallFlags platform compilerVersion
  existingEnvEntries <-
    getExistingEnvEntries verbosity compilerFlavor supportsPkgEnvFiles envFile
  packageDbs <- getPackageDbStack compilerId projectConfigStoreDir projectConfigLogsDir
  installedIndex <- getInstalledPackages verbosity compiler packageDbs progDb

  let
    (envSpecs, nonGlobalEnvEntries) =
      getEnvSpecsAndNonGlobalEntries installedIndex existingEnvEntries installLibs

  -- Second, we need to use a fake project to let Cabal build the
  -- installables correctly. For that, we need a place to put a
  -- temporary dist directory.
  globalTmp <- getTemporaryDirectory

  withTempDirectory verbosity globalTmp "cabal-install." $ \tmpDir -> do
    distDirLayout <- establishDummyDistDirLayout verbosity config tmpDir

    uriSpecs <-
      runRebuild tmpDir $
        fetchAndReadSourcePackages
          verbosity
          distDirLayout
          (projectConfigShared config)
          (projectConfigBuildOnly config)
          [ProjectPackageRemoteTarball uri | uri <- uris]

    -- check for targets already in env
    let getPackageName :: PackageSpecifier UnresolvedSourcePackage -> PackageName
        getPackageName (NamedPackage pn _) = pn
        getPackageName (SpecificSourcePackage (SourcePackage pkgId _ _ _)) = pkgName pkgId
        targetNames = S.fromList $ map getPackageName (specs ++ uriSpecs)
        envNames = S.fromList $ map getPackageName envSpecs
        forceInstall = fromFlagOrDefault False $ installOverrideReinstall installFlags
        nameIntersection = S.intersection targetNames envNames

    -- we check for intersections in targets with the existing env
    (envSpecs', nonGlobalEnvEntries') <-
      if null nameIntersection
        then pure (envSpecs, map snd nonGlobalEnvEntries)
        else
          if forceInstall
            then
              let es = filter (\e -> not $ getPackageName e `S.member` nameIntersection) envSpecs
                  nge = map snd . filter (\e -> not $ fst e `S.member` nameIntersection) $ nonGlobalEnvEntries
               in pure (es, nge)
            else dieWithException verbosity $ PackagesAlreadyExistInEnvfile envFile (map prettyShow $ S.toList nameIntersection)

    -- we construct an installed index of files in the cleaned target environment (absent overwrites) so that we can solve with regards to packages installed locally but not in the upstream repo
    let installedPacks = PI.allPackagesByName installedIndex
        newEnvNames = S.fromList $ map getPackageName envSpecs'
        installedIndex' = PI.fromList . concatMap snd . filter (\p -> fst p `S.member` newEnvNames) $ installedPacks

    baseCtx <-
      establishDummyProjectBaseContext
        verbosity
        config
        distDirLayout
        (envSpecs' ++ specs ++ uriSpecs)
        InstallCommand

    buildCtx <- constructProjectBuildContext verbosity (baseCtx{installedPackages = Just installedIndex'}) targetSelectors

    printPlan verbosity baseCtx buildCtx

    buildOutcomes <- runProjectBuildPhase verbosity baseCtx buildCtx
    runProjectPostBuildPhase verbosity baseCtx buildCtx buildOutcomes

    -- Now that we built everything we can do the installation part.
    -- First, figure out if / what parts we want to install:
    let
      dryRun =
        buildSettingDryRun (buildSettings baseCtx)
          || buildSettingOnlyDownload (buildSettings baseCtx)

    -- Then, install!
    unless dryRun $
      if installLibs
        then
          installLibraries
            verbosity
            buildCtx
            installedIndex
            compiler
            packageDbs
            envFile
            nonGlobalEnvEntries'
        else
          installExes
            verbosity
            baseCtx
            buildCtx
            platform
            compiler
            configFlags
            clientInstallFlags
  where
    configFlags' = disableTestsBenchsByDefault configFlags
    verbosity = fromFlagOrDefault normal (configVerbosity configFlags')
    ignoreProject = flagIgnoreProject projectFlags
    baseCliConfig =
      commandLineFlagsToProjectConfig
        globalFlags
        flags{configFlags = configFlags'}
        clientInstallFlags'
    cliConfig = addLocalConfigToTargets baseCliConfig targetStrings
    globalConfigFlag = projectConfigConfigFile (projectConfigShared cliConfig)

-- | Treat all direct targets of install command as local packages: #8637
addLocalConfigToTargets :: ProjectConfig -> [String] -> ProjectConfig
addLocalConfigToTargets config targetStrings =
  config
    { projectConfigSpecificPackage =
        projectConfigSpecificPackage config
          <> MapMappend (Map.fromList targetPackageConfigs)
    }
  where
    localConfig = projectConfigLocalPackages config
    targetPackageConfigs = map (\x -> (mkPackageName x, localConfig)) targetStrings

-- | Verify that invalid config options were not passed to the install command.
--
-- If an invalid configuration is found the command will @dieWithException@.
verifyPreconditionsOrDie :: Verbosity -> ConfigFlags -> IO ()
verifyPreconditionsOrDie verbosity configFlags = do
  -- We never try to build tests/benchmarks for remote packages.
  -- So we set them as disabled by default and error if they are explicitly
  -- enabled.
  when (configTests configFlags == Flag True) $
    dieWithException verbosity ConfigTests
  when (configBenchmarks configFlags == Flag True) $
    dieWithException verbosity ConfigBenchmarks

getClientInstallFlags :: Verbosity -> GlobalFlags -> ClientInstallFlags -> IO ClientInstallFlags
getClientInstallFlags verbosity globalFlags existingClientInstallFlags = do
  let configFileFlag = globalConfigFile globalFlags
  savedConfig <- loadConfig verbosity configFileFlag
  pure $ savedClientInstallFlags savedConfig `mappend` existingClientInstallFlags

getSpecsAndTargetSelectors
  :: Verbosity
  -> Verbosity
  -> SourcePackageDb
  -> [TargetSelector]
  -> DistDirLayout
  -> ProjectBaseContext
  -> Maybe ComponentKindFilter
  -> IO ([PackageSpecifier UnresolvedSourcePackage], [TargetSelector])
getSpecsAndTargetSelectors verbosity reducedVerbosity pkgDb targetSelectors localDistDirLayout localBaseCtx targetFilter =
  withInstallPlan reducedVerbosity localBaseCtx $ \elaboratedPlan _ -> do
    -- Split into known targets and hackage packages.
    (targets, hackageNames) <-
      partitionToKnownTargetsAndHackagePackages
        verbosity
        pkgDb
        elaboratedPlan
        targetSelectors

    let
      planMap = InstallPlan.toMap elaboratedPlan
      targetIds = Map.keys targets

      sdistize (SpecificSourcePackage spkg) =
        SpecificSourcePackage spkg'
        where
          sdistPath = distSdistFile localDistDirLayout (packageId spkg)
          spkg' = spkg{srcpkgSource = LocalTarballPackage sdistPath}
      sdistize named = named

      local = sdistize <$> localPackages localBaseCtx

      gatherTargets :: UnitId -> TargetSelector
      gatherTargets targetId = TargetPackageNamed pkgName targetFilter
        where
          targetUnit = Map.findWithDefault (error "cannot find target unit") targetId planMap
          PackageIdentifier{..} = packageId targetUnit

      targets' = fmap gatherTargets targetIds

      hackagePkgs :: [PackageSpecifier UnresolvedSourcePackage]
      hackagePkgs = flip NamedPackage [] <$> hackageNames

      hackageTargets :: [TargetSelector]
      hackageTargets =
        flip TargetPackageNamed targetFilter <$> hackageNames

    createDirectoryIfMissing True (distSdistDirectory localDistDirLayout)

    unless (Map.null targets) $ for_ (localPackages localBaseCtx) $ \lpkg -> case lpkg of
      SpecificSourcePackage pkg ->
        packageToSdist
          verbosity
          (distProjectRootDirectory localDistDirLayout)
          TarGzArchive
          (distSdistFile localDistDirLayout (packageId pkg))
          pkg
      NamedPackage pkgName _ -> error $ "Got NamedPackage " ++ prettyShow pkgName

    if null targets
      then return (hackagePkgs, hackageTargets)
      else return (local ++ hackagePkgs, targets' ++ hackageTargets)

-- | Partitions the target selectors into known local targets and hackage packages.
partitionToKnownTargetsAndHackagePackages
  :: Verbosity
  -> SourcePackageDb
  -> ElaboratedInstallPlan
  -> [TargetSelector]
  -> IO (TargetsMap, [PackageName])
partitionToKnownTargetsAndHackagePackages verbosity pkgDb elaboratedPlan targetSelectors = do
  let mTargets =
        resolveTargets
          selectPackageTargets
          selectComponentTarget
          elaboratedPlan
          (Just pkgDb)
          targetSelectors
  case mTargets of
    Right targets ->
      -- Everything is a local dependency.
      return (targets, [])
    Left errs -> do
      -- Not everything is local.
      let
        (errs', hackageNames) = partitionEithers . flip fmap errs $ \case
          TargetAvailableInIndex name -> Right name
          err -> Left err

      -- report incorrect case for known package.
      for_ errs' $ \case
        TargetNotInProject hn ->
          case searchByName (packageIndex pkgDb) (unPackageName hn) of
            [] -> return ()
            xs ->
              dieWithException verbosity $ UnknownPackage (unPackageName hn) (("- " ++) . unPackageName . fst <$> xs)
        _ -> return ()

      when (not . null $ errs') $ reportBuildTargetProblems verbosity errs'

      let
        targetSelectors' = flip filter targetSelectors $ \case
          TargetComponentUnknown name _ _
            | name `elem` hackageNames -> False
          TargetPackageNamed name _
            | name `elem` hackageNames -> False
          _ -> True

      -- This can't fail, because all of the errors are
      -- removed (or we've given up).
      targets <-
        either (reportBuildTargetProblems verbosity) return $
          resolveTargets
            selectPackageTargets
            selectComponentTarget
            elaboratedPlan
            Nothing
            targetSelectors'

      return (targets, hackageNames)

constructProjectBuildContext
  :: Verbosity
  -> ProjectBaseContext
  -- ^ The synthetic base context to use to produce the full build context.
  -> [TargetSelector]
  -> IO ProjectBuildContext
constructProjectBuildContext verbosity baseCtx targetSelectors = do
  runProjectPreBuildPhase verbosity baseCtx $ \elaboratedPlan -> do
    -- Interpret the targets on the command line as build targets
    targets <-
      either (reportBuildTargetProblems verbosity) return $
        resolveTargets
          selectPackageTargets
          selectComponentTarget
          elaboratedPlan
          Nothing
          targetSelectors

    let prunedToTargetsElaboratedPlan =
          pruneInstallPlanToTargets TargetActionBuild targets elaboratedPlan
    prunedElaboratedPlan <-
      if buildSettingOnlyDeps (buildSettings baseCtx)
        then
          either (reportCannotPruneDependencies verbosity) return $
            pruneInstallPlanToDependencies
              (Map.keysSet targets)
              prunedToTargetsElaboratedPlan
        else return prunedToTargetsElaboratedPlan

    return (prunedElaboratedPlan, targets)

-- | Install any built exe by symlinking/copying it
-- we don't use BuildOutcomes because we also need the component names
installExes
  :: Verbosity
  -> ProjectBaseContext
  -> ProjectBuildContext
  -> Platform
  -> Compiler
  -> ConfigFlags
  -> ClientInstallFlags
  -> IO ()
installExes
  verbosity
  baseCtx
  buildCtx
  platform
  compiler
  configFlags
  clientInstallFlags = do
    installPath <- defaultInstallPath
    let storeDirLayout = cabalStoreDirLayout $ cabalDirLayout baseCtx

        prefix = fromFlagOrDefault "" (fmap InstallDirs.fromPathTemplate (configProgPrefix configFlags))
        suffix = fromFlagOrDefault "" (fmap InstallDirs.fromPathTemplate (configProgSuffix configFlags))

        mkUnitBinDir :: UnitId -> FilePath
        mkUnitBinDir =
          InstallDirs.bindir
            . storePackageInstallDirs' storeDirLayout (compilerId compiler)

        mkExeName :: UnqualComponentName -> FilePath
        mkExeName exe = unUnqualComponentName exe <.> exeExtension platform

        mkFinalExeName :: UnqualComponentName -> FilePath
        mkFinalExeName exe = prefix <> unUnqualComponentName exe <> suffix <.> exeExtension platform
        installdirUnknown =
          "installdir is not defined. Set it in your cabal config file "
            ++ "or use --installdir=<path>. Using default installdir: "
            ++ show installPath

    installdir <-
      fromFlagOrDefault
        (warn verbosity installdirUnknown >> pure installPath)
        $ pure <$> cinstInstalldir clientInstallFlags
    createDirectoryIfMissingVerbose verbosity True installdir
    warnIfNoExes verbosity buildCtx

    installMethod <-
      flagElim defaultMethod return $
        cinstInstallMethod clientInstallFlags

    let
      doInstall =
        installUnitExes
          verbosity
          overwritePolicy
          mkUnitBinDir
          mkExeName
          mkFinalExeName
          installdir
          installMethod
     in
      traverse_ doInstall $ Map.toList $ targetsMap buildCtx
    where
      overwritePolicy =
        fromFlagOrDefault NeverOverwrite $
          cinstOverwritePolicy clientInstallFlags
      isWindows = buildOS == Windows

      -- This is in IO as we will make environment checks,
      -- to decide which method is best
      defaultMethod :: IO InstallMethod
      defaultMethod
        -- Try symlinking in temporary directory, if it works default to
        -- symlinking even on windows
        | isWindows = do
            symlinks <- trySymlink verbosity
            return $ if symlinks then InstallMethodSymlink else InstallMethodCopy
        | otherwise = return InstallMethodSymlink

-- | Install any built library by adding it to the default ghc environment
installLibraries
  :: Verbosity
  -> ProjectBuildContext
  -> PI.PackageIndex InstalledPackageInfo
  -> Compiler
  -> PackageDBStack
  -> FilePath
  -- ^ Environment file
  -> [GhcEnvironmentFileEntry]
  -> IO ()
installLibraries
  verbosity
  buildCtx
  installedIndex
  compiler
  packageDbs'
  envFile
  envEntries = do
    if supportsPkgEnvFiles $ getImplInfo compiler
      then do
        let validDb (SpecificPackageDB fp) = doesPathExist fp
            validDb _ = pure True
        -- if a user "installs" a global package and no existing cabal db exists, none will be created.
        -- this ensures we don't add the "phantom" path to the file.
        packageDbs <- filterM validDb packageDbs'
        let
          getLatest =
            (=<<) (maybeToList . safeHead . snd)
              . take 1
              . sortBy (comparing (Down . fst))
              . PI.lookupPackageName installedIndex
          globalLatest = concat (getLatest <$> globalPackages)
          globalEntries = GhcEnvFilePackageId . installedUnitId <$> globalLatest
          baseEntries =
            GhcEnvFileClearPackageDbStack : fmap GhcEnvFilePackageDb packageDbs
          pkgEntries =
            ordNub $
              globalEntries
                ++ envEntries
                ++ entriesForLibraryComponents (targetsMap buildCtx)
          contents' = renderGhcEnvironmentFile (baseEntries ++ pkgEntries)
        createDirectoryIfMissing True (takeDirectory envFile)
        writeFileAtomic envFile (BS.pack contents')
      else
        warn verbosity $
          "The current compiler doesn't support safely installing libraries, "
            ++ "so only executables will be available. (Library installation is "
            ++ "supported on GHC 8.0+ only)"

-- See ticket #8894. This is safe to include any nonreinstallable boot pkg,
-- but the particular package users will always expect to be in scope without specific installation
-- is base, so that they can access prelude, regardles of if they specifically asked for it.
globalPackages :: [PackageName]
globalPackages = mkPackageName <$> ["base"]

warnIfNoExes :: Verbosity -> ProjectBuildContext -> IO ()
warnIfNoExes verbosity buildCtx =
  when noExes $
    warn verbosity $
      "\n"
        <> "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n"
        <> "@ WARNING: Installation might not be completed as desired! @\n"
        <> "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n"
        <> "The command \"cabal install [TARGETS]\" doesn't expose libraries.\n"
        <> "* You might have wanted to add them as dependencies to your package."
        <> " In this case add \""
        <> intercalate ", " (showTargetSelector <$> selectors)
        <> "\" to the build-depends field(s) of your package's .cabal file.\n"
        <> "* You might have wanted to add them to a GHC environment. In this case"
        <> " use \"cabal install --lib "
        <> unwords (showTargetSelector <$> selectors)
        <> "\". "
        <> " The \"--lib\" flag is provisional: see"
        <> " https://github.com/haskell/cabal/issues/6481 for more information."
  where
    targets = concat $ Map.elems $ targetsMap buildCtx
    components = fst <$> targets
    selectors = concatMap (NE.toList . snd) targets
    noExes = null $ catMaybes $ exeMaybe <$> components

    exeMaybe (ComponentTarget (CExeName exe) _) = Just exe
    exeMaybe _ = Nothing

-- | Return the package specifiers and non-global environment file entries.
getEnvSpecsAndNonGlobalEntries
  :: PI.InstalledPackageIndex
  -> [GhcEnvironmentFileEntry]
  -> Bool
  -> ([PackageSpecifier a], [(PackageName, GhcEnvironmentFileEntry)])
getEnvSpecsAndNonGlobalEntries installedIndex entries installLibs =
  if installLibs
    then (envSpecs, envEntries')
    else ([], envEntries')
  where
    (envSpecs, envEntries') = environmentFileToSpecifiers installedIndex entries

environmentFileToSpecifiers
  :: PI.InstalledPackageIndex
  -> [GhcEnvironmentFileEntry]
  -> ([PackageSpecifier a], [(PackageName, GhcEnvironmentFileEntry)])
environmentFileToSpecifiers ipi = foldMap $ \case
  (GhcEnvFilePackageId unitId)
    | Just
        InstalledPackageInfo
          { sourcePackageId = PackageIdentifier{..}
          , installedUnitId
          } <-
        PI.lookupUnitId ipi unitId
    , let pkgSpec =
            NamedPackage
              pkgName
              [PackagePropertyVersion (thisVersion pkgVersion)] ->
        ([pkgSpec], [(pkgName, GhcEnvFilePackageId installedUnitId)])
  _ -> ([], [])

-- | Disables tests and benchmarks if they weren't explicitly enabled.
disableTestsBenchsByDefault :: ConfigFlags -> ConfigFlags
disableTestsBenchsByDefault configFlags =
  configFlags
    { configTests = Flag False <> configTests configFlags
    , configBenchmarks = Flag False <> configBenchmarks configFlags
    }

-- | Symlink/copy every exe from a package from the store to a given location
installUnitExes
  :: Verbosity
  -> OverwritePolicy
  -- ^ Whether to overwrite existing files
  -> (UnitId -> FilePath)
  -- ^ A function to get an UnitId's
  -- ^ store directory
  -> (UnqualComponentName -> FilePath)
  -- ^ A function to get an
  -- ^ exe's filename
  -> (UnqualComponentName -> FilePath)
  -- ^ A function to get an
  -- ^ exe's final possibly
  -- ^ different to the name in the store.
  -> FilePath
  -> InstallMethod
  -> ( UnitId
     , [(ComponentTarget, NonEmpty TargetSelector)]
     )
  -> IO ()
installUnitExes
  verbosity
  overwritePolicy
  mkSourceBinDir
  mkExeName
  mkFinalExeName
  installdir
  installMethod
  (unit, components) =
    traverse_ installAndWarn exes
    where
      exes = catMaybes $ (exeMaybe . fst) <$> components
      exeMaybe (ComponentTarget (CExeName exe) _) = Just exe
      exeMaybe _ = Nothing
      installAndWarn exe = do
        success <-
          installBuiltExe
            verbosity
            overwritePolicy
            (mkSourceBinDir unit)
            (mkExeName exe)
            (mkFinalExeName exe)
            installdir
            installMethod
        let errorMessage = case overwritePolicy of
              NeverOverwrite ->
                "Path '"
                  <> (installdir </> prettyShow exe)
                  <> "' already exists. "
                  <> "Use --overwrite-policy=always to overwrite."
              -- This shouldn't even be possible, but we keep it in case
              -- symlinking/copying logic changes
              _ ->
                case installMethod of
                  InstallMethodSymlink -> "Symlinking"
                  InstallMethodCopy ->
                    "Copying" <> " '" <> prettyShow exe <> "' failed."
        unless success $ dieWithException verbosity $ InstallUnitExes errorMessage

-- | Install a specific exe.
installBuiltExe
  :: Verbosity
  -> OverwritePolicy
  -> FilePath
  -- ^ The directory where the built exe is located
  -> FilePath
  -- ^ The exe's filename
  -> FilePath
  -- ^ The exe's filename in the public install directory
  -> FilePath
  -- ^ the directory where it should be installed
  -> InstallMethod
  -> IO Bool
  -- ^ Whether the installation was successful
installBuiltExe
  verbosity
  overwritePolicy
  sourceDir
  exeName
  finalExeName
  installdir
  InstallMethodSymlink = do
    notice verbosity $ "Symlinking '" <> exeName <> "' to '" <> destination <> "'"
    symlinkBinary
      overwritePolicy
      installdir
      sourceDir
      finalExeName
      exeName
    where
      destination = installdir </> finalExeName
installBuiltExe
  verbosity
  overwritePolicy
  sourceDir
  exeName
  finalExeName
  installdir
  InstallMethodCopy = do
    notice verbosity $ "Copying '" <> exeName <> "' to '" <> destination <> "'"
    exists <- doesPathExist destination
    case (exists, overwritePolicy) of
      (True, NeverOverwrite) -> pure False
      (True, AlwaysOverwrite) -> overwrite
      (True, PromptOverwrite) -> maybeOverwrite
      (False, _) -> copy
    where
      source = sourceDir </> exeName
      destination = installdir </> finalExeName
      remove = do
        isDir <- doesDirectoryExist destination
        if isDir
          then removeDirectory destination
          else removeFile destination
      copy = copyFile source destination >> pure True
      overwrite :: IO Bool
      overwrite = remove >> copy
      maybeOverwrite :: IO Bool
      maybeOverwrite =
        promptRun
          "Existing file found while installing executable. Do you want to overwrite that file? (y/n)"
          overwrite

-- | Create 'GhcEnvironmentFileEntry's for packages with exposed libraries.
entriesForLibraryComponents :: TargetsMap -> [GhcEnvironmentFileEntry]
entriesForLibraryComponents = Map.foldrWithKey' (\k v -> mappend (go k v)) []
  where
    hasLib :: (ComponentTarget, NonEmpty TargetSelector) -> Bool
    hasLib (ComponentTarget (CLibName _) _, _) = True
    hasLib _ = False

    go
      :: UnitId
      -> [(ComponentTarget, NonEmpty TargetSelector)]
      -> [GhcEnvironmentFileEntry]
    go unitId targets
      | any hasLib targets = [GhcEnvFilePackageId unitId]
      | otherwise = []

-- | Gets the file path to the request environment file.
getEnvFile :: ClientInstallFlags -> Platform -> Version -> IO FilePath
getEnvFile clientInstallFlags platform compilerVersion = do
  appDir <- getGhcAppDir
  case flagToMaybe (cinstEnvironmentPath clientInstallFlags) of
    Just spec
      -- Is spec a bare word without any "pathy" content, then it refers to
      -- a named global environment.
      | takeBaseName spec == spec ->
          return (getGlobalEnv appDir platform compilerVersion spec)
      | otherwise -> do
          spec' <- makeAbsolute spec
          isDir <- doesDirectoryExist spec'
          if isDir
            then -- If spec is a directory, then make an ambient environment inside
            -- that directory.
              return (getLocalEnv spec' platform compilerVersion)
            else -- Otherwise, treat it like a literal file path.
              return spec'
    Nothing ->
      return (getGlobalEnv appDir platform compilerVersion "default")

-- | Returns the list of @GhcEnvFilePackageIj@ values already existing in the
--   environment being operated on.
getExistingEnvEntries :: Verbosity -> CompilerFlavor -> Bool -> FilePath -> IO [GhcEnvironmentFileEntry]
getExistingEnvEntries verbosity compilerFlavor supportsPkgEnvFiles envFile = do
  envFileExists <- doesFileExist envFile
  filterEnvEntries
    <$> if (compilerFlavor == GHC || compilerFlavor == GHCJS)
      && supportsPkgEnvFiles
      && envFileExists
      then catch (readGhcEnvironmentFile envFile) $ \(_ :: ParseErrorExc) ->
        warn
          verbosity
          ( "The environment file "
              ++ envFile
              ++ " is unparsable. Libraries cannot be installed."
          )
          >> return []
      else return []
  where
    -- Why? We know what the first part will be, we only care about the packages.
    filterEnvEntries = filter $ \case
      GhcEnvFilePackageId _ -> True
      _ -> False

-- | Constructs the path to the global GHC environment file.
--
-- TODO(m-renaud): Create PkgEnvName newtype wrapper.
getGlobalEnv :: FilePath -> Platform -> Version -> String -> FilePath
getGlobalEnv appDir platform compilerVersion name =
  appDir
    </> ghcPlatformAndVersionString platform compilerVersion
    </> "environments"
    </> name

-- | Constructs the path to a local GHC environment file.
getLocalEnv :: FilePath -> Platform -> Version -> FilePath
getLocalEnv dir platform compilerVersion =
  dir
    </> ".ghc.environment."
    <> ghcPlatformAndVersionString platform compilerVersion

getPackageDbStack
  :: CompilerId
  -> Flag FilePath
  -> Flag FilePath
  -> IO PackageDBStack
getPackageDbStack compilerId storeDirFlag logsDirFlag = do
  mstoreDir <- traverse makeAbsolute $ flagToMaybe storeDirFlag
  let
    mlogsDir = flagToMaybe logsDirFlag
  cabalLayout <- mkCabalDirLayout mstoreDir mlogsDir
  pure $ storePackageDBStack (cabalStoreDirLayout cabalLayout) compilerId

-- | This defines what a 'TargetSelector' means for the @bench@ command.
-- It selects the 'AvailableTarget's that the 'TargetSelector' refers to,
-- or otherwise classifies the problem.
--
-- For the @build@ command select all components except non-buildable
-- and disabled tests\/benchmarks, fail if there are no such
-- components
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
-- selected.
--
-- For the @build@ command we just need the basic checks on being buildable etc.
selectComponentTarget
  :: SubComponentTarget
  -> AvailableTarget k
  -> Either TargetProblem' k
selectComponentTarget = selectComponentTargetBasic

reportBuildTargetProblems :: Verbosity -> [TargetProblem'] -> IO a
reportBuildTargetProblems verbosity problems = reportTargetProblems verbosity "build" problems

reportCannotPruneDependencies :: Verbosity -> CannotPruneDependencies -> IO a
reportCannotPruneDependencies verbosity =
  dieWithException verbosity . SelectComponentTargetError . renderCannotPruneDependencies
