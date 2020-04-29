{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

-- | cabal-install CLI command: install
--
module Distribution.Client.CmdInstall (
    -- * The @install@ CLI and action
    installCommand,
    installAction,

    -- * Internals exposed for testing
    TargetProblem(..),
    selectPackageTargets,
    selectComponentTarget,
    -- * Internals exposed for CmdRepl + CmdRun
    establishDummyDistDirLayout,
    establishDummyProjectBaseContext
  ) where

import Prelude ()
import Distribution.Client.Compat.Prelude
import Distribution.Compat.Directory
         ( doesPathExist )

import Distribution.Client.ProjectOrchestration
import Distribution.Client.CmdSdist

import Distribution.Client.CmdErrorMessages
       ( renderTargetProblemCommon
       , renderTargetProblemNoneEnabled
       , renderTargetProblemNoTargets )
import Distribution.Client.CmdInstall.ClientInstallFlags
import Distribution.Client.CmdInstall.ClientInstallTargetSelector
--import Distribution.Client.CmdInstall.TargetProblem
--         ( TargetProblem(..), reportTargetProblems )
import qualified Distribution.Client.CmdEnv.Install as EnvInstall
         ( installLibraries )
import qualified Distribution.Client.CmdEnv.Utils as EnvUtils
         ( environmentFileToSpecifiers )
import qualified Distribution.Client.CmdInstall.Utils as InstallUtils
         ( reportCannotPruneDependencies, warnIfNoExes )

import Distribution.Client.Setup
         ( GlobalFlags(..), ConfigFlags(..), ConfigExFlags, InstallFlags(..)
         , configureExOptions, haddockOptions, installOptions, testOptions
         , benchmarkOptions, configureOptions, liftOptions )
import Distribution.Solver.Types.ConstraintSource
         ( ConstraintSource(..) )
import Distribution.Client.Types
         ( PackageSpecifier(..), PackageLocation(..), UnresolvedSourcePackage
         , SourcePackageDb(..) )
import qualified Distribution.Client.InstallPlan as InstallPlan
import Distribution.Package
         ( Package(..), unPackageName )
import Distribution.Types.PackageId
         ( PackageIdentifier(..) )
import Distribution.Client.ProjectConfig
         ( ProjectPackageLocation(..)
         , fetchAndReadSourcePackages
         )
import Distribution.Client.ProjectConfig.Types
         ( ProjectConfig(..), ProjectConfigShared(..)
         , ProjectConfigBuildOnly(..), PackageConfig(..)
         , getMapLast, getMapMappend, projectConfigLogsDir
         , projectConfigStoreDir, projectConfigBuildOnly
         , projectConfigConfigFile )
import Distribution.Simple.Program.Db
         ( userSpecifyPaths, userSpecifyArgss, defaultProgramDb
         , modifyProgramSearchPath )
import Distribution.Simple.BuildPaths
         ( exeExtension )
import Distribution.Simple.Program.Find
         ( ProgramSearchPathEntry(..) )
import Distribution.Client.Config
         ( defaultInstallPath, getCabalDir, loadConfig, SavedConfig(..) )
import Distribution.Solver.Types.PackageIndex
         ( lookupPackageName, searchByName )
import Distribution.Types.Version
         ( nullVersion )
import Distribution.Types.VersionRange
         ( thisVersion )
import Distribution.Solver.Types.PackageConstraint
         ( PackageProperty(..) )
import Distribution.Client.IndexUtils
         ( getSourcePackages, getInstalledPackages )
import Distribution.Client.ProjectConfig
         ( projectConfigWithBuilderRepoContext
         , resolveBuildTimeSettings, withProjectOrGlobalConfigIgn )
import Distribution.Client.ProjectPlanning
         ( storePackageInstallDirs' )
import qualified Distribution.Simple.InstallDirs as InstallDirs
import Distribution.Client.DistDirLayout
         ( DistDirLayout(..), mkCabalDirLayout
         , cabalStoreDirLayout
         , CabalDirLayout(..), StoreDirLayout(..) )
import Distribution.Client.RebuildMonad
         ( runRebuild )
import Distribution.Client.InstallSymlink
         ( OverwritePolicy(..), symlinkBinary, trySymlink )
import Distribution.Simple.Flag
         ( fromFlagOrDefault, flagToMaybe, flagElim )
import Distribution.Simple.Setup
         ( Flag(..), HaddockFlags, TestFlags, BenchmarkFlags )
import Distribution.Solver.Types.SourcePackage
         ( SourcePackage(..) )
import Distribution.Simple.Command
         ( CommandUI(..), OptionField(..), usageAlternatives )
import Distribution.Simple.Configure
         ( configCompilerEx )
import Distribution.Simple.Compiler
         ( Compiler(..), CompilerId(..), CompilerFlavor(..) )
import Distribution.Simple.GHC
         ( ghcPlatformAndVersionString
         , GhcImplInfo(..), getImplInfo
         , GhcEnvironmentFileEntry(..)
         , readGhcEnvironmentFile, ParseErrorExc )
import Distribution.System
         ( Platform , buildOS, OS (Windows) )
import Distribution.Types.UnitId
         ( UnitId )
import Distribution.Types.UnqualComponentName
         ( UnqualComponentName, unUnqualComponentName )
import Distribution.Verbosity
         ( Verbosity, normal, lessVerbose )
import Distribution.Simple.Utils
         ( wrapText, die', notice, warn
         , withTempDirectory, createDirectoryIfMissingVerbose )
import Distribution.Deprecated.Text
         ( simpleParse )
import Distribution.Pretty
         ( prettyShow )

import Control.Exception
         ( catch )
import Control.Monad
         ( mapM, forM_ )
import Data.Either
         ( partitionEithers )
import qualified Data.Map as Map
import Distribution.Utils.NubList
         ( fromNubList )
import Network.URI (URI)
import System.Directory
         ( getHomeDirectory, doesFileExist, createDirectoryIfMissing
         , getTemporaryDirectory, makeAbsolute, doesDirectoryExist
         , removeFile, removeDirectory, copyFile )
import System.FilePath
         ( (</>), (<.>), takeBaseName )

installCommand :: CommandUI ( ConfigFlags, ConfigExFlags, InstallFlags
                            , HaddockFlags, TestFlags, BenchmarkFlags
                            , ClientInstallFlags
                            )
installCommand = CommandUI
  { commandName         = "v2-install"
  , commandSynopsis     = "Install packages."
  , commandUsage        = usageAlternatives
                          "v2-install" [ "[TARGETS] [FLAGS]" ]
  , commandDescription  = Just $ \_ -> wrapText $
    "Installs one or more packages. This is done by installing them "
    ++ "in the store and symlinking/copying the executables in the directory "
    ++ "specified by the --installdir flag (`~/.cabal/bin/` by default). "
    ++ "If you want the installed executables to be available globally, "
    ++ "make sure that the PATH environment variable contains that directory. "
    ++ "\n\n"
    ++ "If TARGET is a library, it will be added to the global environment. "
    ++ "When doing this, cabal will try to build a plan that includes all "
    ++ "the previously installed libraries. This is currently not implemented."
  , commandNotes        = Just $ \pname ->
      "Examples:\n"
      ++ "  " ++ pname ++ " v2-install\n"
      ++ "    Install the package in the current directory\n"
      ++ "  " ++ pname ++ " v2-install pkgname\n"
      ++ "    Install the package named pkgname"
      ++ " (fetching it from hackage if necessary)\n"
      ++ "  " ++ pname ++ " v2-install ./pkgfoo\n"
      ++ "    Install the package in the ./pkgfoo directory\n"

      ++ cmdCommonHelpTextNewBuildBeta
  , commandOptions      = \showOrParseArgs ->
        liftOptions get1 set1
        -- Note: [Hidden Flags]
        -- hide "constraint", "dependency", and
        -- "exact-configuration" from the configure options.
        (filter ((`notElem` ["constraint", "dependency"
                            , "exact-configuration"])
                 . optionName) $ configureOptions showOrParseArgs)
     ++ liftOptions get2 set2 (configureExOptions showOrParseArgs
                               ConstraintSourceCommandlineFlag)
     ++ liftOptions get3 set3
        -- hide "target-package-db" and "symlink-bindir" flags from the
        -- install options.
        -- "symlink-bindir" is obsoleted by "installdir" in ClientInstallFlags
        (filter ((`notElem` ["target-package-db", "symlink-bindir"])
                 . optionName) $
                               installOptions showOrParseArgs)
       ++ liftOptions get4 set4
          -- hide "verbose" and "builddir" flags from the
          -- haddock options.
          (filter ((`notElem` ["v", "verbose", "builddir"])
                  . optionName) $
                                haddockOptions showOrParseArgs)
     ++ liftOptions get5 set5 (testOptions showOrParseArgs)
     ++ liftOptions get6 set6 (benchmarkOptions showOrParseArgs)
     ++ liftOptions get7 set7 (clientInstallOptions showOrParseArgs)
  , commandDefaultFlags = ( mempty, mempty, mempty, mempty, mempty, mempty
                          , defaultClientInstallFlags )
  }
  where
    get1 (a,_,_,_,_,_,_) = a; set1 a (_,b,c,d,e,f,g) = (a,b,c,d,e,f,g)
    get2 (_,b,_,_,_,_,_) = b; set2 b (a,_,c,d,e,f,g) = (a,b,c,d,e,f,g)
    get3 (_,_,c,_,_,_,_) = c; set3 c (a,b,_,d,e,f,g) = (a,b,c,d,e,f,g)
    get4 (_,_,_,d,_,_,_) = d; set4 d (a,b,c,_,e,f,g) = (a,b,c,d,e,f,g)
    get5 (_,_,_,_,e,_,_) = e; set5 e (a,b,c,d,_,f,g) = (a,b,c,d,e,f,g)
    get6 (_,_,_,_,_,f,_) = f; set6 f (a,b,c,d,e,_,g) = (a,b,c,d,e,f,g)
    get7 (_,_,_,_,_,_,g) = g; set7 g (a,b,c,d,e,f,_) = (a,b,c,d,e,f,g)


-- | The @install@ command actually serves four different needs. It installs:
-- * exes:
--   For example a program from hackage. The behavior is similar to the old
--   install command, except that now conflicts between separate runs of the
--   command are impossible thanks to the store.
--   Exes are installed in the store like a normal dependency, then they are
--   symlinked/copied in the directory specified by --installdir.
--   To do this we need a dummy projectBaseContext containing the targets as
--   estra packages and using a temporary dist directory.
-- * libraries
--   Libraries install through a similar process, but using GHC environment
--   files instead of symlinks. This means that 'v2-install'ing libraries
--   only works on GHC >= 8.0.
--
-- For more details on how this works, see the module
-- "Distribution.Client.ProjectOrchestration"
--
installAction
  :: ( ConfigFlags, ConfigExFlags, InstallFlags
     , HaddockFlags, TestFlags, BenchmarkFlags
     , ClientInstallFlags)
  -> [String] -> GlobalFlags
  -> IO ()
installAction ( configFlags, configExFlags, installFlags
              , haddockFlags, testFlags, benchmarkFlags
              , clientInstallFlags' )
              targetStrings globalFlags = do
  -- We never try to build tests/benchmarks for remote packages.
  -- So we set them as disabled by default and error if they are explicitly
  -- enabled.
  when (configTests configFlags' == Flag True) $
    die' verbosity $ "--enable-tests was specified, but tests can't "
                  ++ "be enabled in a remote package"
  when (configBenchmarks configFlags' == Flag True) $
    die' verbosity $ "--enable-benchmarks was specified, but benchmarks can't "
                  ++ "be enabled in a remote package"

  -- We cannot use establishDummyProjectBaseContext to get these flags, since
  -- it requires one of them as an argument. Normal establishProjectBaseContext
  -- does not, and this is why this is done only for the install command
  clientInstallFlags <- do
    let configFileFlag = globalConfigFile globalFlags
    savedConfig <- loadConfig verbosity configFileFlag
    pure $ savedClientInstallFlags savedConfig `mappend` clientInstallFlags'

  let
    installLibs    = fromFlagOrDefault False (cinstInstallLibs clientInstallFlags)
    targetFilter   = if installLibs then Just LibKind else Just ExeKind
    targetStrings' = if null targetStrings then ["."] else targetStrings

    withProject :: IO ([PackageSpecifier UnresolvedSourcePackage], [URI], [TargetSelector], ProjectConfig)
    withProject = do
      let verbosity' = lessVerbose verbosity

      -- First, we need to learn about what's available to be installed.
      localBaseCtx <- establishProjectBaseContext verbosity'
                      cliConfig InstallCommand
      let localDistDirLayout = distDirLayout localBaseCtx
      pkgDb <- projectConfigWithBuilderRepoContext verbosity'
               (buildSettings localBaseCtx) (getSourcePackages verbosity)

      let
        (targetStrings'', packageIds) =
          partitionEithers .
          flip fmap targetStrings' $
          \str -> case simpleParse str of
            Just (pkgId :: PackageId)
              | pkgVersion pkgId /= nullVersion -> Right pkgId
            _                                   -> Left str
        packageSpecifiers =
          flip fmap packageIds $ \case
          PackageIdentifier{..}
            | pkgVersion == nullVersion -> NamedPackage pkgName []
            | otherwise                 -> NamedPackage pkgName
                                           [PackagePropertyVersion
                                            (thisVersion pkgVersion)]
        packageTargets =
          flip TargetPackageNamed targetFilter . pkgName <$> packageIds

      if null targetStrings'
        then return (packageSpecifiers, [], packageTargets, projectConfig localBaseCtx)
        else do
          targetSelectors <-
            either (reportTargetSelectorProblems verbosity) return
            =<< readTargetSelectors (localPackages localBaseCtx)
                                    Nothing targetStrings''

          (specs, selectors) <-
            withInstallPlan verbosity' localBaseCtx $ \elaboratedPlan _ -> do
            -- Split into known targets and hackage packages.
            (targets, hackageNames) <- case
              resolveTargets
                selectPackageTargets
                selectComponentTarget
                TargetProblemCommon
                elaboratedPlan
                (Just pkgDb)
                targetSelectors of
              Right targets ->
                -- Everything is a local dependency.
                return (targets, [])
              Left errs     -> do
                -- Not everything is local.
                let
                  (errs', hackageNames) = partitionEithers . flip fmap errs $ \case
                    TargetProblemCommon (TargetAvailableInIndex name) -> Right name
                    err -> Left err

                -- report incorrect case for known package.
                for_ errs' $ \case
                  TargetProblemCommon (TargetNotInProject hn) ->
                    case searchByName (packageIndex pkgDb) (unPackageName hn) of
                      [] -> return ()
                      xs -> die' verbosity . concat $
                        [ "Unknown package \"", unPackageName hn, "\". "
                        , "Did you mean any of the following?\n"
                        , unlines (("- " ++) . unPackageName . fst <$> xs)
                        ]
                  _ -> return ()

                when (not . null $ errs') $ reportTargetProblems verbosity errs'

                let
                  targetSelectors' = flip filter targetSelectors $ \case
                    TargetComponentUnknown name _ _
                      | name `elem` hackageNames -> False
                    TargetPackageNamed name _
                      | name `elem` hackageNames -> False
                    _                            -> True

                -- This can't fail, because all of the errors are
                -- removed (or we've given up).
                targets <-
                  either (reportTargetProblems verbosity) return $
                  resolveTargets
                    selectPackageTargets
                    selectComponentTarget
                    TargetProblemCommon
                    elaboratedPlan
                    Nothing
                    targetSelectors'

                return (targets, hackageNames)

            let
              planMap = InstallPlan.toMap elaboratedPlan
              targetIds = Map.keys targets

              sdistize (SpecificSourcePackage spkg@SourcePackage{..}) =
                SpecificSourcePackage spkg'
                where
                  sdistPath = distSdistFile localDistDirLayout packageInfoId
                  spkg' = spkg { packageSource = LocalTarballPackage sdistPath }
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

            unless (Map.null targets) $ forM_ (localPackages localBaseCtx) $ \lpkg -> case lpkg of
                SpecificSourcePackage pkg -> packageToSdist verbosity
                  (distProjectRootDirectory localDistDirLayout) TarGzArchive
                  (distSdistFile localDistDirLayout (packageId pkg)) pkg
                NamedPackage pkgName _ -> error $ "Got NamedPackage " ++ prettyShow pkgName

            if null targets
              then return (hackagePkgs, hackageTargets)
              else return (local ++ hackagePkgs, targets' ++ hackageTargets)

          return ( specs ++ packageSpecifiers
                 , []
                 , selectors ++ packageTargets
                 , projectConfig localBaseCtx )

    withoutProject :: ProjectConfig -> IO ([PackageSpecifier pkg], [URI], [TargetSelector], ProjectConfig)
    withoutProject globalConfig = do
      tss <- mapM (parseWithoutProjectTargetSelector verbosity) targetStrings'

      cabalDir <- getCabalDir
      let
        projectConfig = globalConfig <> cliConfig

        ProjectConfigBuildOnly {
          projectConfigLogsDir
        } = projectConfigBuildOnly projectConfig

        ProjectConfigShared {
          projectConfigStoreDir
        } = projectConfigShared projectConfig

        mlogsDir = flagToMaybe projectConfigLogsDir
        mstoreDir = flagToMaybe projectConfigStoreDir
        cabalDirLayout = mkCabalDirLayout cabalDir mstoreDir mlogsDir

        buildSettings = resolveBuildTimeSettings
                          verbosity cabalDirLayout
                          projectConfig

      SourcePackageDb { packageIndex } <- projectConfigWithBuilderRepoContext
                                            verbosity buildSettings
                                            (getSourcePackages verbosity)

      for_ (concatMap woPackageNames tss) $ \name -> do
        when (null (lookupPackageName packageIndex name)) $ do
          let xs = searchByName packageIndex (unPackageName name)
          let emptyIf True  _  = []
              emptyIf False zs = zs
          die' verbosity $ concat $
            [ "Unknown package \"", unPackageName name, "\". "
            ] ++ emptyIf (null xs)
            [ "Did you mean any of the following?\n"
            , unlines (("- " ++) . unPackageName . fst <$> xs)
            ]

      let
        (uris, packageSpecifiers) = partitionEithers $ map woPackageSpecifiers tss
        packageTargets            = map woPackageTargets tss

      return (packageSpecifiers, uris, packageTargets, projectConfig)

  let
    ignoreProject = fromFlagOrDefault False (cinstIgnoreProject clientInstallFlags)

  (specs, uris, selectors, config) <-
     withProjectOrGlobalConfigIgn ignoreProject verbosity globalConfigFlag withProject withoutProject

  home <- getHomeDirectory
  let
    ProjectConfig {
      projectConfigBuildOnly = ProjectConfigBuildOnly {
        projectConfigLogsDir
      },
      projectConfigShared = ProjectConfigShared {
        projectConfigHcFlavor,
        projectConfigHcPath,
        projectConfigHcPkg,
        projectConfigStoreDir
      },
      projectConfigLocalPackages = PackageConfig {
        packageConfigProgramPaths,
        packageConfigProgramArgs,
        packageConfigProgramPathExtra
      }
    } = config

    hcFlavor = flagToMaybe projectConfigHcFlavor
    hcPath   = flagToMaybe projectConfigHcPath
    hcPkg    = flagToMaybe projectConfigHcPkg

    -- ProgramDb with directly user specified paths
    preProgDb =
        userSpecifyPaths (Map.toList (getMapLast packageConfigProgramPaths))
      . userSpecifyArgss (Map.toList (getMapMappend packageConfigProgramArgs))
      . modifyProgramSearchPath
          (++ [ ProgramSearchPathDir dir
              | dir <- fromNubList packageConfigProgramPathExtra ])
      $ defaultProgramDb

  -- progDb is a program database with compiler tools configured properly
  (compiler@Compiler { compilerId =
    compilerId@(CompilerId compilerFlavor compilerVersion) }, platform, progDb) <-
      configCompilerEx hcFlavor hcPath hcPkg preProgDb verbosity

  let
    globalEnv name =
      home </> ".ghc" </> ghcPlatformAndVersionString platform compilerVersion
           </> "environments" </> name
    localEnv dir =
      dir </>
      ".ghc.environment." <> ghcPlatformAndVersionString platform compilerVersion

    GhcImplInfo{ supportsPkgEnvFiles } = getImplInfo compiler
    -- Why? We know what the first part will be, we only care about the packages.
    filterEnvEntries = filter $ \case
      GhcEnvFilePackageId _ -> True
      _                     -> False

  envFile <- case flagToMaybe (cinstEnvironmentPath clientInstallFlags) of
    Just spec
      -- Is spec a bare word without any "pathy" content, then it refers to
      -- a named global environment.
      | takeBaseName spec == spec -> return (globalEnv spec)
      | otherwise                 -> do
        spec' <- makeAbsolute spec
        isDir <- doesDirectoryExist spec'
        if isDir
          -- If spec is a directory, then make an ambient environment inside
          -- that directory.
          then return (localEnv spec')
          -- Otherwise, treat it like a literal file path.
          else return spec'
    Nothing                       -> return (globalEnv "default")

  envFileExists <- doesFileExist envFile
  envEntries <- filterEnvEntries <$> if
    (compilerFlavor == GHC || compilerFlavor == GHCJS)
      && supportsPkgEnvFiles && envFileExists
    then catch (readGhcEnvironmentFile envFile) $ \(_ :: ParseErrorExc) ->
      warn verbosity ("The environment file " ++ envFile ++
        " is unparsable. Libraries cannot be installed.") >> return []
    else return []

  cabalDir  <- getCabalDir
  mstoreDir <-
    sequenceA $ makeAbsolute <$> flagToMaybe projectConfigStoreDir
  let
    mlogsDir    = flagToMaybe projectConfigLogsDir
    cabalLayout = mkCabalDirLayout cabalDir mstoreDir mlogsDir
    packageDbs  = storePackageDBStack (cabalStoreDirLayout cabalLayout) compilerId

  installedIndex <- getInstalledPackages verbosity compiler packageDbs progDb

  let (envSpecs, envEntries') =
        EnvUtils.environmentFileToSpecifiers installedIndex envEntries

  -- Second, we need to use a fake project to let Cabal build the
  -- installables correctly. For that, we need a place to put a
  -- temporary dist directory.
  globalTmp <- getTemporaryDirectory

  -- if we are installing executables, we shouldn't take into account
  -- environment specifiers.
  let envSpecs' :: [PackageSpecifier a]
      envSpecs' | installLibs = envSpecs
                | otherwise   = []

  withTempDirectory verbosity globalTmp "cabal-install." $ \tmpDir -> do
    distDirLayout <- establishDummyDistDirLayout verbosity config tmpDir

    uriSpecs <- runRebuild tmpDir $ fetchAndReadSourcePackages
      verbosity
      distDirLayout
      (projectConfigShared config)
      (projectConfigBuildOnly config)
      [ ProjectPackageRemoteTarball uri | uri <- uris ]

    baseCtx <- establishDummyProjectBaseContext
                 verbosity
                 config
                 distDirLayout
                 (envSpecs' ++ specs ++ uriSpecs)
                 InstallCommand

    buildCtx <-
      runProjectPreBuildPhase verbosity baseCtx $ \elaboratedPlan -> do

            -- Interpret the targets on the command line as build targets
            targets <- either (reportTargetProblems verbosity) return
                     $ resolveTargets
                         selectPackageTargets
                         selectComponentTarget
                         TargetProblemCommon
                         elaboratedPlan
                         Nothing
                         selectors

            let elaboratedPlan' = pruneInstallPlanToTargets
                                    TargetActionBuild
                                    targets
                                    elaboratedPlan
            elaboratedPlan'' <-
              if buildSettingOnlyDeps (buildSettings baseCtx)
                then either (InstallUtils.reportCannotPruneDependencies verbosity) return $
                     pruneInstallPlanToDependencies (Map.keysSet targets)
                                                    elaboratedPlan'
                else return elaboratedPlan'

            return (elaboratedPlan'', targets)

    printPlan verbosity baseCtx buildCtx

    buildOutcomes <- runProjectBuildPhase verbosity baseCtx buildCtx
    runProjectPostBuildPhase verbosity baseCtx buildCtx buildOutcomes

    -- Now that we built everything we can do the installation part.
    -- First, figure out if / what parts we want to install:
    let
      dryRun = buildSettingDryRun $ buildSettings baseCtx

    -- Then, install!
    when (not dryRun) $
      if installLibs
      then EnvInstall.installLibraries verbosity
           buildCtx compiler packageDbs progDb envFile envEntries'
      else installExes verbosity
           baseCtx buildCtx platform compiler configFlags clientInstallFlags
  where
    configFlags' = disableTestsBenchsByDefault configFlags
    verbosity = fromFlagOrDefault normal (configVerbosity configFlags')
    cliConfig = commandLineFlagsToProjectConfig
                  globalFlags configFlags' configExFlags
                  installFlags clientInstallFlags'
                  haddockFlags testFlags benchmarkFlags
    globalConfigFlag = projectConfigConfigFile (projectConfigShared cliConfig)

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
installExes verbosity baseCtx buildCtx platform compiler
            configFlags clientInstallFlags = do
  installPath <- defaultInstallPath
  let storeDirLayout = cabalStoreDirLayout $ cabalDirLayout baseCtx

      prefix = fromFlagOrDefault "" (fmap InstallDirs.fromPathTemplate (configProgPrefix configFlags))
      suffix = fromFlagOrDefault "" (fmap InstallDirs.fromPathTemplate (configProgSuffix configFlags))

      mkUnitBinDir :: UnitId -> FilePath
      mkUnitBinDir =
        InstallDirs.bindir .
        storePackageInstallDirs' storeDirLayout (compilerId compiler)

      mkExeName :: UnqualComponentName -> FilePath
      mkExeName exe = unUnqualComponentName exe <.> exeExtension platform

      mkFinalExeName :: UnqualComponentName -> FilePath
      mkFinalExeName exe = prefix <> unUnqualComponentName exe <> suffix <.> exeExtension platform
      installdirUnknown =
        "installdir is not defined. Set it in your cabal config file "
        ++ "or use --installdir=<path>. Using default installdir: " ++ show installPath

  installdir <- fromFlagOrDefault
                (warn verbosity installdirUnknown >> pure installPath) $
                pure <$> cinstInstalldir clientInstallFlags
  createDirectoryIfMissingVerbose verbosity False installdir
  InstallUtils.warnIfNoExes verbosity buildCtx

  installMethod <- flagElim defaultMethod return $
    cinstInstallMethod clientInstallFlags

  let
    doInstall = installUnitExes
                  verbosity
                  overwritePolicy
                  mkUnitBinDir mkExeName mkFinalExeName
                  installdir installMethod
    in traverse_ doInstall $ Map.toList $ targetsMap buildCtx
  where
    overwritePolicy = fromFlagOrDefault NeverOverwrite $
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


-- | Disables tests and benchmarks if they weren't explicitly enabled.
disableTestsBenchsByDefault :: ConfigFlags -> ConfigFlags
disableTestsBenchsByDefault configFlags =
  configFlags { configTests = Flag False <> configTests configFlags
              , configBenchmarks = Flag False <> configBenchmarks configFlags }

-- | Symlink/copy every exe from a package from the store to a given location
installUnitExes
  :: Verbosity
  -> OverwritePolicy -- ^ Whether to overwrite existing files
  -> (UnitId -> FilePath) -- ^ A function to get an UnitId's
                          -- ^ store directory
  -> (UnqualComponentName -> FilePath) -- ^ A function to get an
                                       -- ^ exe's filename
  -> (UnqualComponentName -> FilePath) -- ^ A function to get an
                                       -- ^ exe's final possibly
                                       -- ^ different to the name in the store.
  -> FilePath
  -> InstallMethod
  -> ( UnitId
     , [(ComponentTarget, [TargetSelector])] )
  -> IO ()
installUnitExes verbosity overwritePolicy
                mkSourceBinDir mkExeName mkFinalExeName
                installdir installMethod
                (unit, components) =
  traverse_ installAndWarn exes
  where
    exes = catMaybes $ (exeMaybe . fst) <$> components
    exeMaybe (ComponentTarget (CExeName exe) _) = Just exe
    exeMaybe _ = Nothing
    installAndWarn exe = do
      success <- installBuiltExe
                   verbosity overwritePolicy
                   (mkSourceBinDir unit) (mkExeName exe)
                   (mkFinalExeName exe)
                   installdir installMethod
      let errorMessage = case overwritePolicy of
            NeverOverwrite ->
              "Path '" <> (installdir </> prettyShow exe) <> "' already exists. "
              <> "Use --overwrite-policy=always to overwrite."
            -- This shouldn't even be possible, but we keep it in case
            -- symlinking/copying logic changes
            AlwaysOverwrite ->
              case installMethod of
                InstallMethodSymlink -> "Symlinking"
                InstallMethodCopy    ->
                  "Copying" <> " '" <> prettyShow exe <> "' failed."
      unless success $ die' verbosity errorMessage

-- | Install a specific exe.
installBuiltExe
  :: Verbosity -> OverwritePolicy
  -> FilePath -- ^ The directory where the built exe is located
  -> FilePath -- ^ The exe's filename
  -> FilePath -- ^ The exe's filename in the public install directory
  -> FilePath -- ^ the directory where it should be installed
  -> InstallMethod
  -> IO Bool -- ^ Whether the installation was successful
installBuiltExe verbosity overwritePolicy
                sourceDir exeName finalExeName
                installdir InstallMethodSymlink = do
  notice verbosity $ "Symlinking '" <> exeName <> "' to '" <> destination <> "'"
  symlinkBinary
    overwritePolicy
    installdir
    sourceDir
    finalExeName
    exeName
  where
    destination = installdir </> finalExeName
installBuiltExe verbosity overwritePolicy
                sourceDir exeName finalExeName
                installdir InstallMethodCopy = do
  notice verbosity $ "Copying '" <> exeName <> "' to '" <> destination <> "'"
  exists <- doesPathExist destination
  case (exists, overwritePolicy) of
    (True , NeverOverwrite ) -> pure False
    (True , AlwaysOverwrite) -> remove >> copy
    (False, _              ) -> copy
  where
    source      = sourceDir </> exeName
    destination = installdir </> finalExeName
    remove = do
      isDir <- doesDirectoryExist destination
      if isDir
      then removeDirectory destination
      else removeFile      destination
    copy = copyFile source destination >> pure True

-- | This defines what a 'TargetSelector' means for the @bench@ command.
-- It selects the 'AvailableTarget's that the 'TargetSelector' refers to,
-- or otherwise classifies the problem.
--
-- For the @build@ command select all components except non-buildable
-- and disabled tests\/benchmarks, fail if there are no such
-- components
--
-- TODO(m-renaud): Find a better home for this, the docs reference @bench@ and @build@,
-- neither of which are related to install so it was likely copied from
-- elsewhere.
selectPackageTargets
  :: TargetSelector
  -> [AvailableTarget k] -> Either TargetProblem [k]
selectPackageTargets targetSelector targets

    -- If there are any buildable targets then we select those
  | not (null targetsBuildable)
  = Right targetsBuildable

    -- If there are targets but none are buildable then we report those
  | not (null targets)
  = Left (TargetProblemNoneEnabled targetSelector targets')

    -- If there are no targets at all then we report that
  | otherwise
  = Left (TargetProblemNoTargets targetSelector)
  where
    targets'         = forgetTargetsDetail targets
    targetsBuildable = selectBuildableTargetsWith
                         (buildable targetSelector)
                         targets

    -- When there's a target filter like "pkg:tests" then we do select tests,
    -- but if it's just a target like "pkg" then we don't build tests unless
    -- they are requested by default (i.e. by using --enable-tests)
    buildable (TargetPackage _ _  Nothing) TargetNotRequestedByDefault = False
    buildable (TargetAllPackages  Nothing) TargetNotRequestedByDefault = False
    buildable _ _ = True

-- | For a 'TargetComponent' 'TargetSelector', check if the component can be
-- selected.
--
-- For the @build@ command we just need the basic checks on being buildable etc.
--
selectComponentTarget
  :: SubComponentTarget
  -> AvailableTarget k -> Either TargetProblem k
selectComponentTarget subtarget =
    either (Left . TargetProblemCommon) Right
  . selectComponentTargetBasic subtarget


-- | The various error conditions that can occur when matching a
-- 'TargetSelector' against 'AvailableTarget's for the @build@ command.
--
data TargetProblem =
     TargetProblemCommon       TargetProblemCommon

     -- | The 'TargetSelector' matches targets but none are buildable
   | TargetProblemNoneEnabled TargetSelector [AvailableTarget ()]

     -- | There are no targets at all
   | TargetProblemNoTargets   TargetSelector
  deriving (Eq, Show)

reportTargetProblems :: Verbosity -> [TargetProblem] -> IO a
reportTargetProblems verbosity =
    die' verbosity . unlines . map renderTargetProblem

renderTargetProblem :: TargetProblem -> String
renderTargetProblem (TargetProblemCommon problem) =
    renderTargetProblemCommon "build" problem
renderTargetProblem (TargetProblemNoneEnabled targetSelector targets) =
    renderTargetProblemNoneEnabled "build" targetSelector targets
renderTargetProblem(TargetProblemNoTargets targetSelector) =
    renderTargetProblemNoTargets "build" targetSelector
