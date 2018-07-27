{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

-- | cabal-install CLI command: build
--
module Distribution.Client.CmdInstall (
    -- * The @build@ CLI and action
    installCommand,
    installAction,

    -- * Internals exposed for testing
    TargetProblem(..),
    selectPackageTargets,
    selectComponentTarget
  ) where

import Prelude ()
import Distribution.Client.Compat.Prelude

import Distribution.Client.ProjectOrchestration
import Distribution.Client.CmdErrorMessages
import Distribution.Client.CmdSdist

import Distribution.Client.Setup
         ( GlobalFlags(..), ConfigFlags(..), ConfigExFlags, InstallFlags
         , configureExOptions, installOptions, liftOptions )
import Distribution.Solver.Types.ConstraintSource
         ( ConstraintSource(..) )
import Distribution.Client.Types
         ( PackageSpecifier(..), PackageLocation(..), UnresolvedSourcePackage )
import qualified Distribution.Client.InstallPlan as InstallPlan
import Distribution.Package
         ( Package(..), PackageName, mkPackageName )
import Distribution.Types.PackageId
         ( PackageIdentifier(..) )
import Distribution.Client.ProjectConfig.Types
         ( ProjectConfig(..), ProjectConfigShared(..)
         , ProjectConfigBuildOnly(..), PackageConfig(..)
         , getMapLast, getMapMappend, projectConfigLogsDir
         , projectConfigStoreDir, projectConfigBuildOnly
         , projectConfigDistDir, projectConfigConfigFile )
import Distribution.Simple.Program.Db
         ( userSpecifyPaths, userSpecifyArgss, defaultProgramDb
         , modifyProgramSearchPath )
import Distribution.Simple.Program.Find
         ( ProgramSearchPathEntry(..) )
import Distribution.Client.Config
         ( getCabalDir )
import Distribution.Simple.PackageIndex
         ( InstalledPackageIndex, lookupPackageName, lookupUnitId )
import Distribution.Types.InstalledPackageInfo
         ( InstalledPackageInfo(..) )
import Distribution.Types.Version
         ( nullVersion )
import Distribution.Types.VersionRange
         ( thisVersion )
import Distribution.Solver.Types.PackageConstraint
         ( PackageProperty(..) )
import Distribution.Client.IndexUtils
         ( getSourcePackages, getInstalledPackages )
import Distribution.Client.ProjectConfig
         ( readGlobalConfig, projectConfigWithBuilderRepoContext
         , resolveBuildTimeSettings
         , BadPackageLocations(..), BadPackageLocation(..)
         , ProjectConfigProvenance(..) )
import Distribution.Client.DistDirLayout
         ( defaultDistDirLayout, DistDirLayout(..), mkCabalDirLayout
         , ProjectRoot(ProjectRootImplicit)
         , storePackageDirectory, cabalStoreDirLayout
         , CabalDirLayout(..), StoreDirLayout(..) )
import Distribution.Client.RebuildMonad
         ( runRebuild )
import Distribution.Client.InstallSymlink
         ( symlinkBinary )
import Distribution.Simple.Setup
         ( Flag(Flag), HaddockFlags, fromFlagOrDefault, flagToMaybe, toFlag
         , trueArg, configureOptions, haddockOptions, flagToList )
import Distribution.Solver.Types.SourcePackage
         ( SourcePackage(..) )
import Distribution.ReadE
         ( succeedReadE )     
import Distribution.Simple.Command
         ( CommandUI(..), ShowOrParseArgs(..), OptionField(..)
         , option, usageAlternatives, reqArg )
import Distribution.Simple.Configure
         ( configCompilerEx )
import Distribution.Simple.Compiler
         ( Compiler(..), CompilerId(..), CompilerFlavor(..) )
import Distribution.Simple.GHC
         ( ghcPlatformAndVersionString 
         , GhcImplInfo(..), getImplInfo
         , GhcEnvironmentFileEntry(..)
         , renderGhcEnvironmentFile, readGhcEnvironmentFile, ParseErrorExc )
import Distribution.Types.UnitId
         ( UnitId )
import Distribution.Types.UnqualComponentName
         ( UnqualComponentName, unUnqualComponentName )
import Distribution.Verbosity
         ( Verbosity, normal, lessVerbose )
import Distribution.Simple.Utils
         ( wrapText, die', notice, warn
         , withTempDirectory, createDirectoryIfMissingVerbose
         , ordNub )
import Distribution.Utils.Generic
         ( writeFileAtomic )
import Distribution.Text
         ( simpleParse )

import Control.Exception
         ( catch, throwIO )
import Control.Monad
         ( mapM, mapM_ )
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Either
         ( partitionEithers )
import Data.Ord
         ( comparing, Down(..) )
import qualified Data.Map as Map
import qualified Data.Set as Set
import Distribution.Utils.NubList
         ( fromNubList )
import System.Directory 
         ( getHomeDirectory, doesFileExist, createDirectoryIfMissing
         , getTemporaryDirectory, makeAbsolute, doesDirectoryExist )
import System.FilePath
         ( (</>), takeDirectory, takeBaseName )

data NewInstallFlags = NewInstallFlags
  { ninstInstallLibs :: Flag Bool
  , ninstEnvironmentPath :: Flag FilePath
  }

defaultNewInstallFlags :: NewInstallFlags
defaultNewInstallFlags = NewInstallFlags
  { ninstInstallLibs = toFlag False
  , ninstEnvironmentPath = mempty
  }

newInstallOptions :: ShowOrParseArgs -> [OptionField NewInstallFlags]
newInstallOptions _ = 
  [ option [] ["lib"]
    "Install libraries rather than executables from the target package."
    ninstInstallLibs (\v flags -> flags { ninstInstallLibs = v })
    trueArg
  , option [] ["package-env", "env"]
    "Set the environment file that may be modified."
    ninstEnvironmentPath (\pf flags -> flags { ninstEnvironmentPath = pf })
    (reqArg "ENV" (succeedReadE Flag) flagToList)
  ]

installCommand :: CommandUI ( ConfigFlags, ConfigExFlags, InstallFlags
                            , HaddockFlags, NewInstallFlags
                            )
installCommand = CommandUI
  { commandName         = "new-install"
  , commandSynopsis     = "Install packages."
  , commandUsage        = usageAlternatives
                          "new-install" [ "[TARGETS] [FLAGS]" ]
  , commandDescription  = Just $ \_ -> wrapText $
    "Installs one or more packages. This is done by installing them "
    ++ "in the store and symlinking the executables in the directory "
    ++ "specified by the --symlink-bindir flag (`~/.cabal/bin/` by default). "
    ++ "If you want the installed executables to be available globally, "
    ++ "make sure that the PATH environment variable contains that directory. "
    ++ "\n\n"
    ++ "If TARGET is a library, it will be added to the global environment. "
    ++ "When doing this, cabal will try to build a plan that includes all "
    ++ "the previously installed libraries. This is currently not implemented."
  , commandNotes        = Just $ \pname ->
      "Examples:\n"
      ++ "  " ++ pname ++ " new-install\n"
      ++ "    Install the package in the current directory\n"
      ++ "  " ++ pname ++ " new-install pkgname\n"
      ++ "    Install the package named pkgname"
      ++ " (fetching it from hackage if necessary)\n"
      ++ "  " ++ pname ++ " new-install ./pkgfoo\n"
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
     ++ liftOptions get2 set2 (configureExOptions showOrParseArgs ConstraintSourceCommandlineFlag)
     ++ liftOptions get3 set3
        -- hide "target-package-db" flag from the
        -- install options.
        (filter ((`notElem` ["target-package-db"])
                 . optionName) $
                               installOptions showOrParseArgs)
       ++ liftOptions get4 set4
          -- hide "target-package-db" flag from the
          -- install options.
          (filter ((`notElem` ["v", "verbose"])
                  . optionName) $
                                haddockOptions showOrParseArgs)
     ++ liftOptions get5 set5 (newInstallOptions showOrParseArgs)
  , commandDefaultFlags = (mempty, mempty, mempty, mempty, defaultNewInstallFlags)
  }
  where
    get1 (a,_,_,_,_) = a; set1 a (_,b,c,d,e) = (a,b,c,d,e)
    get2 (_,b,_,_,_) = b; set2 b (a,_,c,d,e) = (a,b,c,d,e)
    get3 (_,_,c,_,_) = c; set3 c (a,b,_,d,e) = (a,b,c,d,e)
    get4 (_,_,_,d,_) = d; set4 d (a,b,c,_,e) = (a,b,c,d,e)
    get5 (_,_,_,_,e) = e; set5 e (a,b,c,d,_) = (a,b,c,d,e)


-- | The @install@ command actually serves four different needs. It installs:
-- * exes:
--   For example a program from hackage. The behavior is similar to the old
--   install command, except that now conflicts between separate runs of the
--   command are impossible thanks to the store.
--   Exes are installed in the store like a normal dependency, then they are
--   symlinked uin the directory specified by --symlink-bindir.
--   To do this we need a dummy projectBaseContext containing the targets as
--   estra packages and using a temporary dist directory.
-- * libraries
--   Libraries install through a similar process, but using GHC environment
--   files instead of symlinks. This means that 'new-install'ing libraries
--   only works on GHC >= 8.0.
--
-- For more details on how this works, see the module
-- "Distribution.Client.ProjectOrchestration"
--
installAction :: (ConfigFlags, ConfigExFlags, InstallFlags, HaddockFlags, NewInstallFlags)
            -> [String] -> GlobalFlags -> IO ()
installAction (configFlags, configExFlags, installFlags, haddockFlags, newInstallFlags)
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

  let
    withProject = do
      let verbosity' = lessVerbose verbosity

      -- First, we need to learn about what's available to be installed.
      localBaseCtx <- establishProjectBaseContext verbosity' cliConfig
      let localDistDirLayout = distDirLayout localBaseCtx
      pkgDb <- projectConfigWithBuilderRepoContext verbosity' (buildSettings localBaseCtx) (getSourcePackages verbosity)
      
      let 
        (targetStrings', packageIds) = partitionEithers . flip fmap targetStrings $ 
          \str -> case simpleParse str of
            Just (pkgId :: PackageId) 
              | pkgVersion pkgId /= nullVersion -> Right pkgId
            _ -> Left str
        packageSpecifiers = flip fmap packageIds $ \case
          PackageIdentifier{..}
            | pkgVersion == nullVersion -> NamedPackage pkgName []
            | otherwise -> 
              NamedPackage pkgName [PackagePropertyVersion (thisVersion pkgVersion)]
        packageTargets = flip TargetPackageNamed Nothing . pkgName <$> packageIds
      
      if null targetStrings'
        then return (packageSpecifiers, packageTargets, projectConfig localBaseCtx)
        else do
          targetSelectors <- either (reportTargetSelectorProblems verbosity) return
                        =<< readTargetSelectors (localPackages localBaseCtx) Nothing targetStrings'
        
          (specs, selectors) <- withInstallPlan verbosity' localBaseCtx $ \elaboratedPlan -> do
            -- Split into known targets and hackage packages.
            (targets, hackageNames) <- case
              resolveTargets
                selectPackageTargets
                selectComponentTarget
                TargetProblemCommon
                elaboratedPlan
                (Just pkgDb)
                targetSelectors of
              Right targets -> do
                -- Everything is a local dependency.
                return (targets, [])
              Left errs -> do
                -- Not everything is local.
                let 
                  (errs', hackageNames) = partitionEithers . flip fmap errs $ \case
                    TargetProblemCommon (TargetAvailableInIndex name) -> Right name
                    err -> Left err
                
                when (not . null $ errs') $ reportTargetProblems verbosity errs'
        
                let 
                  targetSelectors' = flip filter targetSelectors $ \case
                    TargetComponentUnknown name _ _
                      | name `elem` hackageNames -> False
                    TargetPackageNamed name _
                      | name `elem` hackageNames -> False
                    _ -> True
                
                -- This can't fail, because all of the errors are removed (or we've given up).
                targets <- either (reportTargetProblems verbosity) return $ resolveTargets
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
        
              sdistize (SpecificSourcePackage spkg@SourcePackage{..}) = SpecificSourcePackage spkg'
                where
                  sdistPath = distSdistFile localDistDirLayout packageInfoId TargzFormat
                  spkg' = spkg { packageSource = LocalTarballPackage sdistPath }
              sdistize named = named
        
              local = sdistize <$> localPackages localBaseCtx
            
              gatherTargets :: UnitId -> TargetSelector
              gatherTargets targetId = TargetPackageNamed pkgName Nothing
                where          
                  Just targetUnit = Map.lookup targetId planMap
                  PackageIdentifier{..} = packageId targetUnit
        
              targets' = fmap gatherTargets targetIds 
              
              hackagePkgs :: [PackageSpecifier UnresolvedSourcePackage]
              hackagePkgs = flip NamedPackage [] <$> hackageNames
              hackageTargets :: [TargetSelector]
              hackageTargets = flip TargetPackageNamed Nothing <$> hackageNames
        
            createDirectoryIfMissing True (distSdistDirectory localDistDirLayout)

            mapM_ 
              (\(SpecificSourcePackage pkg) -> packageToSdist verbosity 
                (distProjectRootDirectory localDistDirLayout) (Archive TargzFormat)
                (distSdistFile localDistDirLayout (packageId pkg) TargzFormat) pkg
              ) (localPackages localBaseCtx)

            if null targets
              then return (hackagePkgs, hackageTargets)
              else return (local ++ hackagePkgs, targets' ++ hackageTargets)

          return (specs ++ packageSpecifiers, selectors ++ packageTargets, projectConfig localBaseCtx)

    withoutProject = do
      let
        parsePkg pkgName
          | Just (pkg :: PackageId) <- simpleParse pkgName = return pkg
          | otherwise = die' verbosity ("Invalid package ID: " ++ pkgName)
      packageIds <- mapM parsePkg targetStrings
      let 
        packageSpecifiers = flip fmap packageIds $ \case
          PackageIdentifier{..}
            | pkgVersion == nullVersion -> NamedPackage pkgName []
            | otherwise -> 
              NamedPackage pkgName [PackagePropertyVersion (thisVersion pkgVersion)]
        packageTargets = flip TargetPackageNamed Nothing . pkgName <$> packageIds
        globalConfigFlag = projectConfigConfigFile (projectConfigShared cliConfig)
      globalConfig <- runRebuild "" $ readGlobalConfig verbosity globalConfigFlag

      return (packageSpecifiers, packageTargets, globalConfig <> cliConfig)

  (specs, selectors, config) <- catch withProject
    $ \case
      (BadPackageLocations prov locs) 
        | prov == Set.singleton Implicit
        , let 
          isGlobErr (BadLocGlobEmptyMatch _) = True
          isGlobErr _ = False
        , any isGlobErr locs ->
          withoutProject
      err -> throwIO err

  home <- getHomeDirectory
  let
    ProjectConfig {
      projectConfigShared = ProjectConfigShared {
        projectConfigHcFlavor,
        projectConfigHcPath,
        projectConfigHcPkg
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

    progDb =
        userSpecifyPaths (Map.toList (getMapLast packageConfigProgramPaths))
      . userSpecifyArgss (Map.toList (getMapMappend packageConfigProgramArgs))
      . modifyProgramSearchPath
          (++ [ ProgramSearchPathDir dir
              | dir <- fromNubList packageConfigProgramPathExtra ])
      $ defaultProgramDb
    
  (compiler@Compiler { compilerId = 
    compilerId@(CompilerId compilerFlavor compilerVersion) }, platform, progDb') <-
      configCompilerEx hcFlavor hcPath hcPkg progDb verbosity

  let 
    globalEnv name =
      home </> ".ghc" </> ghcPlatformAndVersionString platform compilerVersion
           </> "environments" </> name
    localEnv dir =
      dir </> ".ghc.environment." ++ ghcPlatformAndVersionString platform compilerVersion

    GhcImplInfo{ supportsPkgEnvFiles } = getImplInfo compiler
    -- Why? We know what the first part will be, we only care about the packages.
    filterEnvEntries = filter $ \case
      GhcEnvFilePackageId _ -> True
      _ -> False
  
  envFile <- case flagToMaybe (ninstEnvironmentPath newInstallFlags) of
    Just spec
      -- Is spec a bare word without any "pathy" content, then it refers to
      -- a named global environment.
      | takeBaseName spec == spec -> return (globalEnv spec)
      | otherwise -> do
        spec' <- makeAbsolute spec
        isDir <- doesDirectoryExist spec'
        if isDir
          -- If spec is a directory, then make an ambient environment inside
          -- that directory.
          then return (localEnv spec')
          -- Otherwise, treat it like a literal file path.
          else return spec'
    Nothing -> return (globalEnv "default")

  envFileExists <- doesFileExist envFile
  envEntries <- filterEnvEntries <$> if
    (compilerFlavor == GHC || compilerFlavor == GHCJS)
      && supportsPkgEnvFiles && envFileExists
    then catch (readGhcEnvironmentFile envFile) $ \(_ :: ParseErrorExc) ->
      warn verbosity ("The environment file " ++ envFile ++
        " is unparsable. Libraries cannot be installed.") >> return []
    else return []

  cabalDir <- getCabalDir
  let
    mstoreDir   = flagToMaybe (globalStoreDir globalFlags)
    mlogsDir    = flagToMaybe (globalLogsDir globalFlags)
    cabalLayout = mkCabalDirLayout cabalDir mstoreDir mlogsDir
    packageDbs  = storePackageDBStack (cabalStoreDirLayout cabalLayout) compilerId

  installedIndex <- getInstalledPackages verbosity compiler packageDbs progDb'

  let (envSpecs, envEntries') = environmentFileToSpecifiers installedIndex envEntries

  -- Second, we need to use a fake project to let Cabal build the
  -- installables correctly. For that, we need a place to put a
  -- temporary dist directory.
  globalTmp <- getTemporaryDirectory
  withTempDirectory
    verbosity
    globalTmp
    "cabal-install."
    $ \tmpDir -> do
    baseCtx <- establishDummyProjectBaseContext
                 verbosity
                 config
                 tmpDir
                 (envSpecs ++ specs)
    
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
                then either (reportCannotPruneDependencies verbosity) return $
                     pruneInstallPlanToDependencies (Map.keysSet targets)
                                                    elaboratedPlan'
                else return elaboratedPlan'

            return (elaboratedPlan'', targets)

    printPlan verbosity baseCtx buildCtx

    buildOutcomes <- runProjectBuildPhase verbosity baseCtx buildCtx

    let 
      mkPkgBinDir = (</> "bin") .
                    storePackageDirectory
                       (cabalStoreDirLayout $ cabalDirLayout baseCtx)
                       compilerId
      installLibs = fromFlagOrDefault False (ninstInstallLibs newInstallFlags)

    when (not installLibs) $ do
      -- If there are exes, symlink them
      let symlinkBindirUnknown =
            "symlink-bindir is not defined. Set it in your cabal config file "
            ++ "or use --symlink-bindir=<path>"
      symlinkBindir <- fromFlagOrDefault (die' verbosity symlinkBindirUnknown)
                    $ fmap makeAbsolute
                    $ projectConfigSymlinkBinDir
                    $ projectConfigBuildOnly
                    $ projectConfig $ baseCtx
      createDirectoryIfMissingVerbose verbosity False symlinkBindir
      traverse_ (symlinkBuiltPackage verbosity mkPkgBinDir symlinkBindir)
            $ Map.toList $ targetsMap buildCtx
    runProjectPostBuildPhase verbosity baseCtx buildCtx buildOutcomes

    when installLibs $
      if supportsPkgEnvFiles
        then do
          -- Why do we get it again? If we updated a globalPackage then we need
          -- the new version.
          installedIndex' <- getInstalledPackages verbosity compiler packageDbs progDb'
          let
            getLatest = fmap (head . snd) . take 1 . sortBy (comparing (Down . fst)) 
                      . lookupPackageName installedIndex'
            globalLatest = concat (getLatest <$> globalPackages)

            baseEntries =
              GhcEnvFileClearPackageDbStack : fmap GhcEnvFilePackageDb packageDbs
            globalEntries = GhcEnvFilePackageId . installedUnitId <$> globalLatest
            pkgEntries = ordNub $
                  globalEntries
              ++ envEntries'
              ++ entriesForLibraryComponents (targetsMap buildCtx)
            contents' = renderGhcEnvironmentFile (baseEntries ++ pkgEntries)
          createDirectoryIfMissing True (takeDirectory envFile)
          writeFileAtomic envFile (BS.pack contents')
        else
          warn verbosity $
              "The current compiler doesn't support safely installing libraries, "
            ++ "so only executables will be available. (Library installation is "
            ++ "supported on GHC 8.0+ only)"
  where
    configFlags' = disableTestsBenchsByDefault configFlags
    verbosity = fromFlagOrDefault normal (configVerbosity configFlags')
    cliConfig = commandLineFlagsToProjectConfig
                  globalFlags configFlags' configExFlags
                  installFlags haddockFlags

globalPackages :: [PackageName]
globalPackages = mkPackageName <$>
  [ "ghc", "hoopl", "bytestring", "unix", "base", "time", "hpc", "filepath"
  , "process", "array", "integer-gmp", "containers", "ghc-boot", "binary"
  , "ghc-prim", "ghci", "rts", "terminfo", "transformers", "deepseq"
  , "ghc-boot-th", "pretty", "template-haskell", "directory", "text"
  , "bin-package-db"
  ]

environmentFileToSpecifiers :: InstalledPackageIndex -> [GhcEnvironmentFileEntry] 
                            -> ([PackageSpecifier a], [GhcEnvironmentFileEntry])
environmentFileToSpecifiers ipi = foldMap $ \case
    (GhcEnvFilePackageId unitId) 
        | Just InstalledPackageInfo{ sourcePackageId = PackageIdentifier{..}, installedUnitId }
          <- lookupUnitId ipi unitId
        , let pkgSpec = NamedPackage pkgName [PackagePropertyVersion (thisVersion pkgVersion)]
        -> if pkgName `elem` globalPackages
          then ([pkgSpec], [])
          else ([pkgSpec], [GhcEnvFilePackageId installedUnitId])
    _ -> ([], [])


-- | Disables tests and benchmarks if they weren't explicitly enabled.
disableTestsBenchsByDefault :: ConfigFlags -> ConfigFlags
disableTestsBenchsByDefault configFlags =
  configFlags { configTests = Flag False <> configTests configFlags
              , configBenchmarks = Flag False <> configBenchmarks configFlags }

-- | Symlink every exe from a package from the store to a given location
symlinkBuiltPackage :: Verbosity
                    -> (UnitId -> FilePath) -- ^ A function to get an UnitId's
                                            -- store directory
                    -> FilePath -- ^ Where to put the symlink
                    -> ( UnitId
                        , [(ComponentTarget, [TargetSelector])] )
                     -> IO ()
symlinkBuiltPackage verbosity mkSourceBinDir destDir (pkg, components) =
  traverse_ (symlinkBuiltExe verbosity (mkSourceBinDir pkg) destDir) exes
  where
    exes = catMaybes $ (exeMaybe . fst) <$> components
    exeMaybe (ComponentTarget (CExeName exe) _) = Just exe
    exeMaybe _ = Nothing

-- | Symlink a specific exe.
symlinkBuiltExe :: Verbosity -> FilePath -> FilePath -> UnqualComponentName -> IO Bool
symlinkBuiltExe verbosity sourceDir destDir exe = do
  notice verbosity $ "Symlinking " ++ unUnqualComponentName exe
  symlinkBinary
    destDir
    sourceDir
    exe
    $ unUnqualComponentName exe

-- | Create 'GhcEnvironmentFileEntry's for packages with exposed libraries.
entriesForLibraryComponents :: TargetsMap -> [GhcEnvironmentFileEntry]
entriesForLibraryComponents = Map.foldrWithKey' (\k v -> mappend (go k v)) []
  where
    hasLib :: (ComponentTarget, [TargetSelector]) -> Bool
    hasLib (ComponentTarget CLibName _,        _) = True
    hasLib (ComponentTarget (CSubLibName _) _, _) = True
    hasLib _                                      = False
    
    go :: UnitId -> [(ComponentTarget, [TargetSelector])] -> [GhcEnvironmentFileEntry]
    go unitId targets
      | any hasLib targets = [GhcEnvFilePackageId unitId]
      | otherwise          = []

-- | Create a dummy project context, without a .cabal or a .cabal.project file
-- (a place where to put a temporary dist directory is still needed)
establishDummyProjectBaseContext
  :: Verbosity
  -> ProjectConfig
  -> FilePath
     -- ^ Where to put the dist directory
  -> [PackageSpecifier UnresolvedSourcePackage]
     -- ^ The packages to be included in the project
  -> IO ProjectBaseContext
establishDummyProjectBaseContext verbosity cliConfig tmpDir localPackages = do

    cabalDir <- getCabalDir

    -- Create the dist directories
    createDirectoryIfMissingVerbose verbosity True $ distDirectory distDirLayout
    createDirectoryIfMissingVerbose verbosity True $
      distProjectCacheDirectory distDirLayout

    globalConfig <- runRebuild ""
                  $ readGlobalConfig verbosity
                  $ projectConfigConfigFile
                  $ projectConfigShared cliConfig
    let projectConfig = globalConfig <> cliConfig

    let ProjectConfigBuildOnly {
          projectConfigLogsDir,
          projectConfigStoreDir
        } = projectConfigBuildOnly projectConfig

        mlogsDir = flagToMaybe projectConfigLogsDir
        mstoreDir = flagToMaybe projectConfigStoreDir
        cabalDirLayout = mkCabalDirLayout cabalDir mstoreDir mlogsDir

        buildSettings = resolveBuildTimeSettings
                          verbosity cabalDirLayout
                          projectConfig

    return ProjectBaseContext {
      distDirLayout,
      cabalDirLayout,
      projectConfig,
      localPackages,
      buildSettings
    }
  where
    mdistDirectory = flagToMaybe
                   $ projectConfigDistDir
                   $ projectConfigShared cliConfig
    projectRoot = ProjectRootImplicit tmpDir
    distDirLayout = defaultDistDirLayout projectRoot
                                         mdistDirectory

-- | This defines what a 'TargetSelector' means for the @bench@ command.
-- It selects the 'AvailableTarget's that the 'TargetSelector' refers to,
-- or otherwise classifies the problem.
--
-- For the @build@ command select all components except non-buildable
-- and disabled tests\/benchmarks, fail if there are no such
-- components
--
selectPackageTargets :: TargetSelector
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
selectComponentTarget :: SubComponentTarget
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

reportCannotPruneDependencies :: Verbosity -> CannotPruneDependencies -> IO a
reportCannotPruneDependencies verbosity =
    die' verbosity . renderCannotPruneDependencies
