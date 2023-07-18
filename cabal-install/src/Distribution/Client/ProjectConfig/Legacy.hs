{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

-- | Project configuration, implementation in terms of legacy types.
module Distribution.Client.ProjectConfig.Legacy
  ( -- Project config skeletons
    ProjectConfigSkeleton
  , parseProjectSkeleton
  , instantiateProjectConfigSkeletonFetchingCompiler
  , instantiateProjectConfigSkeletonWithCompiler
  , singletonProjectConfigSkeleton
  , projectSkeletonImports

    -- * Project config in terms of legacy types
  , LegacyProjectConfig
  , parseLegacyProjectConfig
  , showLegacyProjectConfig

    -- * Conversion to and from legacy config types
  , commandLineFlagsToProjectConfig
  , convertLegacyProjectConfig
  , convertLegacyGlobalConfig
  , convertToLegacyProjectConfig

    -- * Internals, just for tests
  , parsePackageLocationTokenQ
  , renderPackageLocationToken
  ) where

import Distribution.Client.Compat.Prelude

import Distribution.Types.Flag (FlagName, parsecFlagAssignment)

import Distribution.Client.ProjectConfig.Types
import Distribution.Client.Types.AllowNewer (AllowNewer (..), AllowOlder (..))
import Distribution.Client.Types.Repo (LocalRepo (..), RemoteRepo (..), emptyRemoteRepo)
import Distribution.Client.Types.RepoName (RepoName (..), unRepoName)
import Distribution.Client.Types.SourceRepo (SourceRepoList, sourceRepositoryPackageGrammar)

import Distribution.Client.Config
  ( SavedConfig (..)
  , postProcessRepo
  , remoteRepoFields
  )

import Distribution.Client.CmdInstall.ClientInstallFlags
  ( ClientInstallFlags (..)
  , clientInstallOptions
  , defaultClientInstallFlags
  )

import Distribution.Compat.Lens (toListOf, view)

import Distribution.Solver.Types.ConstraintSource

import Distribution.Client.NixStyleOptions (NixStyleFlags (..))
import Distribution.Client.ProjectFlags (ProjectFlags (..), defaultProjectFlags, projectFlagsOptions)
import Distribution.Client.Setup
  ( ConfigExFlags (..)
  , GlobalFlags (..)
  , InstallFlags (..)
  , configureExOptions
  , defaultConfigExFlags
  , defaultInstallFlags
  , globalCommand
  , installOptions
  )
import Distribution.FieldGrammar
import Distribution.Package
import Distribution.PackageDescription
  ( Condition (..)
  , ConfVar (..)
  , FlagAssignment
  , dispFlagAssignment
  )
import Distribution.PackageDescription.Configuration (simplifyWithSysParams)
import Distribution.Simple.Compiler
  ( CompilerInfo (..)
  , DebugInfoLevel (..)
  , OptimisationLevel (..)
  )
import Distribution.Simple.InstallDirs (CopyDest (NoCopyDest))
import Distribution.Simple.LocalBuildInfo
  ( fromPathTemplate
  , toPathTemplate
  )
import Distribution.Simple.Program
  ( knownPrograms
  , programName
  )
import Distribution.Simple.Program.Db
  ( ProgramDb
  , defaultProgramDb
  )
import Distribution.Simple.Setup
  ( BenchmarkFlags (..)
  , ConfigFlags (..)
  , DumpBuildInfo (DumpBuildInfo, NoDumpBuildInfo)
  , Flag (..)
  , HaddockFlags (..)
  , TestFlags (..)
  , benchmarkOptions'
  , configureOptions
  , defaultBenchmarkFlags
  , defaultHaddockFlags
  , defaultTestFlags
  , fromFlagOrDefault
  , haddockOptions
  , installDirsOptions
  , programDbPaths'
  , readPackageDb
  , showPackageDb
  , splitArgs
  , testOptions'
  , toFlag
  )
import Distribution.Simple.Utils
  ( lowercase
  )
import Distribution.Types.CondTree
  ( CondBranch (..)
  , CondTree (..)
  , ignoreConditions
  , mapTreeConds
  , traverseCondTreeC
  , traverseCondTreeV
  )
import Distribution.Types.SourceRepo (RepoType)
import Distribution.Utils.NubList
  ( fromNubList
  , overNubList
  , toNubList
  )

import Distribution.Client.ParseUtils
import Distribution.Deprecated.ParseUtils
  ( PError (..)
  , PWarning (..)
  , ParseResult (..)
  , commaNewLineListFieldParsec
  , newLineListField
  , parseFail
  , parseHaskellString
  , parseTokenQ
  , showToken
  , simpleFieldParsec
  , syntaxError
  )
import qualified Distribution.Deprecated.ParseUtils as ParseUtils
import Distribution.Deprecated.ReadP
  ( ReadP
  , (+++)
  )
import qualified Distribution.Deprecated.ReadP as Parse
import Distribution.Parsec (ParsecParser, parsecToken)
import Distribution.Simple.Command
  ( CommandUI (commandOptions)
  , OptionField (..)
  , ShowOrParseArgs (..)
  , option
  , reqArg'
  )
import Distribution.System (Arch, OS)
import Distribution.Types.PackageVersionConstraint
  ( PackageVersionConstraint
  )
import Text.PrettyPrint
  ( Doc
  , ($+$)
  )
import qualified Text.PrettyPrint as Disp

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as Map
import qualified Data.Set as Set

import Network.URI (URI (..), parseURI)

import Distribution.Fields.ConfVar (parseConditionConfVarFromClause)

import Distribution.Client.HttpUtils
import Distribution.Client.ReplFlags (multiReplOption)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (isAbsolute, isPathSeparator, makeValid, takeDirectory, (</>))

------------------------------------------------------------------
-- Handle extended project config files with conditionals and imports.
--

-- | ProjectConfigSkeleton is a tree of conditional blocks and imports wrapping a config. It can be finalized by providing the conditional resolution info
-- and then resolving and downloading the imports
type ProjectConfigSkeleton = CondTree ConfVar [ProjectConfigImport] ProjectConfig

type ProjectConfigImport = String

singletonProjectConfigSkeleton :: ProjectConfig -> ProjectConfigSkeleton
singletonProjectConfigSkeleton x = CondNode x mempty mempty

instantiateProjectConfigSkeletonFetchingCompiler :: Monad m => m (OS, Arch, CompilerInfo) -> FlagAssignment -> ProjectConfigSkeleton -> m ProjectConfig
instantiateProjectConfigSkeletonFetchingCompiler fetch flags skel
  | null (toListOf traverseCondTreeV skel) = pure $ fst (ignoreConditions skel)
  | otherwise = do
      (os, arch, impl) <- fetch
      pure $ instantiateProjectConfigSkeletonWithCompiler os arch impl flags skel

instantiateProjectConfigSkeletonWithCompiler :: OS -> Arch -> CompilerInfo -> FlagAssignment -> ProjectConfigSkeleton -> ProjectConfig
instantiateProjectConfigSkeletonWithCompiler os arch impl _flags skel = go $ mapTreeConds (fst . simplifyWithSysParams os arch impl) skel
  where
    go
      :: CondTree
          FlagName
          [ProjectConfigImport]
          ProjectConfig
      -> ProjectConfig
    go (CondNode l _imps ts) =
      let branches = concatMap processBranch ts
       in l <> mconcat branches
    processBranch (CondBranch cnd t mf) = case cnd of
      (Lit True) -> [go t]
      (Lit False) -> maybe ([]) ((: []) . go) mf
      _ -> error $ "unable to process condition: " ++ show cnd -- TODO it would be nice if there were a pretty printer

projectSkeletonImports :: ProjectConfigSkeleton -> [ProjectConfigImport]
projectSkeletonImports = view traverseCondTreeC

parseProjectSkeleton :: FilePath -> HttpTransport -> Verbosity -> [ProjectConfigImport] -> FilePath -> BS.ByteString -> IO (ParseResult ProjectConfigSkeleton)
parseProjectSkeleton cacheDir httpTransport verbosity seenImports source bs = (sanityWalkPCS False =<<) <$> liftPR (go []) (ParseUtils.readFields bs)
  where
    go :: [ParseUtils.Field] -> [ParseUtils.Field] -> IO (ParseResult ProjectConfigSkeleton)
    go acc (x : xs) = case x of
      (ParseUtils.F l "import" importLoc) ->
        if importLoc `elem` seenImports
          then pure . parseFail $ ParseUtils.FromString ("cyclical import of " ++ importLoc) (Just l)
          else do
            let fs = fmap (\z -> CondNode z [importLoc] mempty) $ fieldsToConfig (reverse acc)
            res <- parseProjectSkeleton cacheDir httpTransport verbosity (importLoc : seenImports) importLoc =<< fetchImportConfig importLoc
            rest <- go [] xs
            pure . fmap mconcat . sequence $ [fs, res, rest]
      (ParseUtils.Section l "if" p xs') -> do
        subpcs <- go [] xs'
        let fs = fmap singletonProjectConfigSkeleton $ fieldsToConfig (reverse acc)
        (elseClauses, rest) <- parseElseClauses xs
        let condNode =
              (\c pcs e -> CondNode mempty mempty [CondBranch c pcs e])
                <$>
                -- we rewrap as as a section so the readFields lexer of the conditional parser doesn't get confused
                adaptParseError l (parseConditionConfVarFromClause . BS.pack $ "if(" <> p <> ")")
                <*> subpcs
                <*> elseClauses
        pure . fmap mconcat . sequence $ [fs, condNode, rest]
      _ -> go (x : acc) xs
    go acc [] = pure . fmap singletonProjectConfigSkeleton . fieldsToConfig $ reverse acc

    parseElseClauses :: [ParseUtils.Field] -> IO (ParseResult (Maybe ProjectConfigSkeleton), ParseResult ProjectConfigSkeleton)
    parseElseClauses x = case x of
      (ParseUtils.Section _l "else" _p xs' : xs) -> do
        subpcs <- go [] xs'
        rest <- go [] xs
        pure (Just <$> subpcs, rest)
      (ParseUtils.Section l "elif" p xs' : xs) -> do
        subpcs <- go [] xs'
        (elseClauses, rest) <- parseElseClauses xs
        let condNode =
              (\c pcs e -> CondNode mempty mempty [CondBranch c pcs e])
                <$> adaptParseError l (parseConditionConfVarFromClause . BS.pack $ "else(" <> p <> ")")
                <*> subpcs
                <*> elseClauses
        pure (Just <$> condNode, rest)
      _ -> (\r -> (pure Nothing, r)) <$> go [] x

    fieldsToConfig xs = fmap (addProvenance . convertLegacyProjectConfig) $ parseLegacyProjectConfigFields source xs
    addProvenance x = x{projectConfigProvenance = Set.singleton (Explicit source)}

    adaptParseError _ (Right x) = pure x
    adaptParseError l (Left e) = parseFail $ ParseUtils.FromString (show e) (Just l)

    liftPR :: (a -> IO (ParseResult b)) -> ParseResult a -> IO (ParseResult b)
    liftPR f (ParseOk ws x) = addWarnings <$> f x
      where
        addWarnings (ParseOk ws' x') = ParseOk (ws' ++ ws) x'
        addWarnings x' = x'
    liftPR _ (ParseFailed e) = pure $ ParseFailed e

    fetchImportConfig :: ProjectConfigImport -> IO BS.ByteString
    fetchImportConfig pci = case parseURI pci of
      Just uri -> do
        let fp = cacheDir </> map (\x -> if isPathSeparator x then '_' else x) (makeValid $ show uri)
        createDirectoryIfMissing True cacheDir
        _ <- downloadURI httpTransport verbosity uri fp
        BS.readFile fp
      Nothing ->
        BS.readFile $
          if isAbsolute pci then pci else takeDirectory source </> pci

    modifiesCompiler :: ProjectConfig -> Bool
    modifiesCompiler pc = isSet projectConfigHcFlavor || isSet projectConfigHcPath || isSet projectConfigHcPkg
      where
        isSet f = f (projectConfigShared pc) /= NoFlag

    sanityWalkPCS :: Bool -> ProjectConfigSkeleton -> ParseResult ProjectConfigSkeleton
    sanityWalkPCS underConditional t@(CondNode d _c comps)
      | underConditional && modifiesCompiler d = parseFail $ ParseUtils.FromString "Cannot set compiler in a conditional clause of a cabal project file" Nothing
      | otherwise = mapM_ sanityWalkBranch comps >> pure t

    sanityWalkBranch :: CondBranch ConfVar [ProjectConfigImport] ProjectConfig -> ParseResult ()
    sanityWalkBranch (CondBranch _c t f) = traverse (sanityWalkPCS True) f >> sanityWalkPCS True t >> pure ()

------------------------------------------------------------------
-- Representing the project config file in terms of legacy types
--

-- | We already have parsers\/pretty-printers for almost all the fields in the
-- project config file, but they're in terms of the types used for the command
-- line flags for Setup.hs or cabal commands. We don't want to redefine them
-- all, at least not yet so for the moment we use the parsers at the old types
-- and use conversion functions.
--
-- Ultimately if\/when this project-based approach becomes the default then we
-- can redefine the parsers directly for the new types.
data LegacyProjectConfig = LegacyProjectConfig
  { legacyPackages :: [String]
  , legacyPackagesOptional :: [String]
  , legacyPackagesRepo :: [SourceRepoList]
  , legacyPackagesNamed :: [PackageVersionConstraint]
  , legacySharedConfig :: LegacySharedConfig
  , legacyAllConfig :: LegacyPackageConfig
  , legacyLocalConfig :: LegacyPackageConfig
  , legacySpecificConfig :: MapMappend PackageName LegacyPackageConfig
  }
  deriving (Show, Generic)

instance Monoid LegacyProjectConfig where
  mempty = gmempty
  mappend = (<>)

instance Semigroup LegacyProjectConfig where
  (<>) = gmappend

data LegacyPackageConfig = LegacyPackageConfig
  { legacyConfigureFlags :: ConfigFlags
  , legacyInstallPkgFlags :: InstallFlags
  , legacyHaddockFlags :: HaddockFlags
  , legacyTestFlags :: TestFlags
  , legacyBenchmarkFlags :: BenchmarkFlags
  }
  deriving (Show, Generic)

instance Monoid LegacyPackageConfig where
  mempty = gmempty
  mappend = (<>)

instance Semigroup LegacyPackageConfig where
  (<>) = gmappend

data LegacySharedConfig = LegacySharedConfig
  { legacyGlobalFlags :: GlobalFlags
  , legacyConfigureShFlags :: ConfigFlags
  , legacyConfigureExFlags :: ConfigExFlags
  , legacyInstallFlags :: InstallFlags
  , legacyClientInstallFlags :: ClientInstallFlags
  , legacyProjectFlags :: ProjectFlags
  , legacyMultiRepl :: Flag Bool
  }
  deriving (Show, Generic)

instance Monoid LegacySharedConfig where
  mempty = gmempty
  mappend = (<>)

instance Semigroup LegacySharedConfig where
  (<>) = gmappend

------------------------------------------------------------------
-- Converting from and to the legacy types
--

-- | Convert configuration from the @cabal configure@ or @cabal build@ command
-- line into a 'ProjectConfig' value that can combined with configuration from
-- other sources.
--
-- At the moment this uses the legacy command line flag types. See
-- 'LegacyProjectConfig' for an explanation.
commandLineFlagsToProjectConfig
  :: GlobalFlags
  -> NixStyleFlags a
  -> ClientInstallFlags
  -> ProjectConfig
commandLineFlagsToProjectConfig globalFlags NixStyleFlags{..} clientInstallFlags =
  mempty
    { projectConfigBuildOnly =
        convertLegacyBuildOnlyFlags
          globalFlags
          configFlags
          installFlags
          clientInstallFlags
          haddockFlags
          testFlags
          benchmarkFlags
    , projectConfigShared =
        convertLegacyAllPackageFlags
          globalFlags
          configFlags
          configExFlags
          installFlags
          projectFlags
          NoFlag
    , projectConfigLocalPackages = localConfig
    , projectConfigAllPackages = allConfig
    }
  where
    (localConfig, allConfig) =
      splitConfig
        ( convertLegacyPerPackageFlags
            configFlags
            installFlags
            haddockFlags
            testFlags
            benchmarkFlags
        )
    -- split the package config (from command line arguments) into
    -- those applied to all packages and those to local only.
    --
    -- for now we will just copy over the ProgramPaths/Extra into
    -- the AllPackages.  The LocalPackages do not inherit them from
    -- AllPackages, and as such need to retain them.
    --
    -- The general decision rule for what to put into allConfig
    -- into localConfig is the following:
    --
    -- - anything that is host/toolchain/env specific should be applied
    --   to all packages, as packagesets have to be host/toolchain/env
    --   consistent.
    -- - anything else should be in the local config and could potentially
    --   be lifted into all-packages vial the `package *` cabal.project
    --   section.
    --
    splitConfig :: PackageConfig -> (PackageConfig, PackageConfig)
    splitConfig pc =
      ( pc
      , mempty
          { packageConfigProgramPaths = packageConfigProgramPaths pc
          , packageConfigProgramPathExtra = packageConfigProgramPathExtra pc
          , -- Some flags to haddock should be passed to dependencies
            packageConfigDocumentation = packageConfigDocumentation pc
          , packageConfigHaddockHoogle = packageConfigHaddockHoogle pc
          , packageConfigHaddockHtml = packageConfigHaddockHtml pc
          , packageConfigHaddockInternal = packageConfigHaddockInternal pc
          , packageConfigHaddockQuickJump = packageConfigHaddockQuickJump pc
          , packageConfigHaddockLinkedSource = packageConfigHaddockLinkedSource pc
          }
      )

-- | Convert from the types currently used for the user-wide Cabal config
-- file into the 'ProjectConfig' type.
--
-- Only a subset of the 'ProjectConfig' can be represented in the user-wide
-- config. In particular it does not include packages that are in the project,
-- and it also doesn't support package-specific configuration (only
-- configuration that applies to all packages).
convertLegacyGlobalConfig :: SavedConfig -> ProjectConfig
convertLegacyGlobalConfig
  SavedConfig
    { savedGlobalFlags = globalFlags
    , savedInstallFlags = installFlags
    , savedClientInstallFlags = clientInstallFlags
    , savedConfigureFlags = configFlags
    , savedConfigureExFlags = configExFlags
    , savedUserInstallDirs = _
    , savedGlobalInstallDirs = _
    , savedUploadFlags = _
    , savedReportFlags = _
    , savedHaddockFlags = haddockFlags
    , savedTestFlags = testFlags
    , savedBenchmarkFlags = benchmarkFlags
    , savedProjectFlags = projectFlags
    , savedReplMulti = replMulti
    } =
    mempty
      { projectConfigBuildOnly = configBuildOnly
      , projectConfigShared = configShared
      , projectConfigAllPackages = configAllPackages
      }
    where
      -- TODO: [code cleanup] eliminate use of default*Flags here and specify the
      -- defaults in the various resolve functions in terms of the new types.
      configExFlags' = defaultConfigExFlags <> configExFlags
      installFlags' = defaultInstallFlags <> installFlags
      clientInstallFlags' = defaultClientInstallFlags <> clientInstallFlags
      haddockFlags' = defaultHaddockFlags <> haddockFlags
      testFlags' = defaultTestFlags <> testFlags
      benchmarkFlags' = defaultBenchmarkFlags <> benchmarkFlags
      projectFlags' = defaultProjectFlags <> projectFlags

      configAllPackages =
        convertLegacyPerPackageFlags
          configFlags
          installFlags'
          haddockFlags'
          testFlags'
          benchmarkFlags'
      configShared =
        convertLegacyAllPackageFlags
          globalFlags
          configFlags
          configExFlags'
          installFlags'
          projectFlags'
          replMulti
      configBuildOnly =
        convertLegacyBuildOnlyFlags
          globalFlags
          configFlags
          installFlags'
          clientInstallFlags'
          haddockFlags'
          testFlags'
          benchmarkFlags'

-- | Convert the project config from the legacy types to the 'ProjectConfig'
-- and associated types. See 'LegacyProjectConfig' for an explanation of the
-- approach.
convertLegacyProjectConfig :: LegacyProjectConfig -> ProjectConfig
convertLegacyProjectConfig
  LegacyProjectConfig
    { legacyPackages
    , legacyPackagesOptional
    , legacyPackagesRepo
    , legacyPackagesNamed
    , legacySharedConfig =
      LegacySharedConfig
        globalFlags
        configShFlags
        configExFlags
        installSharedFlags
        clientInstallFlags
        projectFlags
        multiRepl
    , legacyAllConfig
    , legacyLocalConfig =
      LegacyPackageConfig
        configFlags
        installPerPkgFlags
        haddockFlags
        testFlags
        benchmarkFlags
    , legacySpecificConfig
    } =
    ProjectConfig
      { projectPackages = legacyPackages
      , projectPackagesOptional = legacyPackagesOptional
      , projectPackagesRepo = legacyPackagesRepo
      , projectPackagesNamed = legacyPackagesNamed
      , projectConfigBuildOnly = configBuildOnly
      , projectConfigShared = configPackagesShared
      , projectConfigProvenance = mempty
      , projectConfigAllPackages = configAllPackages
      , projectConfigLocalPackages = configLocalPackages
      , projectConfigSpecificPackage = fmap perPackage legacySpecificConfig
      }
    where
      configAllPackages = convertLegacyPerPackageFlags g i h t b
        where
          LegacyPackageConfig g i h t b = legacyAllConfig
      configLocalPackages =
        convertLegacyPerPackageFlags
          configFlags
          installPerPkgFlags
          haddockFlags
          testFlags
          benchmarkFlags
      configPackagesShared =
        convertLegacyAllPackageFlags
          globalFlags
          (configFlags <> configShFlags)
          configExFlags
          installSharedFlags
          projectFlags
          multiRepl
      configBuildOnly =
        convertLegacyBuildOnlyFlags
          globalFlags
          configShFlags
          installSharedFlags
          clientInstallFlags
          haddockFlags
          testFlags
          benchmarkFlags

      perPackage
        ( LegacyPackageConfig
            perPkgConfigFlags
            perPkgInstallFlags
            perPkgHaddockFlags
            perPkgTestFlags
            perPkgBenchmarkFlags
          ) =
          convertLegacyPerPackageFlags
            perPkgConfigFlags
            perPkgInstallFlags
            perPkgHaddockFlags
            perPkgTestFlags
            perPkgBenchmarkFlags

-- | Helper used by other conversion functions that returns the
-- 'ProjectConfigShared' subset of the 'ProjectConfig'.
convertLegacyAllPackageFlags
  :: GlobalFlags
  -> ConfigFlags
  -> ConfigExFlags
  -> InstallFlags
  -> ProjectFlags
  -> Flag Bool
  -> ProjectConfigShared
convertLegacyAllPackageFlags globalFlags configFlags configExFlags installFlags projectFlags projectConfigMultiRepl =
  ProjectConfigShared{..}
  where
    GlobalFlags
      { globalConfigFile = projectConfigConfigFile
      , globalRemoteRepos = projectConfigRemoteRepos
      , globalLocalNoIndexRepos = projectConfigLocalNoIndexRepos
      , globalActiveRepos = projectConfigActiveRepos
      , globalProgPathExtra = projectConfigProgPathExtra
      , globalStoreDir = projectConfigStoreDir
      } = globalFlags

    ConfigFlags
      { configDistPref = projectConfigDistDir
      , configHcFlavor = projectConfigHcFlavor
      , configHcPath = projectConfigHcPath
      , configHcPkg = projectConfigHcPkg
      , -- configProgramPathExtra    = projectConfigProgPathExtra DELETE ME
      configInstallDirs = projectConfigInstallDirs
      , -- configUserInstall         = projectConfigUserInstall,
      configPackageDBs = projectConfigPackageDBs
      } = configFlags

    ConfigExFlags
      { configCabalVersion = projectConfigCabalVersion
      , configExConstraints = projectConfigConstraints
      , configPreferences = projectConfigPreferences
      , configSolver = projectConfigSolver
      , configAllowOlder = projectConfigAllowOlder
      , configAllowNewer = projectConfigAllowNewer
      , configWriteGhcEnvironmentFilesPolicy =
        projectConfigWriteGhcEnvironmentFilesPolicy
      } = configExFlags

    InstallFlags
      { installHaddockIndex = projectConfigHaddockIndex
      , -- installReinstall          = projectConfigReinstall,
      -- installAvoidReinstalls    = projectConfigAvoidReinstalls,
      -- installOverrideReinstall  = projectConfigOverrideReinstall,
      installIndexState = projectConfigIndexState
      , installMaxBackjumps = projectConfigMaxBackjumps
      , -- installUpgradeDeps        = projectConfigUpgradeDeps,
      installReorderGoals = projectConfigReorderGoals
      , installCountConflicts = projectConfigCountConflicts
      , installFineGrainedConflicts = projectConfigFineGrainedConflicts
      , installMinimizeConflictSet = projectConfigMinimizeConflictSet
      , installPerComponent = projectConfigPerComponent
      , installIndependentGoals = projectConfigIndependentGoals
      , installPreferOldest = projectConfigPreferOldest
      , -- installShadowPkgs         = projectConfigShadowPkgs,
      installStrongFlags = projectConfigStrongFlags
      , installAllowBootLibInstalls = projectConfigAllowBootLibInstalls
      , installOnlyConstrained = projectConfigOnlyConstrained
      } = installFlags

    ProjectFlags
      { flagProjectDir = projectConfigProjectDir
      , flagProjectFile = projectConfigProjectFile
      , flagIgnoreProject = projectConfigIgnoreProject
      } = projectFlags

-- | Helper used by other conversion functions that returns the
-- 'PackageConfig' subset of the 'ProjectConfig'.
convertLegacyPerPackageFlags
  :: ConfigFlags
  -> InstallFlags
  -> HaddockFlags
  -> TestFlags
  -> BenchmarkFlags
  -> PackageConfig
convertLegacyPerPackageFlags
  configFlags
  installFlags
  haddockFlags
  testFlags
  benchmarkFlags =
    PackageConfig{..}
    where
      ConfigFlags
        { configProgramPaths
        , configProgramArgs
        , configProgramPathExtra = packageConfigProgramPathExtra
        , configVanillaLib = packageConfigVanillaLib
        , configProfLib = packageConfigProfLib
        , configSharedLib = packageConfigSharedLib
        , configStaticLib = packageConfigStaticLib
        , configDynExe = packageConfigDynExe
        , configFullyStaticExe = packageConfigFullyStaticExe
        , configProfExe = packageConfigProfExe
        , configProf = packageConfigProf
        , configProfDetail = packageConfigProfDetail
        , configProfLibDetail = packageConfigProfLibDetail
        , configConfigureArgs = packageConfigConfigureArgs
        , configOptimization = packageConfigOptimization
        , configProgPrefix = packageConfigProgPrefix
        , configProgSuffix = packageConfigProgSuffix
        , configGHCiLib = packageConfigGHCiLib
        , configSplitSections = packageConfigSplitSections
        , configSplitObjs = packageConfigSplitObjs
        , configStripExes = packageConfigStripExes
        , configStripLibs = packageConfigStripLibs
        , configExtraLibDirs = packageConfigExtraLibDirs
        , configExtraLibDirsStatic = packageConfigExtraLibDirsStatic
        , configExtraFrameworkDirs = packageConfigExtraFrameworkDirs
        , configExtraIncludeDirs = packageConfigExtraIncludeDirs
        , configConfigurationsFlags = packageConfigFlagAssignment
        , configTests = packageConfigTests
        , configBenchmarks = packageConfigBenchmarks
        , configCoverage = coverage
        , configLibCoverage = libcoverage -- deprecated
        , configDebugInfo = packageConfigDebugInfo
        , configDumpBuildInfo = packageConfigDumpBuildInfo
        , configRelocatable = packageConfigRelocatable
        } = configFlags
      packageConfigProgramPaths = MapLast (Map.fromList configProgramPaths)
      packageConfigProgramArgs = MapMappend (Map.fromListWith (++) configProgramArgs)

      packageConfigCoverage = coverage <> libcoverage
      -- TODO: defer this merging to the resolve phase

      InstallFlags
        { installDocumentation = packageConfigDocumentation
        , installRunTests = packageConfigRunTests
        } = installFlags

      HaddockFlags
        { haddockHoogle = packageConfigHaddockHoogle
        , haddockHtml = packageConfigHaddockHtml
        , haddockHtmlLocation = packageConfigHaddockHtmlLocation
        , haddockForeignLibs = packageConfigHaddockForeignLibs
        , haddockForHackage = packageConfigHaddockForHackage
        , haddockExecutables = packageConfigHaddockExecutables
        , haddockTestSuites = packageConfigHaddockTestSuites
        , haddockBenchmarks = packageConfigHaddockBenchmarks
        , haddockInternal = packageConfigHaddockInternal
        , haddockCss = packageConfigHaddockCss
        , haddockLinkedSource = packageConfigHaddockLinkedSource
        , haddockQuickJump = packageConfigHaddockQuickJump
        , haddockHscolourCss = packageConfigHaddockHscolourCss
        , haddockContents = packageConfigHaddockContents
        , haddockIndex = packageConfigHaddockIndex
        , haddockBaseUrl = packageConfigHaddockBaseUrl
        , haddockLib = packageConfigHaddockLib
        , haddockOutputDir = packageConfigHaddockOutputDir
        } = haddockFlags

      TestFlags
        { testHumanLog = packageConfigTestHumanLog
        , testMachineLog = packageConfigTestMachineLog
        , testShowDetails = packageConfigTestShowDetails
        , testKeepTix = packageConfigTestKeepTix
        , testWrapper = packageConfigTestWrapper
        , testFailWhenNoTestSuites = packageConfigTestFailWhenNoTestSuites
        , testOptions = packageConfigTestTestOptions
        } = testFlags

      BenchmarkFlags
        { benchmarkOptions = packageConfigBenchmarkOptions
        } = benchmarkFlags

-- | Helper used by other conversion functions that returns the
-- 'ProjectConfigBuildOnly' subset of the 'ProjectConfig'.
convertLegacyBuildOnlyFlags
  :: GlobalFlags
  -> ConfigFlags
  -> InstallFlags
  -> ClientInstallFlags
  -> HaddockFlags
  -> TestFlags
  -> BenchmarkFlags
  -> ProjectConfigBuildOnly
convertLegacyBuildOnlyFlags
  globalFlags
  configFlags
  installFlags
  clientInstallFlags
  haddockFlags
  _
  _ =
    ProjectConfigBuildOnly{..}
    where
      projectConfigClientInstallFlags = clientInstallFlags
      GlobalFlags
        { globalCacheDir = projectConfigCacheDir
        , globalLogsDir = projectConfigLogsDir
        , globalHttpTransport = projectConfigHttpTransport
        , globalIgnoreExpiry = projectConfigIgnoreExpiry
        } = globalFlags

      ConfigFlags
        { configVerbosity = projectConfigVerbosity
        } = configFlags

      InstallFlags
        { installDryRun = projectConfigDryRun
        , installOnlyDownload = projectConfigOnlyDownload
        , installOnly = _
        , installOnlyDeps = projectConfigOnlyDeps
        , installRootCmd = _
        , installSummaryFile = projectConfigSummaryFile
        , installLogFile = projectConfigLogFile
        , installBuildReports = projectConfigBuildReports
        , installReportPlanningFailure = projectConfigReportPlanningFailure
        , installSymlinkBinDir = projectConfigSymlinkBinDir
        , installNumJobs = projectConfigNumJobs
        , installUseSemaphore = projectConfigUseSemaphore
        , installKeepGoing = projectConfigKeepGoing
        , installOfflineMode = projectConfigOfflineMode
        } = installFlags

      HaddockFlags
        { haddockKeepTempFiles = projectConfigKeepTempFiles -- TODO: this ought to live elsewhere
        } = haddockFlags

convertToLegacyProjectConfig :: ProjectConfig -> LegacyProjectConfig
convertToLegacyProjectConfig
  projectConfig@ProjectConfig
    { projectPackages
    , projectPackagesOptional
    , projectPackagesRepo
    , projectPackagesNamed
    , projectConfigAllPackages
    , projectConfigLocalPackages
    , projectConfigSpecificPackage
    } =
    LegacyProjectConfig
      { legacyPackages = projectPackages
      , legacyPackagesOptional = projectPackagesOptional
      , legacyPackagesRepo = projectPackagesRepo
      , legacyPackagesNamed = projectPackagesNamed
      , legacySharedConfig = convertToLegacySharedConfig projectConfig
      , legacyAllConfig =
          convertToLegacyPerPackageConfig
            projectConfigAllPackages
      , legacyLocalConfig =
          convertToLegacyAllPackageConfig projectConfig
            <> convertToLegacyPerPackageConfig
              projectConfigLocalPackages
      , legacySpecificConfig =
          fmap
            convertToLegacyPerPackageConfig
            projectConfigSpecificPackage
      }

convertToLegacySharedConfig :: ProjectConfig -> LegacySharedConfig
convertToLegacySharedConfig
  ProjectConfig
    { projectConfigBuildOnly = ProjectConfigBuildOnly{..}
    , projectConfigShared = ProjectConfigShared{..}
    , projectConfigAllPackages =
      PackageConfig
        { packageConfigDocumentation
        }
    } =
    LegacySharedConfig
      { legacyGlobalFlags = globalFlags
      , legacyConfigureShFlags = configFlags
      , legacyConfigureExFlags = configExFlags
      , legacyInstallFlags = installFlags
      , legacyClientInstallFlags = projectConfigClientInstallFlags
      , legacyProjectFlags = projectFlags
      , legacyMultiRepl = projectConfigMultiRepl
      }
    where
      globalFlags =
        GlobalFlags
          { globalVersion = mempty
          , globalNumericVersion = mempty
          , globalConfigFile = projectConfigConfigFile
          , globalConstraintsFile = mempty
          , globalRemoteRepos = projectConfigRemoteRepos
          , globalCacheDir = projectConfigCacheDir
          , globalLocalNoIndexRepos = projectConfigLocalNoIndexRepos
          , globalActiveRepos = projectConfigActiveRepos
          , globalLogsDir = projectConfigLogsDir
          , globalIgnoreExpiry = projectConfigIgnoreExpiry
          , globalHttpTransport = projectConfigHttpTransport
          , globalNix = mempty
          , globalStoreDir = projectConfigStoreDir
          , globalProgPathExtra = projectConfigProgPathExtra
          }

      configFlags =
        mempty
          { configVerbosity = projectConfigVerbosity
          , configDistPref = projectConfigDistDir
          , configPackageDBs = projectConfigPackageDBs
          , configInstallDirs = projectConfigInstallDirs
          }

      configExFlags =
        ConfigExFlags
          { configCabalVersion = projectConfigCabalVersion
          , configAppend = mempty
          , configBackup = mempty
          , configExConstraints = projectConfigConstraints
          , configPreferences = projectConfigPreferences
          , configSolver = projectConfigSolver
          , configAllowOlder = projectConfigAllowOlder
          , configAllowNewer = projectConfigAllowNewer
          , configWriteGhcEnvironmentFilesPolicy =
              projectConfigWriteGhcEnvironmentFilesPolicy
          }

      installFlags =
        InstallFlags
          { installDocumentation = packageConfigDocumentation
          , installHaddockIndex = projectConfigHaddockIndex
          , installDest = Flag NoCopyDest
          , installDryRun = projectConfigDryRun
          , installOnlyDownload = projectConfigOnlyDownload
          , installReinstall = mempty -- projectConfigReinstall,
          , installAvoidReinstalls = mempty -- projectConfigAvoidReinstalls,
          , installOverrideReinstall = mempty -- projectConfigOverrideReinstall,
          , installMaxBackjumps = projectConfigMaxBackjumps
          , installUpgradeDeps = mempty -- projectConfigUpgradeDeps,
          , installReorderGoals = projectConfigReorderGoals
          , installCountConflicts = projectConfigCountConflicts
          , installFineGrainedConflicts = projectConfigFineGrainedConflicts
          , installMinimizeConflictSet = projectConfigMinimizeConflictSet
          , installIndependentGoals = projectConfigIndependentGoals
          , installPreferOldest = projectConfigPreferOldest
          , installShadowPkgs = mempty -- projectConfigShadowPkgs,
          , installStrongFlags = projectConfigStrongFlags
          , installAllowBootLibInstalls = projectConfigAllowBootLibInstalls
          , installOnlyConstrained = projectConfigOnlyConstrained
          , installOnly = mempty
          , installOnlyDeps = projectConfigOnlyDeps
          , installIndexState = projectConfigIndexState
          , installRootCmd = mempty -- no longer supported
          , installSummaryFile = projectConfigSummaryFile
          , installLogFile = projectConfigLogFile
          , installBuildReports = projectConfigBuildReports
          , installReportPlanningFailure = projectConfigReportPlanningFailure
          , installSymlinkBinDir = projectConfigSymlinkBinDir
          , installPerComponent = projectConfigPerComponent
          , installNumJobs = projectConfigNumJobs
          , installUseSemaphore = projectConfigUseSemaphore
          , installKeepGoing = projectConfigKeepGoing
          , installRunTests = mempty
          , installOfflineMode = projectConfigOfflineMode
          }

      projectFlags =
        ProjectFlags
          { flagProjectDir = projectConfigProjectDir
          , flagProjectFile = projectConfigProjectFile
          , flagIgnoreProject = projectConfigIgnoreProject
          }

convertToLegacyAllPackageConfig :: ProjectConfig -> LegacyPackageConfig
convertToLegacyAllPackageConfig
  ProjectConfig
    { projectConfigBuildOnly = ProjectConfigBuildOnly{..}
    , projectConfigShared = ProjectConfigShared{..}
    } =
    LegacyPackageConfig
      { legacyConfigureFlags = configFlags
      , legacyInstallPkgFlags = mempty
      , legacyHaddockFlags = haddockFlags
      , legacyTestFlags = mempty
      , legacyBenchmarkFlags = mempty
      }
    where
      configFlags =
        ConfigFlags
          { configArgs = mempty
          , configPrograms_ = mempty
          , configProgramPaths = mempty
          , configProgramArgs = mempty
          , configProgramPathExtra = mempty
          , configHcFlavor = projectConfigHcFlavor
          , configHcPath = projectConfigHcPath
          , configHcPkg = projectConfigHcPkg
          , configInstantiateWith = mempty
          , configVanillaLib = mempty
          , configProfLib = mempty
          , configSharedLib = mempty
          , configStaticLib = mempty
          , configDynExe = mempty
          , configFullyStaticExe = mempty
          , configProfExe = mempty
          , configProf = mempty
          , configProfDetail = mempty
          , configProfLibDetail = mempty
          , configConfigureArgs = mempty
          , configOptimization = mempty
          , configProgPrefix = mempty
          , configProgSuffix = mempty
          , configInstallDirs = projectConfigInstallDirs
          , configScratchDir = mempty
          , configDistPref = mempty
          , configCabalFilePath = mempty
          , configVerbosity = mempty
          , configUserInstall = mempty -- projectConfigUserInstall,
          , configPackageDBs = mempty
          , configGHCiLib = mempty
          , configSplitSections = mempty
          , configSplitObjs = mempty
          , configStripExes = mempty
          , configStripLibs = mempty
          , configExtraLibDirs = mempty
          , configExtraLibDirsStatic = mempty
          , configExtraFrameworkDirs = mempty
          , configConstraints = mempty
          , configDependencies = mempty
          , configPromisedDependencies = mempty
          , configExtraIncludeDirs = mempty
          , configDeterministic = mempty
          , configIPID = mempty
          , configCID = mempty
          , configConfigurationsFlags = mempty
          , configTests = mempty
          , configCoverage = mempty -- TODO: don't merge
          , configLibCoverage = mempty -- TODO: don't merge
          , configExactConfiguration = mempty
          , configBenchmarks = mempty
          , configFlagError = mempty -- TODO: ???
          , configRelocatable = mempty
          , configDebugInfo = mempty
          , configUseResponseFiles = mempty
          , configDumpBuildInfo = mempty
          , configAllowDependingOnPrivateLibs = mempty
          }

      haddockFlags =
        mempty
          { haddockKeepTempFiles = projectConfigKeepTempFiles
          }

convertToLegacyPerPackageConfig :: PackageConfig -> LegacyPackageConfig
convertToLegacyPerPackageConfig PackageConfig{..} =
  LegacyPackageConfig
    { legacyConfigureFlags = configFlags
    , legacyInstallPkgFlags = installFlags
    , legacyHaddockFlags = haddockFlags
    , legacyTestFlags = testFlags
    , legacyBenchmarkFlags = benchmarkFlags
    }
  where
    configFlags =
      ConfigFlags
        { configArgs = mempty
        , configPrograms_ = configPrograms_ mempty
        , configProgramPaths = Map.toList (getMapLast packageConfigProgramPaths)
        , configProgramArgs = Map.toList (getMapMappend packageConfigProgramArgs)
        , configProgramPathExtra = packageConfigProgramPathExtra
        , configHcFlavor = mempty
        , configHcPath = mempty
        , configHcPkg = mempty
        , configInstantiateWith = mempty
        , configVanillaLib = packageConfigVanillaLib
        , configProfLib = packageConfigProfLib
        , configSharedLib = packageConfigSharedLib
        , configStaticLib = packageConfigStaticLib
        , configDynExe = packageConfigDynExe
        , configFullyStaticExe = packageConfigFullyStaticExe
        , configProfExe = packageConfigProfExe
        , configProf = packageConfigProf
        , configProfDetail = packageConfigProfDetail
        , configProfLibDetail = packageConfigProfLibDetail
        , configConfigureArgs = packageConfigConfigureArgs
        , configOptimization = packageConfigOptimization
        , configProgPrefix = packageConfigProgPrefix
        , configProgSuffix = packageConfigProgSuffix
        , configInstallDirs = mempty
        , configScratchDir = mempty
        , configDistPref = mempty
        , configCabalFilePath = mempty
        , configVerbosity = mempty
        , configUserInstall = mempty
        , configPackageDBs = mempty
        , configGHCiLib = packageConfigGHCiLib
        , configSplitSections = packageConfigSplitSections
        , configSplitObjs = packageConfigSplitObjs
        , configStripExes = packageConfigStripExes
        , configStripLibs = packageConfigStripLibs
        , configExtraLibDirs = packageConfigExtraLibDirs
        , configExtraLibDirsStatic = packageConfigExtraLibDirsStatic
        , configExtraFrameworkDirs = packageConfigExtraFrameworkDirs
        , configConstraints = mempty
        , configDependencies = mempty
        , configPromisedDependencies = mempty
        , configExtraIncludeDirs = packageConfigExtraIncludeDirs
        , configIPID = mempty
        , configCID = mempty
        , configDeterministic = mempty
        , configConfigurationsFlags = packageConfigFlagAssignment
        , configTests = packageConfigTests
        , configCoverage = packageConfigCoverage -- TODO: don't merge
        , configLibCoverage = packageConfigCoverage -- TODO: don't merge
        , configExactConfiguration = mempty
        , configBenchmarks = packageConfigBenchmarks
        , configFlagError = mempty -- TODO: ???
        , configRelocatable = packageConfigRelocatable
        , configDebugInfo = packageConfigDebugInfo
        , configUseResponseFiles = mempty
        , configDumpBuildInfo = packageConfigDumpBuildInfo
        , configAllowDependingOnPrivateLibs = mempty
        }

    installFlags =
      mempty
        { installDocumentation = packageConfigDocumentation
        , installRunTests = packageConfigRunTests
        }

    haddockFlags =
      HaddockFlags
        { haddockProgramPaths = mempty
        , haddockProgramArgs = mempty
        , haddockHoogle = packageConfigHaddockHoogle
        , haddockHtml = packageConfigHaddockHtml
        , haddockHtmlLocation = packageConfigHaddockHtmlLocation
        , haddockForHackage = packageConfigHaddockForHackage
        , haddockForeignLibs = packageConfigHaddockForeignLibs
        , haddockExecutables = packageConfigHaddockExecutables
        , haddockTestSuites = packageConfigHaddockTestSuites
        , haddockBenchmarks = packageConfigHaddockBenchmarks
        , haddockInternal = packageConfigHaddockInternal
        , haddockCss = packageConfigHaddockCss
        , haddockLinkedSource = packageConfigHaddockLinkedSource
        , haddockQuickJump = packageConfigHaddockQuickJump
        , haddockHscolourCss = packageConfigHaddockHscolourCss
        , haddockContents = packageConfigHaddockContents
        , haddockDistPref = mempty
        , haddockKeepTempFiles = mempty
        , haddockVerbosity = mempty
        , haddockCabalFilePath = mempty
        , haddockIndex = packageConfigHaddockIndex
        , haddockBaseUrl = packageConfigHaddockBaseUrl
        , haddockLib = packageConfigHaddockLib
        , haddockOutputDir = packageConfigHaddockOutputDir
        , haddockArgs = mempty
        }

    testFlags =
      TestFlags
        { testDistPref = mempty
        , testVerbosity = mempty
        , testHumanLog = packageConfigTestHumanLog
        , testMachineLog = packageConfigTestMachineLog
        , testShowDetails = packageConfigTestShowDetails
        , testKeepTix = packageConfigTestKeepTix
        , testWrapper = packageConfigTestWrapper
        , testFailWhenNoTestSuites = packageConfigTestFailWhenNoTestSuites
        , testOptions = packageConfigTestTestOptions
        }

    benchmarkFlags =
      BenchmarkFlags
        { benchmarkDistPref = mempty
        , benchmarkVerbosity = mempty
        , benchmarkOptions = packageConfigBenchmarkOptions
        }

------------------------------------------------
-- Parsing and showing the project config file
--

parseLegacyProjectConfigFields :: FilePath -> [ParseUtils.Field] -> ParseResult LegacyProjectConfig
parseLegacyProjectConfigFields source =
  parseFieldsAndSections
    (legacyProjectConfigFieldDescrs constraintSrc)
    legacyPackageConfigSectionDescrs
    legacyPackageConfigFGSectionDescrs
    mempty
  where
    constraintSrc = ConstraintSourceProjectConfig source

parseLegacyProjectConfig :: FilePath -> BS.ByteString -> ParseResult LegacyProjectConfig
parseLegacyProjectConfig source bs = parseLegacyProjectConfigFields source =<< ParseUtils.readFields bs

showLegacyProjectConfig :: LegacyProjectConfig -> String
showLegacyProjectConfig config =
  Disp.render $
    showConfig
      (legacyProjectConfigFieldDescrs constraintSrc)
      legacyPackageConfigSectionDescrs
      legacyPackageConfigFGSectionDescrs
      config
      $+$ Disp.text ""
  where
    -- Note: ConstraintSource is unused when pretty-printing. We fake
    -- it here to avoid having to pass it on call-sites. It's not great
    -- but requires re-work of how we annotate provenance.
    constraintSrc = ConstraintSourceProjectConfig "unused"

legacyProjectConfigFieldDescrs :: ConstraintSource -> [FieldDescr LegacyProjectConfig]
legacyProjectConfigFieldDescrs constraintSrc =
  [ newLineListField
      "packages"
      (Disp.text . renderPackageLocationToken)
      parsePackageLocationTokenQ
      legacyPackages
      (\v flags -> flags{legacyPackages = v})
  , newLineListField
      "optional-packages"
      (Disp.text . renderPackageLocationToken)
      parsePackageLocationTokenQ
      legacyPackagesOptional
      (\v flags -> flags{legacyPackagesOptional = v})
  , commaNewLineListFieldParsec
      "extra-packages"
      pretty
      parsec
      legacyPackagesNamed
      (\v flags -> flags{legacyPackagesNamed = v})
  ]
    ++ map
      ( liftField
          legacySharedConfig
          (\flags conf -> conf{legacySharedConfig = flags})
      )
      (legacySharedConfigFieldDescrs constraintSrc)
    ++ map
      ( liftField
          legacyLocalConfig
          (\flags conf -> conf{legacyLocalConfig = flags})
      )
      legacyPackageConfigFieldDescrs

-- | This is a bit tricky since it has to cover globs which have embedded @,@
-- chars. But we don't just want to parse strictly as a glob since we want to
-- allow http urls which don't parse as globs, and possibly some
-- system-dependent file paths. So we parse fairly liberally as a token, but
-- we allow @,@ inside matched @{}@ braces.
parsePackageLocationTokenQ :: ReadP r String
parsePackageLocationTokenQ =
  parseHaskellString
    Parse.<++ parsePackageLocationToken
  where
    parsePackageLocationToken :: ReadP r String
    parsePackageLocationToken = fmap fst (Parse.gather outerTerm)
      where
        outerTerm = alternateEither1 outerToken (braces innerTerm)
        innerTerm = alternateEither innerToken (braces innerTerm)
        outerToken = Parse.munch1 outerChar >> return ()
        innerToken = Parse.munch1 innerChar >> return ()
        outerChar c = not (isSpace c || c == '{' || c == '}' || c == ',')
        innerChar c = not (isSpace c || c == '{' || c == '}')
        braces = Parse.between (Parse.char '{') (Parse.char '}')

    alternateEither
      , alternateEither1
      , alternatePQs
      , alternate1PQs
      , alternateQsP
      , alternate1QsP
        :: ReadP r () -> ReadP r () -> ReadP r ()

    alternateEither1 p q = alternate1PQs p q +++ alternate1QsP q p
    alternateEither p q = alternateEither1 p q +++ return ()
    alternate1PQs p q = p >> alternateQsP q p
    alternatePQs p q = alternate1PQs p q +++ return ()
    alternate1QsP q p = Parse.many1 q >> alternatePQs p q
    alternateQsP q p = alternate1QsP q p +++ return ()

renderPackageLocationToken :: String -> String
renderPackageLocationToken s
  | needsQuoting = show s
  | otherwise = s
  where
    needsQuoting =
      not (ok 0 s)
        || s == "." -- . on its own on a line has special meaning
        || take 2 s == "--" -- on its own line is comment syntax
        -- TODO: [code cleanup] these "." and "--" escaping issues
        -- ought to be dealt with systematically in ParseUtils.
    ok :: Int -> String -> Bool
    ok n [] = n == 0
    ok _ ('"' : _) = False
    ok n ('{' : cs) = ok (n + 1) cs
    ok n ('}' : cs) = ok (n - 1) cs
    ok n (',' : cs) = (n > 0) && ok n cs
    ok _ (c : _)
      | isSpace c = False
    ok n (_ : cs) = ok n cs

legacySharedConfigFieldDescrs :: ConstraintSource -> [FieldDescr LegacySharedConfig]
legacySharedConfigFieldDescrs constraintSrc =
  concat
    [ liftFields
        legacyGlobalFlags
        (\flags conf -> conf{legacyGlobalFlags = flags})
        . addFields
          [ newLineListField
              "extra-prog-path-shared-only"
              showTokenQ
              parseTokenQ
              (fromNubList . globalProgPathExtra)
              (\v conf -> conf{globalProgPathExtra = toNubList v})
          ]
        . filterFields
          [ "remote-repo-cache"
          , "logs-dir"
          , "store-dir"
          , "ignore-expiry"
          , "http-transport"
          , "active-repositories"
          ]
        . commandOptionsToFields
        $ commandOptions (globalCommand []) ParseArgs
    , liftFields
        legacyConfigureShFlags
        (\flags conf -> conf{legacyConfigureShFlags = flags})
        . addFields
          [ commaNewLineListFieldParsec
              "package-dbs"
              (Disp.text . showPackageDb)
              (fmap readPackageDb parsecToken)
              configPackageDBs
              (\v conf -> conf{configPackageDBs = v})
          ]
        . filterFields (["verbose", "builddir"] ++ map optionName installDirsOptions)
        . commandOptionsToFields
        $ configureOptions ParseArgs
    , liftFields
        legacyConfigureExFlags
        (\flags conf -> conf{legacyConfigureExFlags = flags})
        . addFields
          [ commaNewLineListFieldParsec
              "constraints"
              (pretty . fst)
              (fmap (\constraint -> (constraint, constraintSrc)) parsec)
              configExConstraints
              (\v conf -> conf{configExConstraints = v})
          , commaNewLineListFieldParsec
              "preferences"
              pretty
              parsec
              configPreferences
              (\v conf -> conf{configPreferences = v})
          , monoidFieldParsec
              "allow-older"
              (maybe mempty pretty)
              (fmap Just parsec)
              (fmap unAllowOlder . configAllowOlder)
              (\v conf -> conf{configAllowOlder = fmap AllowOlder v})
          , monoidFieldParsec
              "allow-newer"
              (maybe mempty pretty)
              (fmap Just parsec)
              (fmap unAllowNewer . configAllowNewer)
              (\v conf -> conf{configAllowNewer = fmap AllowNewer v})
          ]
        . filterFields
          [ "cabal-lib-version"
          , "solver"
          , "write-ghc-environment-files"
          -- not "constraint" or "preference", we use our own plural ones above
          ]
        . commandOptionsToFields
        $ configureExOptions ParseArgs constraintSrc
    , liftFields
        legacyInstallFlags
        (\flags conf -> conf{legacyInstallFlags = flags})
        . addFields
          [ newLineListField
              "build-summary"
              (showTokenQ . fromPathTemplate)
              (fmap toPathTemplate parseTokenQ)
              (fromNubList . installSummaryFile)
              (\v conf -> conf{installSummaryFile = toNubList v})
          ]
        . filterFields
          [ "doc-index-file"
          , "root-cmd"
          , "symlink-bindir"
          , "build-log"
          , "remote-build-reporting"
          , "report-planning-failure"
          , "jobs"
          , "semaphore"
          , "keep-going"
          , "offline"
          , "per-component"
          , -- solver flags:
            "max-backjumps"
          , "reorder-goals"
          , "count-conflicts"
          , "fine-grained-conflicts"
          , "minimize-conflict-set"
          , "independent-goals"
          , "prefer-oldest"
          , "strong-flags"
          , "allow-boot-library-installs"
          , "reject-unconstrained-dependencies"
          , "index-state"
          ]
        . commandOptionsToFields
        $ installOptions ParseArgs
    , liftFields
        legacyClientInstallFlags
        (\flags conf -> conf{legacyClientInstallFlags = flags})
        . commandOptionsToFields
        $ clientInstallOptions ParseArgs
    , liftFields
        legacyProjectFlags
        (\flags conf -> conf{legacyProjectFlags = flags})
        . commandOptionsToFields
        $ projectFlagsOptions ParseArgs
    , [liftField legacyMultiRepl (\flags conf -> conf{legacyMultiRepl = flags}) (commandOptionToField multiReplOption)]
    ]

legacyPackageConfigFieldDescrs :: [FieldDescr LegacyPackageConfig]
legacyPackageConfigFieldDescrs =
  ( liftFields
      legacyConfigureFlags
      (\flags conf -> conf{legacyConfigureFlags = flags})
      . addFields
        [ newLineListField
            "extra-include-dirs"
            showTokenQ
            parseTokenQ
            configExtraIncludeDirs
            (\v conf -> conf{configExtraIncludeDirs = v})
        , newLineListField
            "extra-lib-dirs"
            showTokenQ
            parseTokenQ
            configExtraLibDirs
            (\v conf -> conf{configExtraLibDirs = v})
        , newLineListField
            "extra-lib-dirs-static"
            showTokenQ
            parseTokenQ
            configExtraLibDirsStatic
            (\v conf -> conf{configExtraLibDirsStatic = v})
        , newLineListField
            "extra-framework-dirs"
            showTokenQ
            parseTokenQ
            configExtraFrameworkDirs
            (\v conf -> conf{configExtraFrameworkDirs = v})
        , newLineListField
            "extra-prog-path"
            showTokenQ
            parseTokenQ
            (fromNubList . configProgramPathExtra)
            (\v conf -> conf{configProgramPathExtra = toNubList v})
        , newLineListField
            "configure-options"
            showTokenQ
            parseTokenQ
            configConfigureArgs
            (\v conf -> conf{configConfigureArgs = v})
        , simpleFieldParsec
            "flags"
            dispFlagAssignment
            parsecFlagAssignment
            configConfigurationsFlags
            (\v conf -> conf{configConfigurationsFlags = v})
        , overrideDumpBuildInfo
        ]
      . filterFields
        [ "with-compiler"
        , "with-hc-pkg"
        , "program-prefix"
        , "program-suffix"
        , "library-vanilla"
        , "library-profiling"
        , "shared"
        , "static"
        , "executable-dynamic"
        , "executable-static"
        , "profiling"
        , "executable-profiling"
        , "profiling-detail"
        , "library-profiling-detail"
        , "library-for-ghci"
        , "split-objs"
        , "split-sections"
        , "executable-stripping"
        , "library-stripping"
        , "tests"
        , "benchmarks"
        , "coverage"
        , "library-coverage"
        , "relocatable"
        -- not "extra-include-dirs", "extra-lib-dirs", "extra-framework-dirs"
        -- or "extra-prog-path". We use corrected ones above that parse
        -- as list fields.
        ]
      . commandOptionsToFields
  )
    (configureOptions ParseArgs)
    ++ liftFields
      legacyConfigureFlags
      (\flags conf -> conf{legacyConfigureFlags = flags})
      [ overrideFieldCompiler
      , overrideFieldOptimization
      , overrideFieldDebugInfo
      ]
    ++ ( liftFields
          legacyInstallPkgFlags
          (\flags conf -> conf{legacyInstallPkgFlags = flags})
          . filterFields
            [ "documentation"
            , "run-tests"
            ]
          . commandOptionsToFields
       )
      (installOptions ParseArgs)
    ++ ( liftFields
          legacyHaddockFlags
          (\flags conf -> conf{legacyHaddockFlags = flags})
          . mapFieldNames
            ("haddock-" ++)
          . addFields
            [ simpleFieldParsec
                "for-hackage"
                -- TODO: turn this into a library function
                (fromFlagOrDefault Disp.empty . fmap pretty)
                (toFlag <$> parsec <|> pure mempty)
                haddockForHackage
                (\v conf -> conf{haddockForHackage = v})
            ]
          . filterFields
            [ "hoogle"
            , "html"
            , "html-location"
            , "foreign-libraries"
            , "executables"
            , "tests"
            , "benchmarks"
            , "all"
            , "internal"
            , "css"
            , "hyperlink-source"
            , "quickjump"
            , "hscolour-css"
            , "contents-location"
            , "index-location"
            , "keep-temp-files"
            , "base-url"
            , "lib"
            , "output-dir"
            ]
          . commandOptionsToFields
       )
      (haddockOptions ParseArgs)
    ++ ( liftFields
          legacyTestFlags
          (\flags conf -> conf{legacyTestFlags = flags})
          . mapFieldNames
            prefixTest
          . addFields
            [ newLineListField
                "test-options"
                (showTokenQ . fromPathTemplate)
                (fmap toPathTemplate parseTokenQ)
                testOptions
                (\v conf -> conf{testOptions = v})
            ]
          . filterFields
            [ "log"
            , "machine-log"
            , "show-details"
            , "keep-tix-files"
            , "fail-when-no-test-suites"
            , "test-wrapper"
            ]
          . commandOptionsToFields
       )
      (testOptions' ParseArgs)
    ++ ( liftFields
          legacyBenchmarkFlags
          (\flags conf -> conf{legacyBenchmarkFlags = flags})
          . addFields
            [ newLineListField
                "benchmark-options"
                (showTokenQ . fromPathTemplate)
                (fmap toPathTemplate parseTokenQ)
                benchmarkOptions
                (\v conf -> conf{benchmarkOptions = v})
            ]
          . filterFields
            []
          . commandOptionsToFields
       )
      (benchmarkOptions' ParseArgs)
  where
    overrideFieldCompiler =
      simpleFieldParsec
        "compiler"
        (fromFlagOrDefault Disp.empty . fmap pretty)
        (toFlag <$> parsec <|> pure mempty)
        configHcFlavor
        (\v flags -> flags{configHcFlavor = v})

    overrideDumpBuildInfo =
      liftField
        configDumpBuildInfo
        (\v flags -> flags{configDumpBuildInfo = v})
        $ let name = "build-info"
           in FieldDescr
                name
                ( \f -> case f of
                    Flag NoDumpBuildInfo -> Disp.text "False"
                    Flag DumpBuildInfo -> Disp.text "True"
                    _ -> Disp.empty
                )
                ( \line str _ -> case () of
                    _
                      | str == "False" -> ParseOk [] (Flag NoDumpBuildInfo)
                      | str == "True" -> ParseOk [] (Flag DumpBuildInfo)
                      | lstr == "false" -> ParseOk [caseWarning name] (Flag NoDumpBuildInfo)
                      | lstr == "true" -> ParseOk [caseWarning name] (Flag DumpBuildInfo)
                      | otherwise -> ParseFailed (NoParse name line)
                      where
                        lstr = lowercase str
                )

    -- TODO: [code cleanup] The following is a hack. The "optimization" and
    -- "debug-info" fields are OptArg, and viewAsFieldDescr fails on that.
    -- Instead of a hand-written parser and printer, we should handle this case
    -- properly in the library.

    overrideFieldOptimization =
      liftField
        configOptimization
        (\v flags -> flags{configOptimization = v})
        $ let name = "optimization"
           in FieldDescr
                name
                ( \f -> case f of
                    Flag NoOptimisation -> Disp.text "False"
                    Flag NormalOptimisation -> Disp.text "True"
                    Flag MaximumOptimisation -> Disp.text "2"
                    _ -> Disp.empty
                )
                ( \line str _ -> case () of
                    _
                      | str == "False" -> ParseOk [] (Flag NoOptimisation)
                      | str == "True" -> ParseOk [] (Flag NormalOptimisation)
                      | str == "0" -> ParseOk [] (Flag NoOptimisation)
                      | str == "1" -> ParseOk [] (Flag NormalOptimisation)
                      | str == "2" -> ParseOk [] (Flag MaximumOptimisation)
                      | lstr == "false" -> ParseOk [caseWarning name] (Flag NoOptimisation)
                      | lstr == "true" -> ParseOk [caseWarning name] (Flag NormalOptimisation)
                      | otherwise -> ParseFailed (NoParse name line)
                      where
                        lstr = lowercase str
                )

    overrideFieldDebugInfo =
      liftField configDebugInfo (\v flags -> flags{configDebugInfo = v}) $
        let name = "debug-info"
         in FieldDescr
              name
              ( \f -> case f of
                  Flag NoDebugInfo -> Disp.text "False"
                  Flag MinimalDebugInfo -> Disp.text "1"
                  Flag NormalDebugInfo -> Disp.text "True"
                  Flag MaximalDebugInfo -> Disp.text "3"
                  _ -> Disp.empty
              )
              ( \line str _ -> case () of
                  _
                    | str == "False" -> ParseOk [] (Flag NoDebugInfo)
                    | str == "True" -> ParseOk [] (Flag NormalDebugInfo)
                    | str == "0" -> ParseOk [] (Flag NoDebugInfo)
                    | str == "1" -> ParseOk [] (Flag MinimalDebugInfo)
                    | str == "2" -> ParseOk [] (Flag NormalDebugInfo)
                    | str == "3" -> ParseOk [] (Flag MaximalDebugInfo)
                    | lstr == "false" -> ParseOk [caseWarning name] (Flag NoDebugInfo)
                    | lstr == "true" -> ParseOk [caseWarning name] (Flag NormalDebugInfo)
                    | otherwise -> ParseFailed (NoParse name line)
                    where
                      lstr = lowercase str
              )

    caseWarning name =
      PWarning $
        "The '" ++ name ++ "' field is case sensitive, use 'True' or 'False'."

    prefixTest name
      | "test-" `isPrefixOf` name = name
      | otherwise = "test-" ++ name

legacyPackageConfigFGSectionDescrs
  :: ( FieldGrammar c g
     , Applicative (g SourceRepoList)
     , c (Identity RepoType)
     , c (List NoCommaFSep FilePathNT String)
     , c (NonEmpty' NoCommaFSep Token String)
     )
  => [FGSectionDescr g LegacyProjectConfig]
legacyPackageConfigFGSectionDescrs =
  [ packageRepoSectionDescr
  ]

legacyPackageConfigSectionDescrs :: [SectionDescr LegacyProjectConfig]
legacyPackageConfigSectionDescrs =
  [ packageSpecificOptionsSectionDescr
  , liftSection
      legacyLocalConfig
      (\flags conf -> conf{legacyLocalConfig = flags})
      programOptionsSectionDescr
  , liftSection
      legacyLocalConfig
      (\flags conf -> conf{legacyLocalConfig = flags})
      programLocationsSectionDescr
  , liftSection
      legacySharedConfig
      (\flags conf -> conf{legacySharedConfig = flags})
      $ liftSection
        legacyGlobalFlags
        (\flags conf -> conf{legacyGlobalFlags = flags})
        remoteRepoSectionDescr
  ]

packageRepoSectionDescr
  :: ( FieldGrammar c g
     , Applicative (g SourceRepoList)
     , c (Identity RepoType)
     , c (List NoCommaFSep FilePathNT String)
     , c (NonEmpty' NoCommaFSep Token String)
     )
  => FGSectionDescr g LegacyProjectConfig
packageRepoSectionDescr =
  FGSectionDescr
    { fgSectionName = "source-repository-package"
    , fgSectionGrammar = sourceRepositoryPackageGrammar
    , fgSectionGet = map (\x -> ("", x)) . legacyPackagesRepo
    , fgSectionSet =
        \lineno unused pkgrepo projconf -> do
          unless (null unused) $
            syntaxError lineno "the section 'source-repository-package' takes no arguments"
          return
            projconf
              { legacyPackagesRepo = legacyPackagesRepo projconf ++ [pkgrepo]
              }
    }

-- | The definitions of all the fields that can appear in the @package pkgfoo@
-- and @package *@ sections of the @cabal.project@-format files.
packageSpecificOptionsFieldDescrs :: [FieldDescr LegacyPackageConfig]
packageSpecificOptionsFieldDescrs =
  legacyPackageConfigFieldDescrs
    ++ programOptionsFieldDescrs
      (configProgramArgs . legacyConfigureFlags)
      ( \args pkgconf ->
          pkgconf
            { legacyConfigureFlags =
                (legacyConfigureFlags pkgconf)
                  { configProgramArgs = args
                  }
            }
      )
    ++ liftFields
      legacyConfigureFlags
      ( \flags pkgconf ->
          pkgconf
            { legacyConfigureFlags = flags
            }
      )
      programLocationsFieldDescrs

-- | The definition of the @package pkgfoo@ sections of the @cabal.project@-format
-- files. This section is per-package name. The special package @*@ applies to all
-- packages used anywhere by the project, locally or as dependencies.
packageSpecificOptionsSectionDescr :: SectionDescr LegacyProjectConfig
packageSpecificOptionsSectionDescr =
  SectionDescr
    { sectionName = "package"
    , sectionFields = packageSpecificOptionsFieldDescrs
    , sectionSubsections = []
    , sectionGet = \projconf ->
        [ (prettyShow pkgname, pkgconf)
        | (pkgname, pkgconf) <-
            Map.toList
              . getMapMappend
              . legacySpecificConfig
              $ projconf
        ]
          ++ [("*", legacyAllConfig projconf)]
    , sectionSet =
        \lineno pkgnamestr pkgconf projconf -> case pkgnamestr of
          "*" ->
            return
              projconf
                { legacyAllConfig = legacyAllConfig projconf <> pkgconf
                }
          _ -> do
            pkgname <- case simpleParsec pkgnamestr of
              Just pkgname -> return pkgname
              Nothing ->
                syntaxError lineno $
                  "a 'package' section requires a package name "
                    ++ "as an argument"
            return
              projconf
                { legacySpecificConfig =
                    MapMappend $
                      Map.insertWith
                        mappend
                        pkgname
                        pkgconf
                        (getMapMappend $ legacySpecificConfig projconf)
                }
    , sectionEmpty = mempty
    }

programOptionsFieldDescrs
  :: (a -> [(String, [String])])
  -> ([(String, [String])] -> a -> a)
  -> [FieldDescr a]
programOptionsFieldDescrs get' set =
  commandOptionsToFields $
    programDbOptions
      defaultProgramDb
      ParseArgs
      get'
      set

programOptionsSectionDescr :: SectionDescr LegacyPackageConfig
programOptionsSectionDescr =
  SectionDescr
    { sectionName = "program-options"
    , sectionFields =
        programOptionsFieldDescrs
          configProgramArgs
          (\args conf -> conf{configProgramArgs = args})
    , sectionSubsections = []
    , sectionGet =
        (\x -> [("", x)])
          . legacyConfigureFlags
    , sectionSet =
        \lineno unused confflags pkgconf -> do
          unless (null unused) $
            syntaxError lineno "the section 'program-options' takes no arguments"
          return
            pkgconf
              { legacyConfigureFlags = legacyConfigureFlags pkgconf <> confflags
              }
    , sectionEmpty = mempty
    }

programLocationsFieldDescrs :: [FieldDescr ConfigFlags]
programLocationsFieldDescrs =
  commandOptionsToFields $
    programDbPaths'
      (++ "-location")
      defaultProgramDb
      ParseArgs
      configProgramPaths
      (\paths conf -> conf{configProgramPaths = paths})

programLocationsSectionDescr :: SectionDescr LegacyPackageConfig
programLocationsSectionDescr =
  SectionDescr
    { sectionName = "program-locations"
    , sectionFields = programLocationsFieldDescrs
    , sectionSubsections = []
    , sectionGet =
        (\x -> [("", x)])
          . legacyConfigureFlags
    , sectionSet =
        \lineno unused confflags pkgconf -> do
          unless (null unused) $
            syntaxError lineno "the section 'program-locations' takes no arguments"
          return
            pkgconf
              { legacyConfigureFlags = legacyConfigureFlags pkgconf <> confflags
              }
    , sectionEmpty = mempty
    }

-- | For each known program @PROG@ in 'progDb', produce a @PROG-options@
-- 'OptionField'.
programDbOptions
  :: ProgramDb
  -> ShowOrParseArgs
  -> (flags -> [(String, [String])])
  -> ([(String, [String])] -> (flags -> flags))
  -> [OptionField flags]
programDbOptions progDb showOrParseArgs get' set =
  case showOrParseArgs of
    -- we don't want a verbose help text list so we just show a generic one:
    ShowArgs -> [programOptions "PROG"]
    ParseArgs ->
      map
        (programOptions . programName . fst)
        (knownPrograms progDb)
  where
    programOptions prog =
      option
        ""
        [prog ++ "-options"]
        ("give extra options to " ++ prog)
        get'
        set
        ( reqArg'
            "OPTS"
            (\args -> [(prog, splitArgs args)])
            ( \progArgs ->
                [ joinsArgs args
                | (prog', args) <- progArgs
                , prog == prog'
                ]
            )
        )

    joinsArgs = unwords . map escape
    escape arg
      | any isSpace arg = "\"" ++ arg ++ "\""
      | otherwise = arg

-- The implementation is slight hack: we parse all as remote repository
-- but if the url schema is file+noindex, we switch to local.
remoteRepoSectionDescr :: SectionDescr GlobalFlags
remoteRepoSectionDescr =
  SectionDescr
    { sectionName = "repository"
    , sectionEmpty = emptyRemoteRepo (RepoName "")
    , sectionFields = remoteRepoFields
    , sectionSubsections = []
    , sectionGet = getS
    , sectionSet = setS
    }
  where
    getS :: GlobalFlags -> [(String, RemoteRepo)]
    getS gf =
      map (\x -> (unRepoName $ remoteRepoName x, x)) (fromNubList (globalRemoteRepos gf))
        ++ map (\x -> (unRepoName $ localRepoName x, localToRemote x)) (fromNubList (globalLocalNoIndexRepos gf))

    setS :: Int -> String -> RemoteRepo -> GlobalFlags -> ParseResult GlobalFlags
    setS lineno reponame repo0 conf = do
      repo1 <- postProcessRepo lineno reponame repo0
      case repo1 of
        Left repo ->
          return
            conf
              { globalLocalNoIndexRepos = overNubList (++ [repo]) (globalLocalNoIndexRepos conf)
              }
        Right repo ->
          return
            conf
              { globalRemoteRepos = overNubList (++ [repo]) (globalRemoteRepos conf)
              }

    localToRemote :: LocalRepo -> RemoteRepo
    localToRemote (LocalRepo name path sharedCache) =
      (emptyRemoteRepo name)
        { remoteRepoURI = URI "file+noindex:" Nothing path "" (if sharedCache then "#shared-cache" else "")
        }

-------------------------------
-- Local field utils
--

-- | Parser combinator for simple fields which uses the field type's
-- 'Monoid' instance for combining multiple occurrences of the field.
monoidFieldParsec
  :: Monoid a
  => String
  -> (a -> Doc)
  -> ParsecParser a
  -> (b -> a)
  -> (a -> b -> b)
  -> FieldDescr b
monoidFieldParsec name showF readF get' set =
  liftField get' set' $ ParseUtils.fieldParsec name showF readF
  where
    set' xs b = set (get' b `mappend` xs) b

-- TODO: [code cleanup] local redefinition that should replace the version in
-- D.ParseUtils called showFilePath. This version escapes "." and "--" which
-- otherwise are special syntax.
showTokenQ :: String -> Doc
showTokenQ "" = Disp.empty
showTokenQ x@('-' : '-' : _) = Disp.text (show x)
showTokenQ x@('.' : []) = Disp.text (show x)
showTokenQ x = showToken x

-- Handy util
addFields
  :: [FieldDescr a]
  -> ([FieldDescr a] -> [FieldDescr a])
addFields = (++)
