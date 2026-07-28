{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | 'ProjectConfig' Field descriptions
module Distribution.Client.ProjectConfig.FieldGrammar
  ( projectConfigFieldGrammar
  , packageConfigFieldGrammar
  ) where

import Data.Bool (bool)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Set as Set
import Distribution.CabalSpecVersion (CabalSpecVersion (..))
import Distribution.Client.CmdInstall.ClientInstallFlags (clientInstallFlagsGrammar)
import Distribution.Client.ProjectConfig.Legacy (renderPackageLocationToken)
import qualified Distribution.Client.ProjectConfig.Lens as L
import Distribution.Client.ProjectConfig.Types (PackageConfig (..), ProjectConfig (..), ProjectConfigBuildOnly (..), ProjectConfigProvenance (..), ProjectConfigShared (..))
import Distribution.Client.Utils.Parsec
import qualified Distribution.Compat.CharParsing as P
import Distribution.Compat.Lens (Lens')
import Distribution.Compat.Prelude
import Distribution.FieldGrammar
import Distribution.Parsec (CabalParsing, Parsec (..), parsecHaskellString)
import Distribution.Pretty (Pretty (..))
import Distribution.Simple.Flag
import Distribution.Simple.InstallDirs
import Distribution.Solver.Types.ConstraintSource (ConstraintSource (..))
import Distribution.Solver.Types.ProjectConfigPath
import Distribution.Solver.Types.Settings (PreferVersion (..))
import Distribution.Types.PackageVersionConstraint (PackageVersionConstraint (..))
import qualified Text.PrettyPrint as PP

projectConfigFieldGrammar :: ProjectConfigPath -> [String] -> ParsecFieldGrammar' ProjectConfig
projectConfigFieldGrammar source knownPrograms = do
  projectPackages <- getPackageLocationTokens <$> monoidalField "packages" ignoredLens
  projectPackagesOptional <- getPackageLocationTokens <$> monoidalField "optional-packages" ignoredLens
  let projectPackagesRepo = mempty
  projectPackagesNamed <- monoidalFieldAla "extra-packages" formatPackageVersionConstraints L.projectPackagesNamed
  projectConfigBuildOnly <- blurFieldGrammar L.projectConfigBuildOnly projectConfigBuildOnlyFieldGrammar
  projectConfigShared <- blurFieldGrammar L.projectConfigShared (projectConfigSharedFieldGrammar source)
  let projectConfigProvenance = Set.singleton (Explicit source)
      projectConfigAllPackages = mempty
      projectConfigSpecificPackage = mempty
  projectConfigLocalPackages <- blurFieldGrammar L.projectConfigLocalPackages (packageConfigFieldGrammar knownPrograms)
  pure ProjectConfig{..}

newtype PackageLocationTokens = PackageLocationTokens {getPackageLocationTokens :: [String]}

instance Semigroup PackageLocationTokens where
  PackageLocationTokens a <> PackageLocationTokens b = PackageLocationTokens (a <> b)

instance Monoid PackageLocationTokens where
  mempty = PackageLocationTokens mempty

instance Parsec PackageLocationTokens where
  parsec = PackageLocationTokens <$> parseSep (Proxy :: Proxy FSep) parsePackageLocationTokenQ

instance Pretty PackageLocationTokens where
  pretty = prettySep (Proxy :: Proxy FSep) . map (PP.text . renderPackageLocationToken) . getPackageLocationTokens

ignoredLens :: Lens' ProjectConfig PackageLocationTokens
ignoredLens f s = s <$ f mempty

-- | This matches legacy parsing for @packages@ and @optional-packages@:
-- supports quoted strings, and for unquoted tokens allows commas only inside
-- balanced braces (e.g. ../{foo,bar}/).
parsePackageLocationTokenQ :: CabalParsing m => m String
parsePackageLocationTokenQ = parsecHaskellString <|> parsePackageLocationToken
  where
    parsePackageLocationToken = concat <$> some outerTerm
    outerTerm = outerToken <|> braces innerTerm
    innerTerm = concat <$> many (innerToken <|> braces innerTerm)
    outerToken = P.munch1 outerChar
    innerToken = P.munch1 innerChar
    outerChar c = not (isSpace c || c == '{' || c == '}' || c == ',')
    innerChar c = not (isSpace c || c == '{' || c == '}')
    braces p = ("{" <>) . (<> "}") <$> P.between (P.char '{') (P.char '}') p

formatPackageVersionConstraints :: [PackageVersionConstraint] -> List CommaVCat (Identity PackageVersionConstraint) PackageVersionConstraint
formatPackageVersionConstraints = alaList CommaVCat

projectConfigBuildOnlyFieldGrammar :: ParsecFieldGrammar' ProjectConfigBuildOnly
projectConfigBuildOnlyFieldGrammar = do
  projectConfigVerbosity <- optionalFieldDef "verbose" L.projectConfigVerbosity mempty
  let projectConfigDryRun = mempty
      projectConfigOnlyDeps = mempty
      projectConfigOnlyDownload = mempty
  projectConfigSummaryFile <- monoidalFieldAla "build-summary" (alaNubList VCat) L.projectConfigSummaryFile
  projectConfigLogFile <- optionalFieldDef "build-log" L.projectConfigLogFile mempty
  projectConfigBuildReports <- optionalFieldDef "remote-build-reporting" L.projectConfigBuildReports mempty
  projectConfigReportPlanningFailure <- optionalFieldDef "report-planning-failure" L.projectConfigReportPlanningFailure mempty
  projectConfigSymlinkBinDir <- optionalFieldDefAla "symlink-bindir" (alaFlag FilePathNT) L.projectConfigSymlinkBinDir mempty
  projectConfigNumJobs <- optionalFieldDefAla "jobs" (alaFlag NumJobs) L.projectConfigNumJobs mempty
  projectConfigUseSemaphore <- optionalFieldDef "semaphore" L.projectConfigUseSemaphore mempty
  projectConfigKeepGoing <- optionalFieldDef "keep-going" L.projectConfigKeepGoing mempty
  projectConfigOfflineMode <- optionalFieldDef "offline" L.projectConfigOfflineMode mempty
  projectConfigKeepTempFiles <- optionalFieldDef "haddock-keep-temp-files" L.projectConfigKeepTempFiles mempty
  projectConfigHttpTransport <- optionalFieldDefAla "http-transport" (alaFlag Token) L.projectConfigHttpTransport mempty
  projectConfigIgnoreExpiry <- optionalFieldDef "ignore-expiry" L.projectConfigIgnoreExpiry mempty
  projectConfigCacheDir <- optionalFieldDefAla "remote-repo-cache" (alaFlag FilePathNT) L.projectConfigCacheDir mempty
  projectConfigLogsDir <- optionalFieldDefAla "logs-dir" (alaFlag FilePathNT) L.projectConfigLogsDir mempty
  projectConfigClientInstallFlags <- blurFieldGrammar L.projectConfigClientInstallFlags clientInstallFlagsGrammar
  projectConfigBuildTimings <- optionalFieldDef "build-timings" L.projectConfigBuildTimings mempty
  pure ProjectConfigBuildOnly{..}

projectConfigSharedFieldGrammar :: ProjectConfigPath -> ParsecFieldGrammar' ProjectConfigShared
projectConfigSharedFieldGrammar source = do
  projectConfigDistDir <- optionalFieldDefAla "builddir" (alaFlag FilePathNT) L.projectConfigDistDir mempty
  let projectConfigConfigFile = mempty
  projectConfigProjectDir <- optionalFieldDefAla "project-dir" (alaFlag FilePathNT) L.projectConfigProjectDir mempty
  projectConfigProjectFile <- optionalFieldDefAla "project-file" (alaFlag FilePathNT) L.projectConfigProjectFile mempty
  let projectConfigProjectFileParser = mempty -- You can't set the parser type in the project file.
  projectConfigIgnoreProject <- optionalFieldDef "ignore-project" L.projectConfigIgnoreProject mempty
  projectConfigHcFlavor <- optionalFieldDef "compiler" L.projectConfigHcFlavor mempty
  projectConfigHcPath <- optionalFieldDefAla "with-compiler" (alaFlag FilePathNT) L.projectConfigHcPath mempty
  projectConfigHcPkg <- optionalFieldDefAla "with-hc-pkg" (alaFlag FilePathNT) L.projectConfigHcPkg mempty
  projectConfigHaddockIndex <- optionalFieldDef "doc-index-file" L.projectConfigHaddockIndex mempty
  projectConfigInstallDirs <- blurFieldGrammar L.projectConfigInstallDirs installDirsGrammar
  projectConfigPackageDBs <- monoidalFieldAla "package-dbs" (alaList' CommaFSep PackageDBNT) L.projectConfigPackageDBs
  let projectConfigRemoteRepos = mempty
      projectConfigLocalNoIndexRepos = mempty
  projectConfigActiveRepos <- monoidalField "active-repositories" L.projectConfigActiveRepos
  projectConfigIndexState <- monoidalField "index-state" L.projectConfigIndexState
  projectConfigStoreDir <- optionalFieldDefAla "store-dir" (alaFlag FilePathNT) L.projectConfigStoreDir mempty
  projectConfigConstraints <-
    monoidalFieldAla "constraints" (alaList' FSep ProjectConstraints) L.projectConfigConstraints
      ^^^ (fmap . fmap) (\(userConstraint, _) -> (userConstraint, ConstraintSourceProjectConfig source))
  projectConfigPreferences <- monoidalFieldAla "preferences" formatPackageVersionConstraints L.projectConfigPreferences
  projectConfigCabalVersion <- optionalFieldDef "cabal-lib-version" L.projectConfigCabalVersion mempty
  projectConfigSolver <- optionalFieldDef "solver" L.projectConfigSolver mempty
  projectConfigAllowOlder <- monoidalFieldAla "allow-older" AllowOlderNT L.projectConfigAllowOlder
  projectConfigAllowNewer <- monoidalFieldAla "allow-newer" AllowNewerNT L.projectConfigAllowNewer
  projectConfigWriteGhcEnvironmentFilesPolicy <- optionalFieldDef "write-ghc-environment-files" L.projectConfigWriteGhcEnvironmentFilesPolicy mempty
  projectConfigMaxBackjumps <- optionalFieldDefAla "max-backjumps" (alaFlag MaxBackjumps) L.projectConfigMaxBackjumps mempty
  projectConfigReorderGoals <- optionalFieldDef "reorder-goals" L.projectConfigReorderGoals mempty
  projectConfigCountConflicts <- optionalFieldDef "count-conflicts" L.projectConfigCountConflicts mempty
  projectConfigFineGrainedConflicts <- optionalFieldDef "fine-grained-conflicts" L.projectConfigFineGrainedConflicts mempty
  projectConfigMinimizeConflictSet <- optionalFieldDef "minimize-conflict-set" L.projectConfigMinimizeConflictSet mempty
  projectConfigStrongFlags <- optionalFieldDef "strong-flags" L.projectConfigStrongFlags mempty
  projectConfigAllowBootLibInstalls <- optionalFieldDef "allow-boot-library-installs" L.projectConfigAllowBootLibInstalls mempty
  projectConfigOnlyConstrained <- optionalFieldDef "reject-unconstrained-dependencies" L.projectConfigOnlyConstrained mempty
  projectConfigPerComponent <- optionalFieldDef "per-component" L.projectConfigPerComponent mempty
  projectConfigIndependentGoals <- optionalFieldDef "independent-goals" L.projectConfigIndependentGoals mempty
  projectConfigPreferVersion <- packageConfigPreferVersion
  projectConfigProgPathExtra <- monoidalFieldAla "extra-prog-path-shared-only" (alaNubList' FSep FilePathNT) L.projectConfigProgPathExtra
  projectConfigMultiRepl <- optionalFieldDef "multi-repl" L.projectConfigMultiRepl mempty
  pure ProjectConfigShared{..}

packageConfigFieldGrammar :: [String] -> ParsecFieldGrammar' PackageConfig
packageConfigFieldGrammar knownPrograms = do
  haddockAll <-
    optionalFieldDef "haddock-all" noopLens mempty
      ^^^ hiddenField
  let packageConfigProgramPaths = mempty
      packageConfigProgramArgs = mempty
  packageConfigProgramPathExtra <- monoidalFieldAla "extra-prog-path" (alaNubList' FSep FilePathNT) L.packageConfigProgramPathExtra
  packageConfigFlagAssignment <- monoidalField "flags" L.packageConfigFlagAssignment
  packageConfigVanillaLib <- optionalFieldDef "library-vanilla" L.packageConfigVanillaLib mempty
  packageConfigSharedLib <- optionalFieldDef "shared" L.packageConfigSharedLib mempty
  packageConfigStaticLib <- optionalFieldDef "static" L.packageConfigStaticLib mempty
  packageConfigBytecodeLib <- optionalFieldDef "library-bytecode" L.packageConfigBytecodeLib mempty
  packageConfigDynExe <- optionalFieldDef "executable-dynamic" L.packageConfigDynExe mempty
  packageConfigFullyStaticExe <- optionalFieldDef "executable-static" L.packageConfigFullyStaticExe mempty
  packageConfigProf <- optionalFieldDef "profiling" L.packageConfigProf mempty
  packageConfigProfLib <- optionalFieldDef "library-profiling" L.packageConfigProfLib mempty
  packageConfigProfShared <- optionalFieldDef "profiling-shared" L.packageConfigProfShared mempty
  packageConfigProfExe <- optionalFieldDef "executable-profiling" L.packageConfigProfExe mempty
  packageConfigProfDetail <- optionalFieldDef "profiling-detail" L.packageConfigProfDetail mempty
  packageConfigProfLibDetail <- optionalFieldDef "library-profiling-detail" L.packageConfigProfLibDetail mempty
  packageConfigConfigureArgs <- monoidalFieldAla "configure-options" (alaList' NoCommaFSep Token) L.packageConfigConfigureArgs
  packageConfigOptimization <- optionalFieldDef "optimization" L.packageConfigOptimization mempty
  packageConfigProgPrefix <- optionalFieldDef "program-prefix" L.packageConfigProgPrefix mempty
  packageConfigProgSuffix <- optionalFieldDef "program-suffix" L.packageConfigProgSuffix mempty
  packageConfigExtraLibDirs <- monoidalFieldAla "extra-lib-dirs" (alaList' FSep FilePathNT) L.packageConfigExtraLibDirs
  packageConfigExtraLibDirsStatic <- monoidalFieldAla "extra-lib-dirs-static" (alaList' FSep FilePathNT) L.packageConfigExtraLibDirsStatic
  packageConfigExtraFrameworkDirs <- monoidalFieldAla "extra-framework-dirs" (alaList' FSep FilePathNT) L.packageConfigExtraFrameworkDirs
  packageConfigExtraIncludeDirs <- monoidalFieldAla "extra-include-dirs" (alaList' FSep FilePathNT) L.packageConfigExtraIncludeDirs
  packageConfigGHCiLib <- optionalFieldDef "library-for-ghci" L.packageConfigGHCiLib mempty
  packageConfigSplitSections <- optionalFieldDef "split-sections" L.packageConfigSplitSections mempty
  packageConfigSplitObjs <- optionalFieldDef "split-objs" L.packageConfigSplitObjs mempty
  packageConfigStripExes <- optionalFieldDef "executable-stripping" L.packageConfigStripExes mempty
  packageConfigStripLibs <- optionalFieldDef "library-stripping" L.packageConfigStripLibs mempty
  packageConfigTests <- optionalFieldDef "tests" L.packageConfigTests mempty
  packageConfigBenchmarks <- optionalFieldDef "benchmarks" L.packageConfigBenchmarks mempty
  packageConfigCoverage <- packageConfigCoverageGrammar
  packageConfigRelocatable <- optionalFieldDef "relocatable" L.packageConfigRelocatable mempty
  packageConfigDebugInfo <- optionalFieldDef "debug-info" L.packageConfigDebugInfo mempty
  packageConfigDumpBuildInfo <- optionalFieldDef "build-info" L.packageConfigDumpBuildInfo mempty
  packageConfigRunTests <- optionalFieldDef "run-tests" L.packageConfigRunTests mempty
  packageConfigDocumentation <- optionalFieldDef "documentation" L.packageConfigDocumentation mempty
  packageConfigHaddockHoogle <- optionalFieldDef "haddock-hoogle" L.packageConfigHaddockHoogle mempty
  packageConfigHaddockHtml <- optionalFieldDef "haddock-html" L.packageConfigHaddockHtml mempty
  packageConfigHaddockHtmlLocation <- optionalFieldDefAla "haddock-html-location" (alaFlag Token) L.packageConfigHaddockHtmlLocation mempty
  packageConfigHaddockForeignLibs' <- optionalFieldDef "haddock-foreign-libraries" L.packageConfigHaddockForeignLibs mempty
  packageConfigHaddockExecutables' <- optionalFieldDef "haddock-executables" L.packageConfigHaddockExecutables mempty
  packageConfigHaddockTestSuites' <- optionalFieldDef "haddock-tests" L.packageConfigHaddockTestSuites mempty
  packageConfigHaddockBenchmarks' <- optionalFieldDef "haddock-benchmarks" L.packageConfigHaddockBenchmarks mempty
  packageConfigHaddockInternal <- optionalFieldDef "haddock-internal" L.packageConfigHaddockInternal mempty
  packageConfigHaddockCss <- optionalFieldDefAla "haddock-css" (alaFlag FilePathNT) L.packageConfigHaddockCss mempty
  packageConfigHaddockLinkedSource <- optionalFieldDef "haddock-hyperlink-source" L.packageConfigHaddockLinkedSource mempty
  packageConfigHaddockQuickJump <- optionalFieldDef "haddock-quickjump" L.packageConfigHaddockQuickJump mempty
  packageConfigHaddockHscolourCss <- optionalFieldDefAla "haddock-hscolour-css" (alaFlag FilePathNT) L.packageConfigHaddockHscolourCss mempty
  packageConfigHaddockContents <- optionalFieldDef "haddock-contents-location" L.packageConfigHaddockContents mempty
  packageConfigHaddockIndex <- optionalFieldDef "haddock-index-location" L.packageConfigHaddockIndex mempty
  packageConfigHaddockBaseUrl <- optionalFieldDefAla "haddock-base-url" (alaFlag Token) L.packageConfigHaddockBaseUrl mempty
  packageConfigHaddockResourcesDir <- optionalFieldDefAla "haddock-resources-dir" (alaFlag Token) L.packageConfigHaddockResourcesDir mempty
  packageConfigHaddockOutputDir <- optionalFieldDefAla "haddock-output-dir" (alaFlag FilePathNT) L.packageConfigHaddockOutputDir mempty
  packageConfigHaddockUseUnicode <- optionalFieldDef "haddock-use-unicode" L.packageConfigHaddockUseUnicode mempty
  packageConfigHaddockForHackage <- optionalFieldDef "haddock-for-hackage" L.packageConfigHaddockForHackage mempty
  packageConfigTestHumanLog <- optionalFieldDef "test-log" L.packageConfigTestHumanLog mempty
  packageConfigTestMachineLog <- optionalFieldDef "test-machine-log" L.packageConfigTestMachineLog mempty
  packageConfigTestShowDetails <- optionalFieldDef "test-show-details" L.packageConfigTestShowDetails mempty
  packageConfigTestKeepTix <- optionalFieldDef "test-keep-tix-files" L.packageConfigTestKeepTix mempty
  packageConfigTestWrapper <- optionalFieldDefAla "test-wrapper" (alaFlag FilePathNT) L.packageConfigTestWrapper mempty
  packageConfigTestFailWhenNoTestSuites <- optionalFieldDef "test-fail-when-no-test-suites" L.packageConfigTestFailWhenNoTestSuites mempty
  packageConfigTestTestOptions <- monoidalFieldAla "test-options" (alaList NoCommaFSep) L.packageConfigTestTestOptions
  packageConfigBenchmarkOptions <- monoidalFieldAla "benchmark-options" (alaList NoCommaFSep) L.packageConfigBenchmarkOptions
  -- A PackageConfig may contain -options and -location fields inside a package * (projectConfigAllPackages) or package <name> stanza (packageConfigSpecificPackage).
  -- When declared at top level (packageConfigLocalPackages), the PackageConfig must contain a program-options stanza/program-locations for these fields.
  traverse_ (knownField . BS.pack . (<> "-options")) knownPrograms
  traverse_ (knownField . BS.pack . (<> "-location")) knownPrograms
  pure
    PackageConfig
      { -- The haddock-all field provides a default value, but explicit declarations can override it
        packageConfigHaddockForeignLibs = haddockAll <> packageConfigHaddockForeignLibs'
      , packageConfigHaddockExecutables = haddockAll <> packageConfigHaddockExecutables'
      , packageConfigHaddockTestSuites = haddockAll <> packageConfigHaddockTestSuites'
      , packageConfigHaddockBenchmarks = haddockAll <> packageConfigHaddockBenchmarks'
      , ..
      }
  where
    noopLens f s = s <$ f mempty

packageConfigCoverageGrammar :: ParsecFieldGrammar PackageConfig (Distribution.Simple.Flag.Flag Bool)
packageConfigCoverageGrammar =
  (<>)
    <$> optionalFieldDef "coverage" L.packageConfigCoverage mempty
    <*> optionalFieldDef "library-coverage" L.packageConfigCoverage mempty
      ^^^ deprecatedSince CabalSpecV1_22 "Please use 'coverage' field instead."

packageConfigPreferVersion :: ParsecFieldGrammar ProjectConfigShared (Flag PreferVersion)
packageConfigPreferVersion =
  mappend . fmap (bool PreferInstalledOrLatest PreferOldest)
    <$> optionalFieldDef "prefer-oldest" L.projectConfigPreferOldest mempty
      ^^^ deprecatedSince CabalSpecV3_20 "Please use 'prefer-version' field instead."
    <*> optionalFieldDef "prefer-version" L.projectConfigPreferVersion mempty
