-----------------------------------------------------------------------------

-- Module      :  Distribution.Simple.Errors
-- Copyright   :
-- License     :
--
-- Maintainer  :
-- Portability :
--
-- A collection of Exception Types in the Cabal library package

module Distribution.Simple.Errors
  ( CabalException (..)
  , exceptionCode
  , exceptionMessage
  ) where

import Distribution.Compat.Prelude
import Distribution.Compiler
import Distribution.InstalledPackageInfo
import Distribution.ModuleName
import Distribution.Pretty
  ( prettyShow
  )
import Distribution.Types.BenchmarkType
import Distribution.Types.PackageId
import Distribution.Types.TestType
import Distribution.Types.UnitId
import Distribution.Version

-- Types representing exceptions thrown by functions in all the modules of Cabal Package
data CabalException
  = NoBenchMarkProgram FilePath
  | EnableBenchMark
  | BenchMarkNameDisabled String
  | NoBenchMark String
  | NoLibraryFound
  | CompilerNotInstalled CompilerFlavor
  | CantFindIncludeFile String
  | UnsupportedTestSuite String
  | UnsupportedBenchMark String
  | NoIncludeFileFound String
  | NoModuleFound ModuleName [String]
  | RegMultipleInstancePkg
  | SuppressingChecksOnFile
  | NoSupportDirStylePackageDb
  | OnlySupportSpecificPackageDb
  | FailedToParseOutputDescribe String PackageId
  | DumpFailed String String
  | FailedToParseOutputDump String
  | ListFailed String
  | FailedToParseOutputList String
  | ProgramNotFound String
  | NoSupportForHoogle
  | NoSupportForQuickJumpFlag
  | NoGHCVersionFromHaddock
  | NoGHCVersionFromCompiler
  | HaddockAndGHCVersionDoesntMatch Version Version
  | MustHaveSharedLibraries
  | HaddockPackageFlags [(InstalledPackageInfo, [UnitId])]
  | UnknownCompilerFlavor CompilerFlavor
  | FailedToDetermineTarget
  | NoMultipleTargets
  | REPLNotSupported
  | NoSupportBuildingTestSuite TestType
  | NoSupportBuildingBenchMark BenchmarkType
  | BuildingNotSupportedWithCompiler
  | ProvideHaskellSuiteTool String
  | CannotDetermineCompilerVersion
  | PkgDumpFailed
  | FailedToParseOutput
  | CantFindSourceModule ModuleName
  | VersionMismatchJS FilePath Version FilePath Version
  | VersionMismatchGHCJS FilePath Version FilePath Version
  | GlobalPackageDBLimitation
  | GlobalPackageDBSpecifiedFirst
  | MatchDirFileGlob String
  | MatchDirFileGlobErrors [String]
  | ErrorParsingFileDoesntExist FilePath
  | FailedParsing String
  | NotFoundMsg
  | UnrecognisedBuildTarget [String]
  | ReportBuildTargetProblems [(String, [String], String)]
  | UnknownBuildTarget [(String, [(String, String)])]
  | AmbiguousBuildTarget [(String, [(String, String)])]
  | CheckBuildTargets String
  | VersionMismatchGHC FilePath Version FilePath Version
  | CheckPackageDbStackPost76
  | CheckPackageDbStackPre76
  | GlobalPackageDbSpecifiedFirst
  | CantInstallForeignLib
  | NoSupportForPreProcessingTest TestType
  | NoSupportForPreProcessingBenchmark BenchmarkType
  | CantFindSourceForPreProcessFile String
  | NoSupportPreProcessingTestExtras TestType
  | NoSupportPreProcessingBenchmarkExtras BenchmarkType
  | UnlitException String
  deriving (Show, Typeable)

exceptionCode :: CabalException -> Int
exceptionCode e = case e of
  NoBenchMarkProgram{} -> 1678
  EnableBenchMark{} -> 1453
  BenchMarkNameDisabled{} -> 2781
  NoBenchMark{} -> 1654
  NoLibraryFound -> 2546
  CompilerNotInstalled{} -> 7465
  CantFindIncludeFile{} -> 3876
  UnsupportedTestSuite{} -> 3245
  UnsupportedBenchMark{} -> 9123
  NoIncludeFileFound{} -> 2987
  NoModuleFound{} -> 6421
  RegMultipleInstancePkg{} -> 3421
  SuppressingChecksOnFile{} -> 5436
  NoSupportDirStylePackageDb -> 2980
  OnlySupportSpecificPackageDb -> 6547
  FailedToParseOutputDescribe{} -> 7218
  DumpFailed{} -> 6736
  FailedToParseOutputDump{} -> 9076
  ListFailed{} -> 5109
  FailedToParseOutputList{} -> 7650
  ProgramNotFound{} -> 4123
  NoSupportForHoogle{} -> 8706
  NoSupportForQuickJumpFlag{} -> 7086
  NoGHCVersionFromHaddock -> 5045
  NoGHCVersionFromCompiler -> 4098
  HaddockAndGHCVersionDoesntMatch{} -> 1998
  MustHaveSharedLibraries{} -> 6032
  HaddockPackageFlags{} -> 4567
  UnknownCompilerFlavor{} -> 3102
  FailedToDetermineTarget{} -> 5049
  NoMultipleTargets{} -> 6091
  REPLNotSupported{} -> 1098
  NoSupportBuildingTestSuite{} -> 4106
  NoSupportBuildingBenchMark{} -> 5320
  BuildingNotSupportedWithCompiler{} -> 7077
  ProvideHaskellSuiteTool{} -> 7509
  CannotDetermineCompilerVersion{} -> 4519
  PkgDumpFailed{} -> 2290
  FailedToParseOutput{} -> 5500
  CantFindSourceModule{} -> 8870
  VersionMismatchJS{} -> 9001
  VersionMismatchGHCJS{} -> 4001
  GlobalPackageDBLimitation{} -> 5002
  GlobalPackageDBSpecifiedFirst{} -> 3901
  MatchDirFileGlob{} -> 9760
  MatchDirFileGlobErrors{} -> 6661
  ErrorParsingFileDoesntExist{} -> 1234
  FailedParsing{} -> 6565
  NotFoundMsg{} -> 8011
  UnrecognisedBuildTarget{} -> 3410
  ReportBuildTargetProblems{} -> 5504
  UnknownBuildTarget{} -> 4444
  AmbiguousBuildTarget{} -> 7865
  CheckBuildTargets{} -> 4733
  VersionMismatchGHC{} -> 4000
  CheckPackageDbStackPost76{} -> 3000
  CheckPackageDbStackPre76{} -> 5640
  GlobalPackageDbSpecifiedFirst{} -> 2345
  CantInstallForeignLib{} -> 8221
  NoSupportForPreProcessingTest{} -> 3008
  NoSupportForPreProcessingBenchmark{} -> 6990
  CantFindSourceForPreProcessFile{} -> 7554
  NoSupportPreProcessingTestExtras{} -> 7886
  NoSupportPreProcessingBenchmarkExtras{} -> 9999
  UnlitException{} -> 5454

exceptionMessage :: CabalException -> String
exceptionMessage e = case e of
  NoBenchMarkProgram cmd -> "Could not find benchmark program \"" ++ cmd ++ "\". Did you build the package first?"
  EnableBenchMark -> "No benchmarks enabled. Did you remember to configure with " ++ "\'--enable-benchmarks\'?"
  BenchMarkNameDisabled bmName -> "Package configured with benchmark " ++ bmName ++ " disabled."
  NoBenchMark bmName -> "no such benchmark: " ++ bmName
  NoLibraryFound -> "No executables and no library found. Nothing to do."
  CompilerNotInstalled compilerFlavor -> "installing with " ++ prettyShow compilerFlavor ++ "is not implemented"
  CantFindIncludeFile file -> "can't find include file " ++ file
  UnsupportedTestSuite testType -> "Unsupported test suite type: " ++ testType
  UnsupportedBenchMark benchMarkType -> "Unsupported benchmark type: " ++ benchMarkType
  NoIncludeFileFound f -> "can't find include file " ++ f
  NoModuleFound m suffixes ->
    "Could not find module: "
      ++ prettyShow m
      ++ " with any suffix: "
      ++ show suffixes
      ++ ".\n"
      ++ "If the module "
      ++ "is autogenerated it should be added to 'autogen-modules'."
  RegMultipleInstancePkg -> "HcPkg.register: the compiler does not support,registering multiple instances of packages."
  SuppressingChecksOnFile -> "HcPkg.register: the compiler does not support ,suppressing checks on files."
  NoSupportDirStylePackageDb -> "HcPkg.writeRegistrationFileDirectly: compiler does not support dir style package dbs"
  OnlySupportSpecificPackageDb -> "HcPkg.writeRegistrationFileDirectly: only supports SpecificPackageDB for now"
  FailedToParseOutputDescribe programId packageId -> "failed to parse output of '" ++ programId ++ " describe " ++ prettyShow packageId ++ "'"
  DumpFailed programId exception -> programId ++ " dump failed: " ++ exception
  FailedToParseOutputDump programId -> "failed to parse output of '" ++ programId ++ " dump'"
  ListFailed programId -> programId ++ " list failed"
  FailedToParseOutputList programId -> "failed to parse output of '" ++ programId ++ " list'"
  ProgramNotFound progName -> "The program '" ++ progName ++ "' is required but it could not be found"
  NoSupportForHoogle -> "Haddock 2.0 and 2.1 do not support the --hoogle flag."
  NoSupportForQuickJumpFlag -> "Haddock prior to 2.19 does not support the --quickjump flag."
  NoGHCVersionFromHaddock -> "Could not get GHC version from Haddock"
  NoGHCVersionFromCompiler -> "Could not get GHC version from compiler"
  HaddockAndGHCVersionDoesntMatch ghcVersion haddockGhcVersion ->
    "Haddock's internal GHC version must match the configured "
      ++ "GHC version.\n"
      ++ "The GHC version is "
      ++ prettyShow ghcVersion
      ++ " but "
      ++ "haddock is using GHC version "
      ++ prettyShow haddockGhcVersion
  MustHaveSharedLibraries -> "Must have vanilla or shared libraries " ++ "enabled in order to run haddock"
  HaddockPackageFlags inf ->
    "internal error when calculating transitive "
      ++ "package dependencies.\nDebug info: "
      ++ show inf
  UnknownCompilerFlavor compilerFlavor -> "dumpBuildInfo: Unknown compiler flavor: " ++ show compilerFlavor
  FailedToDetermineTarget -> "Failed to determine target."
  NoMultipleTargets -> "The 'repl' command does not support multiple targets at once."
  REPLNotSupported -> "A REPL is not supported with this compiler."
  NoSupportBuildingTestSuite testType -> "No support for building test suite type " ++ show testType
  NoSupportBuildingBenchMark benchMarkType -> "No support for building benchmark type " ++ show benchMarkType
  BuildingNotSupportedWithCompiler -> "Building is not supported with this compiler."
  ProvideHaskellSuiteTool msg -> show msg
  CannotDetermineCompilerVersion -> "haskell-suite: couldn't determine compiler version"
  PkgDumpFailed -> "pkg dump failed"
  FailedToParseOutput -> "failed to parse output of 'pkg dump'"
  CantFindSourceModule moduleName -> "can't find source for module " ++ prettyShow moduleName
  VersionMismatchJS ghcjsProgPath ghcjsVersion ghcjsPkgProgPath ghcjsPkgGhcjsVersion ->
    "Version mismatch between ghcjs and ghcjs-pkg: "
      ++ show ghcjsProgPath
      ++ " is version "
      ++ prettyShow ghcjsVersion
      ++ " "
      ++ show ghcjsPkgProgPath
      ++ " is version "
      ++ prettyShow ghcjsPkgGhcjsVersion
  VersionMismatchGHCJS ghcjsProgPath ghcjsGhcVersion ghcjsPkgProgPath ghcjsPkgVersion ->
    "Version mismatch between ghcjs and ghcjs-pkg: "
      ++ show ghcjsProgPath
      ++ " was built with GHC version "
      ++ prettyShow ghcjsGhcVersion
      ++ " "
      ++ show ghcjsPkgProgPath
      ++ " was built with GHC version "
      ++ prettyShow ghcjsPkgVersion
  GlobalPackageDBLimitation ->
    "With current ghc versions the global package db is always used "
      ++ "and must be listed first. This ghc limitation may be lifted in "
      ++ "future, see https://gitlab.haskell.org/ghc/ghc/-/issues/5977"
  GlobalPackageDBSpecifiedFirst ->
    "If the global package db is specified, it must be "
      ++ "specified first and cannot be specified multiple times"
  MatchDirFileGlob pathError -> pathError
  MatchDirFileGlobErrors errors -> unlines errors
  ErrorParsingFileDoesntExist filePath -> "Error Parsing: file \"" ++ filePath ++ "\" doesn't exist. Cannot continue."
  FailedParsing name -> "Failed parsing \"" ++ name ++ "\"."
  NotFoundMsg ->
    "The package has a './configure' script. "
      ++ "If you are on Windows, This requires a "
      ++ "Unix compatibility toolchain such as MinGW+MSYS or Cygwin. "
      ++ "If you are not on Windows, ensure that an 'sh' command "
      ++ "is discoverable in your path."
  UnrecognisedBuildTarget target ->
    unlines
      [ "Unrecognised build target '" ++ name ++ "'."
      | name <- target
      ]
      ++ "Examples:\n"
      ++ " - build foo          -- component name "
      ++ "(library, executable, test-suite or benchmark)\n"
      ++ " - build Data.Foo     -- module name\n"
      ++ " - build Data/Foo.hsc -- file name\n"
      ++ " - build lib:foo exe:foo   -- component qualified by kind\n"
      ++ " - build foo:Data.Foo      -- module qualified by component\n"
      ++ " - build foo:Data/Foo.hsc  -- file qualified by component"
  ReportBuildTargetProblems targets ->
    unlines
      [ "Unrecognised build target '"
        ++ target
        ++ "'.\n"
        ++ "Expected a "
        ++ intercalate " or " expected
        ++ ", rather than '"
        ++ got
        ++ "'."
      | (target, expected, got) <- targets
      ]
  UnknownBuildTarget targets ->
    unlines
      [ "Unknown build target '"
        ++ target
        ++ "'.\nThere is no "
        ++ intercalate
          " or "
          [ mungeThing thing ++ " '" ++ got ++ "'"
          | (thing, got) <- nosuch
          ]
        ++ "."
      | (target, nosuch) <- targets
      ]
    where
      mungeThing "file" = "file target"
      mungeThing thing = thing
  AmbiguousBuildTarget targets ->
    unlines
      [ "Ambiguous build target '"
        ++ target
        ++ "'. It could be:\n "
        ++ unlines
          [ "   "
            ++ ut
            ++ " ("
            ++ bt
            ++ ")"
          | (ut, bt) <- amb
          ]
      | (target, amb) <- targets
      ]
  CheckBuildTargets errorStr -> errorStr
  VersionMismatchGHC ghcProgPath ghcVersion ghcPkgProgPath ghcPkgVersion ->
    "Version mismatch between ghc and ghc-pkg: "
      ++ ghcProgPath
      ++ " is version "
      ++ prettyShow ghcVersion
      ++ " "
      ++ ghcPkgProgPath
      ++ " is version "
      ++ prettyShow ghcPkgVersion
  CheckPackageDbStackPost76 ->
    "If the global package db is specified, it must be "
      ++ "specified first and cannot be specified multiple times"
  CheckPackageDbStackPre76 ->
    "With current ghc versions the global package db is always used "
      ++ "and must be listed first. This ghc limitation is lifted in GHC 7.6,"
      ++ "see https://gitlab.haskell.org/ghc/ghc/-/issues/5977"
  GlobalPackageDbSpecifiedFirst ->
    "If the global package db is specified, it must be "
      ++ "specified first and cannot be specified multiple times"
  CantInstallForeignLib -> "Can't install foreign-library symlink on non-Linux OS"
  NoSupportForPreProcessingTest tt ->
    "No support for preprocessing test "
      ++ "suite type "
      ++ prettyShow tt
  NoSupportForPreProcessingBenchmark tt ->
    "No support for preprocessing benchmark "
      ++ "type "
      ++ prettyShow tt
  CantFindSourceForPreProcessFile errorStr -> errorStr
  NoSupportPreProcessingTestExtras tt ->
    "No support for preprocessing test suite type "
      ++ prettyShow tt
  NoSupportPreProcessingBenchmarkExtras tt ->
    "No support for preprocessing benchmark "
      ++ "type "
      ++ prettyShow tt
  UnlitException str -> str
