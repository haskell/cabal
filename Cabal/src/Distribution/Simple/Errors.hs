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
  | VersionMisMatch FilePath Version FilePath Version
  | VersionMisMatchGHC FilePath Version FilePath Version
  | GlobalPackageDBLimitation
  | GlobalPackageDBSpecifiedFirst
  | MatchDirFileGlob String
  | MatchDirFileGlobErrors [String]
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
  VersionMisMatch{} -> 9001
  VersionMisMatchGHC{} -> 4001
  GlobalPackageDBLimitation{} -> 5002
  GlobalPackageDBSpecifiedFirst{} -> 3901
  MatchDirFileGlob{} -> 9760
  MatchDirFileGlobErrors{} -> 6661

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
  VersionMisMatch ghcjsProgPath ghcjsVersion ghcjsPkgProgPath ghcjsPkgGhcjsVersion ->
    "Version mismatch between ghcjs and ghcjs-pkg: "
      ++ show ghcjsProgPath
      ++ " is version "
      ++ prettyShow ghcjsVersion
      ++ " "
      ++ show ghcjsPkgProgPath
      ++ " is version "
      ++ prettyShow ghcjsPkgGhcjsVersion
  VersionMisMatchGHC ghcjsProgPath ghcjsGhcVersion ghcjsPkgProgPath ghcjsPkgVersion ->
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
