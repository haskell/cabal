{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

module UnitTests.Distribution.Client.Init.Golden
  ( tests
  ) where

import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit

import qualified Data.ByteString.Lazy.Char8 as BS8
import Data.List.NonEmpty (fromList)
import Data.List.NonEmpty as NEL (NonEmpty, drop)
#if __GLASGOW_HASKELL__ < 804
import Data.Semigroup ((<>))
#endif

import Distribution.CabalSpecVersion
import Distribution.Client.Init.FlagExtractors
import Distribution.Client.Init.Format
import Distribution.Client.Init.Interactive.Command
import Distribution.Client.Init.Types
import Distribution.Client.Types.SourcePackageDb
import Distribution.Fields.Pretty
import Distribution.Simple.Flag
import Distribution.Simple.PackageIndex hiding (fromList)
import Distribution.Types.PackageName (PackageName)
import Distribution.Verbosity

import System.FilePath

import Distribution.Client.Init.Defaults
import UnitTests.Distribution.Client.Init.Utils

-- -------------------------------------------------------------------- --
-- golden test suite

-- | Golden executable tests.
--
-- We test target generation against a golden file in @tests/fixtures/init/@ for
-- executables, libraries, and test targets with the following:
--
-- * Empty flags, non-simple target gen, no special options
-- * Empty flags, simple target gen, no special options
-- * Empty flags, non-simple target gen, with generated comments (no minimal setting)
-- * Empty flags, non-simple target gen, with minimal setting (no generated comments)
-- * Empty flags, non-simple target gen, minimal and generated comments set.
--
-- Additionally, we test whole @.cabal@ file generation for every combination
-- of library, lib + tests, exe, exe + tests, exe + lib, exe + lib + tests
-- and so on against the same options.
tests
  :: Verbosity
  -> InitFlags
  -> InstalledPackageIndex
  -> SourcePackageDb
  -> TestTree
tests v initFlags pkgIx srcDb =
  testGroup
    "golden"
    [ goldenLibTests v pkgIx pkgDir pkgName
    , goldenExeTests v pkgIx pkgDir pkgName
    , goldenTestTests v pkgIx pkgDir pkgName
    , goldenPkgDescTests v srcDb pkgDir pkgName
    , goldenCabalTests v pkgIx srcDb
    ]
  where
    pkgDir =
      evalPrompt (getPackageDir initFlags) $
        fromList ["."]
    pkgName =
      evalPrompt (packageNamePrompt srcDb initFlags) $
        fromList ["test-package", "test-package", "y"]

goldenPkgDescTests
  :: Verbosity
  -> SourcePackageDb
  -> FilePath
  -> PackageName
  -> TestTree
goldenPkgDescTests v srcDb pkgDir pkgName =
  testGroup
    "package description golden tests"
    [ goldenVsString
        "Empty flags, non-simple, no comments"
        (goldenPkgDesc "pkg.golden")
        $ let opts = WriteOpts False False False v pkgDir Library pkgName defaultCabalVersion
           in runPkgDesc opts emptyFlags pkgArgs
    , goldenVsString
        "Empty flags, non-simple, with comments"
        (goldenPkgDesc "pkg-with-comments.golden")
        $ let opts = WriteOpts False False False v pkgDir Library pkgName defaultCabalVersion
           in runPkgDesc opts emptyFlags pkgArgs
    , goldenVsString
        "Dummy flags, >= cabal version syntax, with comments"
        (goldenPkgDesc "pkg-with-flags.golden")
        $ let opts = WriteOpts False False False v pkgDir Library pkgName defaultCabalVersion
           in runPkgDesc opts (dummyFlags{cabalVersion = Flag CabalSpecV1_0}) pkgArgs
    , goldenVsString
        "Dummy flags, old cabal version, with comments"
        (goldenPkgDesc "pkg-old-cabal-with-flags.golden")
        $ let opts = WriteOpts False False False v pkgDir Library pkgName defaultCabalVersion
           in runPkgDesc opts (dummyFlags{cabalVersion = Flag CabalSpecV2_0}) pkgArgs
    ]
  where
    runPkgDesc opts flags args = do
      case _runPrompt (genPkgDescription flags srcDb) args of
        Left e -> assertFailure $ show e
        Right (pkg, _) -> mkStanza $ mkPkgDescription opts pkg

goldenExeTests
  :: Verbosity
  -> InstalledPackageIndex
  -> FilePath
  -> PackageName
  -> TestTree
goldenExeTests v pkgIx pkgDir pkgName =
  testGroup
    "exe golden tests"
    [ goldenVsString
        "Empty flags, not simple, no options, no comments"
        (goldenExe "exe-no-comments.golden")
        $ let opts = WriteOpts False False True v pkgDir Executable pkgName defaultCabalVersion
           in runGoldenExe opts exeArgs emptyFlags
    , goldenVsString
        "Empty flags, not simple, with comments + no minimal"
        (goldenExe "exe-with-comments.golden")
        $ let opts = WriteOpts False False False v pkgDir Executable pkgName defaultCabalVersion
           in runGoldenExe opts exeArgs emptyFlags
    , goldenVsString
        "Empty flags, not simple, with minimal + no comments"
        (goldenExe "exe-minimal-no-comments.golden")
        $ let opts = WriteOpts False True True v pkgDir Executable pkgName defaultCabalVersion
           in runGoldenExe opts exeArgs emptyFlags
    , goldenVsString
        "Empty flags, not simple, with minimal + comments"
        (goldenExe "exe-simple-minimal-with-comments.golden")
        $ let opts = WriteOpts False True False v pkgDir Executable pkgName defaultCabalVersion
           in runGoldenExe opts exeArgs emptyFlags
    , goldenVsString
        "Build tools flag, not simple, with comments + no minimal"
        (goldenExe "exe-build-tools-with-comments.golden")
        $ let opts = WriteOpts False False False v pkgDir Executable pkgName defaultCabalVersion
           in runGoldenExe opts exeArgs (emptyFlags{buildTools = Flag ["happy"]})
    ]
  where
    runGoldenExe opts args flags =
      case _runPrompt (genExeTarget flags pkgIx) args of
        Right (t, _) -> mkStanza [mkExeStanza opts $ t{_exeDependencies = mangleBaseDep t _exeDependencies}]
        Left e -> assertFailure $ show e

goldenLibTests
  :: Verbosity
  -> InstalledPackageIndex
  -> FilePath
  -> PackageName
  -> TestTree
goldenLibTests v pkgIx pkgDir pkgName =
  testGroup
    "lib golden tests"
    [ goldenVsString
        "Empty flags, not simple, no options, no comments"
        (goldenLib "lib-no-comments.golden")
        $ let opts = WriteOpts False False True v pkgDir Library pkgName defaultCabalVersion
           in runGoldenLib opts libArgs emptyFlags
    , goldenVsString
        "Empty flags, simple, no options, no comments"
        (goldenLib "lib-simple-no-comments.golden")
        $ let opts = WriteOpts False False True v pkgDir Library pkgName defaultCabalVersion
           in runGoldenLib opts libArgs emptyFlags
    , goldenVsString
        "Empty flags, not simple, with comments + no minimal"
        (goldenLib "lib-with-comments.golden")
        $ let opts = WriteOpts False False False v pkgDir Library pkgName defaultCabalVersion
           in runGoldenLib opts libArgs emptyFlags
    , goldenVsString
        "Empty flags, not simple, with minimal + no comments"
        (goldenLib "lib-minimal-no-comments.golden")
        $ let opts = WriteOpts False True True v pkgDir Library pkgName defaultCabalVersion
           in runGoldenLib opts libArgs emptyFlags
    , goldenVsString
        "Empty flags, not simple, with minimal + comments"
        (goldenLib "lib-simple-minimal-with-comments.golden")
        $ let opts = WriteOpts False True False v pkgDir Library pkgName defaultCabalVersion
           in runGoldenLib opts libArgs emptyFlags
    , goldenVsString
        "Build tools flag, not simple, with comments + no minimal"
        (goldenLib "lib-build-tools-with-comments.golden")
        $ let opts = WriteOpts False False False v pkgDir Library pkgName defaultCabalVersion
           in runGoldenLib opts libArgs (emptyFlags{buildTools = Flag ["happy"]})
    ]
  where
    runGoldenLib opts args flags =
      case _runPrompt (genLibTarget flags pkgIx) args of
        Right (t, _) -> mkStanza [mkLibStanza opts $ t{_libDependencies = mangleBaseDep t _libDependencies}]
        Left e -> assertFailure $ show e

goldenTestTests
  :: Verbosity
  -> InstalledPackageIndex
  -> FilePath
  -> PackageName
  -> TestTree
goldenTestTests v pkgIx pkgDir pkgName =
  testGroup
    "test golden tests"
    [ goldenVsString
        "Empty flags, not simple, no options, no comments"
        (goldenTest "test-no-comments.golden")
        $ let opts = WriteOpts False False True v pkgDir Library pkgName defaultCabalVersion
           in runGoldenTest opts testArgs emptyFlags
    , goldenVsString
        "Empty flags, not simple, with comments + no minimal"
        (goldenTest "test-with-comments.golden")
        $ let opts = WriteOpts False False False v pkgDir Library pkgName defaultCabalVersion
           in runGoldenTest opts testArgs emptyFlags
    , goldenVsString
        "Empty flags, not simple, with minimal + no comments"
        (goldenTest "test-minimal-no-comments.golden")
        $ let opts = WriteOpts False True True v pkgDir Library pkgName defaultCabalVersion
           in runGoldenTest opts testArgs emptyFlags
    , goldenVsString
        "Empty flags, not simple, with minimal + comments"
        (goldenTest "test-simple-minimal-with-comments.golden")
        $ let opts = WriteOpts False True False v pkgDir Library pkgName defaultCabalVersion
           in runGoldenTest opts testArgs emptyFlags
    , goldenVsString
        "Build tools flag, not simple, with comments + no minimal"
        (goldenTest "test-build-tools-with-comments.golden")
        $ let opts = WriteOpts False False False v pkgDir Library pkgName defaultCabalVersion
           in runGoldenTest opts testArgs (emptyFlags{buildTools = Flag ["happy"]})
    , goldenVsString
        "Standalone tests, empty flags, not simple, no options, no comments"
        (goldenTest "standalone-test-no-comments.golden")
        $ let opts = WriteOpts False False True v pkgDir TestSuite pkgName defaultCabalVersion
           in runGoldenTest opts testArgs emptyFlags
    , goldenVsString
        "Standalone tests, empty flags, not simple, with comments + no minimal"
        (goldenTest "standalone-test-with-comments.golden")
        $ let opts = WriteOpts False False False v pkgDir TestSuite pkgName defaultCabalVersion
           in runGoldenTest opts testArgs emptyFlags
    ]
  where
    runGoldenTest opts args flags =
      case _runPrompt (genTestTarget flags pkgIx) args of
        Left e -> assertFailure $ show e
        Right (Nothing, _) ->
          assertFailure
            "goldenTestTests: Tests not enabled."
        Right (Just t, _) -> mkStanza [mkTestStanza opts $ t{_testDependencies = mangleBaseDep t _testDependencies}]

-- | Full cabal file golden tests
goldenCabalTests
  :: Verbosity
  -> InstalledPackageIndex
  -> SourcePackageDb
  -> TestTree
goldenCabalTests v pkgIx srcDb =
  testGroup
    ".cabal file golden tests"
    [ goldenVsString
        "Library and executable, empty flags, not simple, with comments + no minimal"
        (goldenCabal "cabal-lib-and-exe-with-comments.golden")
        $ runGoldenTest (fullProjArgs "Y") emptyFlags
    , goldenVsString
        "Library and executable, empty flags, not simple, no comments + no minimal"
        (goldenCabal "cabal-lib-and-exe-no-comments.golden")
        $ runGoldenTest (fullProjArgs "N") emptyFlags
    , goldenVsString
        "Library, empty flags, not simple, with comments + no minimal"
        (goldenCabal "cabal-lib-with-comments.golden")
        $ runGoldenTest (libProjArgs "Y") emptyFlags
    , goldenVsString
        "Library, empty flags, not simple, no comments + no minimal"
        (goldenCabal "cabal-lib-no-comments.golden")
        $ runGoldenTest (libProjArgs "N") emptyFlags
    , goldenVsString
        "Test suite, empty flags, not simple, with comments + no minimal"
        (goldenCabal "cabal-test-suite-with-comments.golden")
        $ runGoldenTest (testProjArgs "Y") emptyFlags
    , goldenVsString
        "Test suite, empty flags, not simple, no comments + no minimal"
        (goldenCabal "cabal-test-suite-no-comments.golden")
        $ runGoldenTest (testProjArgs "N") emptyFlags
    ]
  where
    runGoldenTest args flags =
      case _runPrompt (createProject v pkgIx srcDb flags) args of
        Left e -> assertFailure $ show e
        (Right (ProjectSettings opts pkgDesc (Just libTarget) (Just exeTarget) (Just testTarget), _)) -> do
          let pkgFields = mkPkgDescription opts pkgDesc
              commonStanza = mkCommonStanza opts
              libStanza = mkLibStanza opts $ libTarget{_libDependencies = mangleBaseDep libTarget _libDependencies}
              exeStanza = mkExeStanza opts $ exeTarget{_exeDependencies = mangleBaseDep exeTarget _exeDependencies}
              testStanza = mkTestStanza opts $ testTarget{_testDependencies = mangleBaseDep testTarget _testDependencies}

          mkStanza $ pkgFields ++ [commonStanza, libStanza, exeStanza, testStanza]
        (Right (ProjectSettings opts pkgDesc (Just libTarget) Nothing (Just testTarget), _)) -> do
          let pkgFields = mkPkgDescription opts pkgDesc
              commonStanza = mkCommonStanza opts
              libStanza = mkLibStanza opts $ libTarget{_libDependencies = mangleBaseDep libTarget _libDependencies}
              testStanza = mkTestStanza opts $ testTarget{_testDependencies = mangleBaseDep testTarget _testDependencies}

          mkStanza $ pkgFields ++ [commonStanza, libStanza, testStanza]
        (Right (ProjectSettings opts pkgDesc Nothing Nothing (Just testTarget), _)) -> do
          let pkgFields = mkPkgDescription opts pkgDesc
              commonStanza = mkCommonStanza opts
              testStanza = mkTestStanza opts $ testTarget{_testDependencies = mangleBaseDep testTarget _testDependencies}

          mkStanza $ pkgFields ++ [commonStanza, testStanza]
        (Right (ProjectSettings _ _ l e t, _)) ->
          assertFailure $
            show l ++ "\n" ++ show e ++ "\n" ++ show t

-- -------------------------------------------------------------------- --
-- utils

mkStanza :: [PrettyField FieldAnnotation] -> IO BS8.ByteString
mkStanza fields =
  return . BS8.pack $
    showFields'
      annCommentLines
      postProcessFieldLines
      4
      fields

golden :: FilePath
golden = "tests" </> "fixtures" </> "init" </> "golden"

goldenExe :: FilePath -> FilePath
goldenExe file = golden </> "exe" </> file

goldenTest :: FilePath -> FilePath
goldenTest file = golden </> "test" </> file

goldenLib :: FilePath -> FilePath
goldenLib file = golden </> "lib" </> file

goldenCabal :: FilePath -> FilePath
goldenCabal file = golden </> "cabal" </> file

goldenPkgDesc :: FilePath -> FilePath
goldenPkgDesc file = golden </> "pkg-desc" </> file

libArgs :: NonEmpty String
libArgs = fromList ["1", "2"]

exeArgs :: NonEmpty String
exeArgs = fromList ["1", "2", "1"]

testArgs :: NonEmpty String
testArgs = fromList ["y", "1", "test", "1"]

pkgArgs :: NonEmpty String
pkgArgs =
  fromList
    [ "5"
    , "foo-package"
    , "foo-package"
    , "y"
    , "0.1.0.0"
    , "2"
    , "git username"
    , "foo-kmett"
    , "git email"
    , "foo-kmett@kmett.kmett"
    , "home"
    , "synopsis"
    , "4"
    ]

testProjArgs :: String -> NonEmpty String
testProjArgs comments =
  fromList ["4", "n", "foo-package"]
    <> pkgArgs
    <> fromList (NEL.drop 1 testArgs)
    <> fromList [comments]

libProjArgs :: String -> NonEmpty String
libProjArgs comments =
  fromList ["1", "n", "foo-package"]
    <> pkgArgs
    <> libArgs
    <> testArgs
    <> fromList [comments]

fullProjArgs :: String -> NonEmpty String
fullProjArgs comments =
  fromList ["3", "n", "foo-package"]
    <> pkgArgs
    <> libArgs
    <> exeArgs
    <> testArgs
    <> fromList [comments]
