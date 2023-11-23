module UnitTests.Distribution.Client.Init.Simple
  ( tests
  ) where

import Test.Tasty
import Test.Tasty.HUnit
import Prelude as P

import Distribution.Client.Init.Defaults
import Distribution.Client.Init.Simple
import Distribution.Client.Init.Types

import Data.List.NonEmpty hiding (zip)
import Distribution.Client.Types
import Distribution.Simple.PackageIndex hiding (fromList)
import Distribution.Types.PackageName
import Distribution.Verbosity

import qualified Data.List.NonEmpty as NEL
import qualified Data.Set as Set
import Distribution.Client.Init.FlagExtractors (getCabalVersionNoPrompt)
import Distribution.Client.Init.Utils (getBaseDep, mkPackageNameDep)
import Distribution.Simple.Setup
import Distribution.Types.Dependency
import UnitTests.Distribution.Client.Init.Utils

tests
  :: Verbosity
  -> InitFlags
  -> InstalledPackageIndex
  -> SourcePackageDb
  -> TestTree
tests v _initFlags pkgIx srcDb =
  testGroup
    "Distribution.Client.Init.Simple.hs"
    [ simpleCreateProjectTests v pkgIx srcDb pkgName
    ]
  where
    pkgName = mkPackageName "simple-test"

simpleCreateProjectTests
  :: Verbosity
  -> InstalledPackageIndex
  -> SourcePackageDb
  -> PackageName
  -> TestTree
simpleCreateProjectTests v pkgIx srcDb pkgName =
  testGroup
    "Simple createProject tests"
    [ testCase "Simple lib createProject - no tests" $ do
        let inputs =
              fromList
                [ "1" -- package type: Library
                , "simple.test" -- package dir: used for determining package name;
                -- note that . will be replaced with - in a sanitization step,
                -- and we get the expected "simple-test" -- regression test for #8404
                , "simple.test" -- package dir again: the prompt monad needs extra parameter for every
                -- IO call, and this one will be used for canonicalizePath,
                -- which is called as a part of sanitization
                , "n" -- no tests
                ]

            flags = emptyFlags{packageType = Flag Library}
            settings =
              ProjectSettings
                (WriteOpts False False False v "/home/test/1" Library pkgName defaultCabalVersion)
                (simplePkgDesc pkgName)
                (Just $ simpleLibTarget baseDep)
                Nothing
                Nothing

        case _runPrompt (createProject v pkgIx srcDb flags) inputs of
          Left e -> assertFailure $ "Failed to create simple lib project: " ++ show e
          Right (settings', _) -> settings @=? settings'
    , testCase "Simple lib createProject - with tests" $ do
        let inputs = fromList ["1", "simple-test", "simple-test", "y", "1"]
            flags = emptyFlags{packageType = Flag Library}
            settings =
              ProjectSettings
                (WriteOpts False False False v "/home/test/1" Library pkgName defaultCabalVersion)
                (simplePkgDesc pkgName)
                (Just $ simpleLibTarget baseDep)
                Nothing
                (Just $ simpleTestTarget (Just pkgName) baseDep)

        case _runPrompt (createProject v pkgIx srcDb flags) inputs of
          Left e -> assertFailure $ "Failed to create simple lib (with tests)project: " ++ show e
          Right (settings', _) -> settings @=? settings'
    , testCase "Simple exe createProject" $ do
        let inputs = fromList ["2", "simple-test", "simple-test"]
            flags = emptyFlags{packageType = Flag Executable}
            settings =
              ProjectSettings
                (WriteOpts False False False v "/home/test/2" Executable pkgName defaultCabalVersion)
                (simplePkgDesc pkgName)
                Nothing
                (Just $ simpleExeTarget Nothing baseDep)
                Nothing

        case _runPrompt (createProject v pkgIx srcDb flags) inputs of
          Left e -> assertFailure $ "Failed to create simple exe project: " ++ show e
          Right (settings', _) -> settings @=? settings'
    , testCase "Simple lib+exe createProject - no tests" $ do
        let inputs = fromList ["2", "simple-test", "simple-test", "n"]
            flags = emptyFlags{packageType = Flag LibraryAndExecutable}
            settings =
              ProjectSettings
                (WriteOpts False False False v "/home/test/2" LibraryAndExecutable pkgName defaultCabalVersion)
                (simplePkgDesc pkgName)
                (Just $ simpleLibTarget baseDep)
                (Just $ simpleExeTarget (Just pkgName) baseDep)
                Nothing

        case _runPrompt (createProject v pkgIx srcDb flags) inputs of
          Left e -> assertFailure $ "Failed to create simple lib+exe project: " ++ show e
          Right (settings', _) -> settings @=? settings'
    , testCase "Simple lib+exe createProject - with tests" $ do
        let inputs = fromList ["2", "simple-test", "simple-test", "y", "1"]
            flags = emptyFlags{packageType = Flag LibraryAndExecutable}
            settings =
              ProjectSettings
                (WriteOpts False False False v "/home/test/2" LibraryAndExecutable pkgName defaultCabalVersion)
                (simplePkgDesc pkgName)
                (Just $ simpleLibTarget baseDep)
                (Just $ simpleExeTarget (Just pkgName) baseDep)
                (Just $ simpleTestTarget (Just pkgName) baseDep)

        case _runPrompt (createProject v pkgIx srcDb flags) inputs of
          Left e -> assertFailure $ "Failed to create simple lib+exe (with tests) project: " ++ show e
          Right (settings', _) -> settings @=? settings'
    , testCase "Simple standalone tests" $ do
        let inputs = fromList ["2", "simple-test", "simple-test", "y", "1"]
            flags = emptyFlags{packageType = Flag TestSuite}
            settings =
              ProjectSettings
                (WriteOpts False False False v "/home/test/2" TestSuite pkgName defaultCabalVersion)
                (simplePkgDesc pkgName)
                Nothing
                Nothing
                (Just $ simpleTestTarget Nothing baseDep)

        case _runPrompt (createProject v pkgIx srcDb flags) inputs of
          Left e -> assertFailure $ "Failed to create simple standalone test project: " ++ show e
          Right (settings', _) -> settings @=? settings'
    ]
  where
    baseDep = case _runPrompt (getBaseDep pkgIx emptyFlags) $ fromList [] of
      Left e -> error $ show e
      Right a -> fst a

-- -------------------------------------------------------------------- --
-- Utils

mkPkgDep :: Maybe PackageName -> [Dependency]
mkPkgDep Nothing = []
mkPkgDep (Just pn) = [mkPackageNameDep pn]

simplePkgDesc :: PackageName -> PkgDescription
simplePkgDesc pkgName =
  PkgDescription
    defaultCabalVersion
    pkgName
    defaultVersion
    (defaultLicense $ getCabalVersionNoPrompt dummyFlags)
    ""
    ""
    ""
    ""
    ""
    mempty
    (Just $ Set.singleton defaultChangelog)

simpleLibTarget :: [Dependency] -> LibTarget
simpleLibTarget baseDep =
  LibTarget
    [defaultSourceDir]
    defaultLanguage
    (myLibModule NEL.:| [])
    []
    []
    baseDep
    []

simpleExeTarget :: Maybe PackageName -> [Dependency] -> ExeTarget
simpleExeTarget pn baseDep =
  ExeTarget
    defaultMainIs
    [defaultApplicationDir]
    defaultLanguage
    []
    []
    (baseDep ++ mkPkgDep pn)
    []

simpleTestTarget :: Maybe PackageName -> [Dependency] -> TestTarget
simpleTestTarget pn baseDep =
  TestTarget
    defaultMainIs
    [defaultTestDir]
    defaultLanguage
    []
    []
    (baseDep ++ mkPkgDep pn)
    []
