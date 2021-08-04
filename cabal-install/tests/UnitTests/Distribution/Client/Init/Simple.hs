module UnitTests.Distribution.Client.Init.Simple
( tests
) where


import Prelude as P
import Test.Tasty
import Test.Tasty.HUnit

import Distribution.Client.Init.Defaults
import Distribution.Client.Init.Simple
import Distribution.Client.Init.Types


import Data.List.NonEmpty hiding (zip)
import Distribution.Client.Types
import Distribution.Simple.PackageIndex hiding (fromList)
import Distribution.Types.PackageName
import Distribution.Verbosity


import UnitTests.Distribution.Client.Init.Utils
import Distribution.Simple.Setup
import qualified Data.List.NonEmpty as NEL
import Distribution.Types.Dependency
import Distribution.Client.Init.Utils (mkPackageNameDep)
import qualified Data.Set as Set

tests
    :: Verbosity
    -> InitFlags
    -> InstalledPackageIndex
    -> SourcePackageDb
    -> TestTree
tests v _initFlags pkgIx srcDb = testGroup "Distribution.Client.Init.Simple.hs"
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
    testGroup "Simple createProject tests"
    [ testCase "Simple lib createProject - no tests" $ do
      let inputs = fromList
            [ "1"           -- package type: Library
            , "simple-test" -- package dir (ignored, piped to current dir due to prompt monad)
            , "n"           -- no tests
            ]

          flags = emptyFlags { packageType = Flag Library }
          settings = ProjectSettings
            (WriteOpts False False False v "/home/test/1" Library pkgName defaultCabalVersion)
            (simplePkgDesc pkgName) (Just simpleLibTarget)
            Nothing Nothing

      case _runPrompt (createProject v pkgIx srcDb flags) inputs of
        Left e -> assertFailure $ "Failed to create simple lib project: " ++ show e
        Right (settings', _) -> settings @=? settings'

    , testCase "Simple lib createProject - with tests" $ do
      let inputs = fromList ["1", "simple-test", "y", "1"]
          flags = emptyFlags { packageType = Flag Library }
          settings = ProjectSettings
            (WriteOpts False False False v "/home/test/1" Library pkgName defaultCabalVersion)
            (simplePkgDesc pkgName) (Just simpleLibTarget)
            Nothing (Just $ simpleTestTarget (Just pkgName))

      case _runPrompt (createProject v pkgIx srcDb flags) inputs of
        Left e -> assertFailure $ "Failed to create simple lib (with tests)project: " ++ show e
        Right (settings', _) -> settings @=? settings'

    , testCase "Simple exe createProject" $ do
      let inputs = fromList ["2", "simple-test"]
          flags = emptyFlags { packageType = Flag Executable }
          settings = ProjectSettings
            (WriteOpts False False False v "/home/test/2" Executable pkgName defaultCabalVersion)
            (simplePkgDesc pkgName) Nothing
            (Just $ simpleExeTarget Nothing) Nothing

      case _runPrompt (createProject v pkgIx srcDb flags) inputs of
        Left e -> assertFailure $ "Failed to create simple exe project: " ++ show e
        Right (settings', _) -> settings @=? settings'

    , testCase "Simple lib+exe createProject - no tests" $ do
      let inputs = fromList ["2", "simple-test", "n"]
          flags = emptyFlags { packageType = Flag LibraryAndExecutable }
          settings = ProjectSettings
            (WriteOpts False False False v "/home/test/2" LibraryAndExecutable pkgName defaultCabalVersion)
            (simplePkgDesc pkgName) (Just simpleLibTarget)
            (Just $ simpleExeTarget (Just pkgName)) Nothing

      case _runPrompt (createProject v pkgIx srcDb flags) inputs of
        Left e -> assertFailure $ "Failed to create simple lib+exe project: " ++ show e
        Right (settings', _) -> settings @=? settings'
    , testCase "Simple lib+exe createProject - with tests" $ do
      let inputs = fromList ["2", "simple-test", "y", "1"]
          flags = emptyFlags { packageType = Flag LibraryAndExecutable }
          settings = ProjectSettings
            (WriteOpts False False False v "/home/test/2" LibraryAndExecutable pkgName defaultCabalVersion)
            (simplePkgDesc pkgName) (Just simpleLibTarget)
            (Just $ simpleExeTarget (Just pkgName))
            (Just $ simpleTestTarget (Just pkgName))

      case _runPrompt (createProject v pkgIx srcDb flags) inputs of
        Left e -> assertFailure $ "Failed to create simple lib+exe (with tests) project: " ++ show e
        Right (settings', _) -> settings @=? settings'
    
    , testCase "Simple standalone tests" $ do
      let inputs = fromList ["2", "simple-test", "y", "1"]
          flags = emptyFlags { packageType = Flag TestSuite }
          settings = ProjectSettings
            (WriteOpts False False False v "/home/test/2" TestSuite pkgName defaultCabalVersion)
            (simplePkgDesc pkgName) Nothing Nothing
            (Just $ simpleTestTarget Nothing)

      case _runPrompt (createProject v pkgIx srcDb flags) inputs of
        Left e -> assertFailure $ "Failed to create simple standalone test project: " ++ show e
        Right (settings', _) -> settings @=? settings'
    ]

-- -------------------------------------------------------------------- --
-- Utils

mkPkgDep :: Maybe PackageName -> [Dependency]
mkPkgDep Nothing = []
mkPkgDep (Just pn) = [mkPackageNameDep pn]

simplePkgDesc :: PackageName -> PkgDescription
simplePkgDesc pkgName = PkgDescription
    defaultCabalVersion
    pkgName
    defaultVersion
    defaultLicense
    "" "" "" "" ""
    mempty
    (Just $ Set.singleton defaultChangelog)

simpleLibTarget :: LibTarget
simpleLibTarget = LibTarget
    [defaultSourceDir]
    defaultLanguage
    (myLibModule NEL.:| [])
    [] [] [] []

simpleExeTarget :: Maybe PackageName -> ExeTarget
simpleExeTarget pn = ExeTarget
    defaultMainIs
    [defaultApplicationDir]
    defaultLanguage
    [] [] (mkPkgDep pn) []

simpleTestTarget :: Maybe PackageName -> TestTarget
simpleTestTarget pn = TestTarget
    defaultMainIs
    [defaultTestDir]
    defaultLanguage
    [] [] (mkPkgDep pn) []
