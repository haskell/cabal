module UnitTests.Distribution.Client.Init.Interactive
  ( tests
  ) where

import Test.Tasty
import Test.Tasty.HUnit
import Prelude as P

import Distribution.Client.Init.Defaults
import Distribution.Client.Init.Interactive.Command
import Distribution.Client.Init.Types

import qualified Distribution.SPDX as SPDX

import Data.List.NonEmpty hiding (zip)
import Distribution.Client.Types
import Distribution.Simple.PackageIndex hiding (fromList)
import Distribution.Types.PackageName
import Distribution.Types.Version
import Distribution.Verbosity

import Language.Haskell.Extension

import qualified Data.Set as Set
import Distribution.CabalSpecVersion
import Distribution.Client.Init.FlagExtractors
import Distribution.FieldGrammar.Newtypes
import Distribution.Simple.Setup
import UnitTests.Distribution.Client.Init.Utils

-- -------------------------------------------------------------------- --
-- Init Test main

tests
  :: Verbosity
  -> InitFlags
  -> InstalledPackageIndex
  -> SourcePackageDb
  -> TestTree
tests _v initFlags pkgIx srcDb =
  testGroup
    "Distribution.Client.Init.Interactive.Command.hs"
    [ createProjectTest pkgIx srcDb
    , fileCreatorTests pkgIx srcDb pkgName
    , interactiveTests srcDb
    ]
  where
    pkgName =
      evalPrompt (packageNamePrompt srcDb initFlags) $
        fromList ["test-package", "y"]

-- pkgNm  = evalPrompt (getPackageName srcDb initFlags) $ fromList ["test-package", "y"]

createProjectTest
  :: InstalledPackageIndex
  -> SourcePackageDb
  -> TestTree
createProjectTest pkgIx srcDb =
  testGroup
    "createProject tests"
    [ testGroup
        "with flags"
        [ testCase "Check the interactive workflow" $ do
            let dummyFlags' =
                  dummyFlags
                    { packageType = Flag LibraryAndExecutable
                    , minimal = Flag False
                    , overwrite = Flag False
                    , packageDir = Flag "/home/test/test-package"
                    , extraSrc = NoFlag
                    , exposedModules = Flag []
                    , otherModules = Flag []
                    , otherExts = Flag []
                    , buildTools = Flag []
                    , mainIs = Flag "quxApp/Main.hs"
                    , dependencies = Flag []
                    }

            case (_runPrompt $ createProject silent pkgIx srcDb dummyFlags') (fromList ["[]", "3", "quxTest/Main.hs"]) of
              Right (ProjectSettings opts desc (Just lib) (Just exe) (Just test), _) -> do
                _optOverwrite opts @?= False
                _optMinimal opts @?= False
                _optNoComments opts @?= True
                _optVerbosity opts @?= silent
                _optPkgDir opts @?= "/home/test/test-package"
                _optPkgType opts @?= LibraryAndExecutable
                _optPkgName opts @?= mkPackageName "QuxPackage"

                _pkgCabalVersion desc @?= CabalSpecV2_2
                _pkgName desc @?= mkPackageName "QuxPackage"
                _pkgVersion desc @?= mkVersion [4, 2, 6]
                _pkgLicense desc @?! (SpecLicense . Left $ SPDX.NONE)
                _pkgAuthor desc @?= "Foobar"
                _pkgEmail desc @?= "foobar@qux.com"
                _pkgHomePage desc @?= "qux.com"
                _pkgSynopsis desc @?= "We are Qux, and this is our package"
                _pkgCategory desc @?= "Control"
                _pkgExtraSrcFiles desc @?= mempty
                _pkgExtraDocFiles desc @?= pure (Set.singleton "CHANGELOG.md")

                _libSourceDirs lib @?= ["quxSrc"]
                _libLanguage lib @?= Haskell98
                _libExposedModules lib @?= myLibModule :| []
                _libOtherModules lib @?= []
                _libOtherExts lib @?= []
                _libDependencies lib @?= []
                _libBuildTools lib @?= []

                _exeMainIs exe @?= HsFilePath "quxApp/Main.hs" Standard
                _exeApplicationDirs exe @?= ["quxApp"]
                _exeLanguage exe @?= Haskell98
                _exeOtherModules exe @?= []
                _exeOtherExts exe @?= []
                _exeDependencies exe @?! []
                _exeBuildTools exe @?= []

                _testMainIs test @?= HsFilePath "quxTest/Main.hs" Standard
                _testDirs test @?= ["quxTest"]
                _testLanguage test @?= Haskell98
                _testOtherModules test @?= []
                _testOtherExts test @?= []
                _testDependencies test @?! []
                _testBuildTools test @?= []
              Right (ProjectSettings _ _ lib exe test, _) -> do
                lib @?! Nothing
                exe @?! Nothing
                test @?! Nothing
              Left e -> assertFailure $ show e
        ]
    , testGroup
        "with tests"
        [ testCase "Check the interactive library and executable workflow" $ do
            let inputs =
                  fromList
                    -- package type
                    [ "3"
                    , -- overwrite (not asked, pristine folder)
                      "test-package-dir"
                    , "[]"
                    , -- package dir
                      "test-package"
                    , -- package description
                      -- cabal version
                      "4"
                    , -- package name
                      "test-package"
                    , "test-package"
                    , "test-package"
                    , -- version
                      "3.1.2.3"
                    , -- license
                      "3"
                    , -- author
                      "git username"
                    , "Foobar"
                    , -- email
                      "git email"
                    , "foobar@qux.com"
                    , -- homepage
                      "qux.com"
                    , -- synopsis
                      "Qux's package"
                    , -- category
                      "3"
                    , -- library target
                      -- source dir
                      "1"
                    , -- language
                      "2"
                    , -- executable target
                      -- main file
                      "1"
                    , -- application dir
                      "2"
                    , -- language
                      "2"
                    , -- test target
                      "y"
                    , -- main file
                      "1"
                    , -- test dir
                      "test"
                    , -- language
                      "1"
                    , -- comments
                      "y"
                    ]

            case (_runPrompt $ createProject silent pkgIx srcDb emptyFlags) inputs of
              Right (ProjectSettings opts desc (Just lib) (Just exe) (Just test), _) -> do
                _optOverwrite opts @?= False
                _optMinimal opts @?= False
                _optNoComments opts @?= False
                _optVerbosity opts @?= silent
                _optPkgDir opts @?= "/home/test/test-package"
                _optPkgType opts @?= LibraryAndExecutable
                _optPkgName opts @?= mkPackageName "test-package"

                _pkgCabalVersion desc @?= CabalSpecV2_4
                _pkgName desc @?= mkPackageName "test-package"
                _pkgVersion desc @?= mkVersion [3, 1, 2, 3]
                _pkgLicense desc @?! (SpecLicense . Left $ SPDX.NONE)
                _pkgAuthor desc @?= "Foobar"
                _pkgEmail desc @?= "foobar@qux.com"
                _pkgHomePage desc @?= "qux.com"
                _pkgSynopsis desc @?= "Qux's package"
                _pkgCategory desc @?= "Control"
                _pkgExtraSrcFiles desc @?= mempty
                _pkgExtraDocFiles desc @?= pure (Set.singleton "CHANGELOG.md")

                _libSourceDirs lib @?= ["src"]
                _libLanguage lib @?= Haskell98
                _libExposedModules lib @?= myLibModule :| []
                _libOtherModules lib @?= []
                _libOtherExts lib @?= []
                _libDependencies lib @?! []
                _libBuildTools lib @?= []

                _exeMainIs exe @?= HsFilePath "Main.hs" Standard
                _exeApplicationDirs exe @?= ["exe"]
                _exeLanguage exe @?= Haskell98
                _exeOtherModules exe @?= []
                _exeOtherExts exe @?= []
                _exeDependencies exe @?! []
                _exeBuildTools exe @?= []

                _testMainIs test @?= HsFilePath "Main.hs" Standard
                _testDirs test @?= ["test"]
                _testLanguage test @?= Haskell2010
                _testOtherModules test @?= []
                _testOtherExts test @?= []
                _testDependencies test @?! []
                _testBuildTools test @?= []
              Right (ProjectSettings _ _ lib exe test, _) -> do
                lib @?! Nothing
                exe @?! Nothing
                test @?! Nothing
              Left e -> assertFailure $ show e
        , testCase "Check the interactive library workflow" $ do
            let inputs =
                  fromList
                    -- package type
                    [ "1"
                    , -- overwrite (not asked, pristine folder)
                      "test-package-dir"
                    , "[]"
                    , -- package dir
                      "test-package"
                    , -- package description
                      -- cabal version
                      "4"
                    , -- package name
                      "test-package"
                    , "test-package"
                    , "test-package"
                    , -- version
                      "3.1.2.3"
                    , -- license
                      "3"
                    , -- author
                      "git username"
                    , "Foobar"
                    , -- email
                      "git email"
                    , "foobar@qux.com"
                    , -- homepage
                      "qux.com"
                    , -- synopsis
                      "Qux's package"
                    , -- category
                      "3"
                    , -- library target
                      -- source dir
                      "1"
                    , -- language
                      "2"
                    , -- test target
                      "y"
                    , -- main file
                      "1"
                    , -- test dir
                      "test"
                    , -- language
                      "1"
                    , -- comments
                      "y"
                    ]

            case (_runPrompt $ createProject silent pkgIx srcDb emptyFlags) inputs of
              Right (ProjectSettings opts desc (Just lib) Nothing (Just test), _) -> do
                _optOverwrite opts @?= False
                _optMinimal opts @?= False
                _optNoComments opts @?= False
                _optVerbosity opts @?= silent
                _optPkgDir opts @?= "/home/test/test-package"
                _optPkgType opts @?= Library
                _optPkgName opts @?= mkPackageName "test-package"

                _pkgCabalVersion desc @?= CabalSpecV2_4
                _pkgName desc @?= mkPackageName "test-package"
                _pkgVersion desc @?= mkVersion [3, 1, 2, 3]
                _pkgLicense desc @?! (SpecLicense . Left $ SPDX.NONE)
                _pkgAuthor desc @?= "Foobar"
                _pkgEmail desc @?= "foobar@qux.com"
                _pkgHomePage desc @?= "qux.com"
                _pkgSynopsis desc @?= "Qux's package"
                _pkgCategory desc @?= "Control"
                _pkgExtraSrcFiles desc @?= mempty
                _pkgExtraDocFiles desc @?= pure (Set.singleton "CHANGELOG.md")

                _libSourceDirs lib @?= ["src"]
                _libLanguage lib @?= Haskell98
                _libExposedModules lib @?= myLibModule :| []
                _libOtherModules lib @?= []
                _libOtherExts lib @?= []
                _libDependencies lib @?! []
                _libBuildTools lib @?= []

                _testMainIs test @?= HsFilePath "Main.hs" Standard
                _testDirs test @?= ["test"]
                _testLanguage test @?= Haskell2010
                _testOtherModules test @?= []
                _testOtherExts test @?= []
                _testDependencies test @?! []
                _testBuildTools test @?= []
              Right (ProjectSettings _ _ lib exe test, _) -> do
                lib @?! Nothing
                exe @?= Nothing
                test @?! Nothing
              Left e -> assertFailure $ show e
        , testCase "Check the interactive library workflow" $ do
            let inputs =
                  fromList
                    -- package type
                    [ "4"
                    , -- overwrite (not asked, pristine folder)
                      "test-package-dir"
                    , "[]"
                    , -- package dir
                      "test-package"
                    , -- package description
                      -- cabal version
                      "4"
                    , -- package name
                      "test-package"
                    , "test-package"
                    , "test-package"
                    , -- version
                      "3.1.2.3"
                    , -- license
                      "3"
                    , -- author
                      "git username"
                    , "Foobar"
                    , -- email
                      "git email"
                    , "foobar@qux.com"
                    , -- homepage
                      "qux.com"
                    , -- synopsis
                      "Qux's package"
                    , -- category
                      "3"
                    , -- test target
                      -- main file
                      "1"
                    , -- test dir
                      "test"
                    , -- language
                      "1"
                    , -- comments
                      "y"
                    ]

            case (_runPrompt $ createProject silent pkgIx srcDb emptyFlags) inputs of
              Right (ProjectSettings opts desc Nothing Nothing (Just test), _) -> do
                _optOverwrite opts @?= False
                _optMinimal opts @?= False
                _optNoComments opts @?= False
                _optVerbosity opts @?= silent
                _optPkgDir opts @?= "/home/test/test-package"
                _optPkgType opts @?= TestSuite
                _optPkgName opts @?= mkPackageName "test-package"

                _pkgCabalVersion desc @?= CabalSpecV2_4
                _pkgName desc @?= mkPackageName "test-package"
                _pkgVersion desc @?= mkVersion [3, 1, 2, 3]
                _pkgLicense desc @?! (SpecLicense . Left $ SPDX.NONE)
                _pkgAuthor desc @?= "Foobar"
                _pkgEmail desc @?= "foobar@qux.com"
                _pkgHomePage desc @?= "qux.com"
                _pkgSynopsis desc @?= "Qux's package"
                _pkgCategory desc @?= "Control"
                _pkgExtraSrcFiles desc @?= mempty
                _pkgExtraDocFiles desc @?= pure (Set.singleton "CHANGELOG.md")

                _testMainIs test @?= HsFilePath "Main.hs" Standard
                _testDirs test @?= ["test"]
                _testLanguage test @?= Haskell2010
                _testOtherModules test @?= []
                _testOtherExts test @?= []
                _testDependencies test @?! []
                _testBuildTools test @?= []
              Right (ProjectSettings _ _ lib exe test, _) -> do
                lib @?= Nothing
                exe @?= Nothing
                test @?! Nothing
              Left e -> assertFailure $ show e
        ]
    , testGroup
        "without tests"
        [ testCase "Check the interactive library and executable workflow" $ do
            let inputs =
                  fromList
                    -- package type
                    [ "3"
                    , -- overwrite (not asked, pristine folder)
                      "test-package-dir"
                    , "[]"
                    , -- package dir
                      "test-package"
                    , -- package description
                      -- cabal version
                      "4"
                    , -- package name
                      "test-package"
                    , "test-package"
                    , "test-package"
                    , -- version
                      "3.1.2.3"
                    , -- license
                      "3"
                    , -- author
                      "git username"
                    , "Foobar"
                    , -- email
                      "git email"
                    , "foobar@qux.com"
                    , -- homepage
                      "qux.com"
                    , -- synopsis
                      "Qux's package"
                    , -- category
                      "3"
                    , -- library target
                      -- source dir
                      "1"
                    , -- language
                      "2"
                    , -- executable target
                      -- main file
                      "1"
                    , -- application dir
                      "2"
                    , -- language
                      "2"
                    , -- test suite
                      "n"
                    , -- comments
                      "y"
                    ]

            case (_runPrompt $ createProject silent pkgIx srcDb emptyFlags) inputs of
              Right (ProjectSettings opts desc (Just lib) (Just exe) Nothing, _) -> do
                _optOverwrite opts @?= False
                _optMinimal opts @?= False
                _optNoComments opts @?= False
                _optVerbosity opts @?= silent
                _optPkgDir opts @?= "/home/test/test-package"
                _optPkgType opts @?= LibraryAndExecutable
                _optPkgName opts @?= mkPackageName "test-package"

                _pkgCabalVersion desc @?= CabalSpecV2_4
                _pkgName desc @?= mkPackageName "test-package"
                _pkgVersion desc @?= mkVersion [3, 1, 2, 3]
                _pkgLicense desc @?! (SpecLicense . Left $ SPDX.NONE)
                _pkgAuthor desc @?= "Foobar"
                _pkgEmail desc @?= "foobar@qux.com"
                _pkgHomePage desc @?= "qux.com"
                _pkgSynopsis desc @?= "Qux's package"
                _pkgCategory desc @?= "Control"
                _pkgExtraSrcFiles desc @?= mempty
                _pkgExtraDocFiles desc @?= pure (Set.singleton "CHANGELOG.md")

                _libSourceDirs lib @?= ["src"]
                _libLanguage lib @?= Haskell98
                _libExposedModules lib @?= myLibModule :| []
                _libOtherModules lib @?= []
                _libOtherExts lib @?= []
                _libDependencies lib @?! []
                _libBuildTools lib @?= []

                _exeMainIs exe @?= HsFilePath "Main.hs" Standard
                _exeApplicationDirs exe @?= ["exe"]
                _exeLanguage exe @?= Haskell98
                _exeOtherModules exe @?= []
                _exeOtherExts exe @?= []
                _exeDependencies exe @?! []
                _exeBuildTools exe @?= []
              Right (ProjectSettings _ _ lib exe test, _) -> do
                lib @?! Nothing
                exe @?! Nothing
                test @?= Nothing
              Left e -> assertFailure $ show e
        , testCase "Check the interactive library workflow" $ do
            let inputs =
                  fromList
                    -- package type
                    [ "1"
                    , -- overwrite (not asked, pristine folder)
                      "test-package-dir"
                    , "[]"
                    , -- package dir
                      "test-package"
                    , -- package description
                      -- cabal version
                      "4"
                    , -- package name
                      "test-package"
                    , "test-package"
                    , "test-package"
                    , -- version
                      "3.1.2.3"
                    , -- license
                      "3"
                    , -- author
                      "git username"
                    , "Foobar"
                    , -- email
                      "git email"
                    , "foobar@qux.com"
                    , -- homepage
                      "qux.com"
                    , -- synopsis
                      "Qux's package"
                    , -- category
                      "3"
                    , -- library target
                      -- source dir
                      "1"
                    , -- language
                      "2"
                    , -- test suite
                      "n"
                    , -- comments
                      "y"
                    ]

            case (_runPrompt $ createProject silent pkgIx srcDb emptyFlags) inputs of
              Right (ProjectSettings opts desc (Just lib) Nothing Nothing, _) -> do
                _optOverwrite opts @?= False
                _optMinimal opts @?= False
                _optNoComments opts @?= False
                _optVerbosity opts @?= silent
                _optPkgDir opts @?= "/home/test/test-package"
                _optPkgType opts @?= Library
                _optPkgName opts @?= mkPackageName "test-package"

                _pkgCabalVersion desc @?= CabalSpecV2_4
                _pkgName desc @?= mkPackageName "test-package"
                _pkgVersion desc @?= mkVersion [3, 1, 2, 3]
                _pkgLicense desc @?! (SpecLicense . Left $ SPDX.NONE)
                _pkgAuthor desc @?= "Foobar"
                _pkgEmail desc @?= "foobar@qux.com"
                _pkgHomePage desc @?= "qux.com"
                _pkgSynopsis desc @?= "Qux's package"
                _pkgCategory desc @?= "Control"
                _pkgExtraSrcFiles desc @?= mempty
                _pkgExtraDocFiles desc @?= pure (Set.singleton "CHANGELOG.md")

                _libSourceDirs lib @?= ["src"]
                _libLanguage lib @?= Haskell98
                _libExposedModules lib @?= myLibModule :| []
                _libOtherModules lib @?= []
                _libOtherExts lib @?= []
                _libDependencies lib @?! []
                _libBuildTools lib @?= []
              Right (ProjectSettings _ _ lib exe test, _) -> do
                lib @?! Nothing
                exe @?= Nothing
                test @?= Nothing
              Left e -> assertFailure $ show e
        , testCase "Check the interactive library workflow - cabal < 1.18" $ do
            let inputs =
                  fromList
                    -- package type
                    [ "1"
                    , -- overwrite (not asked, pristine folder)
                      "test-package-dir"
                    , "[]"
                    , -- package dir
                      "test-package"
                    , -- package description
                      -- cabal version
                      "4"
                    , -- package name
                      "test-package"
                    , "test-package"
                    , "test-package"
                    , -- version
                      "3.1.2.3"
                    , -- license
                      "3"
                    , -- author
                      "git username"
                    , "Foobar"
                    , -- email
                      "git email"
                    , "foobar@qux.com"
                    , -- homepage
                      "qux.com"
                    , -- synopsis
                      "Qux's package"
                    , -- category
                      "3"
                    , -- library target
                      -- source dir
                      "1"
                    , -- language
                      "2"
                    , -- test suite
                      "n"
                    , -- comments
                      "y"
                    ]

                flags =
                  emptyFlags
                    { cabalVersion = Flag CabalSpecV1_10
                    , extraDoc = Flag [defaultChangelog]
                    , extraSrc = Flag ["README.md"]
                    }

            case (_runPrompt $ createProject silent pkgIx srcDb flags) inputs of
              Right (ProjectSettings opts desc (Just lib) Nothing Nothing, _) -> do
                _optOverwrite opts @?= False
                _optMinimal opts @?= False
                _optNoComments opts @?= False
                _optVerbosity opts @?= silent
                _optPkgDir opts @?= "/home/test/test-package"
                _optPkgType opts @?= Library
                _optPkgName opts @?= mkPackageName "test-package"

                _pkgCabalVersion desc @?= CabalSpecV1_10
                _pkgName desc @?= mkPackageName "test-package"
                _pkgVersion desc @?= mkVersion [3, 1, 2, 3]
                _pkgLicense desc @?! (SpecLicense . Left $ SPDX.NONE)
                _pkgAuthor desc @?= "Foobar"
                _pkgEmail desc @?= "foobar@qux.com"
                _pkgHomePage desc @?= "qux.com"
                _pkgSynopsis desc @?= "Qux's package"
                _pkgCategory desc @?= "Control"
                _pkgExtraSrcFiles desc @?= Set.fromList [defaultChangelog, "README.md"]
                _pkgExtraDocFiles desc @?= Nothing

                _libSourceDirs lib @?= ["src"]
                _libLanguage lib @?= Haskell98
                _libExposedModules lib @?= myLibModule :| []
                _libOtherModules lib @?= []
                _libOtherExts lib @?= []
                _libDependencies lib @?! []
                _libBuildTools lib @?= []
              Right (ProjectSettings _ _ lib exe test, _) -> do
                lib @?! Nothing
                exe @?= Nothing
                test @?= Nothing
              Left e -> assertFailure $ show e
        , testCase "Check the interactive executable workflow" $ do
            let inputs =
                  fromList
                    -- package type
                    [ "2"
                    , -- overwrite (not asked, pristine folder)
                      "test-package-dir"
                    , "[]"
                    , -- package dir
                      "test-package"
                    , -- package description
                      -- cabal version
                      "4"
                    , -- package name
                      "test-package"
                    , "test-package"
                    , "test-package"
                    , -- version
                      "3.1.2.3"
                    , -- license
                      "3"
                    , -- author
                      "git username"
                    , "Foobar"
                    , -- email
                      "git email"
                    , "foobar@qux.com"
                    , -- homepage
                      "qux.com"
                    , -- synopsis
                      "Qux's package"
                    , -- category
                      "3"
                    , -- executable target
                      -- main file
                      "1"
                    , -- application dir
                      "2"
                    , -- language
                      "2"
                    , -- comments
                      "y"
                    ]

            case (_runPrompt $ createProject silent pkgIx srcDb emptyFlags) inputs of
              Right (ProjectSettings opts desc Nothing (Just exe) Nothing, _) -> do
                _optOverwrite opts @?= False
                _optMinimal opts @?= False
                _optNoComments opts @?= False
                _optVerbosity opts @?= silent
                _optPkgDir opts @?= "/home/test/test-package"
                _optPkgType opts @?= Executable
                _optPkgName opts @?= mkPackageName "test-package"

                _pkgCabalVersion desc @?= CabalSpecV2_4
                _pkgName desc @?= mkPackageName "test-package"
                _pkgVersion desc @?= mkVersion [3, 1, 2, 3]
                _pkgLicense desc @?! (SpecLicense . Left $ SPDX.NONE)
                _pkgAuthor desc @?= "Foobar"
                _pkgEmail desc @?= "foobar@qux.com"
                _pkgHomePage desc @?= "qux.com"
                _pkgSynopsis desc @?= "Qux's package"
                _pkgCategory desc @?= "Control"
                _pkgExtraSrcFiles desc @?= mempty
                _pkgExtraDocFiles desc @?= pure (Set.singleton "CHANGELOG.md")

                _exeMainIs exe @?= HsFilePath "Main.hs" Standard
                _exeApplicationDirs exe @?= ["exe"]
                _exeLanguage exe @?= Haskell98
                _exeOtherModules exe @?= []
                _exeOtherExts exe @?= []
                _exeDependencies exe @?! []
                _exeBuildTools exe @?= []
              Right (ProjectSettings _ _ lib exe test, _) -> do
                lib @?= Nothing
                exe @?! Nothing
                test @?= Nothing
              Left e -> assertFailure $ show e
        ]
    ]

fileCreatorTests :: InstalledPackageIndex -> SourcePackageDb -> PackageName -> TestTree
fileCreatorTests pkgIx srcDb _pkgName =
  testGroup
    "generators"
    [ testGroup
        "genPkgDescription"
        [ testCase "Check common package flags workflow" $ do
            let inputs =
                  fromList
                    [ "1" -- pick the first cabal version in the list
                    , "my-test-package" -- package name
                    , "my-test-package" -- current dir for the purpose of guessing the package name
                    , "y" -- "yes to prompt internal to package name"
                    , "0.2.0.1" -- package version
                    , "2" -- pick the second license in the list
                    , "git username" -- name guessed by calling "git config user.name"
                    , "Foobar" -- author name
                    , "git email" -- email guessed by calling "git config user.email"
                    , "foobar@qux.com" -- maintainer email
                    , "qux.com" -- package homepage
                    , "Qux's package" -- package synopsis
                    , "3" -- pick the third category in the list
                    ]
            runGenTest inputs $ genPkgDescription emptyFlags srcDb
        ]
    , testGroup
        "genLibTarget"
        [ testCase "Check library package flags workflow" $ do
            let inputs =
                  fromList
                    [ "1" -- pick the first source directory in the list
                    , "2" -- pick the second language in the list
                    ]

            runGenTest inputs $ genLibTarget emptyFlags pkgIx
        ]
    , testGroup
        "genExeTarget"
        [ testCase "Check executable package flags workflow" $ do
            let inputs =
                  fromList
                    [ "1" -- pick the first main file option in the list
                    , "2" -- pick the second application directory in the list
                    , "1" -- pick the first language in the list
                    ]

            runGenTest inputs $ genExeTarget emptyFlags pkgIx
        ]
    , testGroup
        "genTestTarget"
        [ testCase "Check test package flags workflow" $ do
            let inputs =
                  fromList
                    [ "y" -- say yes to tests
                    , "1" -- pick the first main file option in the list
                    , "test" -- package test dir
                    , "1" -- pick the first language in the list
                    ]

            runGenTest inputs $ genTestTarget emptyFlags pkgIx
        ]
    ]
  where
    runGenTest inputs go = case _runPrompt go inputs of
      Left e -> assertFailure $ show e
      Right{} -> return ()

interactiveTests :: SourcePackageDb -> TestTree
interactiveTests srcDb =
  testGroup
    "Check top level getter functions"
    [ testGroup
        "Simple prompt tests"
        [ testGroup
            "Check packageNamePrompt output"
            [ testSimplePrompt
                "New package name 1"
                (packageNamePrompt srcDb)
                (mkPackageName "test-package")
                [ "test-package"
                , "test-package"
                , "test-package"
                ]
            , testSimplePrompt
                "New package name 2"
                (packageNamePrompt srcDb)
                (mkPackageName "test-package")
                [ "test-package"
                , "test-package"
                , ""
                ]
            , testSimplePrompt
                "Existing package name 1"
                (packageNamePrompt srcDb)
                (mkPackageName "test-package")
                [ "test-package"
                , "test-package"
                , "cabal-install"
                , "y"
                , "test-package"
                ]
            , testSimplePrompt
                "Existing package name 2"
                (packageNamePrompt srcDb)
                (mkPackageName "cabal-install")
                [ "test-package"
                , "test-package"
                , "cabal-install"
                , "n"
                ]
            ]
        , testGroup
            "Check mainFilePrompt output"
            [ testSimplePrompt
                "New valid main file"
                mainFilePrompt
                defaultMainIs
                [ "1"
                ]
            , testSimplePrompt
                "New valid other main file"
                mainFilePrompt
                (HsFilePath "Main.hs" Standard)
                [ "3"
                , "Main.hs"
                ]
            , testSimplePrompt
                "Invalid other main file"
                mainFilePrompt
                (HsFilePath "Main.lhs" Literate)
                [ "3"
                , "Yoink.jl"
                , "2"
                ]
            ]
        , testGroup
            "Check versionPrompt output"
            [ testSimplePrompt
                "Proper PVP"
                versionPrompt
                (mkVersion [0, 3, 1, 0])
                [ "0.3.1.0"
                ]
            , testSimplePrompt
                "No PVP"
                versionPrompt
                (mkVersion [0, 3, 1, 0])
                [ "yee-haw"
                , "0.3.1.0"
                ]
            ]
        , testGroup
            "Check synopsisPrompt output"
            [ testSimplePrompt
                "1"
                synopsisPrompt
                "We are Qux, and this is our package"
                ["We are Qux, and this is our package"]
            , testSimplePrompt
                "2"
                synopsisPrompt
                "Resistance is futile, you will be assimilated"
                ["Resistance is futile, you will be assimilated"]
            ]
        , testSimplePrompt
            "Check authorPrompt output (name supplied by the user)"
            authorPrompt
            "Foobar"
            ["git username", "Foobar"]
        , testSimplePrompt
            "Check authorPrompt output (name guessed from git config)"
            authorPrompt
            "git username"
            ["git username", ""]
        , testSimplePrompt
            "Check emailPrompt output (email supplied by the user)"
            emailPrompt
            "foobar@qux.com"
            ["git email", "foobar@qux.com"]
        , testSimplePrompt
            "Check emailPrompt output (email guessed from git config)"
            emailPrompt
            "git@email"
            ["git@email", ""]
        , testSimplePrompt
            "Check homepagePrompt output"
            homepagePrompt
            "qux.com"
            ["qux.com"]
        , testSimplePrompt
            "Check testDirsPrompt output"
            testDirsPrompt
            ["quxTest"]
            ["quxTest"]
        , -- this tests 4) other, and can be used to model more inputs in case of failure
          testSimplePrompt
            "Check srcDirsPrompt output"
            srcDirsPrompt
            ["app"]
            ["4", "app"]
        ]
    , testGroup
        "Numbered prompt tests"
        [ testGroup
            "Check categoryPrompt output"
            [ testNumberedPrompt
                "Category indices"
                categoryPrompt
                defaultCategories
            , testSimplePrompt
                "Other category"
                categoryPrompt
                "Unlisted"
                [ show $ P.length defaultCategories + 1
                , "Unlisted"
                ]
            , testSimplePrompt
                "No category"
                categoryPrompt
                ""
                [ ""
                ]
            ]
        , testGroup "Check licensePrompt output" $
            let other = show (1 + P.length defaultLicenseIds)
             in [ testNumberedPrompt "License indices" licensePrompt $
                    fmap (\l -> SpecLicense . Left . SPDX.License $ SPDX.ELicense (SPDX.ELicenseId l) Nothing) defaultLicenseIds
                , testSimplePrompt
                    "Other license 1"
                    licensePrompt
                    (SpecLicense . Left $ mkLicense SPDX.CC_BY_NC_ND_4_0)
                    [ other
                    , "CC-BY-NC-ND-4.0"
                    ]
                , testSimplePrompt
                    "Other license 2"
                    licensePrompt
                    (SpecLicense . Left $ mkLicense SPDX.D_FSL_1_0)
                    [ other
                    , "D-FSL-1.0"
                    ]
                , testSimplePrompt
                    "Other license 3"
                    licensePrompt
                    (SpecLicense . Left $ mkLicense SPDX.NPOSL_3_0)
                    [ other
                    , "NPOSL-3.0"
                    ]
                , testSimplePrompt
                    "Invalid license"
                    licensePrompt
                    (SpecLicense $ Left SPDX.NONE)
                    [ other
                    , "yay"
                    , other
                    , "NONE"
                    ]
                , testPromptBreak
                    "Invalid index"
                    licensePrompt
                    [ "42"
                    ]
                ]
        , testGroup
            "Check languagePrompt output"
            [ testNumberedPrompt
                "Language indices"
                (`languagePrompt` "test")
                [Haskell2010, Haskell98, GHC2021]
            , testSimplePrompt
                "Other language"
                (`languagePrompt` "test")
                (UnknownLanguage "Haskell2022")
                [ "4"
                , "Haskell2022"
                ]
            , testSimplePrompt
                "Invalid language"
                (`languagePrompt` "test")
                (UnknownLanguage "Lang_TS!")
                [ "4"
                , "Lang_TS!"
                ]
            ]
        , testGroup
            "Check srcDirsPrompt output"
            [ testNumberedPrompt
                "Source dirs indices"
                srcDirsPrompt
                [[defaultSourceDir], ["lib"], ["src-lib"]]
            , testSimplePrompt
                "Other source dir"
                srcDirsPrompt
                ["src"]
                [ "4"
                , "src"
                ]
            ]
        , testGroup
            "Check appDirsPrompt output"
            [ testNumberedPrompt
                "App dirs indices"
                appDirsPrompt
                [[defaultApplicationDir], ["exe"], ["src-exe"]]
            , testSimplePrompt
                "Other app dir"
                appDirsPrompt
                ["app"]
                [ "4"
                , "app"
                ]
            ]
        , testNumberedPrompt
            "Check packageTypePrompt output"
            packageTypePrompt
            [Library, Executable, LibraryAndExecutable]
        , testNumberedPrompt
            "Check cabalVersionPrompt output"
            cabalVersionPrompt
            defaultCabalVersions
        ]
    , testGroup
        "Bool prompt tests"
        [ testBoolPrompt "Check noCommentsPrompt output - y" noCommentsPrompt False "y"
        , testBoolPrompt "Check noCommentsPrompt output - Y" noCommentsPrompt False "Y"
        , testBoolPrompt "Check noCommentsPrompt output - n" noCommentsPrompt True "n"
        , testBoolPrompt "Check noCommentsPrompt output - N" noCommentsPrompt True "N"
        ]
    ]

-- -------------------------------------------------------------------- --
-- Prompt test utils

testSimplePrompt
  :: Eq a
  => Show a
  => String
  -> (InitFlags -> PurePrompt a)
  -> a
  -> [String]
  -> TestTree
testSimplePrompt label f target =
  testPrompt label f (assertFailure . show) (\(a, _) -> target @=? a)

testPromptBreak
  :: Eq a
  => Show a
  => String
  -> (InitFlags -> PurePrompt a)
  -> [String]
  -> TestTree
testPromptBreak label f =
  testPrompt label f go (assertFailure . show)
  where
    go BreakException{} =
      return ()

testPrompt
  :: Eq a
  => Show a
  => String
  -> (InitFlags -> PurePrompt a)
  -> (BreakException -> Assertion)
  -> ((a, NonEmpty String) -> Assertion)
  -> [String]
  -> TestTree
testPrompt label f g h input = testCase label $
  case (_runPrompt $ f emptyFlags) (fromList input) of
    Left x -> g x -- :: BreakException
    Right x -> h x -- :: (a, other inputs)

testNumberedPrompt :: (Eq a, Show a) => String -> (InitFlags -> PurePrompt a) -> [a] -> TestTree
testNumberedPrompt label act = testGroup label . (++ goBreak) . fmap go . indexed1
  where
    indexed1 = zip [1 :: Int ..]
    mkLabel a n =
      "testing index "
        ++ show n
        ++ ") with: "
        ++ show a

    go (n, a) =
      testSimplePrompt (mkLabel a n) act a [show n]
    goBreak =
      [ testPromptBreak "testing index -1" act ["-1"]
      , testPromptBreak "testing index 1000" act ["1000"]
      ]

testBoolPrompt
  :: String
  -> (InitFlags -> PurePrompt Bool)
  -> Bool
  -> String
  -> TestTree
testBoolPrompt label act target b =
  testSimplePrompt label act target [b]
