{-# LANGUAGE LambdaCase #-}

module UnitTests.Distribution.Client.Init.NonInteractive
  ( tests
  ) where

import Test.Tasty
import Test.Tasty.HUnit

import UnitTests.Distribution.Client.Init.Utils

import qualified Data.List.NonEmpty as NEL
import qualified Distribution.SPDX as SPDX

import Data.List (foldl')
import qualified Data.Set as Set
import Distribution.CabalSpecVersion
import Distribution.Client.Init.Defaults
import Distribution.Client.Init.NonInteractive.Command
import Distribution.Client.Init.Types
import Distribution.Client.Init.Utils (mkPackageNameDep, mkStringyDep)
import Distribution.Client.Setup (initCommand)
import Distribution.Client.Types
import Distribution.FieldGrammar.Newtypes
import Distribution.ModuleName (fromString)
import Distribution.Simple
import Distribution.Simple.Command
import Distribution.Simple.Flag
import Distribution.Simple.PackageIndex
import Distribution.Verbosity

tests
  :: Verbosity
  -> InitFlags
  -> Compiler
  -> InstalledPackageIndex
  -> SourcePackageDb
  -> TestTree
tests _v _initFlags comp pkgIx srcDb =
  testGroup
    "Distribution.Client.Init.NonInteractive.Command"
    [ testGroup
        "driver function test"
        [ driverFunctionTest pkgIx srcDb comp
        ]
    , testGroup
        "target creator tests"
        [ fileCreatorTests pkgIx srcDb comp
        ]
    , testGroup
        "non-interactive tests"
        [ nonInteractiveTests pkgIx srcDb comp
        ]
    , testGroup
        "cli parser tests"
        [ cliListParserTests
        ]
    ]

driverFunctionTest
  :: InstalledPackageIndex
  -> SourcePackageDb
  -> Compiler
  -> TestTree
driverFunctionTest pkgIx srcDb comp =
  testGroup
    "createProject"
    [ testGroup
        "with flags"
        [ testCase "Check the non-interactive workflow 1" $ do
            let dummyFlags' =
                  dummyFlags
                    { packageType = Flag LibraryAndExecutable
                    , minimal = Flag False
                    , overwrite = Flag False
                    , packageDir = Flag "/home/test/test-package"
                    , extraDoc = Flag ["CHANGELOG.md"]
                    , exposedModules = Flag []
                    , otherModules = Flag []
                    , otherExts = Flag []
                    , buildTools = Flag []
                    , mainIs = Flag "quxApp/Main.hs"
                    , dependencies = Flag []
                    }
                inputs =
                  NEL.fromList
                    [ "Foobar"
                    , "foobar@qux.com"
                    , "True"
                    , "[\"quxTest/Main.hs\"]"
                    ]

            case (_runPrompt $ createProject comp silent pkgIx srcDb dummyFlags') inputs of
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
                _libExposedModules lib @?= myLibModule NEL.:| []
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

                assertBool "The library should be a dependency of the executable" $
                  mkPackageNameDep (_optPkgName opts) `elem` _exeDependencies exe
                assertBool "The library should be a dependency of the test executable" $
                  mkPackageNameDep (_optPkgName opts) `elem` _testDependencies test
              Right (ProjectSettings _ _ lib exe test, _) -> do
                lib @?! Nothing
                exe @?! Nothing
                test @?! Nothing
              Left e -> assertFailure $ show e
        , testCase "Check the non-interactive workflow 2" $ do
            let dummyFlags' =
                  dummyFlags
                    { packageType = Flag LibraryAndExecutable
                    , minimal = Flag False
                    , overwrite = Flag False
                    , packageDir = Flag "/home/test/test-package"
                    , extraSrc = Flag []
                    , exposedModules = Flag []
                    , otherModules = NoFlag
                    , otherExts = Flag []
                    , buildTools = Flag []
                    , mainIs = Flag "quxApp/Main.hs"
                    , dependencies = Flag []
                    }
                inputs =
                  NEL.fromList
                    [ "Foobar"
                    , "foobar@qux.com"
                    , -- extra sources
                      "[\"CHANGELOG.md\"]"
                    , -- lib other modules
                      "False"
                    , -- exe other modules
                      "False"
                    , -- test main file
                      "True"
                    , "[\"quxTest/Main.hs\"]"
                    , -- test other modules
                      "False"
                    ]

            case (_runPrompt $ createProject comp silent pkgIx srcDb dummyFlags') inputs of
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
                _libExposedModules lib @?= myLibModule NEL.:| []
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

                assertBool "The library should be a dependency of the executable" $
                  mkPackageNameDep (_optPkgName opts) `elem` _exeDependencies exe
                assertBool "The library should be a dependency of the test executable" $
                  mkPackageNameDep (_optPkgName opts) `elem` _testDependencies test
              Right (ProjectSettings _ _ lib exe test, _) -> do
                lib @?! Nothing
                exe @?! Nothing
                test @?! Nothing
              Left e -> assertFailure $ show e
        ]
    , testGroup
        "with tests"
        [ testCase "Check the non-interactive library and executable workflow" $ do
            let inputs =
                  NEL.fromList
                    -- package dir
                    [ "test-package"
                    , -- package description
                      -- cabal version
                      "cabal-install version 3.4.0.0\ncompiled using version 3.4.0.0 of the Cabal library \n"
                    , -- package name
                      "test-package"
                    , "test-package"
                    , -- author name
                      ""
                    , "Foobar"
                    , -- author email
                      ""
                    , "foobar@qux.com"
                    , -- extra source files
                      "test-package"
                    , "[]"
                    , -- library target
                      -- source dirs
                      "src"
                    , "True"
                    , -- exposed modules
                      "src"
                    , "True"
                    , "True"
                    , "[\"src/Foo.hs\", \"src/Bar.hs\"]"
                    , "module Foo where"
                    , "module Bar where"
                    , "test-package"
                    , "True"
                    , "[\"src/Foo.hs\", \"src/Bar.hs\"]"
                    , "module Foo where"
                    , "module Bar where"
                    , -- other modules
                      "test-package"
                    , "True"
                    , "[\"src/Foo.hs\", \"src/Bar.hs\", \"src/Baz/Internal.hs\"]"
                    , "module Foo where"
                    , "module Bar where"
                    , "module Baz.Internal where"
                    , -- other extensions
                      "True"
                    , "[\"src/Foo.hs\", \"src/Bar.hs\"]"
                    , "\"{-# LANGUAGE OverloadedStrings, LambdaCase #-}\n{-# LANGUAGE RankNTypes #-}\""
                    , "\"{-# LANGUAGE RecordWildCards #-}\""
                    , -- dependencies
                      "True"
                    , "[\"src/Foo.hs\"]"
                    , "True"
                    , "test-package"
                    , "module Main where"
                    , "import Control.Monad.Extra"
                    , "{-# LANGUAGE OverloadedStrings, LambdaCase #-}"
                    , -- build tools
                      "True"
                    , "[\"app/Main.hs\", \"src/Foo.hs\", \"src/bar.y\"]"
                    , -- executable target
                      -- application dirs
                      "app"
                    , "[]"
                    , -- main file
                      "test-package"
                    , "[\"test-package/app/\"]"
                    , "True"
                    , "[]"
                    , -- other modules
                      "test-package"
                    , "True"
                    , "[\"app/Main.hs\", \"app/Foo.hs\", \"app/Bar.hs\"]"
                    , "module Foo where"
                    , "module Bar where"
                    , -- other extensions
                      "True"
                    , "[\"app/Foo.hs\", \"app/Bar.hs\"]"
                    , "\"{-# LANGUAGE OverloadedStrings, LambdaCase #-}\n{-# LANGUAGE RankNTypes #-}\""
                    , "\"{-# LANGUAGE RecordWildCards #-}\""
                    , -- dependencies
                      "True"
                    , "[\"app/Main.hs\"]"
                    , "True"
                    , "test-package"
                    , "module Main where"
                    , "import Control.Monad.Extra"
                    , "{-# LANGUAGE OverloadedStrings, DataKinds #-}"
                    , -- build tools
                      "True"
                    , "[\"app/Main.hs\", \"src/Foo.hs\", \"src/bar.y\"]"
                    , -- test target
                      -- main file
                      "True"
                    , "[\"test-package/test/\"]"
                    , -- other modules
                      "test-package"
                    , "True"
                    , "[\"test/Main.hs\", \"test/Foo.hs\", \"test/Bar.hs\"]"
                    , "module Foo where"
                    , "module Bar where"
                    , -- other extensions
                      "True"
                    , "[\"test/Foo.hs\", \"test/Bar.hs\"]"
                    , "\"{-# LANGUAGE OverloadedStrings, LambdaCase #-}\n{-# LANGUAGE RankNTypes #-}\""
                    , "\"{-# LANGUAGE RecordWildCards #-}\""
                    , -- dependencies
                      "True"
                    , "[\"test/Main.hs\"]"
                    , "True"
                    , "test-package"
                    , "module Main where"
                    , "import Test.Tasty\nimport Test.Tasty.HUnit"
                    , "{-# LANGUAGE OverloadedStrings, LambdaCase #-}"
                    , -- build tools
                      "True"
                    , "[\"test/Main.hs\", \"test/Foo.hs\", \"test/bar.y\"]"
                    ]

            case ( _runPrompt $
                    createProject
                      comp
                      silent
                      pkgIx
                      srcDb
                      ( emptyFlags
                          { initializeTestSuite = Flag True
                          , packageType = Flag LibraryAndExecutable
                          }
                      )
                 )
              inputs of
              Right (ProjectSettings opts desc (Just lib) (Just exe) (Just test), _) -> do
                _optOverwrite opts @?= False
                _optMinimal opts @?= False
                _optNoComments opts @?= False
                _optVerbosity opts @?= silent
                _optPkgDir opts @?= "/home/test/test-package"
                _optPkgType opts @?= LibraryAndExecutable
                _optPkgName opts @?= mkPackageName "test-package"

                _pkgCabalVersion desc @?= CabalSpecV3_4
                _pkgName desc @?= mkPackageName "test-package"
                _pkgVersion desc @?= mkVersion [0, 1, 0, 0]
                _pkgLicense desc @?= (SpecLicense . Left $ SPDX.NONE)
                _pkgAuthor desc @?= "Foobar"
                _pkgEmail desc @?= "foobar@qux.com"
                _pkgHomePage desc @?= ""
                _pkgSynopsis desc @?= ""
                _pkgCategory desc @?= ""
                _pkgExtraSrcFiles desc @?= mempty
                _pkgExtraDocFiles desc @?= pure (Set.singleton "CHANGELOG.md")

                _libSourceDirs lib @?= ["src"]
                _libLanguage lib @?= Haskell2010
                _libExposedModules lib @?= NEL.fromList (map fromString ["Foo", "Bar"])
                _libOtherModules lib @?= map fromString ["Baz.Internal"]
                _libOtherExts lib @?= map EnableExtension [OverloadedStrings, LambdaCase, RankNTypes, RecordWildCards]
                _libDependencies lib @?! []
                _libBuildTools lib @?= [mkStringyDep "happy:happy"]

                _exeMainIs exe @?= HsFilePath "Main.hs" Standard
                _exeApplicationDirs exe @?= ["app"]
                _exeLanguage exe @?= Haskell2010
                _exeOtherModules exe @?= map fromString ["Foo", "Bar"]
                _exeOtherExts exe @?= map EnableExtension [OverloadedStrings, LambdaCase, RankNTypes, RecordWildCards]
                _exeDependencies exe @?! []
                _exeBuildTools exe @?= [mkStringyDep "happy:happy"]

                _testMainIs test @?= HsFilePath "Main.hs" Standard
                _testDirs test @?= ["test"]
                _testLanguage test @?= Haskell2010
                _testOtherModules test @?= map fromString ["Foo", "Bar"]
                _testOtherExts test @?= map EnableExtension [OverloadedStrings, LambdaCase, RankNTypes, RecordWildCards]
                _testDependencies test @?! []
                _testBuildTools test @?= [mkStringyDep "happy:happy"]

                assertBool "The library should be a dependency of the executable" $
                  mkPackageNameDep (_optPkgName opts) `elem` _exeDependencies exe
                assertBool "The library should be a dependency of the test executable" $
                  mkPackageNameDep (_optPkgName opts) `elem` _testDependencies test
              Right (ProjectSettings _ _ lib exe test, _) -> do
                lib @?! Nothing
                exe @?! Nothing
                test @?! Nothing
              Left e -> assertFailure $ show e
        , testCase "Check the non-interactive library workflow" $ do
            let inputs =
                  NEL.fromList
                    -- package dir
                    [ "test-package"
                    , -- package description
                      -- cabal version
                      "cabal-install version 3.4.0.0\ncompiled using version 3.4.0.0 of the Cabal library \n"
                    , -- package name
                      "test-package"
                    , "test-package"
                    , -- author name
                      "Foobar"
                    , -- author email
                      "foobar@qux.com"
                    , -- extra source files
                      "test-package"
                    , "[]"
                    , -- library target
                      -- source dirs
                      "src"
                    , "True"
                    , -- exposed modules
                      "src"
                    , "True"
                    , "True"
                    , "[\"src/Foo.hs\", \"src/Bar.hs\"]"
                    , "module Foo where"
                    , "module Bar where"
                    , "test-package"
                    , "True"
                    , "[\"src/Foo.hs\", \"src/Bar.hs\"]"
                    , "module Foo where"
                    , "module Bar where"
                    , -- other modules
                      "test-package"
                    , "True"
                    , "[\"src/Foo.hs\", \"src/Bar.hs\", \"src/Baz/Internal.hs\"]"
                    , "module Foo where"
                    , "module Bar where"
                    , "module Baz.Internal where"
                    , -- other extensions
                      "True"
                    , "[\"src/Foo.hs\", \"src/Bar.hs\"]"
                    , "\"{-# LANGUAGE OverloadedStrings, LambdaCase #-}\n{-# LANGUAGE RankNTypes #-}\""
                    , "\"{-# LANGUAGE RecordWildCards #-}\""
                    , -- dependencies
                      "True"
                    , "[\"src/Foo.hs\"]"
                    , "True"
                    , "test-package"
                    , "module Main where"
                    , "import Control.Monad.Extra"
                    , "{-# LANGUAGE OverloadedStrings, LambdaCase #-}"
                    , -- build tools
                      "True"
                    , "[\"app/Main.hs\", \"src/Foo.hs\", \"src/bar.y\"]"
                    , -- test target
                      -- main file
                      "True"
                    , "[\"test-package/test/\"]"
                    , -- other modules
                      "test-package"
                    , "True"
                    , "[\"test/Main.hs\", \"test/Foo.hs\", \"test/Bar.hs\"]"
                    , "module Foo where"
                    , "module Bar where"
                    , -- other extensions
                      "True"
                    , "[\"test/Foo.hs\", \"test/Bar.hs\"]"
                    , "\"{-# LANGUAGE OverloadedStrings, LambdaCase #-}\n{-# LANGUAGE RankNTypes #-}\""
                    , "\"{-# LANGUAGE RecordWildCards #-}\""
                    , -- dependencies
                      "True"
                    , "[\"test/Main.hs\"]"
                    , "True"
                    , "test-package"
                    , "module Main where"
                    , "import Test.Tasty\nimport Test.Tasty.HUnit"
                    , "{-# LANGUAGE OverloadedStrings, LambdaCase #-}"
                    , -- build tools
                      "True"
                    , "[\"test/Main.hs\", \"test/Foo.hs\", \"test/bar.y\"]"
                    ]

            case ( _runPrompt $
                    createProject
                      comp
                      silent
                      pkgIx
                      srcDb
                      ( emptyFlags
                          { initializeTestSuite = Flag True
                          , packageType = Flag Library
                          }
                      )
                 )
              inputs of
              Right (ProjectSettings opts desc (Just lib) Nothing (Just test), _) -> do
                _optOverwrite opts @?= False
                _optMinimal opts @?= False
                _optNoComments opts @?= False
                _optVerbosity opts @?= silent
                _optPkgDir opts @?= "/home/test/test-package"
                _optPkgType opts @?= Library
                _optPkgName opts @?= mkPackageName "test-package"

                _pkgCabalVersion desc @?= CabalSpecV3_4
                _pkgName desc @?= mkPackageName "test-package"
                _pkgVersion desc @?= mkVersion [0, 1, 0, 0]
                _pkgLicense desc @?= (SpecLicense . Left $ SPDX.NONE)
                _pkgAuthor desc @?= "Foobar"
                _pkgEmail desc @?= "foobar@qux.com"
                _pkgHomePage desc @?= ""
                _pkgSynopsis desc @?= ""
                _pkgCategory desc @?= ""
                _pkgExtraSrcFiles desc @?= mempty
                _pkgExtraDocFiles desc @?= pure (Set.singleton "CHANGELOG.md")

                _libSourceDirs lib @?= ["src"]
                _libLanguage lib @?= Haskell2010
                _libExposedModules lib @?= NEL.fromList (map fromString ["Foo", "Bar"])
                _libOtherModules lib @?= map fromString ["Baz.Internal"]
                _libOtherExts lib @?= map EnableExtension [OverloadedStrings, LambdaCase, RankNTypes, RecordWildCards]
                _libDependencies lib @?! []
                _libBuildTools lib @?= [mkStringyDep "happy:happy"]

                _testMainIs test @?= HsFilePath "Main.hs" Standard
                _testDirs test @?= ["test"]
                _testLanguage test @?= Haskell2010
                _testOtherModules test @?= map fromString ["Foo", "Bar"]
                _testOtherExts test @?= map EnableExtension [OverloadedStrings, LambdaCase, RankNTypes, RecordWildCards]
                _testDependencies test @?! []
                _testBuildTools test @?= [mkStringyDep "happy:happy"]

                assertBool "The library should be a dependency of the test executable" $
                  mkPackageNameDep (_optPkgName opts) `elem` _testDependencies test
              Right (ProjectSettings _ _ lib exe test, _) -> do
                lib @?! Nothing
                exe @?= Nothing
                test @?! Nothing
              Left e -> assertFailure $ show e
        ]
    , testGroup
        "without tests"
        [ testCase "Check the non-interactive library and executable workflow" $ do
            let inputs =
                  NEL.fromList
                    -- package type
                    [ "test-package"
                    , "[\".\", \"..\", \"src/\", \"app/Main.hs\"]"
                    , "[\".\", \"..\", \"src/\", \"app/Main.hs\"]"
                    , -- package dir
                      "test-package"
                    , -- package description
                      -- cabal version
                      "cabal-install version 3.4.0.0\ncompiled using version 3.4.0.0 of the Cabal library \n"
                    , -- package name
                      "test-package"
                    , "test-package"
                    , -- author name
                      ""
                    , "Foobar"
                    , -- author email
                      ""
                    , "foobar@qux.com"
                    , -- extra source files
                      "test-package"
                    , "[]"
                    , -- library target
                      -- source dirs
                      "src"
                    , "True"
                    , -- exposed modules
                      "src"
                    , "True"
                    , "True"
                    , "[\"src/Foo.hs\", \"src/Bar.hs\"]"
                    , "module Foo where"
                    , "module Bar where"
                    , "test-package"
                    , "True"
                    , "[\"src/Foo.hs\", \"src/Bar.hs\"]"
                    , "module Foo where"
                    , "module Bar where"
                    , -- other modules
                      "test-package"
                    , "True"
                    , "[\"src/Foo.hs\", \"src/Bar.hs\", \"src/Baz/Internal.hs\"]"
                    , "module Foo where"
                    , "module Bar where"
                    , "module Baz.Internal where"
                    , -- other extensions
                      "True"
                    , "[\"src/Foo.hs\", \"src/Bar.hs\"]"
                    , "\"{-# LANGUAGE OverloadedStrings, LambdaCase #-}\n{-# LANGUAGE RankNTypes #-}\""
                    , "\"{-# LANGUAGE RecordWildCards #-}\""
                    , -- dependencies
                      "True"
                    , "[\"src/Foo.hs\"]"
                    , "True"
                    , "test-package"
                    , "module Main where"
                    , "import Control.Monad.Extra"
                    , "{-# LANGUAGE OverloadedStrings, LambdaCase #-}"
                    , -- build tools
                      "True"
                    , "[\"app/Main.hs\", \"src/Foo.hs\", \"src/bar.y\"]"
                    , -- executable target
                      -- application dirs
                      "app"
                    , "[]"
                    , -- main file
                      "test-package"
                    , "[\"test-package/app/\"]"
                    , "True"
                    , "[]"
                    , -- other modules
                      "test-package"
                    , "True"
                    , "[\"app/Main.hs\", \"app/Foo.hs\", \"app/Bar.hs\"]"
                    , "module Foo where"
                    , "module Bar where"
                    , -- other extensions
                      "True"
                    , "[\"app/Foo.hs\", \"app/Bar.hs\"]"
                    , "\"{-# LANGUAGE OverloadedStrings, LambdaCase #-}\n{-# LANGUAGE RankNTypes #-}\""
                    , "\"{-# LANGUAGE RecordWildCards #-}\""
                    , -- dependencies
                      "True"
                    , "[\"app/Main.hs\"]"
                    , "True"
                    , "test-package"
                    , "module Main where"
                    , "import Control.Monad.Extra"
                    , "{-# LANGUAGE OverloadedStrings, DataKinds #-}"
                    , -- build tools
                      "True"
                    , "[\"app/Main.hs\", \"src/Foo.hs\", \"src/bar.y\"]"
                    ]

            case (_runPrompt $ createProject comp silent pkgIx srcDb emptyFlags) inputs of
              Right (ProjectSettings opts desc (Just lib) (Just exe) Nothing, _) -> do
                _optOverwrite opts @?= False
                _optMinimal opts @?= False
                _optNoComments opts @?= False
                _optVerbosity opts @?= silent
                _optPkgDir opts @?= "/home/test/test-package"
                _optPkgType opts @?= LibraryAndExecutable
                _optPkgName opts @?= mkPackageName "test-package"

                _pkgCabalVersion desc @?= CabalSpecV3_4
                _pkgName desc @?= mkPackageName "test-package"
                _pkgVersion desc @?= mkVersion [0, 1, 0, 0]
                _pkgLicense desc @?= (SpecLicense . Left $ SPDX.NONE)
                _pkgAuthor desc @?= "Foobar"
                _pkgEmail desc @?= "foobar@qux.com"
                _pkgHomePage desc @?= ""
                _pkgSynopsis desc @?= ""
                _pkgCategory desc @?= ""
                _pkgExtraSrcFiles desc @?= mempty
                _pkgExtraDocFiles desc @?= pure (Set.singleton "CHANGELOG.md")

                _libSourceDirs lib @?= ["src"]
                _libLanguage lib @?= Haskell2010
                _libExposedModules lib @?= NEL.fromList (map fromString ["Foo", "Bar"])
                _libOtherModules lib @?= map fromString ["Baz.Internal"]
                _libOtherExts lib @?= map EnableExtension [OverloadedStrings, LambdaCase, RankNTypes, RecordWildCards]
                _libDependencies lib @?! []
                _libBuildTools lib @?= [mkStringyDep "happy:happy"]

                _exeMainIs exe @?= HsFilePath "Main.hs" Standard
                _exeApplicationDirs exe @?= ["app"]
                _exeLanguage exe @?= Haskell2010
                _exeOtherModules exe @?= map fromString ["Foo", "Bar"]
                _exeOtherExts exe @?= map EnableExtension [OverloadedStrings, LambdaCase, RankNTypes, RecordWildCards]
                _exeDependencies exe @?! []
                _exeBuildTools exe @?= [mkStringyDep "happy:happy"]

                assertBool "The library should be a dependency of the executable" $
                  mkPackageNameDep (_optPkgName opts) `elem` _exeDependencies exe
              Right (ProjectSettings _ _ lib exe test, _) -> do
                lib @?! Nothing
                exe @?! Nothing
                test @?= Nothing
              Left e -> assertFailure $ show e
        , testCase "Check the non-interactive library workflow" $ do
            let inputs =
                  NEL.fromList
                    -- package type
                    [ "test-package"
                    , "[\".\", \"..\", \"src/\"]"
                    , "[\".\", \"..\", \"src/\"]"
                    , -- package dir
                      "test-package"
                    , -- package description
                      -- cabal version
                      "cabal-install version 3.4.0.0\ncompiled using version 3.4.0.0 of the Cabal library \n"
                    , -- package name
                      "test-package"
                    , "test-package"
                    , -- author name
                      ""
                    , "Foobar"
                    , -- author email
                      ""
                    , "foobar@qux.com"
                    , -- extra source files
                      "test-package"
                    , "[]"
                    , -- library target
                      -- source dirs
                      "src"
                    , "True"
                    , -- exposed modules
                      "src"
                    , "True"
                    , "True"
                    , "[\"src/Foo.hs\", \"src/Bar.hs\"]"
                    , "module Foo where"
                    , "module Bar where"
                    , "test-package"
                    , "True"
                    , "[\"src/Foo.hs\", \"src/Bar.hs\"]"
                    , "module Foo where"
                    , "module Bar where"
                    , -- other modules
                      "test-package"
                    , "True"
                    , "[\"src/Foo.hs\", \"src/Bar.hs\", \"src/Baz/Internal.hs\"]"
                    , "module Foo where"
                    , "module Bar where"
                    , "module Baz.Internal where"
                    , -- other extensions
                      "True"
                    , "[\"src/Foo.hs\", \"src/Bar.hs\"]"
                    , "\"{-# LANGUAGE OverloadedStrings, LambdaCase #-}\n{-# LANGUAGE RankNTypes #-}\""
                    , "\"{-# LANGUAGE RecordWildCards #-}\""
                    , -- dependencies
                      "True"
                    , "[\"src/Foo.hs\"]"
                    , "True"
                    , "test-package"
                    , "module Main where"
                    , "import Control.Monad.Extra"
                    , "{-# LANGUAGE OverloadedStrings, LambdaCase #-}"
                    , -- build tools
                      "True"
                    , "[\"app/Main.hs\", \"src/Foo.hs\", \"src/bar.y\"]"
                    ]

            case (_runPrompt $ createProject comp silent pkgIx srcDb emptyFlags) inputs of
              Right (ProjectSettings opts desc (Just lib) Nothing Nothing, _) -> do
                _optOverwrite opts @?= False
                _optMinimal opts @?= False
                _optNoComments opts @?= False
                _optVerbosity opts @?= silent
                _optPkgDir opts @?= "/home/test/test-package"
                _optPkgType opts @?= Library
                _optPkgName opts @?= mkPackageName "test-package"

                _pkgCabalVersion desc @?= CabalSpecV3_4
                _pkgName desc @?= mkPackageName "test-package"
                _pkgVersion desc @?= mkVersion [0, 1, 0, 0]
                _pkgLicense desc @?= (SpecLicense . Left $ SPDX.NONE)
                _pkgAuthor desc @?= "Foobar"
                _pkgEmail desc @?= "foobar@qux.com"
                _pkgHomePage desc @?= ""
                _pkgSynopsis desc @?= ""
                _pkgCategory desc @?= ""
                _pkgExtraSrcFiles desc @?= mempty
                _pkgExtraDocFiles desc @?= pure (Set.singleton "CHANGELOG.md")

                _libSourceDirs lib @?= ["src"]
                _libLanguage lib @?= Haskell2010
                _libExposedModules lib @?= NEL.fromList (map fromString ["Foo", "Bar"])
                _libOtherModules lib @?= map fromString ["Baz.Internal"]
                _libOtherExts lib @?= map EnableExtension [OverloadedStrings, LambdaCase, RankNTypes, RecordWildCards]
                _libDependencies lib @?! []
                _libBuildTools lib @?= [mkStringyDep "happy:happy"]
              Right (ProjectSettings _ _ lib exe test, _) -> do
                lib @?! Nothing
                exe @?= Nothing
                test @?= Nothing
              Left e -> assertFailure $ show e
        , testCase "Check the non-interactive executable workflow" $ do
            let inputs =
                  NEL.fromList
                    -- package type
                    [ "test-package"
                    , "[\".\", \"..\", \"app/Main.hs\"]"
                    , "[\".\", \"..\", \"app/Main.hs\"]"
                    , -- package dir
                      "test-package"
                    , -- package description
                      -- cabal version
                      "cabal-install version 3.4.0.0\ncompiled using version 3.4.0.0 of the Cabal library \n"
                    , -- package name
                      "test-package"
                    , "test-package"
                    , -- author name
                      ""
                    , "Foobar"
                    , -- author email
                      ""
                    , "foobar@qux.com"
                    , -- extra source files
                      "test-package"
                    , "[]"
                    , -- executable target
                      -- application dirs
                      "app"
                    , "[]"
                    , -- main file
                      "test-package"
                    , "[\"test-package/app/\"]"
                    , "True"
                    , "[]"
                    , -- other modules
                      "test-package"
                    , "True"
                    , "[\"app/Main.hs\", \"app/Foo.hs\", \"app/Bar.hs\"]"
                    , "module Foo where"
                    , "module Bar where"
                    , -- other extensions
                      "True"
                    , "[\"app/Foo.hs\", \"app/Bar.hs\"]"
                    , "\"{-# LANGUAGE OverloadedStrings, LambdaCase #-}\n{-# LANGUAGE RankNTypes #-}\""
                    , "\"{-# LANGUAGE RecordWildCards #-}\""
                    , -- dependencies
                      "True"
                    , "[\"app/Main.hs\"]"
                    , "True"
                    , "test-package"
                    , "module Main where"
                    , "import Control.Monad.Extra"
                    , "{-# LANGUAGE OverloadedStrings, DataKinds #-}"
                    , -- build tools
                      "True"
                    , "[\"app/Main.hs\", \"src/Foo.hs\", \"src/bar.y\"]"
                    ]

            case (_runPrompt $ createProject comp silent pkgIx srcDb emptyFlags) inputs of
              Right (ProjectSettings opts desc Nothing (Just exe) Nothing, _) -> do
                _optOverwrite opts @?= False
                _optMinimal opts @?= False
                _optNoComments opts @?= False
                _optVerbosity opts @?= silent
                _optPkgDir opts @?= "/home/test/test-package"
                _optPkgType opts @?= Executable
                _optPkgName opts @?= mkPackageName "test-package"

                _pkgCabalVersion desc @?= CabalSpecV3_4
                _pkgName desc @?= mkPackageName "test-package"
                _pkgVersion desc @?= mkVersion [0, 1, 0, 0]
                _pkgLicense desc @?= (SpecLicense . Left $ SPDX.NONE)
                _pkgAuthor desc @?= "Foobar"
                _pkgEmail desc @?= "foobar@qux.com"
                _pkgHomePage desc @?= ""
                _pkgSynopsis desc @?= ""
                _pkgCategory desc @?= ""
                _pkgExtraSrcFiles desc @?= mempty
                _pkgExtraDocFiles desc @?= pure (Set.singleton "CHANGELOG.md")

                _exeMainIs exe @?= HsFilePath "Main.hs" Standard
                _exeApplicationDirs exe @?= ["app"]
                _exeLanguage exe @?= Haskell2010
                _exeOtherModules exe @?= map fromString ["Foo", "Bar"]
                _exeOtherExts exe @?= map EnableExtension [OverloadedStrings, LambdaCase, RankNTypes, RecordWildCards]
                _exeDependencies exe @?! []
                _exeBuildTools exe @?= [mkStringyDep "happy:happy"]
              Right (ProjectSettings _ _ lib exe test, _) -> do
                lib @?= Nothing
                exe @?! Nothing
                test @?= Nothing
              Left e -> assertFailure $ show e
        ]
    ]

fileCreatorTests
  :: InstalledPackageIndex
  -> SourcePackageDb
  -> Compiler
  -> TestTree
fileCreatorTests pkgIx srcDb comp =
  testGroup
    "generators"
    [ testGroup
        "genPkgDescription"
        [ testCase "Check common package flags workflow" $ do
            let inputs =
                  NEL.fromList
                    -- cabal version
                    [ "cabal-install version 2.4.0.0\ncompiled using version 2.4.0.0 of the Cabal library \n"
                    , -- package name
                      "test-package"
                    , "test-package"
                    , -- author name
                      ""
                    , "Foobar"
                    , -- author email
                      ""
                    , "foobar@qux.com"
                    , -- extra source files
                      "test-package"
                    , "[]"
                    ]

            case (_runPrompt $ genPkgDescription emptyFlags srcDb) inputs of
              Left e -> assertFailure $ show e
              Right{} -> return ()
        ]
    , testGroup
        "genLibTarget"
        [ testCase "Check library package flags workflow" $ do
            let inputs =
                  NEL.fromList
                    -- source dirs
                    [ "src"
                    , "True"
                    , -- exposed modules
                      "src"
                    , "True"
                    , "True"
                    , "[\"src/Foo.hs\", \"src/Bar.hs\"]"
                    , "module Foo where"
                    , "module Bar where"
                    , "test-package"
                    , "True"
                    , "[\"src/Foo.hs\", \"src/Bar.hs\"]"
                    , "module Foo where"
                    , "module Bar where"
                    , -- other modules
                      "test-package"
                    , "True"
                    , "[\"src/Foo.hs\", \"src/Bar.hs\", \"src/Baz/Internal.hs\"]"
                    , "module Foo where"
                    , "module Bar where"
                    , "module Baz.Internal where"
                    , -- other extensions
                      "True"
                    , "[\"src/Foo.hs\", \"src/Bar.hs\"]"
                    , "\"{-# LANGUAGE OverloadedStrings, LambdaCase #-}\n{-# LANGUAGE RankNTypes #-}\""
                    , "\"{-# LANGUAGE RecordWildCards #-}\""
                    , -- dependencies
                      "True"
                    , "[\"src/Foo.hs\"]"
                    , "True"
                    , "test-package"
                    , "module Main where"
                    , "import Control.Monad.Extra"
                    , "{-# LANGUAGE OverloadedStrings, LambdaCase #-}"
                    , -- build tools
                      "True"
                    , "[\"app/Main.hs\", \"src/Foo.hs\", \"src/bar.y\"]"
                    ]

            case (_runPrompt $ genLibTarget emptyFlags comp pkgIx defaultCabalVersion) inputs of
              Left e -> assertFailure $ show e
              Right{} -> return ()
        ]
    , testGroup
        "genExeTarget"
        [ testCase "Check executable package flags workflow" $ do
            let inputs =
                  NEL.fromList
                    -- application dirs
                    [ "app"
                    , "[]"
                    , -- main file
                      "test-package"
                    , "[\"test-package/app/\"]"
                    , "True"
                    , "[]"
                    , -- other modules
                      "test-package"
                    , "True"
                    , "[\"app/Main.hs\", \"app/Foo.hs\", \"app/Bar.hs\"]"
                    , "module Foo where"
                    , "module Bar where"
                    , -- other extensions
                      "True"
                    , "[\"app/Foo.hs\", \"app/Bar.hs\"]"
                    , "\"{-# LANGUAGE OverloadedStrings, LambdaCase #-}\n{-# LANGUAGE RankNTypes #-}\""
                    , "\"{-# LANGUAGE RecordWildCards #-}\""
                    , -- dependencies
                      "True"
                    , "[\"app/Main.hs\"]"
                    , "True"
                    , "test-package"
                    , "module Main where"
                    , "import Control.Monad.Extra"
                    , "{-# LANGUAGE OverloadedStrings, LambdaCase #-}"
                    , -- build tools
                      "True"
                    , "[\"app/Main.hs\", \"src/Foo.hs\", \"src/bar.y\"]"
                    ]

            case (_runPrompt $ genExeTarget emptyFlags comp pkgIx defaultCabalVersion) inputs of
              Left e -> assertFailure $ show e
              Right{} -> return ()
        ]
    , testGroup
        "genTestTarget"
        [ testCase "Check test package flags workflow" $ do
            let inputs =
                  NEL.fromList
                    -- main file
                    [ "True"
                    , "[]"
                    , -- other modules
                      "test-package"
                    , "True"
                    , "[\"test/Main.hs\", \"test/Foo.hs\", \"test/Bar.hs\"]"
                    , "module Foo where"
                    , "module Bar where"
                    , -- other extensions
                      "True"
                    , "[\"test/Foo.hs\", \"test/Bar.hs\"]"
                    , "\"{-# LANGUAGE OverloadedStrings, LambdaCase #-}\n{-# LANGUAGE RankNTypes #-}\""
                    , "\"{-# LANGUAGE RecordWildCards #-}\""
                    , -- dependencies
                      "True"
                    , "[\"test/Main.hs\"]"
                    , "True"
                    , "test-package"
                    , "module Main where"
                    , "import Test.Tasty\nimport Test.Tasty.HUnit"
                    , "{-# LANGUAGE OverloadedStrings, LambdaCase #-}"
                    , -- build tools
                      "True"
                    , "[\"test/Main.hs\", \"test/Foo.hs\", \"test/bar.y\"]"
                    ]
                flags = emptyFlags{initializeTestSuite = Flag True}

            case (_runPrompt $ genTestTarget flags comp pkgIx defaultCabalVersion) inputs of
              Left e -> assertFailure $ show e
              Right{} -> return ()
        ]
    ]

nonInteractiveTests
  :: InstalledPackageIndex
  -> SourcePackageDb
  -> Compiler
  -> TestTree
nonInteractiveTests pkgIx srcDb comp =
  testGroup
    "Check top level getter functions"
    [ testGroup
        "Simple heuristics tests"
        [ testGroup
            "Check packageNameHeuristics output"
            [ testSimple
                "New package name"
                (packageNameHeuristics srcDb)
                (mkPackageName "test-package")
                [ "test-package"
                , "test-package"
                ]
            , testSimple
                "Existing package name"
                (packageNameHeuristics srcDb)
                (mkPackageName "cabal-install")
                [ "test-package"
                , "cabal-install"
                ]
            ]
        , testSimple
            "Check authorHeuristics output"
            authorHeuristics
            "Foobar"
            [ ""
            , "Foobar"
            ]
        , testSimple
            "Check emailHeuristics output"
            emailHeuristics
            "foobar@qux.com"
            [ ""
            , "foobar@qux.com"
            ]
        , testSimple
            "Check srcDirsHeuristics output"
            srcDirsHeuristics
            ["src"]
            [ "src"
            , "True"
            ]
        , testSimple
            "Check appDirsHeuristics output"
            appDirsHeuristics
            ["app"]
            [ "test-package"
            , "[\"test-package/app/\"]"
            ]
        , testGroup
            "Check packageTypeHeuristics output"
            [ testSimple
                "Library"
                packageTypeHeuristics
                Library
                [ "test-package"
                , "[\".\", \"..\", \"test/Main.hs\", \"src/\"]"
                , "[\".\", \"..\", \"test/Main.hs\", \"src/\"]"
                ]
            , testSimple
                "Executable"
                packageTypeHeuristics
                Executable
                [ "test-package"
                , "[\".\", \"..\", \"app/Main.hs\"]"
                , "[\".\", \"..\", \"app/Main.hs\"]"
                ]
            , testSimple
                "Library and Executable"
                packageTypeHeuristics
                LibraryAndExecutable
                [ "test-package"
                , "[\".\", \"..\", \"src/\", \"app/Main.hs\"]"
                , "[\".\", \"..\", \"src/\", \"app/Main.hs\"]"
                ]
            , testSimple
                "TestSuite"
                packageTypeHeuristics
                TestSuite
                [ "test-package"
                , "[\".\", \"..\", \"test/Main.hs\"]"
                , "[\".\", \"..\", \"test/Main.hs\"]"
                ]
            ]
        , testGroup
            "Check cabalVersionHeuristics output"
            [ testSimple
                "Broken command"
                cabalVersionHeuristics
                defaultCabalVersion
                [""]
            , testSimple
                "Proper answer"
                cabalVersionHeuristics
                CabalSpecV2_4
                ["cabal-install version 2.4.0.0\ncompiled using version 2.4.0.0 of the Cabal library \n"]
            ]
        , testGroup
            "Check languageHeuristics output"
            [ testSimple
                "Non GHC compiler"
                (`languageHeuristics` (comp{compilerId = CompilerId Helium $ mkVersion [1, 8, 1]}))
                Haskell2010
                []
            , testSimple
                "Higher version compiler"
                (`languageHeuristics` (comp{compilerId = CompilerId GHC $ mkVersion [8, 10, 4]}))
                Haskell2010
                []
            , testSimple
                "Lower version compiler"
                (`languageHeuristics` (comp{compilerId = CompilerId GHC $ mkVersion [6, 0, 1]}))
                Haskell98
                []
            ]
        , testGroup
            "Check extraDocFileHeuristics output"
            [ testSimple
                "No extra sources"
                extraDocFileHeuristics
                (pure (Set.singleton "CHANGELOG.md"))
                [ "test-package"
                , "[]"
                ]
            , testSimple
                "Extra doc files present"
                extraDocFileHeuristics
                (pure $ Set.singleton "README.md")
                [ "test-package"
                , "[\"README.md\"]"
                ]
            ]
        , testGroup
            "Check mainFileHeuristics output"
            [ testSimple
                "No main file defined"
                mainFileHeuristics
                (toHsFilePath "Main.hs")
                [ "test-package"
                , "[\"test-package/app/\"]"
                , "True"
                , "[]"
                ]
            , testSimple
                "Main file already defined"
                mainFileHeuristics
                (toHsFilePath "app/Main.hs")
                [ "test-package"
                , "[\"test-package/app/\"]"
                , "True"
                , "[\"app/Main.hs\"]"
                ]
            , testSimple
                "Main lhs file already defined"
                mainFileHeuristics
                (toHsFilePath "app/Main.lhs")
                [ "test-package"
                , "[\"test-package/app/\"]"
                , "True"
                , "[\"app/Main.lhs\"]"
                ]
            ]
        , testGroup
            "Check exposedModulesHeuristics output"
            [ testSimple
                "Default exposed modules"
                exposedModulesHeuristics
                (myLibModule NEL.:| [])
                [ "src"
                , "True"
                , "True"
                , "[]"
                , "test-package"
                , "True"
                , "[]"
                ]
            , testSimple
                "Contains exposed modules"
                exposedModulesHeuristics
                (NEL.fromList $ map fromString ["Foo", "Bar"])
                [ "src"
                , "True"
                , "True"
                , "[\"src/Foo.hs\", \"src/Bar.hs\"]"
                , "module Foo where"
                , "module Bar where"
                , "test-package"
                , "True"
                , "[\"src/Foo.hs\", \"src/Bar.hs\"]"
                , "module Foo where"
                , "module Bar where"
                ]
            ]
        , testGroup
            "Check libOtherModulesHeuristics output"
            [ testSimple
                "Library directory exists"
                libOtherModulesHeuristics
                (map fromString ["Baz.Internal"])
                [ "test-package"
                , "True"
                , "[\"src/Foo.hs\", \"src/Bar.hs\", \"src/Baz/Internal.hs\"]"
                , "module Foo where"
                , "module Bar where"
                , "module Baz.Internal where"
                ]
            , testSimple
                "Library directory doesn't exist"
                libOtherModulesHeuristics
                []
                [ "test-package"
                , "False"
                ]
            ]
        , testGroup
            "Check exeOtherModulesHeuristics output"
            [ testSimple
                "Executable directory exists"
                exeOtherModulesHeuristics
                (map fromString ["Foo", "Bar"])
                [ "test-package"
                , "True"
                , "[\"app/Main.hs\", \"app/Foo.hs\", \"app/Bar.hs\"]"
                , "module Foo where"
                , "module Bar where"
                ]
            , testSimple
                "Executable directory doesn't exist"
                exeOtherModulesHeuristics
                []
                [ "test-package"
                , "False"
                ]
            ]
        , testGroup
            "Check testOtherModulesHeuristics output"
            [ testSimple
                "Test directory exists"
                testOtherModulesHeuristics
                (map fromString ["Foo", "Bar"])
                [ "test-package"
                , "True"
                , "[\"test/Main.hs\", \"test/Foo.hs\", \"test/Bar.hs\"]"
                , "module Foo where"
                , "module Bar where"
                ]
            , testSimple
                "Test directory doesn't exist"
                testOtherModulesHeuristics
                []
                [ "test-package"
                , "False"
                ]
            ]
        , testGroup
            "Check dependenciesHeuristics output"
            [ testSimple
                "base version bounds is correct"
                ( fmap
                    ( flip foldl' anyVersion $ \a (Dependency n v _) ->
                        if unPackageName n == "base" && baseVersion comp /= anyVersion
                          then v
                          else a
                    )
                    . (\x -> dependenciesHeuristics x "" pkgIx)
                )
                (baseVersion comp)
                [ "True"
                , "[]"
                ]
            ]
        , testSimple
            "Check buildToolsHeuristics output"
            (\a -> buildToolsHeuristics a "" defaultCabalVersion)
            [mkStringyDep "happy:happy"]
            [ "True"
            , "[\"app/Main.hs\", \"src/Foo.hs\", \"src/bar.y\"]"
            ]
        , testSimple
            "Check otherExtsHeuristics output"
            (`otherExtsHeuristics` "")
            (map EnableExtension [OverloadedStrings, LambdaCase, RankNTypes, RecordWildCards])
            [ "True"
            , "[\"src/Foo.hs\", \"src/Bar.hs\"]"
            , "\"{-# LANGUAGE OverloadedStrings, LambdaCase #-}\n{-# LANGUAGE RankNTypes #-}\""
            , "\"{-# LANGUAGE RecordWildCards #-}\""
            ]
        , testSimple "Check versionHeuristics output" versionHeuristics (mkVersion [0, 1, 0, 0]) [""]
        , testSimple "Check homepageHeuristics output" homepageHeuristics "" [""]
        , testSimple "Check synopsisHeuristics output" synopsisHeuristics "" [""]
        , testSimple "Check testDirsHeuristics output" testDirsHeuristics ["test"] [""]
        , testSimple "Check categoryHeuristics output" categoryHeuristics "" [""]
        , testSimple "Check minimalHeuristics output" minimalHeuristics False [""]
        , testSimple "Check overwriteHeuristics output" overwriteHeuristics False [""]
        , testSimple "Check initializeTestSuiteHeuristics output" initializeTestSuiteHeuristics False [""]
        , testSimple "Check licenseHeuristics output" licenseHeuristics (SpecLicense $ Left SPDX.NONE) [""]
        ]
    , testGroup
        "Bool heuristics tests"
        [ testBool "Check noCommentsHeuristics output" noCommentsHeuristics False ""
        ]
    ]

testSimple
  :: Eq a
  => Show a
  => String
  -> (InitFlags -> PurePrompt a)
  -> a
  -> [String]
  -> TestTree
testSimple label f target =
  testGo label f (assertFailure . show) (\(a, _) -> target @=? a)

testBool
  :: String
  -> (InitFlags -> PurePrompt Bool)
  -> Bool
  -> String
  -> TestTree
testBool label f target input =
  testSimple label f target [input]

testGo
  :: Eq a
  => Show a
  => String
  -> (InitFlags -> PurePrompt a)
  -> (BreakException -> Assertion)
  -> ((a, NEL.NonEmpty String) -> Assertion)
  -> [String]
  -> TestTree
testGo label f g h inputs = testCase label $
  case (_runPrompt $ f emptyFlags) (NEL.fromList inputs) of
    Left x -> g x
    Right x -> h x

cliListParserTests :: TestTree
cliListParserTests =
  testGroup
    "cli list parser"
    [ testCase "Single extraSrc" $ do
        flags <- runParserTest ["-x", "Generated.hs"]
        flags
          @?= emptyFlags
            { extraSrc = Flag ["Generated.hs"]
            }
    , testCase "Multiple extraSrc" $ do
        flags <- runParserTest ["-x", "Gen1.hs", "-x", "Gen2.hs", "-x", "Gen3.hs"]
        flags
          @?= emptyFlags
            { extraSrc = Flag ["Gen1.hs", "Gen2.hs", "Gen3.hs"]
            }
    , testCase "Single extraDoc" $ do
        flags <- runParserTest ["--extra-doc-file", "README"]
        flags
          @?= emptyFlags
            { extraDoc = Flag $ ["README"]
            }
    , testCase "Multiple extraDoc" $ do
        flags <-
          runParserTest
            [ "--extra-doc-file"
            , "README"
            , "--extra-doc-file"
            , "CHANGELOG"
            , "--extra-doc-file"
            , "LICENSE"
            ]
        flags
          @?= emptyFlags
            { extraDoc = Flag $ map fromString ["README", "CHANGELOG", "LICENSE"]
            }
    , testCase "Single exposedModules" $ do
        flags <- runParserTest ["-o", "Test"]
        flags
          @?= emptyFlags
            { exposedModules = Flag $ map fromString ["Test"]
            }
    , testCase "Multiple exposedModules" $ do
        flags <- runParserTest ["-o", "Test", "-o", "Test2", "-o", "Test3"]
        flags
          @?= emptyFlags
            { exposedModules = Flag $ map fromString ["Test", "Test2", "Test3"]
            }
    , -- there is no otherModules cli flag
      -- , testCase "Single otherModules" $ do
      --     flags <- runParserTest ["-o", "Test"]
      --     flags @?= dummyFlags
      --       { otherModules = Flag $ map fromString ["Test"]
      --       }
      -- , testCase "Multiple otherModules" $ do
      --     flags <- runParserTest ["-o", "Test", "-o", "Test2", "-o", "Test3"]
      --     flags @?= dummyFlags
      --       { otherModules = Flag $ map fromString ["Test", "Test2", "Test3"]
      --       }
      testCase "Single otherExts" $ do
        flags <- runParserTest ["--extension", "OverloadedStrings"]
        flags
          @?= emptyFlags
            { otherExts = Flag [EnableExtension OverloadedStrings]
            }
    , testCase "Multiple otherExts" $ do
        flags <-
          runParserTest
            [ "--extension"
            , "OverloadedStrings"
            , "--extension"
            , "FlexibleInstances"
            , "--extension"
            , "FlexibleContexts"
            ]
        flags
          @?= emptyFlags
            { otherExts =
                Flag
                  [ EnableExtension OverloadedStrings
                  , EnableExtension FlexibleInstances
                  , EnableExtension FlexibleContexts
                  ]
            }
    , testCase "Single dependency" $ do
        flags <- runParserTest ["-d", "base"]
        flags
          @?= emptyFlags
            { dependencies = Flag [mkStringyDep "base"]
            }
    , testCase "Multiple dependency flags" $ do
        flags <- runParserTest ["-d", "base", "-d", "vector"]
        flags
          @?= emptyFlags
            { dependencies = Flag $ fmap mkStringyDep ["base", "vector"]
            }
    , testCase "Comma separated list of dependencies" $ do
        flags <- runParserTest ["-d", "base,vector"]
        flags
          @?= emptyFlags
            { dependencies = Flag $ fmap mkStringyDep ["base", "vector"]
            }
    , testCase "Single applicationDirs" $ do
        flags <- runParserTest ["--application-dir", "app"]
        flags
          @?= emptyFlags
            { applicationDirs = Flag ["app"]
            }
    , testCase "Multiple applicationDirs" $ do
        flags <-
          runParserTest
            [ "--application-dir"
            , "app"
            , "--application-dir"
            , "exe"
            , "--application-dir"
            , "srcapp"
            ]
        flags
          @?= emptyFlags
            { applicationDirs = Flag ["app", "exe", "srcapp"]
            }
    , testCase "Single sourceDirs" $ do
        flags <- runParserTest ["--source-dir", "src"]
        flags
          @?= emptyFlags
            { sourceDirs = Flag ["src"]
            }
    , testCase "Multiple sourceDirs" $ do
        flags <-
          runParserTest
            [ "--source-dir"
            , "src"
            , "--source-dir"
            , "lib"
            , "--source-dir"
            , "sources"
            ]
        flags
          @?= emptyFlags
            { sourceDirs = Flag ["src", "lib", "sources"]
            }
    , testCase "Single buildTools" $ do
        flags <- runParserTest ["--build-tool", "happy"]
        flags
          @?= emptyFlags
            { buildTools = Flag ["happy"]
            }
    , testCase "Multiple buildTools" $ do
        flags <-
          runParserTest
            [ "--build-tool"
            , "happy"
            , "--build-tool"
            , "alex"
            , "--build-tool"
            , "make"
            ]
        flags
          @?= emptyFlags
            { buildTools = Flag ["happy", "alex", "make"]
            }
    , testCase "Single testDirs" $ do
        flags <- runParserTest ["--test-dir", "test"]
        flags
          @?= emptyFlags
            { testDirs = Flag ["test"]
            }
    , testCase "Multiple testDirs" $ do
        flags <-
          runParserTest
            [ "--test-dir"
            , "test"
            , "--test-dir"
            , "tests"
            , "--test-dir"
            , "testsuite"
            ]
        flags
          @?= emptyFlags
            { testDirs = Flag ["test", "tests", "testsuite"]
            }
    ]
  where
    assumeAllParse :: CommandParse (InitFlags -> InitFlags, [String]) -> IO InitFlags
    assumeAllParse = \case
      CommandReadyToGo (flagsF, []) -> pure (flagsF emptyFlags)
      _ -> assertFailure "Expected successful parse"

    runParserTest :: [String] -> IO InitFlags
    runParserTest opts = do
      assumeAllParse $ commandParseArgs initCommand False opts
