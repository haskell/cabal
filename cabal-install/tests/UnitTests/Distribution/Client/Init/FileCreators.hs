{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module UnitTests.Distribution.Client.Init.FileCreators
  ( tests
  ) where

import Test.Tasty
import Test.Tasty.HUnit

import UnitTests.Distribution.Client.Init.Utils

import Distribution.Client.Init.FileCreators
import Distribution.Client.Init.NonInteractive.Command
import Distribution.Client.Init.Types
import Distribution.Client.Types
import Distribution.Simple
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
    "Distribution.Client.Init.FileCreators"
    [ testCase "Check . as source directory" $ do
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
                , sourceDirs = Flag ["."]
                }
            inputs =
              -- createProject stuff
              [ "Foobar"
              , "foobar@qux.com"
              , "True"
              , "[\"quxTest/Main.hs\"]"
              , -- writeProject stuff
                -- writeLicense
                "2021"
              , -- writeFileSafe
                "True"
              , -- findNewPath
                "False"
              , -- writeChangeLog
                -- writeFileSafe
                "False"
              , -- prepareLibTarget
                -- writeDirectoriesSafe
                "True"
              , -- findNewPath
                "False"
              , -- prepareExeTarget
                -- writeDirectoriesSafe
                "False"
              , -- writeFileSafe
                "False"
              , -- prepareTestTarget
                -- writeDirectoriesSafe
                "False"
              , -- writeFileSafe
                "False"
              , -- writeCabalFile
                -- writeFileSafe
                "False"
              ]

        case flip _runPrompt inputs $ do
          projSettings <- createProject comp silent pkgIx srcDb dummyFlags'
          writeProject projSettings of
          Left (BreakException ex) -> assertFailure $ show ex
          Right _ -> return ()
    ]
