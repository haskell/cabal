{-# LANGUAGE RecordWildCards #-}

module UnitTests.Distribution.Client.Configure (tests) where

import Distribution.Client.CmdConfigure

import Control.Monad
import qualified Data.Map as Map
import Distribution.Client.NixStyleOptions
import Distribution.Client.ProjectConfig.Types
import Distribution.Client.ProjectFlags
import Distribution.Client.Setup
import Distribution.Simple
import Distribution.Simple.Flag
import Distribution.Verbosity
import System.Directory
import System.FilePath
import Test.Tasty
import Test.Tasty.HUnit

tests :: [TestTree]
tests =
  [ configureTests
  ]

configureTests :: TestTree
configureTests =
  testGroup
    "Configure tests"
    [ testCase "New config" $ do
        let flags =
              (defaultNixStyleFlags ())
                { configFlags =
                    mempty
                      { configOptimization = Flag MaximumOptimisation
                      , configVerbosity = Flag silent
                      }
                }
        projConfig <- configureAction' flags [] defaultGlobalFlags

        Flag MaximumOptimisation
          @=? (packageConfigOptimization . projectConfigLocalPackages $ snd projConfig)
    , testCase "Replacement + new config" $ do
        let flags =
              (defaultNixStyleFlags ())
                { configExFlags =
                    mempty
                      { configAppend = Flag True
                      }
                , configFlags =
                    mempty
                      { configOptimization = Flag NoOptimisation
                      , configVerbosity = Flag silent
                      }
                , projectFlags =
                    mempty
                      { flagProjectDir = Flag projectDir
                      }
                }
        (_, ProjectConfig{..}) <- configureAction' flags [] defaultGlobalFlags

        Flag NoOptimisation @=? packageConfigOptimization projectConfigLocalPackages
        Flag silent @=? projectConfigVerbosity projectConfigBuildOnly
    , testCase "Old + new config" $ do
        let flags =
              (defaultNixStyleFlags ())
                { configExFlags =
                    mempty
                      { configAppend = Flag True
                      }
                , configFlags =
                    mempty
                      { configVerbosity = Flag silent
                      }
                , projectFlags =
                    mempty
                      { flagProjectDir = Flag projectDir
                      }
                }
        (_, ProjectConfig{..}) <- configureAction' flags [] defaultGlobalFlags

        Flag MaximumOptimisation @=? packageConfigOptimization projectConfigLocalPackages
        Flag silent @=? projectConfigVerbosity projectConfigBuildOnly
    , testCase "Old + new config, no appending" $ do
        let flags =
              (defaultNixStyleFlags ())
                { configFlags =
                    mempty
                      { configVerbosity = Flag silent
                      }
                , projectFlags =
                    mempty
                      { flagProjectDir = Flag projectDir
                      }
                }
        (_, ProjectConfig{..}) <- configureAction' flags [] defaultGlobalFlags

        NoFlag @=? packageConfigOptimization projectConfigLocalPackages
        Flag silent @=? projectConfigVerbosity projectConfigBuildOnly
    , testCase "Old + new config, backup check" $ do
        let flags =
              (defaultNixStyleFlags ())
                { configFlags =
                    mempty
                      { configVerbosity = Flag silent
                      }
                , projectFlags =
                    mempty
                      { flagProjectDir = Flag projectDir
                      }
                }
            backup = projectDir </> "cabal.project.local~"

        exists <- doesFileExist backup
        when exists $
          removeFile backup

        _ <- configureAction' flags [] defaultGlobalFlags

        doesFileExist backup
          >>= assertBool ("No file found, expected: " ++ backup)
    , testCase "Local program options" $ do
        let ghcFlags = ["-fno-full-laziness"]
            flags =
              (defaultNixStyleFlags ())
                { configFlags =
                    mempty
                      { configVerbosity = Flag silent
                      , configProgramArgs = [("ghc", ghcFlags)]
                      }
                , projectFlags =
                    mempty
                      { flagProjectDir = Flag projectDir
                      }
                }
        (_, ProjectConfig{..}) <- configureAction' flags [] defaultGlobalFlags

        assertEqual
          "global"
          Nothing
          (Map.lookup "ghc" (getMapMappend (packageConfigProgramArgs projectConfigAllPackages)))

        assertEqual
          "local"
          (Just ghcFlags)
          (Map.lookup "ghc" (getMapMappend (packageConfigProgramArgs projectConfigLocalPackages)))
    ]

projectDir :: FilePath
projectDir = "tests" </> "fixtures" </> "configure"
