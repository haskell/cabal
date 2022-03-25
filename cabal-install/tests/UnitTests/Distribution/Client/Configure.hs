{-# LANGUAGE RecordWildCards #-}
module UnitTests.Distribution.Client.Configure (tests) where

import Distribution.Client.CmdConfigure

import Test.Tasty
import Test.Tasty.HUnit
import Control.Monad
import qualified Data.Map as Map
import System.Directory
import System.FilePath
import Distribution.Verbosity
import Distribution.Client.Setup
import Distribution.Client.NixStyleOptions
import Distribution.Client.ProjectConfig.Types
import Distribution.Client.ProjectFlags
import Distribution.Simple
import Distribution.Simple.Flag

tests :: [TestTree]
tests = 
    [ configureTests
    ]

configureTests :: TestTree
configureTests = testGroup "Configure tests"
    [ testCase "New config" $ do
        let flags = (defaultNixStyleFlags ()) 
              { configFlags = mempty
                  { configOptimization = Flag MaximumOptimisation
                  , configVerbosity = Flag silent
                  }
              }
        projConfig <- configureAction' flags [] defaultGlobalFlags
        
        Flag MaximumOptimisation @=?
          (packageConfigOptimization . projectConfigLocalPackages $ snd projConfig)

    , testCase "Replacement + new config" $ do
        let flags = (defaultNixStyleFlags ()) 
              { configExFlags = mempty
                  { configAppend = Flag True }
              , configFlags = mempty
                  { configOptimization = Flag NoOptimisation
                  , configVerbosity = Flag silent
                  }
              , projectFlags = mempty
                  { flagProjectFileName = Flag projectFile }
              }
        (_, ProjectConfig {..}) <- configureAction' flags [] defaultGlobalFlags

        Flag NoOptimisation @=? packageConfigOptimization projectConfigLocalPackages
        Flag silent         @=? projectConfigVerbosity projectConfigBuildOnly
    
    , testCase "Old + new config" $ do
        let flags = (defaultNixStyleFlags ()) 
              { configExFlags = mempty
                  { configAppend = Flag True }
              , configFlags = mempty
                  { configVerbosity = Flag silent }
              , projectFlags = mempty
                  { flagProjectFileName = Flag projectFile }
              }
        (_, ProjectConfig {..}) <- configureAction' flags [] defaultGlobalFlags

        Flag MaximumOptimisation @=? packageConfigOptimization projectConfigLocalPackages
        Flag silent              @=? projectConfigVerbosity projectConfigBuildOnly
    
    , testCase "Old + new config, no appending" $ do
        let flags = (defaultNixStyleFlags ()) 
              { configFlags = mempty
                  { configVerbosity = Flag silent }
              , projectFlags = mempty
                  { flagProjectFileName = Flag projectFile }
              }
        (_, ProjectConfig {..}) <- configureAction' flags [] defaultGlobalFlags

        NoFlag      @=? packageConfigOptimization projectConfigLocalPackages
        Flag silent @=? projectConfigVerbosity projectConfigBuildOnly
    
    , testCase "Old + new config, backup check" $ do
        let flags = (defaultNixStyleFlags ()) 
              { configFlags = mempty
                  { configVerbosity = Flag silent }
              , projectFlags = mempty
                  { flagProjectFileName = Flag projectFile }
              }
            backup = projectFile <.> "local~"

        exists <- doesFileExist backup
        when exists $ 
          removeFile backup

        _ <- configureAction' flags [] defaultGlobalFlags

        doesFileExist backup >>=
          assertBool ("No file found, expected: " ++ backup)

    , testCase "Local program options" $ do
        let ghcFlags = ["-fno-full-laziness"]
            flags = (defaultNixStyleFlags ())
              { configFlags = mempty
                  { configVerbosity = Flag silent
                  , configProgramArgs = [("ghc", ghcFlags)]
                  }
              , projectFlags = mempty
                  { flagProjectFileName = Flag projectFile }
              }
        (_, ProjectConfig {..}) <- configureAction' flags [] defaultGlobalFlags


        assertEqual "global"
                    Nothing
                    (Map.lookup "ghc" (getMapMappend (packageConfigProgramArgs projectConfigAllPackages)))

        assertEqual "local"
                    (Just ghcFlags)
                    (Map.lookup "ghc" (getMapMappend (packageConfigProgramArgs projectConfigLocalPackages)))
    ]

projectFile :: FilePath
projectFile = "tests" </> "fixtures" </> "configure" </> "cabal.project"
