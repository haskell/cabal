{-# LANGUAGE PatternSynonyms #-}

module UnitTests.Distribution.Client.UserConfig
  ( tests
  ) where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (IOException, bracket, try)
import Control.Monad (replicateM, replicateM_)
import Data.List (nub, sort)
import System.Directory
  ( doesFileExist
  , getCurrentDirectory
  , getTemporaryDirectory
  )
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)

import Test.Tasty
import Test.Tasty.HUnit

import Distribution.Client.Config
import Distribution.Client.Setup (GlobalFlags (..), InstallFlags (..))
import Distribution.Simple.Setup (ConfigFlags (..), fromFlag, pattern Flag)
import Distribution.Simple.Utils (removeFileForcibly, withTempDirectory)
import Distribution.Utils.NubList (fromNubList)
import Distribution.Verbosity

tests :: [TestTree]
tests =
  [ testCase "nullDiffOnCreate" nullDiffOnCreateTest
  , testCase "canDetectDifference" canDetectDifference
  , testCase "canUpdateConfig" canUpdateConfig
  , testCase "doubleUpdateConfig" doubleUpdateConfig
  , testCase "concurrentUpdateConfig" concurrentUpdateConfig
  , testCase "newDefaultConfig" newDefaultConfig
  ]

nullDiffOnCreateTest :: Assertion
nullDiffOnCreateTest = bracketTest $ \configFile -> do
  -- Create a new default config file in our test directory.
  _ <- createDefaultConfigFile (mkVerbosity defaultVerbosityHandles silent) [] configFile
  -- Now we read it in and compare it against the default.
  diff <- userConfigDiff (mkVerbosity defaultVerbosityHandles silent) (globalFlags configFile) []
  assertBool (unlines $ "Following diff should be empty:" : diff) $ null diff

canDetectDifference :: Assertion
canDetectDifference = bracketTest $ \configFile -> do
  -- Create a new default config file in our test directory.
  _ <- createDefaultConfigFile (mkVerbosity defaultVerbosityHandles silent) [] configFile
  appendFile configFile "verbose: 0\n"
  diff <- userConfigDiff (mkVerbosity defaultVerbosityHandles silent) (globalFlags configFile) []
  assertBool (unlines $ "Should detect a difference:" : diff) $
    diff == ["+ verbose: 0"]

canUpdateConfig :: Assertion
canUpdateConfig = bracketTest $ \configFile -> do
  -- Write a trivial cabal file.
  writeFile configFile "tests: True\n"
  -- Update the config file.
  userConfigUpdate (mkVerbosity defaultVerbosityHandles silent) (globalFlags configFile) []
  -- Load it again.
  updated <- loadConfig (mkVerbosity defaultVerbosityHandles silent) (Flag configFile)
  assertBool ("Field 'tests' should be True") $
    fromFlag (configTests $ savedConfigureFlags updated)

doubleUpdateConfig :: Assertion
doubleUpdateConfig = bracketTest $ \configFile -> do
  -- Create a new default config file in our test directory.
  _ <- createDefaultConfigFile (mkVerbosity defaultVerbosityHandles silent) [] configFile
  -- Update it twice.
  replicateM_ 2 $ userConfigUpdate (mkVerbosity defaultVerbosityHandles silent) (globalFlags configFile) []
  -- Load it again.
  updated <- loadConfig (mkVerbosity defaultVerbosityHandles silent) (Flag configFile)

  assertBool ("Field 'remote-repo' doesn't contain duplicates") $
    listUnique (map show . fromNubList . globalRemoteRepos $ savedGlobalFlags updated)
  assertBool ("Field 'extra-prog-path' doesn't contain duplicates") $
    listUnique (map show . fromNubList . configProgramPathExtra $ savedConfigureFlags updated)
  assertBool ("Field 'build-summary' doesn't contain duplicates") $
    listUnique (map show . fromNubList . installSummaryFile $ savedInstallFlags updated)

newDefaultConfig :: Assertion
newDefaultConfig = do
  sysTmpDir <- getTemporaryDirectory
  withTempDirectory sysTmpDir "cabal-test" $ \tmpDir -> do
    let configFile = tmpDir </> "tmp.config"
    _ <- createDefaultConfigFile (mkVerbosity defaultVerbosityHandles silent) [] configFile
    exists <- doesFileExist configFile
    assertBool ("Config file should be written to " ++ configFile) exists

concurrentUpdateConfig :: Assertion
concurrentUpdateConfig = bracketTest $ \configFile -> do
  _ <- createDefaultConfigFile (mkVerbosity defaultVerbosityHandles silent) [] configFile

  doneVars <- replicateM numConcurrentUpdates newEmptyMVar
  mapM_
    ( \doneVar ->
        forkIO $ do
          result <- try (userConfigUpdate (mkVerbosity defaultVerbosityHandles silent) (globalFlags configFile) []) :: IO (Either IOException ())
          putMVar doneVar result
    )
    doneVars

  results <- mapM takeMVar doneVars
  assertBool "Concurrent userConfigUpdate should not throw" (all isSuccess results)
  where
    numConcurrentUpdates = 16 :: Int
    isSuccess (Right ()) = True
    isSuccess _ = False

globalFlags :: FilePath -> GlobalFlags
globalFlags configFile = mempty{globalConfigFile = Flag configFile}

listUnique :: Ord a => [a] -> Bool
listUnique xs =
  let sorted = sort xs
   in nub sorted == xs

bracketTest :: (FilePath -> IO ()) -> Assertion
bracketTest =
  bracket testSetup testTearDown
  where
    testSetup :: IO FilePath
    testSetup = do
      cwd <- getCurrentDirectory
      (configFile, h) <- openTempFile cwd "test-user-config"
      hClose h
      pure configFile

    testTearDown :: FilePath -> IO ()
    testTearDown configFile =
      mapM_ removeFileForcibly [configFile, configFile ++ ".backup"]
