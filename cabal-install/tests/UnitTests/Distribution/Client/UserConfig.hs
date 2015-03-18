{-# LANGUAGE CPP #-}
module UnitTests.Distribution.Client.UserConfig
    ( tests
    ) where

import Control.Exception (bracket)
import Data.List (sort, nub)
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid
#endif
import System.Directory (getCurrentDirectory, removeDirectoryRecursive, createDirectoryIfMissing)
import System.FilePath (takeDirectory)

import Test.Framework as TF (Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assertBool)

import Distribution.Client.Compat.Environment (lookupEnv, setEnv)
import Distribution.Client.Config
import Distribution.Utils.NubList (fromNubList)
import Distribution.Client.Setup (GlobalFlags (..), InstallFlags (..))
import Distribution.Simple.Setup (ConfigFlags (..), fromFlag)
import Distribution.Verbosity (silent)

tests :: [TF.Test]
tests = [ testCase "nullDiffOnCreate" nullDiffOnCreateTest
        , testCase "canDetectDifference" canDetectDifference
        , testCase "canUpdateConfig" canUpdateConfig
        , testCase "doubleUpdateConfig" doubleUpdateConfig
        ]

nullDiffOnCreateTest :: Assertion
nullDiffOnCreateTest = bracketTest . const $ do
    -- Create a new default config file in our test directory.
    _ <- loadConfig silent mempty mempty
    -- Now we read it in and compare it against the default.
    diff <- userConfigDiff mempty
    assertBool (unlines $ "Following diff should be empty:" : diff) $ null diff


canDetectDifference :: Assertion
canDetectDifference = bracketTest . const $ do
    -- Create a new default config file in our test directory.
    _ <- loadConfig silent mempty mempty
    cabalFile <- defaultConfigFile
    appendFile cabalFile "verbose: 0\n"
    diff <- userConfigDiff mempty
    assertBool (unlines $ "Should detect a difference:" : diff) $
        diff == [ "- verbose: 1", "+ verbose: 0" ]


canUpdateConfig :: Assertion
canUpdateConfig = bracketTest . const $ do
    cabalFile <- defaultConfigFile
    createDirectoryIfMissing True $ takeDirectory cabalFile
    -- Write a trivial cabal file.
    writeFile cabalFile "tests: True\n"
    -- Update the config file.
    userConfigUpdate silent mempty
    -- Load it again.
    updated <- loadConfig silent mempty mempty
    assertBool ("Field 'tests' should be True") $
        fromFlag (configTests $ savedConfigureFlags updated)


doubleUpdateConfig :: Assertion
doubleUpdateConfig = bracketTest . const $ do
    -- Create a new default config file in our test directory.
    _ <- loadConfig silent mempty mempty
    -- Update it.
    userConfigUpdate silent mempty
    userConfigUpdate silent mempty
    -- Load it again.
    updated <- loadConfig silent mempty mempty

    assertBool ("Field 'remote-repo' doesn't contain duplicates") $
        listUnique (map show . fromNubList . globalRemoteRepos $ savedGlobalFlags updated)
    assertBool ("Field 'extra-prog-path' doesn't contain duplicates") $
        listUnique (map show . fromNubList . configProgramPathExtra $ savedConfigureFlags updated)
    assertBool ("Field 'build-summary' doesn't contain duplicates") $
        listUnique (map show . fromNubList . installSummaryFile $ savedInstallFlags updated)


listUnique :: Ord a => [a] -> Bool
listUnique xs =
    let sorted = sort xs
    in nub sorted == xs


bracketTest :: ((FilePath, FilePath) -> IO ()) -> Assertion
bracketTest =
    bracket testSetup testTearDown
  where
    testSetup :: IO (FilePath, FilePath)
    testSetup = do
        Just oldHome <- lookupEnv "HOME"
        testdir <- fmap (++ "/test-user-config") getCurrentDirectory
        setEnv "HOME" testdir
        return (oldHome, testdir)

    testTearDown :: (FilePath, FilePath) -> IO ()
    testTearDown (oldHome, testdir) = do
        setEnv "HOME" oldHome
        removeDirectoryRecursive testdir
