{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad (when)
import System.Directory (createDirectory, doesFileExist, getCurrentDirectory, setCurrentDirectory)
import System.Environment (getEnv)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import MyLib (getDataFileName)
import Control.Exception

main :: IO ()
main = do
    -- Print the datadir environment variable
    dataDirEnv <- getEnv "datadir_test_datadir"
    putStrLn $ "datadir_test_datadir: " ++ dataDirEnv

    -- Get path to our test data file
    dataFilePath <- getDataFileName "testdata/sample.txt"
    putStrLn $ "Data file path: " ++ dataFilePath

    -- Check that we can access the file
    fileExists <- doesFileExist dataFilePath
    putStrLn $ "File exists: " ++ show fileExists

    -- Create a subdirectory and change into it
    currentDir <- getCurrentDirectory
    putStrLn $ "Current directory: " ++ currentDir
    createDirectory "subdir" `catch` \(_ :: SomeException) -> pure ()
    setCurrentDirectory "subdir"
    newDir <- getCurrentDirectory
    putStrLn $ "New directory: " ++ newDir

    -- Try to access the data file again after changing directory
    dataFilePathAfterCd <- getDataFileName "testdata/sample.txt"
    putStrLn $ "Data file path after cd: " ++ dataFilePathAfterCd

    fileExistsAfterCd <- doesFileExist dataFilePathAfterCd
    putStrLn $ "File exists after cd: " ++ show fileExistsAfterCd

    -- Exit with error if we can't find the file
    when (not fileExistsAfterCd) $ do
        hPutStrLn stderr "ERROR: Could not find data file after changing directory!"
        hPutStrLn stderr $ "datadir_test_datadir was set to: " ++ dataDirEnv
        exitFailure

    putStrLn "SUCCESS: Data file found correctly even after changing directory!"
    exitSuccess
