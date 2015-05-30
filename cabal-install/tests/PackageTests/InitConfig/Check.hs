{-# LANGUAGE ScopedTypeVariables #-}

module PackageTests.InitConfig.Check
       ( tests
       ) where

import PackageTests.PackageTester

import Test.Tasty
import Test.Tasty.HUnit

import qualified Control.Exception.Extensible as E
import System.Directory (removeFile)
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError)

dir :: FilePath
dir = packageTestsDirectory 

tests :: TestsPaths -> [TestTree]
tests paths =
    [ testCase "runs without error" $ do
          removeCabalConfig
          result <- cabal_initconfig paths dir []
          assertInitConfigSucceeded result

    , testCase "doesn't overwrite without -f" $ do
          removeCabalConfig
          _      <- cabal_initconfig paths dir []
          result <- cabal_initconfig paths dir []
          assertInitConfigFailed result

    , testCase "overwrites with -f" $ do
          removeCabalConfig
          _      <- cabal_initconfig paths dir []
          result <- cabal_initconfig paths dir ["-f"]
          assertInitConfigSucceeded result
    ]

removeCabalConfig :: IO ()
removeCabalConfig = do
    removeFile (dir </> "cabal-config")
    `E.catch` \ (e :: IOError) ->
        if isDoesNotExistError e
        then return ()
        else E.throw e
