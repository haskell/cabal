{-# LANGUAGE ScopedTypeVariables #-}

module PackageTests.UserConfig.Check
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
          result <- cabal_userconfig paths dir ["init"]
          assertUserConfigSucceeded result

    , testCase "doesn't overwrite without -f" $ do
          removeCabalConfig
          _      <- cabal_userconfig paths dir ["init"]
          result <- cabal_userconfig paths dir ["init"]
          assertUserConfigFailed result

    , testCase "overwrites with -f" $ do
          removeCabalConfig
          _      <- cabal_userconfig paths dir ["init"]
          result <- cabal_userconfig paths dir ["init", "-f"]
          assertUserConfigSucceeded result
    ]

removeCabalConfig :: IO ()
removeCabalConfig = do
    removeFile (dir </> "cabal-config")
    `E.catch` \ (e :: IOError) ->
        if isDoesNotExistError e
        then return ()
        else E.throw e
