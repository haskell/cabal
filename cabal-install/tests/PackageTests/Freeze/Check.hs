{-# LANGUAGE ScopedTypeVariables #-}

module PackageTests.Freeze.Check
       ( tests
       ) where

import PackageTests.PackageTester

import Test.Framework                 as TF (Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit                     (assertBool)

import qualified Control.Exception.Extensible as E
import Data.List (intercalate, isInfixOf)
import System.Directory (doesFileExist, removeFile)
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError)

dir :: FilePath
dir = "PackageTests" </> "Freeze"

tests :: FilePath -> [TF.Test]
tests cabalPath =
    [ testCase "runs without error" $ do
          removeCabalConfig
          result <- cabal_freeze dir [] cabalPath
          assertFreezeSucceeded result

    , testCase "freezes direct dependencies" $ do
          removeCabalConfig
          result <- cabal_freeze dir [] cabalPath
          assertFreezeSucceeded result
          c <- readCabalConfig
          assertBool ("should have frozen base\n" ++ c) $
              " base ==" `isInfixOf` (intercalate " " $ lines $ c)

    , testCase "freezes transitory dependencies" $ do
          removeCabalConfig
          result <- cabal_freeze dir [] cabalPath
          assertFreezeSucceeded result
          c <- readCabalConfig
          assertBool ("should have frozen ghc-prim\n" ++ c) $
              " ghc-prim ==" `isInfixOf` (intercalate " " $ lines $ c)

    , testCase "does not freeze packages which are not dependend upon" $ do
          -- XXX Test this against a package installed in the sandbox but
          -- not depended upon.
          removeCabalConfig
          result <- cabal_freeze dir [] cabalPath
          assertFreezeSucceeded result
          c <- readCabalConfig
          assertBool ("should not have frozen exceptions\n" ++ c) $ not $
              " exceptions ==" `isInfixOf` (intercalate " " $ lines $ c)

    , testCase "--dry-run does not modify the cabal.config file" $ do
          removeCabalConfig
          result <- cabal_freeze dir ["--dry-run"] cabalPath
          assertFreezeSucceeded result
          c <- doesFileExist $ dir </> "cabal.config"
          assertBool "cabal.config file should not have been created" (not c)

    , testCase "--enable-tests freezes test dependencies" $ do
          removeCabalConfig
          result <- cabal_freeze dir ["--enable-tests"] cabalPath
          assertFreezeSucceeded result
          c <- readCabalConfig
          assertBool ("should have frozen test-framework\n" ++ c) $
              " test-framework ==" `isInfixOf` (intercalate " " $ lines $ c)

    , testCase "--disable-tests does not freeze test dependencies" $ do
          removeCabalConfig
          result <- cabal_freeze dir ["--disable-tests"] cabalPath
          assertFreezeSucceeded result
          c <- readCabalConfig
          assertBool ("should not have frozen test-framework\n" ++ c) $ not $
              " test-framework ==" `isInfixOf` (intercalate " " $ lines $ c)

    , testCase "--enable-benchmarks freezes benchmark dependencies" $ do
          removeCabalConfig
          result <- cabal_freeze dir ["--disable-benchmarks"] cabalPath
          assertFreezeSucceeded result
          c <- readCabalConfig
          assertBool ("should not have frozen criterion\n" ++ c) $ not $
              " criterion ==" `isInfixOf` (intercalate " " $ lines $ c)

    , testCase "--disable-benchmarks does not freeze benchmark dependencies" $ do
          removeCabalConfig
          result <- cabal_freeze dir ["--disable-benchmarks"] cabalPath
          assertFreezeSucceeded result
          c <- readCabalConfig
          assertBool ("should not have frozen criterion\n" ++ c) $ not $
              " criterion ==" `isInfixOf` (intercalate " " $ lines $ c)
    ]


removeCabalConfig :: IO ()
removeCabalConfig = do
    removeFile (dir </> "cabal.config")
    `E.catch` \ (e :: IOError) ->
        if isDoesNotExistError e
        then return ()
        else E.throw e


readCabalConfig :: IO String
readCabalConfig = do
    readFile $ dir </> "cabal.config"
