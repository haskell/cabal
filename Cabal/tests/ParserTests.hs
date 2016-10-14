{-# LANGUAGE DeriveDataTypeable #-}
module Main
    ( main
    ) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Maybe (isJust)
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription)
import Distribution.Parsec.Types.Common (PWarnType (..), PWarning (..))
import Distribution.Parsec.Types.ParseResult (runParseResult)
import System.FilePath ((</>))

import qualified Data.ByteString as BS

tests :: TestTree
tests = testGroup "parsec tests"
    [ warningTests
    ]

-- Verify that we trigger warnings
warningTests :: TestTree
warningTests = testGroup "warnings triggered"
    [ warningTest PWTLexBOM "bom.cabal"
    ]

warningTest :: PWarnType -> FilePath -> TestTree
warningTest wt fp = testCase (show wt) $ do
    contents <- BS.readFile $ "tests" </> "ParserTests" </> "warnings" </> fp
    let res =  parseGenericPackageDescription contents
    let (warns, errs, x) = runParseResult res
    
    assertBool "parses successfully" $ isJust x
    assertBool "parses without errors" $ null errs

    case warns of
        [PWarning wt' _ _] -> assertEqual "warning type" wt wt'
        []                 -> assertFailure "got no warnings"
        _                  -> assertFailure $ "got multiple warnings: " ++ show warns

main :: IO ()
main = defaultMain tests
