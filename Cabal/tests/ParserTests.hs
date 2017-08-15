module Main
    ( main
    ) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Golden (goldenVsString)

import Data.Maybe (isJust)
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription)
import Distribution.PackageDescription.PrettyPrint (showGenericPackageDescription)
import Distribution.Parsec.Types.Common (PWarnType (..), PWarning (..))
import Distribution.Parsec.Types.ParseResult (runParseResult)
import Distribution.Utils.Generic (toUTF8LBS)
import System.FilePath ((</>), (-<.>))

import qualified Data.ByteString as BS

tests :: TestTree
tests = testGroup "parsec tests"
    [ warningTests
    , regressionTests
    ]

-------------------------------------------------------------------------------
-- Warnings
-------------------------------------------------------------------------------

-- Verify that we trigger warnings
warningTests :: TestTree
warningTests = testGroup "warnings triggered"
    [ warningTest PWTLexBOM            "bom.cabal"
    , warningTest PWTLexNBSP           "nbsp.cabal"
    , warningTest PWTUTF               "utf8.cabal"
    , warningTest PWTBoolCase          "bool.cabal"
    , warningTest PWTVersionTag        "versiontag.cabal"
    , warningTest PWTNewSyntax         "newsyntax.cabal"
    , warningTest PWTOldSyntax         "oldsyntax.cabal"
    , warningTest PWTDeprecatedField   "deprecatedfield.cabal"
    , warningTest PWTInvalidSubsection "subsection.cabal"
    , warningTest PWTUnknownField      "unknownfield.cabal"
    , warningTest PWTUnknownSection    "unknownsection.cabal"
    , warningTest PWTTrailingFields    "trailingfield.cabal"
    -- TODO: not implemented yet
    -- , warningTest PWTExtraTestModule   "extratestmodule.cabal"
    ]

warningTest :: PWarnType -> FilePath -> TestTree
warningTest wt fp = testCase (show wt) $ do
    contents <- BS.readFile $ "tests" </> "ParserTests" </> "warnings" </> fp
    let res =  parseGenericPackageDescription contents
    let (warns, errs, x) = runParseResult res

    assertBool ("should parse successfully: " ++ show errs) $ isJust x
    assertBool ("should parse without errors:  " ++  show errs) $ null errs

    case warns of
        [PWarning wt' _ _] -> assertEqual "warning type" wt wt'
        []                 -> assertFailure "got no warnings"
        _                  -> assertFailure $ "got multiple warnings: " ++ show warns

-------------------------------------------------------------------------------
-- Regressions
-------------------------------------------------------------------------------

regressionTests :: TestTree
regressionTests = testGroup "regressions"
    [ regressionTest "encoding-0.8.cabal"
    , regressionTest "Octree-0.5.cabal"
    ]

regressionTest :: FilePath -> TestTree
regressionTest fp = goldenVsString fp correct $ do
    contents <- BS.readFile input
    let res =  parseGenericPackageDescription contents
    let (_, errs, x) = runParseResult res

    return $ toUTF8LBS $ case x of
        Just gpd | null errs ->
            showGenericPackageDescription gpd
        _ ->
            unlines $ "ERROR" : map show errs
  where
    input = "tests" </> "ParserTests" </> "regressions" </> fp
    correct = input -<.> "golden"

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = defaultMain tests
