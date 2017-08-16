module Main
    ( main
    ) where

import Test.Tasty
import Test.Tasty.Golden (goldenVsString)

import Distribution.PackageDescription.Parsec (parseGenericPackageDescription)
import Distribution.PackageDescription.Check (checkPackage)
import Distribution.Parsec.Types.ParseResult (runParseResult)
import Distribution.Utils.Generic (toUTF8LBS)
import System.FilePath ((</>), replaceExtension)

import qualified Data.ByteString as BS

tests :: TestTree
tests = checkTests

-------------------------------------------------------------------------------
-- Regressions
-------------------------------------------------------------------------------

checkTests :: TestTree
checkTests = testGroup "regressions"
    [ checkTest "nothing-unicode.cabal"
    ]

checkTest :: FilePath -> TestTree
checkTest fp = goldenVsString fp correct $ do
    contents <- BS.readFile input
    let res =  parseGenericPackageDescription contents
    let (_, errs, x) = runParseResult res

    return $ toUTF8LBS $ case x of
        Just gpd | null errs ->
            unlines $ map show (checkPackage gpd Nothing)
        _ ->
            unlines $ "ERROR" : map show errs
  where
    input = "tests" </> "ParserTests" </> "regressions" </> fp
    correct = replaceExtension input "check"

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = defaultMain tests
