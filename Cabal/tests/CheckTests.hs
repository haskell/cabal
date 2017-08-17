module Main
    ( main
    ) where

import Test.Tasty
import Test.Tasty.Golden.Advanced (goldenTest)

import Distribution.PackageDescription.Parsec (parseGenericPackageDescription)
import Distribution.PackageDescription.Check (checkPackage)
import Distribution.Parsec.Types.ParseResult (runParseResult)
import Distribution.Utils.Generic (toUTF8LBS, fromUTF8LBS)
import System.FilePath ((</>), replaceExtension)
import Data.Algorithm.Diff (Diff (..), getGroupedDiff)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8

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
checkTest fp = cabalGoldenTest fp correct $ do
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

cabalGoldenTest :: TestName -> FilePath -> IO LBS.ByteString -> TestTree
cabalGoldenTest name ref act = goldenTest name (LBS.readFile ref) act cmp upd
  where
    upd = LBS.writeFile ref
    cmp x y | x == y = return Nothing
    cmp x y = return $ Just $ unlines $
        concatMap f (getGroupedDiff (LBS8.lines x) (LBS8.lines y))
      where
        f (First xs)  = map (cons3 '-' . fromUTF8LBS) xs
        f (Second ys) = map (cons3 '+' . fromUTF8LBS) ys
        -- we print unchanged lines too. It shouldn't be a problem while we have
        -- reasonably small examples
        f (Both xs _) = map (cons3 ' ' . fromUTF8LBS) xs
        -- we add three characters, so the changed lines are easier to spot
        cons3 c cs = c : c : c : ' ' : cs
