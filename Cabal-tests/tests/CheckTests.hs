module Main
    ( main
    ) where

import Test.Tasty
import Test.Tasty.Golden.Advanced (goldenTest)

import Data.Algorithm.Diff                    (PolyDiff (..), getGroupedDiff)
import Distribution.Fields                    (runParseResult)
import Distribution.PackageDescription.Check  (checkPackage)
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription)
import Distribution.Parsec
import Distribution.Utils.Generic             (fromUTF8BS, toUTF8BS)
import System.Directory                       (setCurrentDirectory)
import System.Environment                     (getArgs, withArgs)
import System.FilePath                        (replaceExtension, (</>))

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.List.NonEmpty    as NE

tests :: TestTree
tests = checkTests

-------------------------------------------------------------------------------
-- Regressions
-------------------------------------------------------------------------------

checkTests :: TestTree
checkTests = testGroup "regressions"
    [ checkTest "nothing-unicode.cabal"
    , checkTest "haddock-api-2.18.1-check.cabal"
    , checkTest "issue-774.cabal"
    , checkTest "extensions-paths-5054.cabal"
    , checkTest "pre-1.6-glob.cabal"
    , checkTest "pre-2.4-globstar.cabal"
    , checkTest "bad-glob-syntax.cabal"
    , checkTest "globstar-literal.cabal"
    , checkTest "pre-3.8-globstar-literal.cabal"
    , checkTest "cc-options-with-optimization.cabal"
    , checkTest "cxx-options-with-optimization.cabal"
    , checkTest "ghc-option-j.cabal"
    , checkTest "multiple-libs-2.cabal"
    , checkTest "assoc-cpp-options.cabal"
    , checkTest "public-multilib-1.cabal"
    , checkTest "public-multilib-2.cabal"
    , checkTest "all-upper-bound.cabal"
    , checkTest "issue-6288-a.cabal"
    , checkTest "issue-6288-b.cabal"
    , checkTest "issue-6288-c.cabal"
    , checkTest "issue-6288-d.cabal"
    , checkTest "issue-6288-e.cabal"
    , checkTest "issue-6288-f.cabal"
    , checkTest "denormalised-paths.cabal"
    , checkTest "issue-7776-a.cabal"
    , checkTest "issue-7776-b.cabal"
    , checkTest "issue-7776-c.cabal"
    , checkTest "issue-8646.cabal"
    , checkTest "decreasing-indentation.cabal"
    ]

checkTest :: FilePath -> TestTree
checkTest fp = cabalGoldenTest fp correct $ do
    contents <- BS.readFile input
    let res =  parseGenericPackageDescription contents
    let (ws, x) = runParseResult res

    return $ toUTF8BS $ case x of
        Right gpd      ->
            -- Note: parser warnings are reported by `cabal check`, but not by
            -- D.PD.Check functionality.
            unlines (map (showPWarning fp) ws) ++
            unlines (map show (checkPackage gpd))
        Left (_, errs) -> unlines $ map (("ERROR: " ++) . showPError fp) $ NE.toList errs
  where
    input = "tests" </> "ParserTests" </> "regressions" </> fp
    correct = replaceExtension input "check"

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = do
    args <- getArgs
    case args of
        ("--cwd" : cwd : args') -> do
            setCurrentDirectory cwd
            withArgs args' $ defaultMain tests
        _ -> defaultMain tests

cabalGoldenTest :: TestName -> FilePath -> IO BS.ByteString -> TestTree
cabalGoldenTest name ref act = goldenTest name (BS.readFile ref) act cmp upd
  where
    upd = BS.writeFile ref
    cmp x y | x == y = return Nothing
    cmp x y = return $ Just $ unlines $
        concatMap f (getGroupedDiff (BS8.lines x) (BS8.lines y))
      where
        f (First xs)  = map (cons3 '-' . fromUTF8BS) xs
        f (Second ys) = map (cons3 '+' . fromUTF8BS) ys
        -- we print unchanged lines too. It shouldn't be a problem while we have
        -- reasonably small examples
        f (Both xs _) = map (cons3 ' ' . fromUTF8BS) xs
        -- we add three characters, so the changed lines are easier to spot
        cons3 c cs = c : c : c : ' ' : cs
