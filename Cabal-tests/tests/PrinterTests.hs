{-# LANGUAGE CPP #-}
module Main
    ( main
    ) where

import Prelude ()
import Prelude.Compat

import Data.Foldable(fold)
import Data.Maybe(catMaybes)
import Test.Tasty
import Data.Text(unpack)
import Test.Tasty.Golden.Advanced (goldenTest)
import Test.Tasty.HUnit

import Control.Monad                               (unless, void)
import Data.Algorithm.Diff                         (PolyDiff (..), getGroupedDiff)
import Data.Maybe                                  (isNothing)
import Distribution.Fields                         (runParseResult)
import Distribution.PackageDescription             (GenericPackageDescription)
import Distribution.PackageDescription.Parsec      (parseGenericPackageDescription)
import Distribution.PackageDescription.PrettyPrint (showGenericPackageDescription)
import Distribution.Parsec                         (PWarnType (..), PWarning (..), showPError, showPWarning)
import Distribution.Pretty                         (prettyShow)
import Distribution.Utils.Generic                  (fromUTF8BS, toUTF8BS)
import System.Directory                            (setCurrentDirectory)
import System.Environment                          (getArgs, withArgs)
import System.FilePath                             (replaceExtension, (</>))
import Data.Text.Encoding(encodeUtf8, decodeUtf8)
import Distribution.PackageDescription.ExactPrint(exactPrint)
import Data.TreeDiff
import Text.PrettyPrint hiding ((<>))

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.List.NonEmpty    as NE

import qualified Distribution.InstalledPackageInfo as IPI

#ifdef MIN_VERSION_tree_diff
import Data.TreeDiff                 (ansiWlEditExpr, ediff, toExpr)
import Data.TreeDiff.Golden          (ediffGolden)
import Data.TreeDiff.Instances.Cabal ()
#endif

tests :: TestTree
tests = testGroup "printer tests"
    [ printExact
    ]

-------------------------------------------------------------------------------
-- Warnings
-------------------------------------------------------------------------------

-- Parse some cabal file - print it like cabal file
printExact :: TestTree
printExact = testGroup "printExact"
    [ testParsePrintExact "anynone.cabal"
    -- , warningTest  "nbsp.cabal"
    -- , warningTest  "tab.cabal"
    -- , warningTest  "utf8.cabal"
    -- , warningTest  "bool.cabal"
    -- , warningTest  "versiontag.cabal"
    -- , warningTest  "newsyntax.cabal"
    -- , warningTest  "oldsyntax.cabal"
    -- , warningTest  "deprecatedfield.cabal"
    -- , warningTest  "subsection.cabal"
    -- , warningTest  "unknownfield.cabal"
    -- , warningTest  "unknownsection.cabal"
    -- , warningTest  "trailingfield.cabal"
    -- , warningTest  "doubledash.cabal"
    -- , warningTest  "multiplesingular.cabal"
    -- , warningTest  "wildcard.cabal"
    -- , warningTest  "operator.cabal"
    -- , warningTest  "specversion-a.cabal"
    -- , warningTest  "specversion-b.cabal"
    -- , warningTest  "specversion-c.cabal"
    -- -- TODO: not implemented yet
    -- , warningTest PWTExtraTestModule   "extratestmodule.cabal"
    ]

testParsePrintExact :: FilePath -> TestTree
testParsePrintExact fp = testCase ("testParsePrintExact " <> fp) $ do
    contents <- BS.readFile $ "tests" </> "ParserTests" </> "exactPrint" </> fp

    let res =  parseGenericPackageDescription contents
    let (_warns, descirption) = runParseResult res

    case descirption of
      Left someFailure -> error $ "failed parsing" <> show someFailure
      Right generic -> assertEqualStrings "should be the same cabalfiles" (unpack (decodeUtf8 contents))  (unpack (exactPrint generic))

main :: IO ()
main = do
    args <- getArgs
    case args of
        ("--cwd" : cwd : args') -> do
            setCurrentDirectory cwd
            withArgs args' $ defaultMain tests
        _ -> defaultMain tests

assertEqualStrings
  :: (HasCallStack)
  => String -- ^ The message prefix
  -> String      -- ^ The expected value
  -> String      -- ^ The actual value
  -> Assertion
assertEqualStrings preface expected actual =
  unless (actual == expected) (assertFailure msg)
 where msg = (if null preface then "" else preface ++ "\n") ++
             "expected:\n---\n" ++ expected ++ "\n---\nbut got: \n---\n" ++
             actual ++ "\n---\ndifference:\n---\n" ++  difference expected actual


difference :: String -> String -> String
difference expected actual = render $ prettyEditExpr zipped
   where
     zipped :: Edit EditExpr
     zipped = ediff (fst <$> removeEq) (snd <$> removeEq)

     removeEq = catMaybes $ zipWith (\x y -> if x == y then Nothing else Just (x,y)) expectedLines actualLines

     expectedLines = lines expected
     actualLines = lines actual
