{-# LANGUAGE CPP #-}
module Main
    ( main
    ) where

import Prelude ()
import Prelude.Compat

import Data.Maybe(catMaybes)
import Test.Tasty
import Data.Text(unpack)
import Test.Tasty.HUnit

import Control.Monad                               (unless)
import Distribution.Fields                         (runParseResult)
import Distribution.PackageDescription.Parsec      (parseGenericPackageDescription)
import System.Directory                            (setCurrentDirectory)
import System.Environment                          (getArgs, withArgs)
import System.FilePath                             ((</>))
import Data.Text.Encoding(decodeUtf8)
import Distribution.PackageDescription.ExactPrint(exactPrint)
import Data.TreeDiff
import Text.PrettyPrint hiding ((<>))

import qualified Data.ByteString       as BS

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
    [
      testParsePrintExact "bounded.cabal"
    -- , testParsePrintExact "anynone.cabal" -- TODO is this neccessary? I think we're allowed to pretty print a range?
    -- , testParsePrintExact "multiple-depends.cabal" -- TODO is this neccisary? I think we're allowed to be oppinionated on comma placement?
    , testParsePrintExact "two-sections.cabal" -- this is required
    , testParsePrintExact "two-sections-spacing.cabal" -- this is required
    -- , testParsePrintExact "comment.cabal" -- this is required
    -- , testParsePrintExact "comments.cabal" -- TODO this is required
    -- broken by: instance Pretty VersionRange where
    -- however we currently don't retain enough information to do this exact!
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
