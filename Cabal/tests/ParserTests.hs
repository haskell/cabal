{-# LANGUAGE CPP #-}
module Main
    ( main
    ) where

import Prelude ()
import Prelude.Compat

import Test.Tasty
import Test.Tasty.Golden.Advanced (goldenTest)
import Test.Tasty.HUnit

import Control.Monad                               (void)
import Data.Algorithm.Diff                         (Diff (..), getGroupedDiff)
import Data.Maybe                                  (isNothing)
import Distribution.PackageDescription             (GenericPackageDescription)
import Distribution.PackageDescription.Parsec      (parseGenericPackageDescription)
import Distribution.PackageDescription.PrettyPrint (showGenericPackageDescription)
import Distribution.Parsec.Common
       (PWarnType (..), PWarning (..), showPError, showPWarning)
import Distribution.Parsec.ParseResult             (runParseResult)
import Distribution.Utils.Generic                  (fromUTF8BS, toUTF8BS)
import System.FilePath                             (replaceExtension, (</>))

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BS8

import qualified Distribution.InstalledPackageInfo as IPI
import qualified Distribution.ParseUtils           as ReadP

#ifdef MIN_VERSION_tree_diff
import Data.TreeDiff        (toExpr)
import Data.TreeDiff.Golden (ediffGolden)
import Instances.TreeDiff ()
#endif

tests :: TestTree
tests = testGroup "parsec tests"
    [ regressionTests
    , warningTests
    , errorTests
    , ipiTests
    ]

-------------------------------------------------------------------------------
-- Warnings
-------------------------------------------------------------------------------

-- Verify that we trigger warnings
warningTests :: TestTree
warningTests = testGroup "warnings triggered"
    [ warningTest PWTLexBOM            "bom.cabal"
    , warningTest PWTLexNBSP           "nbsp.cabal"
    , warningTest PWTLexTab            "tab.cabal"
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
    , warningTest PWTDoubleDash        "doubledash.cabal"
    , warningTest PWTMultipleSingularField "multiplesingular.cabal"
    -- TODO: not implemented yet
    -- , warningTest PWTExtraTestModule   "extratestmodule.cabal"
    ]

warningTest :: PWarnType -> FilePath -> TestTree
warningTest wt fp = testCase (show wt) $ do
    contents <- BS.readFile $ "tests" </> "ParserTests" </> "warnings" </> fp

    let res =  parseGenericPackageDescription contents
    let (warns, x) = runParseResult res

    assertBool ("should parse successfully: " ++ show x) $ isRight x

    case warns of
        [PWarning wt' _ _] -> assertEqual "warning type" wt wt'
        []                 -> assertFailure "got no warnings"
        _                  -> assertFailure $ "got multiple warnings: " ++ show warns
  where
    isRight (Right _) = True
    isRight _         = False

-------------------------------------------------------------------------------
-- Errors
-------------------------------------------------------------------------------

errorTests :: TestTree
errorTests = testGroup "errors"
    [ errorTest "common1.cabal"
    , errorTest "common2.cabal"
    , errorTest "common3.cabal"
    , errorTest "leading-comma.cabal"
    , errorTest "range-ge-wild.cabal"
    , errorTest "forward-compat.cabal"
    , errorTest "forward-compat2.cabal"
    , errorTest "forward-compat3.cabal"
    , errorTest "issue-5055.cabal"
    , errorTest "issue-5055-2.cabal"
    , errorTest "noVersion.cabal"
    , errorTest "noVersion2.cabal"
    , errorTest "spdx-1.cabal"
    , errorTest "spdx-2.cabal"
    , errorTest "spdx-3.cabal"
    , errorTest "big-version.cabal"
    ]

errorTest :: FilePath -> TestTree
errorTest fp = cabalGoldenTest fp correct $ do
    contents <- BS.readFile input
    let res =  parseGenericPackageDescription contents
    let (_, x) = runParseResult res

    return $ toUTF8BS $ case x of
        Right gpd ->
            "UNXPECTED SUCCESS\n" ++
            showGenericPackageDescription gpd
        Left (v, errs) ->
            unlines $ ("VERSION: " ++ show v) : map (showPError fp) errs
  where
    input = "tests" </> "ParserTests" </> "errors" </> fp
    correct = replaceExtension input "errors"

-------------------------------------------------------------------------------
-- Regressions
-------------------------------------------------------------------------------

regressionTests :: TestTree
regressionTests = testGroup "regressions"
    [ regressionTest "encoding-0.8.cabal"
    , regressionTest "Octree-0.5.cabal"
    , regressionTest "nothing-unicode.cabal"
    , regressionTest "issue-774.cabal"
    , regressionTest "generics-sop.cabal"
    , regressionTest "elif.cabal"
    , regressionTest "elif2.cabal"
    , regressionTest "shake.cabal"
    , regressionTest "common.cabal"
    , regressionTest "common2.cabal"
    , regressionTest "leading-comma.cabal"
    , regressionTest "wl-pprint-indef.cabal"
    , regressionTest "th-lift-instances.cabal"
    , regressionTest "issue-5055.cabal"
    , regressionTest "noVersion.cabal"
    , regressionTest "spdx-1.cabal"
    , regressionTest "spdx-2.cabal"
    , regressionTest "spdx-3.cabal"
    , regressionTest "big-version.cabal"
    ]

regressionTest :: FilePath -> TestTree
regressionTest fp = testGroup fp
    [ formatGoldenTest fp
    , formatRoundTripTest fp
#ifdef MIN_VERSION_tree_diff
    , treeDiffGoldenTest fp
#endif
    ]

formatGoldenTest :: FilePath -> TestTree
formatGoldenTest fp = cabalGoldenTest "format" correct $ do
    contents <- BS.readFile input
    let res = parseGenericPackageDescription contents
    let (warns, x) = runParseResult res

    return $ toUTF8BS $ case x of
        Right gpd ->
            unlines (map (showPWarning fp) warns)
            ++ showGenericPackageDescription gpd
        Left (_, errs) ->
            unlines $ "ERROR" : map (showPError fp) errs
  where
    input = "tests" </> "ParserTests" </> "regressions" </> fp
    correct = replaceExtension input "format"

#ifdef MIN_VERSION_tree_diff
treeDiffGoldenTest :: FilePath -> TestTree
treeDiffGoldenTest fp = ediffGolden goldenTest "expr" exprFile $ do
    contents <- BS.readFile input
    let res =  parseGenericPackageDescription contents
    let (_, x) = runParseResult res
    case x of
        Right gpd      -> pure (toExpr gpd)
        Left (_, errs) -> fail $ unlines $ "ERROR" : map (showPError fp) errs
  where
    input = "tests" </> "ParserTests" </> "regressions" </> fp
    exprFile = replaceExtension input "expr"
#endif

formatRoundTripTest :: FilePath -> TestTree
formatRoundTripTest fp = testCase "roundtrip" $ do
    contents <- BS.readFile input
    x <- parse contents
    let contents' = showGenericPackageDescription x
    y <- parse (toUTF8BS contents')
    -- previously we mangled licenses a bit
    let y' = y
    assertEqual "re-parsed doesn't match" x y'
  where
    parse :: BS.ByteString -> IO GenericPackageDescription
    parse c = do
        let (_, x') = runParseResult $ parseGenericPackageDescription c
        case x' of
            Right gpd      -> pure gpd
            Left (_, errs) -> do
                void $ assertFailure $ unlines (map (showPError fp) errs)
                fail "failure"
    input = "tests" </> "ParserTests" </> "regressions" </> fp

-------------------------------------------------------------------------------
-- InstalledPackageInfo regressions
-------------------------------------------------------------------------------

ipiTests :: TestTree
ipiTests = testGroup "ipis"
    [ ipiTest "transformers.cabal"
    , ipiTest "Includes2.cabal"
    , ipiTest "issue-2276-ghc-9885.cabal"
    , ipiTest "internal-preprocessor-test.cabal"
    ]

ipiTest :: FilePath -> TestTree
ipiTest fp = testGroup fp $
#ifdef MIN_VERSION_tree_diff
    [ ipiTreeDiffGoldenTest fp ] ++
#endif
    [ ipiFormatGoldenTest fp
    , ipiFormatRoundTripTest fp
    ]

ipiFormatGoldenTest :: FilePath -> TestTree
ipiFormatGoldenTest fp = cabalGoldenTest "format" correct $ do
    contents <- readFile input
    let res = IPI.parseInstalledPackageInfo contents
    return $ toUTF8BS $ case res of
        ReadP.ParseFailed err -> "ERROR " ++ show err
        ReadP.ParseOk ws ipi  ->
            unlines (map (ReadP.showPWarning fp) ws)
            ++ IPI.showInstalledPackageInfo ipi
  where
    input = "tests" </> "ParserTests" </> "ipi" </> fp
    correct = replaceExtension input "format"

#ifdef MIN_VERSION_tree_diff
ipiTreeDiffGoldenTest :: FilePath -> TestTree
ipiTreeDiffGoldenTest fp = ediffGolden goldenTest "expr" exprFile $ do
    contents <- readFile input
    let res = IPI.parseInstalledPackageInfo contents
    case res of
        ReadP.ParseFailed err -> fail $ "ERROR " ++ show err
        ReadP.ParseOk _ws ipi -> pure (toExpr ipi)
  where
    input = "tests" </> "ParserTests" </> "ipi" </> fp
    exprFile = replaceExtension input "expr"
#endif

ipiFormatRoundTripTest :: FilePath -> TestTree
ipiFormatRoundTripTest fp = testCase "roundtrip" $ do
    contents <- readFile input
    x <- parse contents
    let contents' = IPI.showInstalledPackageInfo x
    y <- parse contents'

    -- ghc-pkg prints pkgroot itself, based on cli arguments!
    let x' = x { IPI.pkgRoot = Nothing }
    let y' = y
    assertBool "pkgRoot isn't shown" (isNothing (IPI.pkgRoot y))
    assertEqual "re-parsed doesn't match" x' y'

    -- Complete round-trip
    let contents2 = IPI.showFullInstalledPackageInfo x
    z <- parse contents2
    assertEqual "re-parsed doesn't match" x z

  where
    parse :: String -> IO IPI.InstalledPackageInfo
    parse c = do
        case IPI.parseInstalledPackageInfo c of
            ReadP.ParseOk _ ipi   -> return ipi
            ReadP.ParseFailed err -> do
              void $ assertFailure $ show err
              fail "failure"
    input = "tests" </> "ParserTests" </> "ipi" </> fp

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = defaultMain tests

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
