{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main
    ( main
    ) where

import Prelude ()
import Prelude.Compat

import Test.Tasty
import Test.Tasty.Golden.Advanced (goldenTest)
import Test.Tasty.HUnit

import Control.Monad                               (void)
import Data.Algorithm.Diff                         (PolyDiff (..), getGroupedDiff)
import Data.Maybe                                  (isNothing)
import Distribution.CabalSpecVersion
import Distribution.Fields                         (pwarning, readFields)
import Distribution.PackageDescription
  ( GenericPackageDescription
  , packageDescription
  , gpdScannedVersion
  , genPackageFlags
  , condLibrary
  , condSubLibraries
  , condForeignLibs
  , condExecutables
  , condTestSuites
  , condBenchmarks
  )
import Distribution.PackageDescription.PrettyPrint (ppGenericPackageDescriptionAnn)
import Distribution.PackageDescription.FieldGrammar(buildInfoFieldGrammar, miniBuildInfoFieldGrammar, MiniBuildInfo (..))
import Distribution.PackageDescription.Parsec      (parseGenericPackageDescription, sectionizeFields, takeFields)
import Distribution.PackageDescription.PrettyPrint (showGenericPackageDescription)
import Distribution.Parsec                         (Parsec (..), explicitEitherParsec', PWarnType (..), PWarning (..), showPErrorWithSource, showPWarningWithSource)
import Distribution.Pretty                         (Pretty (..), prettyShow)
import Distribution.Fields.Parser                  (readFields')
import Distribution.Fields.ParseResult
import Distribution.Fields.Pretty                  (PrettyFieldWith (..), exactShowFields, filterFields)
import Distribution.FieldGrammar.Parsec            (ParsecFieldGrammar, parseFieldGrammar)
import Distribution.FieldGrammar.Pretty            (prettyFieldGrammar)
import Distribution.Utils.Generic                  (fromUTF8BS, toUTF8BS)
import qualified Distribution.Types.Modify as Mod
import System.Directory                            (setCurrentDirectory)
import System.Environment                          (getArgs, withArgs)
import System.FilePath                             (replaceExtension, (</>))
import Distribution.Parsec.Source

import Distribution.Types.Dependency (DependencyAnn)
import Distribution.Types.PackageName (PackageName)
import Distribution.FieldGrammar.Newtypes
  ( CommaVCat
  , CommaFSep
  , VCat
  , FSep
  , NoCommaFSep
  , ListAnn
  )

import Data.Functor ((<&>))
import Data.Functor.Identity (Identity)
import Control.Monad (unless)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.List.NonEmpty    as NE

-- NOTE(leana8959): remove this after demo is done
import Text.Pretty.Simple

import qualified Distribution.InstalledPackageInfo as IPI

#ifdef MIN_VERSION_tree_diff
import Data.TreeDiff                 (ansiWlEditExpr, ediff, toExpr)
import Data.TreeDiff.Class           (ToExpr)
import Data.TreeDiff.Golden          (ediffGolden)
import Data.TreeDiff.Instances.Cabal ()
#endif

tests :: TestTree
tests = testGroup "parsec tests"
    [ regressionTests
    , warningTests
    , errorTests
    , ipiTests
    , parsecPrettyTests
    , miniBuildInfoAnnTest
    , miniBuildInfoTest
    , smallCabalFileTest
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
    , warningTest PWTVersionWildcard   "wildcard.cabal"
    , warningTest PWTVersionOperator   "operator.cabal"
    , warningTest PWTSpecVersion       "specversion-a.cabal"
    , warningTest PWTSpecVersion       "specversion-b.cabal"
    , warningTest PWTSpecVersion       "specversion-c.cabal"
    -- TODO: not implemented yet
    -- , warningTest PWTExtraTestModule   "extratestmodule.cabal"
    ]

warningTest :: PWarnType -> FilePath -> TestTree
warningTest wt fp = testCase (show wt) $ do
    contents <- BS.readFile $ "tests" </> "ParserTests" </> "warnings" </> fp

    let res =  withSource (PCabalFile (fp, contents)) $ (parseGenericPackageDescription @Mod.HasNoAnn) contents
    let (warns, x) = runParseResult res

    assertBool ("should parse successfully: " ++ show x) $ isRight x

    case map pwarning warns of
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
    , errorTest "leading-comma-2.cabal"
    , errorTest "leading-comma-2b.cabal"
    , errorTest "leading-comma-2c.cabal"
    , errorTest "range-ge-wild.cabal"
    , errorTest "forward-compat.cabal"
    , errorTest "forward-compat2.cabal"
    , errorTest "forward-compat3.cabal"
    , errorTest "issue-5055.cabal"
    , errorTest "issue-5055-2.cabal"
    , errorTest "noVersion.cabal"
    , errorTest "noVersion2.cabal"
    , errorTest "multiple-libs.cabal"
    , errorTest "spdx-1.cabal"
    , errorTest "spdx-2.cabal"
    , errorTest "spdx-3.cabal"
    , errorTest "removed-fields.cabal"
    , errorTest "version-sets-1.cabal"
    , errorTest "version-sets-2.cabal"
    , errorTest "version-sets-3.cabal"
    , errorTest "version-sets-4.cabal"
    , errorTest "undefined-flag.cabal"
    , errorTest "mixin-1.cabal"
    , errorTest "mixin-2.cabal"
    , errorTest "libpq1.cabal"
    , errorTest "libpq2.cabal"
    , errorTest "MiniAgda.cabal"
    , errorTest "big-version.cabal"
    , errorTest "anynone.cabal"
    ]

errorTest :: FilePath -> TestTree
errorTest fp = cabalGoldenTest fp correct $ do
    contents <- BS.readFile input
    let res =  withSource (PCabalFile (fp, contents)) $ parseGenericPackageDescription contents
    let (_, x) = runParseResult res

    return $ toUTF8BS $ case x of
        Right gpd ->
            "UNEXPECTED SUCCESS\n" ++
            showGenericPackageDescription gpd
        Left (v, errs) ->
            unlines $ ("VERSION: " ++ show v) : map (showPErrorWithSource . fmap renderCabalFileSource) (NE.toList errs)
  where
    input = "tests" </> "ParserTests" </> "errors" </> fp
    correct = replaceExtension input "errors"

-------------------------------------------------------------------------------
-- Parsec/Pretty roundtrip test
-------------------------------------------------------------------------------
parsecPrettyTests :: TestTree
parsecPrettyTests = testGroup "parsec pretty roundtrip" $
  [ CabalSpecV1_0 .. ] <&> \specVer -> testGroup (show specVer) $
    optionals (specVer >= CabalSpecV2_0)
      [ parsecPrettyTest @DependencyAnn specVer "Dependency ^>=" "text ^>=   1"
      , parsecPrettyTest @DependencyAnn specVer "Dependency || and &&" "text (^>=   1 ||    >  2) && == 3"
      , parsecPrettyTest @DependencyAnn specVer "Dependency ||" "text ^>=   1 ||    >  2"
      ]
    ++
      [ parsecPrettyTest @DependencyAnn specVer "Dependency ==" "text   == 1"
      , parsecPrettyTest @DependencyAnn specVer "Dependency >" "text >   1"
      , parsecPrettyTest @DependencyAnn specVer "Dependency >=" "text >=   1"
      , parsecPrettyTest @DependencyAnn specVer "Dependency <" "text<   1"
      , parsecPrettyTest @DependencyAnn specVer "Dependency <=" "text  <=  1"
      , parsecPrettyTest @DependencyAnn specVer "Dependency || and &&" "text (>=   1 ||    >  2) && == 3"
      , parsecPrettyTest @DependencyAnn specVer "Dependency ||" "text >=   1 ||    >  2"
      ]
    ++
      -- Test list combinators using PackageName because it has a simple Parsec instance.
      parsecPrettyTest @PackageName specVer "PackageName simple" "foo" -- make sure PackageName itself parses.
      :
      optionals (specVer >= CabalSpecV2_2)
        [ parsecPrettyTest @(ListAnn CommaVCat (Identity PackageName) PackageName) specVer "CommaVCat leading" ", foo , bar"
        , parsecPrettyTest @(ListAnn CommaFSep (Identity PackageName) PackageName) specVer "CommaFSep leading" ", foo , bar"
        , parsecPrettyTest @(ListAnn CommaVCat (Identity PackageName) PackageName) specVer "CommaVCat trailing" "foo \n , bar  \n, "
        , parsecPrettyTest @(ListAnn CommaFSep (Identity PackageName) PackageName) specVer "CommaFSep trailing" "foo \n , bar , "
        ]
      ++
      optionals (specVer >= CabalSpecV3_0)
        [ parsecPrettyTest @(ListAnn VCat (Identity PackageName) PackageName) specVer "VCat leading" ", foo , bar"
        , parsecPrettyTest @(ListAnn FSep (Identity PackageName) PackageName) specVer "FSep leading" ", foo , bar"
        ]
      ++
        [ parsecPrettyTest @(ListAnn CommaVCat (Identity PackageName) PackageName) specVer "CommaVCat simple" "foo , bar ,   baz"
        , parsecPrettyTest @(ListAnn CommaVCat (Identity PackageName) PackageName) specVer "CommaVCat newline" "foo ,\n bar ,   baz"
        , parsecPrettyTest @(ListAnn CommaVCat (Identity PackageName) PackageName) specVer "CommaVCat newline" "foo ,\n bar \n,   baz"

        , parsecPrettyTest @(ListAnn CommaFSep (Identity PackageName) PackageName) specVer "CommaFSep simple" "foo , bar ,   baz"
        , parsecPrettyTest @(ListAnn CommaFSep (Identity PackageName) PackageName) specVer "CommaFSep newline" "foo ,\n bar ,   baz"
        , parsecPrettyTest @(ListAnn CommaFSep (Identity PackageName) PackageName) specVer "CommaFSep newline" "foo ,\n bar \n,   baz"

        , parsecPrettyTest @(ListAnn VCat (Identity PackageName) PackageName) specVer "VCat simple" "foo \n bar"
        , parsecPrettyTest @(ListAnn VCat (Identity PackageName) PackageName) specVer "VCat trailing" "foo \n bar   \n"
        , parsecPrettyTest @(ListAnn VCat (Identity PackageName) PackageName) specVer "VCat trailing" "foo \n bar  \n\n"
        , parsecPrettyTest @(ListAnn VCat (Identity PackageName) PackageName) specVer "VCat optional comma" "foo , \n bar  \n\n"

        , parsecPrettyTest @(ListAnn FSep (Identity PackageName) PackageName) specVer "FSep simple" "foo \n bar"
        , parsecPrettyTest @(ListAnn FSep (Identity PackageName) PackageName) specVer "FSep trailing" "foo \n bar   \n"
        , parsecPrettyTest @(ListAnn FSep (Identity PackageName) PackageName) specVer "FSep trailing" "foo \n bar  \n\n"
        , parsecPrettyTest @(ListAnn FSep (Identity PackageName) PackageName) specVer "FSep optional comma" "foo , \n bar  \n\n"

        , parsecPrettyTest @(ListAnn NoCommaFSep (Identity PackageName) PackageName) specVer "NoCommaFSep simple" "foo \n bar"
        , parsecPrettyTest @(ListAnn NoCommaFSep (Identity PackageName) PackageName) specVer "NoCommaFSep trailing" "foo \n bar   \n"
        , parsecPrettyTest @(ListAnn NoCommaFSep (Identity PackageName) PackageName) specVer "NoCommaFSep trailing" "foo \n bar  \n\n"
        , parsecPrettyTest @(ListAnn NoCommaFSep (Identity PackageName) PackageName) specVer "NoCommaFSep optional comma" "foo  \n bar  \n\n"
      ]

  where
    optionals cond ifTrue = if cond then ifTrue else []

miniBuildInfoAnnTest :: TestTree
miniBuildInfoAnnTest = testCase "miniBuildInfo Ann" $ do
  fields <- readFields <$> BS.readFile input >>= \case
      Left err -> fail $ "readFields: err"
      Right ok -> pure ok

  -- We ignore sections now, which necessite goSections to dispatch field gramamr parsers
  let (frontFields, _sections) = takeFields fields
      pr :: ParseResult src (MiniBuildInfo Mod.HasAnn)
      pr = parseFieldGrammar CabalSpecV3_0 frontFields miniBuildInfoFieldGrammar

      (_warns, pr') = runParseResult pr

  pr'' <- case pr' of
    Left (_, errs) -> fail "ERROR in running field grammar"
    Right ok -> pure $ ok

  let prettyFields :: [PrettyFieldWith Mod.HasAnn] = prettyFieldGrammar CabalSpecV3_0 miniBuildInfoFieldGrammar pr''
  putStrLn
    $ exactShowFields prettyFields
  where
    input = "tests" </> "ParserTests" </> "miniBuildInfoDemo.cabal"

smallCabalFileTest :: TestTree
smallCabalFileTest = testCase "smallCabalFile" $ do
  contents <- BS.readFile input
  let res = withSource (PCabalFile (fp, contents)) $ (parseGenericPackageDescription @Mod.HasAnn) contents
  let (_, x) = runParseResult res
  gpd <- case x of
      Right ok -> pure ok
      Left (_, errs) -> fail $ unlines $ "ERROR" : map (showPErrorWithSource . fmap renderCabalFileSource) (NE.toList errs)

  let prettyFields = ppGenericPackageDescriptionAnn CabalSpecV3_0 gpd
      prettyFields' = filterFields prettyFields

  putStrLn $
    exactShowFields prettyFields'
  where
    input = "tests" </> "ParserTests" </> fp
    fp = "smallCabalFile.cabal"

miniBuildInfoTest :: TestTree
miniBuildInfoTest = testCase "miniBuildInfo NoAnn" $ do
  fields <- readFields <$> BS.readFile input >>= \case
      Left err -> fail $ "readFields: err"
      Right ok -> pure ok

  -- We ignore sections now, which necessite goSections to dispatch field gramamr parsers
  let (frontFields, _sections) = takeFields fields
      pr :: ParseResult src (MiniBuildInfo Mod.HasNoAnn)
      pr = parseFieldGrammar CabalSpecV3_0 frontFields miniBuildInfoFieldGrammar

      (_warns, pr') = runParseResult pr

  pr'' <- case pr' of
    Left (_, errs) -> fail "ERROR in running field grammar"
    Right ok -> pure $ ok

  pPrint pr''
  where
    input = "tests" </> "ParserTests" </> "miniBuildInfoDemo.cabal"

parsecPrettyTest :: forall a. (Parsec a, Pretty a) => CabalSpecVersion -> String -> String -> TestTree
parsecPrettyTest specVer testName input = testCase testName $ do
    parsed <- case explicitEitherParsec' specVer parsec input of
      Left err -> fail $ unlines $ "ERROR" : show err : []
      Right ok -> pure $ ok

    -- TODO(leana8959): should we handle different layout configurations?
    let reprinted = show (pretty @a parsed)

{- FOURMOLU_DISABLE -}
    unless (input == reprinted) $
#ifdef MIN_VERSION_tree_diff
        assertFailure $ unlines
            [ "re-parsed doesn't match"
            , show $ ansiWlEditExpr $ ediff input reprinted
            ]
#else
        assertFailure $ unlines
            [ "re-printed doesn't match"
            , "expected"
            , show input
            , "actual"
            , show reprinted
            ]
#endif
{- FOURMOLU_ENABLE -}

-------------------------------------------------------------------------------
-- Regressions
-------------------------------------------------------------------------------

regressionTests :: TestTree
regressionTests = testGroup "regressions"
    [ regressionTest "encoding-0.8.cabal"
    , regressionTest "Octree-0.5.cabal"
    , regressionTest "nothing-unicode.cabal"
    , regressionTest "multiple-libs-2.cabal"
    , regressionTest "issue-774.cabal"
    , regressionTest "generics-sop.cabal"
    , regressionTest "elif.cabal"
    , regressionTest "elif2.cabal"
    , regressionTest "shake.cabal"
    , regressionTest "common.cabal"
    , regressionTest "common2.cabal"
    , regressionTest "common3.cabal"
    , regressionTest "common-conditional.cabal"
    , regressionTest "leading-comma.cabal"
    , regressionTest "leading-comma-2.cabal"
    , regressionTest "wl-pprint-indef.cabal"
    , regressionTest "th-lift-instances.cabal"
    , regressionTest "issue-5055.cabal"
    , regressionTest "issue-6083-pkg-pkg.cabal"
    , regressionTest "issue-6083-a.cabal"
    , regressionTest "issue-6083-b.cabal"
    , regressionTest "issue-6083-c.cabal"
    , regressionTest "noVersion.cabal"
    , regressionTest "spdx-1.cabal"
    , regressionTest "spdx-2.cabal"
    , regressionTest "spdx-3.cabal"
    , regressionTest "hidden-main-lib.cabal"
    , regressionTest "jaeger-flamegraph.cabal"
    , regressionTest "version-sets.cabal"
    , regressionTest "mixin-1.cabal"
    , regressionTest "mixin-2.cabal"
    , regressionTest "mixin-3.cabal"
    , regressionTest "libpq1.cabal"
    , regressionTest "libpq2.cabal"
    , regressionTest "issue-5846.cabal"
    , regressionTest "indentation.cabal"
    , regressionTest "indentation2.cabal"
    , regressionTest "indentation3.cabal"
    , regressionTest "big-version.cabal"
    , regressionTest "anynone.cabal"
    , regressionTest "monad-param.cabal"
    , regressionTest "hasktorch.cabal"
    ]

regressionTest :: FilePath -> TestTree
regressionTest fp = let formatTests = [ formatGoldenTest fp, formatRoundTripTest fp ] in
#ifdef MIN_VERSION_tree_diff
    testGroup fp $ formatTests ++ [ mkTreeDiffGoldenTest "regressions" fp ]
#else
    testGroup fp formatTests
#endif

formatGoldenTest :: FilePath -> TestTree
formatGoldenTest fp = cabalGoldenTest "format" correct $ do
    contents <- BS.readFile input
    let res = withSource (PCabalFile (fp, contents)) $ parseGenericPackageDescription contents
    let (warns, x) = runParseResult res

    return $ toUTF8BS $ case x of
        Right gpd ->
            unlines (map (showPWarningWithSource . fmap renderCabalFileSource) warns)
            ++ showGenericPackageDescription gpd
        Left (csv, errs) ->
            unlines $
                "ERROR" :
                maybe "unknown-version" prettyShow csv :
                map (showPErrorWithSource . fmap renderCabalFileSource) (NE.toList errs)
  where
    input = "tests" </> "ParserTests" </> "regressions" </> fp
    correct = replaceExtension input "format"

#ifdef MIN_VERSION_tree_diff
mkTreeDiffGoldenTest :: FilePath -> FilePath -> TestTree
mkTreeDiffGoldenTest goldenFileDir fp = ediffGolden goldenTest "expr" exprFile $ do
  contents <- BS.readFile input
  let res = withSource (PCabalFile (fp, contents)) $ (parseGenericPackageDescription @Mod.HasNoAnn) contents
  let (_, x) = runParseResult res
  case x of
      Right gpd -> pure (toExpr gpd)
      Left (_, errs) -> fail $ unlines $ "ERROR" : map (showPErrorWithSource . fmap renderCabalFileSource) (NE.toList errs)
  where
    input = "tests" </> "ParserTests" </> goldenFileDir </> fp
    exprFile = replaceExtension input "expr"
#endif

formatRoundTripTest :: FilePath -> TestTree
formatRoundTripTest fp = testCase "roundtrip" $ do
    contents <- BS.readFile input
    x <- parse contents
    let contents' = showGenericPackageDescription x
    y <- parse (toUTF8BS contents')

    let checkField field =
          field x == field y @?
#ifdef MIN_VERSION_tree_diff
            unlines
                [ "re-parsed doesn't match"
                , show $ ansiWlEditExpr $ ediff x y
                ]
#else
            unlines
                [ "re-parsed doesn't match"
                , "expected"
                , show x
                , "actual"
                , show y
                ]
#endif
    sequence_
      [ checkField packageDescription
      , checkField gpdScannedVersion
      , checkField genPackageFlags
      , checkField condLibrary
      , checkField condSubLibraries
      , checkField condForeignLibs
      , checkField condExecutables
      , checkField condTestSuites
      , checkField condBenchmarks
      ]

  where
    parse :: BS.ByteString -> IO GenericPackageDescription
    parse c = do
        let (_, x') = runParseResult $ withSource (PCabalFile (fp, c)) $ parseGenericPackageDescription c
        case x' of
            Right gpd      -> pure gpd
            Left (_, errs) -> do
                void $ assertFailure $ unlines (map (showPErrorWithSource . fmap renderCabalFileSource) $ NE.toList errs)
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
ipiTest fp = let formatTests = [ ipiFormatGoldenTest fp , ipiFormatRoundTripTest fp ] in
#ifdef MIN_VERSION_tree_diff
    testGroup fp $ [ ipiTreeDiffGoldenTest fp ] ++ formatTests
#else
    testGroup fp formatTests
#endif

ipiFormatGoldenTest :: FilePath -> TestTree
ipiFormatGoldenTest fp = cabalGoldenTest "format" correct $ do
    contents <- BS.readFile input
    let res = IPI.parseInstalledPackageInfo contents
    return $ toUTF8BS $ case res of
        Left err -> "ERROR " ++ show err
        Right (ws, ipi)  ->
            unlines ws ++ IPI.showInstalledPackageInfo ipi
  where
    input = "tests" </> "ParserTests" </> "ipi" </> fp
    correct = replaceExtension input "format"

#ifdef MIN_VERSION_tree_diff
ipiTreeDiffGoldenTest :: FilePath -> TestTree
ipiTreeDiffGoldenTest fp = ediffGolden goldenTest "expr" exprFile $ do
    contents <- BS.readFile input
    let res = IPI.parseInstalledPackageInfo contents
    case res of
        Left err -> fail $ "ERROR " ++ show err
        Right (_ws, ipi) -> pure (toExpr ipi)
  where
    input = "tests" </> "ParserTests" </> "ipi" </> fp
    exprFile = replaceExtension input "expr"
#endif

ipiFormatRoundTripTest :: FilePath -> TestTree
ipiFormatRoundTripTest fp = testCase "roundtrip" $ do
    contents <- BS.readFile input
    x <- parse contents
    let contents' = IPI.showInstalledPackageInfo x
    y <- parse (toUTF8BS contents')

    -- ghc-pkg prints pkgroot itself, based on cli arguments!
    let x' = x { IPI.pkgRoot = Nothing }
    let y' = y
    assertBool "pkgRoot isn't shown" (isNothing (IPI.pkgRoot y))
    assertEqual "re-parsed doesn't match" x' y'

    -- Complete round-trip
    let contents2 = IPI.showFullInstalledPackageInfo x
    z <- parse (toUTF8BS contents2)
    assertEqual "re-parsed doesn't match" x z

  where
    parse :: BS.ByteString -> IO IPI.InstalledPackageInfo
    parse c = do
        case IPI.parseInstalledPackageInfo c of
            Right (_, ipi) -> return ipi
            Left err       -> do
              void $ assertFailure $ show err
              fail "failure"
    input = "tests" </> "ParserTests" </> "ipi" </> fp

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
