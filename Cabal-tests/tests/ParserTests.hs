{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}
module Main
    ( main
    ) where

import Prelude ()
import Prelude.Compat

import Test.Tasty
import Test.Tasty.Golden.Advanced (goldenTest)
import Test.Tasty.HUnit

import Control.Applicative
import Control.Monad                               (void, unless)
import Data.Algorithm.Diff                         (PolyDiff (..), getGroupedDiff)
import Data.Maybe                                  (isNothing)
import Distribution.Fields                         (pwarning)
import Distribution.Fields.Parser                  (readFieldsConcrete', formatError)
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
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription)
import Distribution.PackageDescription.PrettyPrint (showGenericPackageDescription)
import Distribution.Parsec                         (PWarnType (..), PWarning (..), showPErrorWithSource, showPWarningWithSource)
import Distribution.Pretty                         (prettyShow)
import Distribution.Fields.ParseResult
import Distribution.Utils.Generic                  (fromUTF8BS, toUTF8BS)
import System.Directory                            (setCurrentDirectory)
import System.Environment                          (getArgs, withArgs)
import System.FilePath                             (replaceExtension, (</>), dropExtension, addExtension)
import Distribution.Parsec.Source
import Data.Function ((&))
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.List.NonEmpty    as NE
import Data.List.NonEmpty (NonEmpty (..))
import qualified Distribution.InstalledPackageInfo as IPI
#ifdef MIN_VERSION_tree_diff
import Data.TreeDiff                 (ansiWlEditExpr, ediff, toExpr, ToExpr)
import Data.TreeDiff.Golden          (ediffGolden)
import Data.TreeDiff.Instances.Cabal ()
#endif
import Distribution.FieldGrammar.Parsec
import Data.Functor.Identity
import Distribution.FieldGrammar.Newtypes
import Distribution.Types.PackageName
import Distribution.Fields.Field
import Data.Char
import Distribution.CabalSpecVersion
import Distribution.Types.Dependency
import Distribution.Types.VersionRange
import Distribution.Types.Version
import Distribution.Types.LibraryName
import Distribution.Types.PackageName
import Distribution.Fields.Transform
import qualified Distribution.Compat.NonEmptySet as NES
import Distribution.Parsec.Position
import Distribution.Annotation
import Data.Kind
import Data.Coerce
import Distribution.Parsec
import Distribution.Pretty
import Language.Haskell.Extension
import qualified Text.PrettyPrint as PP
import Debug.Trace
import Distribution.Fields.ExactPretty
import qualified Data.Text.Lazy.IO as TIO
import Text.Pretty.Simple
import System.IO (hPutStr, stderr, stdout)
import qualified Data.Bifunctor        as Bi

tests :: TestTree
tests = testGroup "parsec tests"
    [ regressionTests
    , warningTests
    , commentTests
    , errorTests
    , ipiTests
    , modifyValueAtomBSAlaTest
    , modifyValueListBSTest
    , prependValueListBSTest
    , joinFieldLinesTest
    , splitFieldLinesTest
    , splitBSAtPositionTests
    , substituteSubBSAtTests
    , exactPrettyFieldTests
    , editFieldGoldenTests
    , editFieldPrintedTests
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

    let res =  withSource (PCabalFile (fp, contents)) $ parseGenericPackageDescription contents
    let (warns, x) = runParseResult res

    assertBool ("should parse successfully: " ++ show x) $ isRight x

    case map pwarning warns of
        [PWarning wt' _ _] -> assertEqual "warning type" wt wt'
        []                 -> assertFailure "got no warnings"
        _                  -> assertFailure $ "got multiple warnings: " ++ show warns
  where
    isRight (Right _) = True
    isRight _         = False

editFieldGoldenTests :: TestTree
editFieldGoldenTests = testGroup "edit-golden"
  [ mkEditFieldGoldenTest "add-field-end" "simple.cabal" $
      addField AddEnd (mkName (WithComments [] ()) "its-a-new-field") []
  , mkEditFieldGoldenTest "add-field-start" "simple.cabal" $
      addField AddStart (mkName (WithComments [] ()) "its-a-new-field") []

  , mkEditFieldGoldenTest "remove-field" "simple.cabal" $
      removeField RemoveFirst (\fname _ -> getName fname == "version")
  , mkEditFieldGoldenTest "remove-field-in-section" "simple.cabal" $
      modifySection ModifyFirst (\sname sargs _ -> getName sname == "library" && null sargs) id $
        removeField RemoveAll (\fname _ -> getName fname == "build-depends")
  , mkEditFieldGoldenTest "modify-field-in-section" "simple.cabal" $
      modifySection ModifyFirst (\sname sargs _ -> getName sname == "library" && null sargs) id $
        modifyField ModifyFirst (\fname _ -> getName fname == "build-depends") $
          modifyValueList @CommaVCat @(Identity Dependency) @Dependency
            ( \case
                Dependency pname _ libname | unPackageName pname == "base" -> Just (Dependency pname funnyVersionRange libname)
                  where funnyVersionRange = thisVersion (mkVersion [1,3,3,7])
                _ -> Nothing
            )

  -- The example doesn't have the field "depends" but "build-depends" to demonstrate what would happen if the matcher doesn't match anything.
  , mkEditFieldGoldenTest "remove-field-unchanged" "simple.cabal" $
      modifySection ModifyFirst (\sname sargs _ -> getName sname == "library" && null sargs) id $
        removeField RemoveAll (\fname _ -> getName fname == "depends")

  , mkEditFieldGoldenTest "remove-field-alternative" "simple.cabal" $
     modifySection ModifyFirst (\sname sargs _ -> getName sname == "library" && null sargs) id
      ( removeField RemoveAll (\fname _ -> getName fname == "depends")
        `orFallback`
        removeField RemoveAll (\fname _ -> getName fname == "build-depends")
      )
  ]

mkEditFieldGoldenTest :: String -> FilePath -> Edit [Field (WithComments Position)] -> TestTree
mkEditFieldGoldenTest name fname edit = ediffGolden goldenTest name exprFile $ do
  contents <- BS.readFile input
  let res = readFieldsConcrete' contents

  case res of
    Left perr -> fail $ formatError contents perr
    Right (fs, warns) -> do
      unless (null warns) (fail $ unlines (map show warns))
      pure $ runEdit edit fs

  where
    input = "tests" </> "ParserTests" </> "edit" </> fname
    exprFile = addExtension (dropExtension input <> "_" <> name) "expr"

editFieldPrintedTests :: TestTree
editFieldPrintedTests = testGroup "edit-printed"
  [ mkEditFieldPrintedTest "modify-field-in-section" "simple.cabal" $
      modifySection ModifyFirst (\sname sargs _ -> getName sname == "library" && null sargs) id $
        modifyField ModifyFirst (\fname _ -> getName fname == "build-depends") $
          modifyValueList @CommaVCat @(Identity Dependency) @Dependency
            ( \case
                Dependency pname _ libname | unPackageName pname == "base" -> Just (Dependency pname funnyVersionRange libname)
                  where funnyVersionRange = thisVersion (mkVersion [1,3,3,7])
                _ -> Nothing
            )
  ]

mkEditFieldPrintedTest :: String -> FilePath -> Edit [Field (WithComments Position)] -> TestTree
mkEditFieldPrintedTest name fname edit = ediffGolden goldenTest name exprFile $ do
  contents <- BS.readFile input
  let res = readFieldsConcrete' contents

  editResult <- case res of
    Left perr -> fail $ formatError contents perr
    Right (fs, warns) -> do
      unless (null warns) (fail $ unlines (map show warns))
      pure $ runEdit edit fs

  case editResult of
    EditOk ok -> pure $ toExpr (runRenderFields ok)
    EditUnchanged u -> pure (toExpr @String "unchanged")
    EditErr err -> pure (toExpr err)

  where
    input = "tests" </> "ParserTests" </> "edit-printed" </> fname
    exprFile = addExtension (dropExtension input <> "_" <> name) "expr"

modifyValueAtomBSAlaTest :: TestTree
modifyValueAtomBSAlaTest = testGroup "modifyValueAtomBSAla"
  [ mkModifyValueAtomAlaBSTest @SpecVersion @CabalSpecVersion
      "spec-version"
      "1.10"
      (Just . succ)
      "1.12"
  , mkModifyValueAtomAlaBSTest @(Identity Language) @Language
      "default-language"
      "Haskell98"
      (\_ -> Just GHC2024)
      "GHC2024"
  ]

mkModifyValueAtomAlaBSTest
  :: forall (b :: Type) (a :: Type)
   . (Coercible a b, Parsec b, Pretty b)
  => String
  -> BS.ByteString
  -> (a -> Maybe a)
  -> BS.ByteString
  -> TestTree
mkModifyValueAtomAlaBSTest
  name
  original
  transformA
  expected
  = testCase name $ do
  let output = modifyValueAtomBSAla @b @a transformA original
  assertEqDiff "output = expected" expected output

modifyValueListBSTest :: TestTree
modifyValueListBSTest = testGroup "modifyValueListBS"
  [ mkModifyValueListTest @CommaVCat @(Identity Dependency) @Dependency
      "modify-middle-dependency-bound"
      ( BS8.unlines
          [ "  base         > 4.8    , text > 2"
          , ""
          , ", megaparsec > 5"
          , ""
          , ", containers > 0.6"
          ]
      )
      ( \case
          Dependency pname _ libname | unPackageName pname == "megaparsec" -> Just (Dependency pname funnyVersionRange libname)
          _ -> Nothing
      )
      ( BS8.unlines
          [ "  base         > 4.8    , text > 2"
          , ""
          , ", megaparsec ==1.3.3.7"
          , ""
          , ", containers > 0.6"
          ]
      )

  , mkModifyValueListTest @CommaVCat @(Identity Dependency) @Dependency
      "modify-first-dependency-bound"
      ( BS8.unlines
          [ "  base         > 4.8    , text > 2"
          , ""
          , ", megaparsec > 5"
          , ""
          , ", containers > 0.6"
          ]
      )
      ( \case
          Dependency pname _ libname | unPackageName pname == "base" -> Just (Dependency pname funnyVersionRange libname)
          _ -> Nothing
      )
      ( BS8.unlines
          [ "  base ==1.3.3.7, text > 2"
          , ""
          , ", megaparsec > 5"
          , ""
          , ", containers > 0.6"
          ]
      )

  , mkModifyValueListTest @CommaVCat @(Identity Dependency) @Dependency
      "modify-last-dependency-bound"
      ( BS8.unlines
          [ "  base         > 4.8    , text > 2"
          , ""
          , ", megaparsec > 5"
          , ""
          , ", containers > 0.6"
          ]
      )
      ( \case
          Dependency pname _ libname | unPackageName pname == "containers" -> Just (Dependency pname funnyVersionRange libname)
          _ -> Nothing
      )
      ( BS8.unlines
          [ "  base         > 4.8    , text > 2"
          , ""
          , ", megaparsec > 5"
          , ""
          , ", containers ==1.3.3.7"
          ]
      )

  , mkModifyValueListTest @NoCommaFSep @Token' @String
      "modify-ghc-options-start"
      ( BS8.unlines
          [ "-Wall -Wcompat -Widentities -Wincomplete-record-updates"
          , "-Wincomplete-patterns -Wincomplete-uni-patterns"
          , "-Wredundant-constraints -Werror=missing-fields"
          ]
      )
      ( \case
          "-Wall" -> Just "-Werror=all"
          _ -> Nothing
      )
      ( BS8.unlines
          [ "-Werror=all -Wcompat -Widentities -Wincomplete-record-updates"
          , "-Wincomplete-patterns -Wincomplete-uni-patterns"
          , "-Wredundant-constraints -Werror=missing-fields"
          ]
      )

  , mkModifyValueListTest @NoCommaFSep @Token' @String
      "modify-ghc-options-middle"
      ( BS8.unlines
          [ "-Wall -Wcompat -Widentities -Wincomplete-record-updates"
          , "-Wincomplete-patterns -Wincomplete-uni-patterns"
          , "  -Wredundant-meow -Werror=missing-fields"
          ]
      )
      ( \case
          "-Wredundant-meow" -> Just "-Wredundant-constraints"
          _ -> Nothing
      )
      ( BS8.unlines
          [ "-Wall -Wcompat -Widentities -Wincomplete-record-updates"
          , "-Wincomplete-patterns -Wincomplete-uni-patterns"
          , "  -Wredundant-constraints -Werror=missing-fields"
          ]
      )

  , mkModifyValueListTest @NoCommaFSep @Token' @String
      "modify-ghc-options-end"
      ( BS8.unlines
          [ "-Wall -Wcompat -Widentities -Wincomplete-record-updates"
          , "-Wincomplete-patterns -Wincomplete-uni-patterns"
          , "-Wredundant-constraints -Werror=missing-fields"
          ]
      )
      ( \case
          "-Werror=missing-fields" -> Just "-Werror=all"
          _ -> Nothing
      )
      ( BS8.unlines
          [ "-Wall -Wcompat -Widentities -Wincomplete-record-updates"
          , "-Wincomplete-patterns -Wincomplete-uni-patterns"
          , "-Wredundant-constraints -Werror=all"
          ]
      )
  ]
  where
    funnyVersionRange = thisVersion (mkVersion [1,3,3,7])

mkModifyValueListTest
  :: forall (sep :: Type) (b :: Type) (a :: Type)
   . ( Coercible a b
     , Sep sep
     , Pretty b
     , Parsec b
     , Parsec (List sep (Located b) (Located a))
     )
  => String
  -> BS.ByteString
  -> (a -> Maybe a)
  -> BS.ByteString
  -> TestTree
mkModifyValueListTest
  name
  original
  transformA
  expected
  = testCase name $ do
  let output = modifyValueListBS @sep @b @a transformA original
  assertEqDiff "output = expected" expected output


prependValueListBSTest :: TestTree
prependValueListBSTest = testGroup "prependValueListBS"
  [ mkPrependValueListBSTest @CommaVCat @(Identity Dependency) @Dependency
      "add-non-empty-no-leading-sep"
      ( BS8.unlines
          [ "  base         > 4.8    , text > 2"
          , ""
          , ", megaparsec > 5"
          , ""
          , ", containers > 0.6"
          ]
      )
      ( Dependency (mkPackageName "meow-meow") funnyVersionRange (NES.singleton LMainLibName) )
      ( BS8.unlines
          [ "meow-meow ==1.3.3.7,"
          , "  base         > 4.8    , text > 2"
          , ""
          , ", megaparsec > 5"
          , ""
          , ", containers > 0.6"
          ]
      )
  ]
  where
    funnyVersionRange = thisVersion (mkVersion [1,3,3,7])

mkPrependValueListBSTest
  :: forall (sep :: Type) (b :: Type) (a :: Type)
   . ( Coercible a b
     , Sep sep
     , Pretty b
     , Parsec b
     , Parsec (List sep (Located b) (Located a))
     )
  => String
  -> BS.ByteString
  -> a
  -> BS.ByteString
  -> TestTree
mkPrependValueListBSTest
  name
  original
  newItem
  expected
  = testCase name $ do
  let output = prependValueListBS @sep @b @a newItem original
  assertEqDiff "output = expected" expected output

joinFieldLinesTest :: TestTree
joinFieldLinesTest = testCase "joinFieldLines" $ do
  let input =
        FieldLine (Position 2 5) "base > 4.8"
        :|
        [ FieldLine (Position 3 3) ", megaparsec > 5"
        , FieldLine (Position 4 3) ",  containers > 0.6"
        ]
  let output = joinFieldLines input
  assertEqDiff "output is correct" output $
    FieldLine (Position 2 3) "  base > 4.8\n, megaparsec > 5\n,  containers > 0.6"

splitFieldLinesTest :: TestTree
splitFieldLinesTest = testCase "splitFieldLines" $ do
  let input =
        FieldLine (Position 2 3) "  base > 4.8\n, megaparsec > 5\n,  containers > 0.6"
  let output = splitFieldLines input
  assertEqDiff "output is correct" output
        [ FieldLine (Position 2 3) "  base > 4.8"
        , FieldLine (Position 3 3) ", megaparsec > 5"
        , FieldLine (Position 4 3) ",  containers > 0.6"
        ]

substituteSubBSAtTests :: TestTree
substituteSubBSAtTests = testGroup "substituteSubBSAt"
  [ mkSubstituteSubBSAtTest
      "middle"
      ", foo\n, bar\n, baz, qux\n"
      (SrcSpan (Position 2 3) (Position 2 6))
      "foo"
      ", foo\n, foo\n, baz, qux\n"
  , mkSubstituteSubBSAtTest
      "start"
      ", foo\n, bar\n, baz, qux\n"
      (SrcSpan (Position 1 1) (Position 1 6))
      "bar"
      "bar\n, bar\n, baz, qux\n"
  , mkSubstituteSubBSAtTest
      "end"
      ", foo\n, bar\n, baz, qux\n"
      (SrcSpan (Position 3 8) (Position 3 11))
      "bar"
      ", foo\n, bar\n, baz, bar\n"
  ]

mkSubstituteSubBSAtTest
  :: String
  -> BS.ByteString
  -> SrcSpan
  -> BS.ByteString
  -> BS.ByteString
  -> TestTree
mkSubstituteSubBSAtTest
  name
  input
  spn
  substitutor
  expected = testCase name $ do
    let output = substituteSubBSAt spn input substitutor
    assertEqDiff "output = expected" output expected

splitBSAtPositionTests :: TestTree
splitBSAtPositionTests = testGroup "splitBSAtPosition" $
  [ mkSplitBSAtPositionTest
      "start-of-all"
      ", foo\n, bar\n, baz, qux\n"
      (Position 1 1)
      ""
      ", foo\n, bar\n, baz, qux\n"
  , mkSplitBSAtPositionTest
      "start-of-line"
      ", foo\n, bar\n, baz, qux\n"
      (Position 2 1)
      ", foo\n"
      ", bar\n, baz, qux\n"
  , mkSplitBSAtPositionTest
      "middle-of-line"
      ", foo\n, bar\n, baz, qux\n"
      (Position 2 2)
      ", foo\n,"
      " bar\n, baz, qux\n"
  , mkSplitBSAtPositionTest
      "end-of-line"
      ", foo\n, bar\n, baz, qux\n"
      (Position 2 6)
      ", foo\n, bar"
      "\n, baz, qux\n"
  ]

mkSplitBSAtPositionTest
  :: String
  -> BS.ByteString
  -> Position
  -> BS.ByteString
  -> BS.ByteString
  -> TestTree
mkSplitBSAtPositionTest
  name
  input
  splitPos
  expectedLeft
  expectedRight = testCase name $ do
    let (pre, post) = splitBSAtPosition splitPos input
    assertEqDiff "input = pre <> post" input (pre <> post)
    assertEqDiff "left is wrong" expectedLeft pre
    assertEqDiff "right is wrong" expectedRight post

#ifdef MIN_VERSION_tree_diff
assertEqDiff :: (ToExpr a, Eq a) => String -> a -> a -> Assertion
assertEqDiff label x y = x == y @?
          unlines
              [ label
              , show $ ansiWlEditExpr $ ediff x y
              ]
#else
assertEqDiff :: (Eq a) => String -> a -> a -> Assertion
assertEqDiff label x y = x == y @?
          unlines
              [ label
              , "expected"
              , show x
              , "actual"
              , show y
              ]
#endif

exactPrettyFieldTests :: TestTree
exactPrettyFieldTests =
  testGroup "warnings triggered"
  $ map
    ( exactPrettyFieldTest . (\p -> "tests" </> "ParserTests" </> p)
    )
  [
    -- "project-files" </> "0-local.project"
  -- , "project-files" </> "1-local-constraints-import.project"
  -- , "project-files" </> "1-local-import-constraints.project"
  -- , "project-files" </> "1-web-constraints-import.project"
  -- , "project-files" </> "1-web-import-constraints.project"
  -- , "project-files" </> "2-local-constraints-import.project"
  -- , "project-files" </> "2-local-import-constraints.project"
  -- , "project-files" </> "2-web-constraints-import.project"
  -- , "project-files" </> "2-web-import-constraints.project"
  -- , "project-files" </> "3-web-constraints-import.project"
  -- , "project-files" </> "3-web-import-constraints.project"
  -- , "project-files" </> "alt.project"
  -- , "project-files" </> "bad-conditional.project"
  -- , "project-files" </> "cabal-cyclical-1-hop.project"
  -- , "project-files" </> "cabal-cyclical-2-hop.project"
  -- , "project-files" </> "cabal-missing-package.project"
  -- , "project-files" </> "cabal.bootstrap.project"
  -- , "project-files" </> "cabal.dot-uv.project"
  -- , "project-files" </> "cabal.external.project"
  -- , "project-files" </> "cabal.freeze-only.project"
  -- , "project-files" </> "cabal.internal.project"
  -- , "project-files" </> "cabal.local-only.project"
  -- , "project-files" </> "cabal.meta.project"
  -- , "project-files" </> "cabal.negative.project"
  -- , "project-files" </> "cabal.positive.project"
  -- , "project-files" </> "cabal.project"
  -- , "project-files" </> "cabal.release.project"
  -- , "project-files" </> "cabal.repo.project"
  -- , "project-files" </> "cabal.sub-pq.project"
  -- , "project-files" </> "cabal.sub-rs.project"
  -- , "project-files" </> "cabal.validate-libonly.project"
  -- , "project-files" </> "cabal.validate.project"
  -- , "project-files" </> "cyclical-0-self.project"
  -- , "project-files" </> "cyclical-1-out-back.project"
  -- , "project-files" </> "cyclical-1-out-self.project"
  -- , "project-files" </> "cyclical-2-out-out-back.project"
  -- , "project-files" </> "cyclical-2-out-out-backback.project"
  -- , "project-files" </> "cyclical-2-out-out-self.project"
  -- , "project-files" </> "cyclical-same-filename-out-out-back.project"
  -- , "project-files" </> "cyclical-same-filename-out-out-backback.project"
  -- , "project-files" </> "cyclical-same-filename-out-out-self.project"
  -- , "project-files" </> "elif.project"
  -- , "project-files" </> "else.project"
  -- , "project-files" </> "empty.project"
  -- , "project-files" </> "extra.project"
  -- , "project-files" </> "fake.cabal.project"
  -- , "project-files" </> "foo.project"
  -- , "project-files" </> "hops-0.project"
  -- , "project-files" </> "if.project"
  -- , "project-files" </> "no-pkgs.project"
  -- , "project-files" </> "noncyclical-same-filename-a.project"
  -- , "project-files" </> "noncyclical-same-filename-b.project"
  -- , "project-files" </> "oops-0.project"
  -- , "project-files" </> "reverse.project"
  -- , "project-files" </> "some.project"
  -- , "project-files" </> "tabs-and-spaces.project"
  -- , "project-files" </> "trailing-space.project"
  -- , "project-files" </> "variant.project"
  -- , "project-files" </> "woops-0.project"
  -- , "project-files" </> "yops-0.project"

 -- "exact-pretty.cabal"

  -- "no-braces" </> "oeis.cabal"
  -- "no-braces" </> "music-util.cabal" -- ok
  -- "no-braces" </> "modulo.cabal" -- ok
  -- "no-braces" </> "cryptohash-sha512.cabal"
  ]


exactPrettyFieldTest :: FilePath -> TestTree
exactPrettyFieldTest input = testCase "exact-pretty" $ do
  contents <- patchUpCasesWeDon'tHandle <$> BS.readFile input
  let res = readFieldsConcrete' contents

  fs <- case res of
    Left perr -> fail $ formatError contents perr
    Right (ok, warns) -> do
      -- unless (null warns) (fail $ unlines (map show warns))
      pure ok

  pPrint fs

  let reprinted = runRenderFields fs
  contents == reprinted @?
#ifdef MIN_VERSION_tree_diff
            unlines
                [ "re-parsed doesn't match"
                , show $ ansiWlEditExpr $ ediff contents reprinted
                ]
#else
            unlines
                [ "re-parsed doesn't match"
                , "expected"
                , show contents
                , "actual"
                , show reprinted
                ]
#endif
  pure ()
  where
    patchUpCasesWeDon'tHandle =
      ( (<> "\n")
        . BS8.dropWhileEnd ( \c -> isSpace c || c == '\n' )
        )
      . ( BS8.intercalate "\n"
        . map (BS8.dropWhileEnd isSpace)
        . map (\l -> if BS8.all isSpace l then "" else l)
        . map (\l -> case BS8.unsnoc l of { Just (l', '\r') -> l' ; _ -> l })
        . BS8.split '\n'
        )
      . BS8.map (\case { '\t' -> ' '; c -> c })


-------------------------------------------------------------------------------
-- comment
-------------------------------------------------------------------------------

-- Verify that comments are parsed correctly
commentTests :: TestTree
commentTests = testGroup "comments"
    [
#ifdef MIN_VERSION_tree_diff
      readFieldTest "layout-complex-indented-comments.cabal"
    , readFieldTest "layout-comment-in-fieldline.cabal" -- aligned leading comma after comment

    , commentTest "layout-nosections-before.cabal"
    , commentTest "layout-nosections-after.cabal"
    , commentTest "layout-nosections-mixed.cabal"
    , commentTest "layout-many-sections.cabal"
    , commentTest "layout-interleaved-in-section.cabal"
    , commentTest "layout-fieldline-is-flag.cabal"

    , commentTest "hasktorch.cabal" -- Imported from regression test, has a lot of comments
#endif
    ]

#ifdef MIN_VERSION_tree_diff
-- Use this test to bypass the more sophisticated checks of whether a cabal file is valid
readFieldTest :: FilePath -> TestTree
readFieldTest fname = ediffGolden goldenTest fname exprFile $ do
  contents <- BS.readFile input
  let res = readFieldsConcrete' contents

  case res of
    Left perr -> fail $ formatError contents perr
    Right (fs, warns) -> do
      unless (null warns) (fail $ unlines (map show warns))
      pure fs

  where
    input = "tests" </> "ParserTests" </> "comments" </> fname
    exprFile = replaceExtension input "expr"
#endif

#ifdef MIN_VERSION_tree_diff
-- | Assert the comment structure of a given cabal file.
commentTest :: FilePath -> TestTree
commentTest fname = ediffGolden goldenTest fname exprFile $ do
  contents <- BS.readFile input
  let res = readFieldsConcrete' contents

  case res of
    Left perr -> fail $ formatError contents perr
    Right (ok, warns) -> do
      unless (null warns) (fail $ unlines (map show warns))
      pure (foldMap extractComments ok)

  where
    input = "tests" </> "ParserTests" </> "comments" </> fname
    exprFile = replaceExtension input "expr"
#endif

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
    testGroup fp $ formatTests ++ [ treeDiffGoldenTest fp ]
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
treeDiffGoldenTest :: FilePath -> TestTree
treeDiffGoldenTest fp = ediffGolden goldenTest "expr" exprFile $ do
  contents <- BS.readFile input
  let res = withSource (PCabalFile (fp, contents)) $ parseGenericPackageDescription contents
  let (_, x) = runParseResult res
  case x of
      Right gpd -> pure (toExpr gpd)
      Left (_, errs) -> fail $ unlines $ "ERROR" : map (showPErrorWithSource . fmap renderCabalFileSource) (NE.toList errs)
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
