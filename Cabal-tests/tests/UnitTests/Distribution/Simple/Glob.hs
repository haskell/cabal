{-# LANGUAGE LambdaCase #-}
module UnitTests.Distribution.Simple.Glob
    ( tests
    ) where

import Control.Monad
import Data.Foldable (for_)
import Data.Function (on)
import Data.List (sort)
import Data.Maybe (mapMaybe)
import Distribution.Simple.Glob
import qualified Distribution.Verbosity as Verbosity
import Distribution.CabalSpecVersion
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), splitFileName, normalise)
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

sampleFileNames :: [FilePath]
sampleFileNames =
  [ "a"
  , "a.html"
  , "b.html"
  , "b.html.gz"
  , "foo/.blah.html"
  , "foo/.html"
  , "foo/a"
  , "foo/a.html"
  , "foo/a.html.gz"
  , "foo/a.tex"
  , "foo/a.tex.gz"
  , "foo/b.html"
  , "foo/b.html.gz"
  , "foo/x.gz"
  , "foo/bar/.html"
  , "foo/bar/a.html"
  , "foo/bar/a.html.gz"
  , "foo/bar/a.tex"
  , "foo/bar/a.tex.gz"
  , "foo/bar/b.html"
  , "foo/bar/b.html.gz"
  , "foo/c.html/blah"
  , "xyz/foo/a.html"
  ]

makeSampleFiles :: FilePath -> IO ()
makeSampleFiles dir = for_ sampleFileNames $ \filename -> do
  let (dir', name) = splitFileName filename
  createDirectoryIfMissing True (dir </> dir')
  writeFile (dir </> dir' </> name) $ "This is " ++ filename

compatibilityTests :: CabalSpecVersion -> [TestTree]
compatibilityTests version =
  [ testCase "literal match" $
      testMatches "foo/a" [GlobMatch "foo/a"]
  , testCase "literal no match on prefix" $
      testMatches "foo/c.html" [GlobMatchesDirectory "foo/c.html"]
  , testCase "literal no match on suffix" $
      testMatches "foo/a.html" [GlobMatch "foo/a.html"]
  , testCase "literal no prefix" $
      testMatches "a" [GlobMatch "a"]
  , testCase "literal multiple prefix" $
      testMatches "foo/bar/a.html" [GlobMatch "foo/bar/a.html"]
  , testCase "glob" $
      testMatches "*.html" [GlobMatch "a.html", GlobMatch "b.html"]
  , testCase "glob in subdir" $
      testMatches "foo/*.html" [GlobMatchesDirectory "foo/c.html", GlobMatch "foo/b.html", GlobMatch "foo/a.html"]
  , testCase "glob multiple extensions" $
      testMatches "foo/*.html.gz" [GlobMatch "foo/a.html.gz", GlobMatch "foo/b.html.gz"]
  , testCase "glob in deep subdir" $
      testMatches "foo/bar/*.tex" [GlobMatch "foo/bar/a.tex"]
  , testCase "star in directory" $
      testFailParse "blah/*/foo" StarInDirectory
  , testCase "star plus text in segment" $
      testFailParse "xyz*/foo" StarInDirectory
  , testCase "star in filename plus text" $
      testFailParse "foo*.bar" StarInFileName
  , testCase "no extension on star" $
      testFailParse "foo/*" NoExtensionOnStar
  , testCase "star in extension" $
      testFailParse "foo.*.gz" StarInExtension
  ]
  where
    testMatches = testMatchesVersion version
    testFailParse = testFailParseVersion version

-- For efficiency reasons, matchDirFileGlob isn't a simple call to
-- getDirectoryContentsRecursive and then a filter with
-- fileGlobMatches. So test both that naive approach and the actual
-- approach to make sure they are both correct.
--
-- TODO: Work out how to construct the sample tree once for all tests,
-- rather than once for each test.
testMatchesVersion :: CabalSpecVersion -> FilePath -> [GlobResult FilePath] -> Assertion
testMatchesVersion version pat expected = do
  globPat <- case parseFileGlob version pat of
    Left _ -> assertFailure "Couldn't compile the pattern."
    Right globPat -> return globPat
  checkPure globPat
  checkIO globPat
  where
    isEqual = (==) `on` (sort . fmap (fmap normalise))
    checkPure globPat = do
      let actual = mapMaybe (\p -> (p <$) <$> fileGlobMatches version globPat p) sampleFileNames
          -- We drop directory matches from the expected results since the pure
          -- check can't identify that kind of match.
          expected' = filter (\case GlobMatchesDirectory _ -> False; _ -> True) expected
      unless (sort expected' == sort actual) $
        assertFailure $ "Unexpected result (pure matcher): " ++ show actual
    checkIO globPat =
      withSystemTempDirectory "globstar-sample" $ \tmpdir -> do
        makeSampleFiles tmpdir
        actual <- runDirFileGlob Verbosity.normal (Just version) tmpdir globPat
        unless (isEqual actual expected) $
          assertFailure $ "Unexpected result (impure matcher): " ++ show actual

testFailParseVersion :: CabalSpecVersion -> FilePath -> GlobSyntaxError -> Assertion
testFailParseVersion version pat expected =
  case parseFileGlob version pat of
    Left err -> unless (expected == err) $
      assertFailure $ "Unexpected error: " ++ show err
    Right _ -> assertFailure "Unexpected success in parsing."

globstarTests :: [TestTree]
globstarTests =
  [ testCase "fails to parse on early spec version" $
      testFailParseVersion CabalSpecV2_2 "**/*.html" VersionDoesNotSupportGlobStar
  , testCase "out-of-place double star" $
      testFailParse "blah/**/blah/*.foo" StarInDirectory
  , testCase "multiple double star" $
      testFailParse "blah/**/**/*.foo" StarInDirectory
  , testCase "fails with literal filename" $
      testFailParse "**/a.html" LiteralFileNameGlobStar
  , testCase "with glob filename" $
      testMatches "**/*.html" [GlobMatch "a.html", GlobMatch "b.html", GlobMatch "foo/a.html", GlobMatch "foo/b.html", GlobMatch "foo/bar/a.html", GlobMatch "foo/bar/b.html", GlobMatch "xyz/foo/a.html"]
  , testCase "glob with prefix" $
      testMatches "foo/**/*.html" [GlobMatch "foo/a.html", GlobMatch "foo/b.html", GlobMatch "foo/bar/a.html", GlobMatch "foo/bar/b.html"]
  ]
  where
    testFailParse = testFailParseVersion CabalSpecV2_4
    testMatches = testMatchesVersion CabalSpecV2_4

multiDotTests :: [TestTree]
multiDotTests =
  [ testCase "pre-2.4 single extension not matching multiple" $
      testMatchesVersion CabalSpecV2_2 "foo/*.gz" [GlobWarnMultiDot "foo/a.html.gz", GlobWarnMultiDot "foo/a.tex.gz", GlobWarnMultiDot "foo/b.html.gz", GlobMatch "foo/x.gz"]
  , testCase "doesn't match literal" $
      testMatches "foo/a.tex" [GlobMatch "foo/a.tex"]
  , testCase "works" $
      testMatches "foo/*.gz" [GlobMatch "foo/a.html.gz", GlobMatch "foo/a.tex.gz", GlobMatch "foo/b.html.gz", GlobMatch "foo/x.gz"]
  , testCase "works with globstar" $
      testMatches "foo/**/*.gz" [GlobMatch "foo/a.html.gz", GlobMatch "foo/a.tex.gz", GlobMatch "foo/b.html.gz", GlobMatch "foo/x.gz", GlobMatch "foo/bar/a.html.gz", GlobMatch "foo/bar/a.tex.gz", GlobMatch "foo/bar/b.html.gz"]
  ]
  where
    testMatches = testMatchesVersion CabalSpecV2_4

tests :: [TestTree]
tests =
  [ testGroup "pre-2.4 compatibility" $
      compatibilityTests CabalSpecV2_2
  , testGroup "post-2.4 compatibility" $
      compatibilityTests CabalSpecV2_4
  , testGroup "globstar" globstarTests
  , testCase "pre-1.6 rejects globbing" $
      testFailParseVersion CabalSpecV1_4 "foo/*.bar" VersionDoesNotSupportGlob
  , testGroup "multi-dot globbing" multiDotTests
  ]
