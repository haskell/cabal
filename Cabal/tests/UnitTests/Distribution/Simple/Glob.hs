module UnitTests.Distribution.Simple.Glob
    ( tests
    ) where

import Control.Monad
import Data.Foldable (for_)
import Data.Function (on)
import Data.List (sort)
import Distribution.Simple.Glob
import qualified Distribution.Verbosity as Verbosity
import Distribution.Version
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
  , "c.en.html"
  , "foo/a"
  , "foo/a.html"
  , "foo/a.html.gz"
  , "foo/a.tex"
  , "foo/a.tex.gz"
  , "foo/b.html"
  , "foo/b.html.gz"
  , "foo/x.gz"
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

compatibilityTests :: Version -> [TestTree]
compatibilityTests version =
  [ testCase "literal match" $
      testMatches "foo/a" ["foo/a"]
  , testCase "literal no match on prefix" $
      testNoMatches "foo/c.html"
  , testCase "literal no match on suffix" $
      testMatches "foo/a.html" ["foo/a.html"]
  , testCase "literal no prefix" $
      testMatches "a" ["a"]
  , testCase "literal multiple prefix" $
      testMatches "foo/bar/a.html" ["foo/bar/a.html"]
  , testCase "glob" $
      testMatches "*.html" ["a.html", "b.html"]
  , testCase "glob in subdir" $
      testMatches "foo/*.html" ["foo/a.html", "foo/b.html"]
  , testCase "glob multiple extensions" $
      testMatches "foo/*.html.gz" ["foo/a.html.gz", "foo/b.html.gz"]
  , testCase "glob single extension not matching multiple" $
      testMatches "foo/*.gz" ["foo/x.gz"]
  , testCase "glob in deep subdir" $
      testMatches "foo/bar/*.tex" ["foo/bar/a.tex"]
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
    testNoMatches = testNoMatchesVersion version
    testFailParse = testFailParseVersion version

-- For efficiency reasons, matchDirFileGlob isn't a simple call to
-- getDirectoryContentsRecursive and then a filter with
-- fileGlobMatches. So test both that naive approach and the actual
-- approach to make sure they are both correct.
--
-- TODO: Work out how to construct the sample tree once for all tests,
-- rather than once for each test.
testMatchesVersion :: Version -> FilePath -> [FilePath] -> Assertion
testMatchesVersion version pat expected = do
  -- Test the pure glob matcher.
  case parseFileGlob version pat of
    Left _ -> assertFailure "Couldn't compile the pattern."
    Right globPat ->
      let actual = filter (fileGlobMatches globPat) sampleFileNames
      in unless (sort expected == sort actual) $
           assertFailure $ "Unexpected result (pure matcher): " ++ show actual
  -- ...and the impure glob matcher.
  withSystemTempDirectory "globstar-sample" $ \tmpdir -> do
    makeSampleFiles tmpdir
    actual <- matchDirFileGlob Verbosity.normal version tmpdir pat
    unless (isEqual actual expected) $
      assertFailure $ "Unexpected result (impure matcher): " ++ show actual
  where
    isEqual = (==) `on` (sort . fmap normalise)

-- TODO: Unify this and testMatchesVersion. Can't do this yet because
-- matchDirFileGlob calls die' when it doesn't match anything.
testNoMatchesVersion :: Version -> FilePath -> Assertion
testNoMatchesVersion version pat =
  case parseFileGlob version pat of
    Left _ -> assertFailure "Couldn't compile the pattern."
    Right globPat ->
      let actual = filter (fileGlobMatches globPat) sampleFileNames
      in unless (null actual) $
           assertFailure $ "Unexpected result (pure matcher): " ++ show actual

testFailParseVersion :: Version -> FilePath -> GlobSyntaxError -> Assertion
testFailParseVersion version pat expected =
  case parseFileGlob version pat of
    Left err -> unless (expected == err) $
      assertFailure $ "Unexpected error: " ++ show err
    Right _ -> assertFailure "Unexpected success in parsing."

globstarTests :: [TestTree]
globstarTests =
  [ testCase "fails to parse on early spec version" $
      testFailParseVersion (mkVersion [2,2]) "**/*.html" VersionDoesNotSupportGlobStar
  , testCase "out-of-place double star" $
      testFailParse "blah/**/blah/*.foo" StarInDirectory
  , testCase "multiple double star" $
      testFailParse "blah/**/**/*.foo" StarInDirectory
  , testCase "fails with literal filename" $
      testFailParse "**/a.html" LiteralFileNameGlobStar
  , testCase "with glob filename" $
      testMatches "**/*.html" ["a.html", "b.html", "foo/a.html", "foo/b.html", "foo/bar/a.html", "foo/bar/b.html", "xyz/foo/a.html"]
  , testCase "glob with prefix" $
      testMatches "foo/**/*.html" ["foo/a.html", "foo/b.html", "foo/bar/a.html", "foo/bar/b.html"]
  ]
  where
    testFailParse = testFailParseVersion (mkVersion [3,0])
    testMatches = testMatchesVersion (mkVersion [3,0])

tests :: [TestTree]
tests =
  [ testGroup "pre-3.0 compatibility" $
      compatibilityTests (mkVersion [2,2])
  , testGroup "post-3.0 compatibility" $
      compatibilityTests (mkVersion [3,0])
  , testGroup "globstar" globstarTests
  , testCase "pre-1.6 rejects globbing" $
      testFailParseVersion (mkVersion [1,4]) "foo/*.bar" VersionDoesNotSupportGlob
  ]
