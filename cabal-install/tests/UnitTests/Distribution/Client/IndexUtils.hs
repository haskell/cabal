module UnitTests.Distribution.Client.IndexUtils where

import Distribution.Client.IndexUtils
import qualified Distribution.Compat.NonEmptySet as NES
import Distribution.Simple.Utils (toUTF8LBS)
import Distribution.Types.Dependency
import Distribution.Types.LibraryName
import Distribution.Types.PackageName
import Distribution.Version

import Test.Tasty
import Test.Tasty.HUnit

tests :: [TestTree]
tests =
  [ simpleVersionsParserTests
  ]

simpleVersionsParserTests :: TestTree
simpleVersionsParserTests =
  testGroup
    "Simple preferred-versions Parser Tests"
    [ testCase "simple deprecation dependency" $ do
        let prefs = parsePreferredVersionsWarnings (toUTF8LBS "binary < 0.9.0.0 || > 0.9.0.0")
        prefs
          @?= [ Right
                  ( Dependency
                      (mkPackageName "binary")
                      ( unionVersionRanges
                          (earlierVersion $ mkVersion [0, 9, 0, 0])
                          (laterVersion $ mkVersion [0, 9, 0, 0])
                      )
                      (NES.singleton LMainLibName)
                  )
              ]
    , testCase "multiple deprecation dependency" $ do
        let prefs = parsePreferredVersionsWarnings (toUTF8LBS "binary < 0.9.0.0 || > 0.9.0.0\ncontainers == 0.6.4.1")
        prefs
          @?= [ Right
                  ( Dependency
                      (mkPackageName "binary")
                      ( unionVersionRanges
                          (earlierVersion $ mkVersion [0, 9, 0, 0])
                          (laterVersion $ mkVersion [0, 9, 0, 0])
                      )
                      (NES.singleton LMainLibName)
                  )
              , Right
                  ( Dependency
                      (mkPackageName "containers")
                      (thisVersion $ mkVersion [0, 6, 4, 1])
                      (NES.singleton LMainLibName)
                  )
              ]
    , testCase "unparsable dependency" $ do
        let prefs = parsePreferredVersionsWarnings (toUTF8LBS "binary 0.9.0.0 || > 0.9.0.0")
        prefs
          @?= [ Left binaryDepParseError
              ]
    , testCase "partial parse" $ do
        let prefs = parsePreferredVersionsWarnings (toUTF8LBS "binary 0.9.0.0 || > 0.9.0.0\ncontainers == 0.6.4.1")
        prefs
          @?= [ Left binaryDepParseError
              , Right
                  ( Dependency
                      (mkPackageName "containers")
                      (thisVersion $ mkVersion [0, 6, 4, 1])
                      (NES.singleton LMainLibName)
                  )
              ]
    ]
  where
    binaryDepParseError =
      PreferredVersionsParseError
        { preferredVersionsParsecError =
            mconcat
              [ "\"<eitherParsec>\" (line 1, column 8):\n"
              , "unexpected '0'\n"
              , "expecting space, white space, opening paren, operator or end of input"
              ]
        , preferredVersionsOriginalDependency = "binary 0.9.0.0 || > 0.9.0.0"
        }
