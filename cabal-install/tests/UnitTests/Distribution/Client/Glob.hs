{-# OPTIONS_GHC -Wno-orphans #-}

module UnitTests.Distribution.Client.Glob (tests) where

import Distribution.Client.Compat.Prelude hiding (last)
import Prelude ()

import Distribution.Client.Glob
import Distribution.Utils.Structured (structureHash)
import UnitTests.Distribution.Client.ArbitraryInstances ()

import GHC.Fingerprint (Fingerprint (..))
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

tests :: [TestTree]
tests =
  [ testProperty "print/parse roundtrip" prop_roundtrip_printparse
  , testCase "parse examples" testParseCases
  , testGroup
      "Structured hashes"
      [ testCase "GlobPiece" $ structureHash (Proxy :: Proxy GlobPiece) @?= Fingerprint 0x7cd6657c30b525da 0x1566aae929ba38ff
      , testCase "Glob" $ structureHash (Proxy :: Proxy Glob) @?= Fingerprint 0x61e37abfb606d902 0x7a7f760092e71d94
      , testCase "FilePathRoot" $ structureHash (Proxy :: Proxy FilePathRoot) @?= Fingerprint 0x298a077474c23cf9 0x4fc71c704dba6c5d
      , testCase "RootedGlob" $ structureHash (Proxy :: Proxy RootedGlob) @?= Fingerprint 0x7e542e5dccb3536c 0xda5d0840a2c6086a
      ]
  ]

-- TODO: [nice to have] tests for trivial globs, tests for matching,
-- tests for windows style file paths

prop_roundtrip_printparse :: RootedGlob -> Property
prop_roundtrip_printparse pathglob =
  counterexample (prettyShow pathglob) $
    eitherParsec (prettyShow pathglob) === Right pathglob

-- first run, where we don't even call updateMonitor
testParseCases :: Assertion
testParseCases = do
  RootedGlob (FilePathRoot "/") GlobDirTrailing <- testparse "/"
  RootedGlob FilePathHomeDir GlobDirTrailing <- testparse "~/"

  RootedGlob (FilePathRoot "A:\\") GlobDirTrailing <- testparse "A:/"
  RootedGlob (FilePathRoot "Z:\\") GlobDirTrailing <- testparse "z:/"
  RootedGlob (FilePathRoot "C:\\") GlobDirTrailing <- testparse "C:\\"
  RootedGlob FilePathRelative (GlobFile [Literal "_:"]) <- testparse "_:"

  RootedGlob
    FilePathRelative
    (GlobDirRecursive [WildCard]) <-
    testparse "**/*"
  RootedGlob
    FilePathRelative
    (GlobDir [Literal "foo"] (GlobDirRecursive [WildCard])) <-
    testparse "foo/**/*"
  RootedGlob
    FilePathRelative
    (GlobDir [Literal "foo"] (GlobDirRecursive [WildCard, Literal ".txt"])) <-
    testparse "foo/**/*.txt"

  RootedGlob
    FilePathRelative
    (GlobFile [Literal "."]) <-
    testparse "."

  RootedGlob
    FilePathRelative
    (GlobFile [Literal "~"]) <-
    testparse "~"

  RootedGlob
    FilePathRelative
    (GlobDir [Literal "."] GlobDirTrailing) <-
    testparse "./"

  RootedGlob
    FilePathRelative
    (GlobFile [Literal "foo"]) <-
    testparse "foo"

  RootedGlob
    FilePathRelative
    ( GlobDir
        [Literal "foo"]
        (GlobFile [Literal "bar"])
      ) <-
    testparse "foo/bar"

  RootedGlob
    FilePathRelative
    ( GlobDir
        [Literal "foo"]
        (GlobDir [Literal "bar"] GlobDirTrailing)
      ) <-
    testparse "foo/bar/"

  RootedGlob
    (FilePathRoot "/")
    ( GlobDir
        [Literal "foo"]
        (GlobDir [Literal "bar"] GlobDirTrailing)
      ) <-
    testparse "/foo/bar/"

  RootedGlob
    (FilePathRoot "C:\\")
    ( GlobDir
        [Literal "foo"]
        (GlobDir [Literal "bar"] GlobDirTrailing)
      ) <-
    testparse "C:\\foo\\bar\\"

  RootedGlob
    FilePathRelative
    (GlobFile [WildCard]) <-
    testparse "*"

  RootedGlob
    FilePathRelative
    (GlobFile [WildCard, WildCard]) <-
    testparse "**" -- not helpful, but valid (?)
  RootedGlob
    FilePathRelative
    (GlobFile [WildCard, Literal "foo", WildCard]) <-
    testparse "*foo*"

  RootedGlob
    FilePathRelative
    (GlobFile [Literal "foo", WildCard, Literal "bar"]) <-
    testparse "foo*bar"

  RootedGlob
    FilePathRelative
    (GlobFile [Union [[WildCard], [Literal "foo"]]]) <-
    testparse "{*,foo}"

  parseFail "{"
  parseFail "}"
  parseFail ","
  parseFail "{"
  parseFail "{{}"
  parseFail "{}"
  parseFail "{,}"
  parseFail "{foo,}"
  parseFail "{,foo}"

  return ()

testparse :: String -> IO RootedGlob
testparse s =
  case eitherParsec s of
    Right p -> return p
    Left err -> throwIO $ HUnitFailure Nothing ("expected parse of: " ++ s ++ " -- " ++ err)

parseFail :: String -> Assertion
parseFail s =
  case eitherParsec s :: Either String RootedGlob of
    Right p -> throwIO $ HUnitFailure Nothing ("expected no parse of: " ++ s ++ " -- " ++ show p)
    Left _ -> return ()
