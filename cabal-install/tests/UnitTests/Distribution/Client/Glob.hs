{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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
      [ testCase "GlobPiece" $ structureHash (Proxy :: Proxy GlobPiece) @?= Fingerprint 0xd5e5361866a30ea2 0x31fbfe7b58864782
      , testCase "FilePathGlobRel" $ structureHash (Proxy :: Proxy FilePathGlobRel) @?= Fingerprint 0x76fa5bcb865a8501 0xb152f68915316f98
      , testCase "FilePathRoot" $ structureHash (Proxy :: Proxy FilePathRoot) @?= Fingerprint 0x713373d51426ec64 0xda7376a38ecee5a5
      , testCase "FilePathGlob" $ structureHash (Proxy :: Proxy FilePathGlob) @?= Fingerprint 0x3c11c41f3f03a1f0 0x96e69d85c37d0024
      ]
  ]

-- TODO: [nice to have] tests for trivial globs, tests for matching,
-- tests for windows style file paths

prop_roundtrip_printparse :: FilePathGlob -> Property
prop_roundtrip_printparse pathglob =
  counterexample (prettyShow pathglob) $
    eitherParsec (prettyShow pathglob) === Right pathglob

-- first run, where we don't even call updateMonitor
testParseCases :: Assertion
testParseCases = do
  FilePathGlob (FilePathRoot "/") GlobDirTrailing <- testparse "/"
  FilePathGlob FilePathHomeDir GlobDirTrailing <- testparse "~/"

  FilePathGlob (FilePathRoot "A:\\") GlobDirTrailing <- testparse "A:/"
  FilePathGlob (FilePathRoot "Z:\\") GlobDirTrailing <- testparse "z:/"
  FilePathGlob (FilePathRoot "C:\\") GlobDirTrailing <- testparse "C:\\"
  FilePathGlob FilePathRelative (GlobFile [Literal "_:"]) <- testparse "_:"

  FilePathGlob
    FilePathRelative
    (GlobFile [Literal "."]) <-
    testparse "."

  FilePathGlob
    FilePathRelative
    (GlobFile [Literal "~"]) <-
    testparse "~"

  FilePathGlob
    FilePathRelative
    (GlobDir [Literal "."] GlobDirTrailing) <-
    testparse "./"

  FilePathGlob
    FilePathRelative
    (GlobFile [Literal "foo"]) <-
    testparse "foo"

  FilePathGlob
    FilePathRelative
    ( GlobDir
        [Literal "foo"]
        (GlobFile [Literal "bar"])
      ) <-
    testparse "foo/bar"

  FilePathGlob
    FilePathRelative
    ( GlobDir
        [Literal "foo"]
        (GlobDir [Literal "bar"] GlobDirTrailing)
      ) <-
    testparse "foo/bar/"

  FilePathGlob
    (FilePathRoot "/")
    ( GlobDir
        [Literal "foo"]
        (GlobDir [Literal "bar"] GlobDirTrailing)
      ) <-
    testparse "/foo/bar/"

  FilePathGlob
    (FilePathRoot "C:\\")
    ( GlobDir
        [Literal "foo"]
        (GlobDir [Literal "bar"] GlobDirTrailing)
      ) <-
    testparse "C:\\foo\\bar\\"

  FilePathGlob
    FilePathRelative
    (GlobFile [WildCard]) <-
    testparse "*"

  FilePathGlob
    FilePathRelative
    (GlobFile [WildCard, WildCard]) <-
    testparse "**" -- not helpful but valid
  FilePathGlob
    FilePathRelative
    (GlobFile [WildCard, Literal "foo", WildCard]) <-
    testparse "*foo*"

  FilePathGlob
    FilePathRelative
    (GlobFile [Literal "foo", WildCard, Literal "bar"]) <-
    testparse "foo*bar"

  FilePathGlob
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

testparse :: String -> IO FilePathGlob
testparse s =
  case eitherParsec s of
    Right p -> return p
    Left err -> throwIO $ HUnitFailure Nothing ("expected parse of: " ++ s ++ " -- " ++ err)

parseFail :: String -> Assertion
parseFail s =
  case eitherParsec s :: Either String FilePathGlob of
    Right p -> throwIO $ HUnitFailure Nothing ("expected no parse of: " ++ s ++ " -- " ++ show p)
    Left _ -> return ()
