module UnitTests.Distribution.Glob
  ( tests
  ) where

import Data.Maybe
    ( isNothing )
import Distribution.Glob
import Test.Tasty
import Test.Tasty.HUnit

data Result
    = DoesMatch
    | DoesNotMatch
    deriving (Show, Eq, Ord)

tests :: [TestTree]
tests =
  [ testGroup "Unparseable globs"
      (map testDoesNotCompile dataDoesNotCompile)
  , testGroup "Glob matches"
      (map (testMatches DoesMatch) dataDoesMatch)
  , testGroup "Glob mismatches"
      (map (testMatches DoesNotMatch) dataDoesNotMatch)
  ]
  where
  testDoesNotCompile str =
    testCase str
      (assertBool "Expected parse to fail"
        (isNothing (parseFileGlob str)))

  testMatches r (input, expecteds) =
    case parseFileGlob input of
      Just glob ->
        testGroup input (map (testMatch r) (map (\e -> (glob, e)) expecteds))
      Nothing ->
        testCase input (assertFailure "Expected parse to succeed")

  testMatch r (glob, filepath) =
    testCase filepath (assertGlob r)
    where
    matchSuccess = isMatch glob filepath

    assertGlob DoesMatch =
      assertBool "Expected glob to match" matchSuccess
    assertGlob DoesNotMatch =
      assertBool "Expected glob to not match" (not matchSuccess)

-- TODO: Test with Unicode filenames.

dataDoesNotCompile :: [String]
dataDoesNotCompile =
  [ "{unterminated,"
  , "[unterminated"

  -- empty choice
  , "{}"

  -- bad range
  , "[z-a]"

  -- unescaped "!"
  , "[abc!]"

  -- unescaped "^"
  , "[ads^]"

  -- escaped path separator
  , "hello\\/world"

  -- Path separator in CharList
  , "[abc/]"
  , "[\\]"
  , "[abc\\/]"
  ]

dataDoesMatch :: [(String, [String])]
dataDoesMatch =
  [ ("dictionary.txt",
      [ "dictionary.txt"
      ])

  , ("hello/world.txt",
      [ "hello/world.txt"
      ])

  , ("hello/world/a.txt",
      [ "hello/world/a.txt"
      ])

  , ("[abc]",
      [ "a"
      , "b"
      , "c"
      ])

  , ("[a-z0-9]",
      [ "a"
      , "m"
      , "y"
      , "z"
      , "0"
      , "5"
      ])

  , ("[a-z][0-9]",
      [ "a3"
      , "m0"
      , "y9"
      , "z2"
      , "a4"
      , "b8"
      ])

  , ("hello[wW]orld",
      [ "helloworld"
      , "helloWorld"
      ])

  , ("hello[!AaBb]orld",
      [ "helloworld"
      , "helloWorld"
      ])

  , ("*",
      [ "hello"
      , "helloworld"
      ])

  , ("**",
      [ "hello"
      , "helloworld"
      , "hello/world"
      ])

  , ("*.hs",
      [ "foo.hs"
      , "bar.hs"
      ])

  , ("Foo*",
      [ "Foo.hs"
      , "FooBar.hs"
      , "Foo"
      ])

  , ("test/*.hs",
      [ "test/Foo.hs"
      , "test/Bar.hs"
      ])

  , ("test/Foo.*",
      [ "test/Foo."
      , "test/Foo.txt"
      , "test/Foo.hs"
      ])

  , ("{hello,goodbye}",
      [ "hello"
      , "goodbye"
      ])

  , ("tests/**/*.hs",
      [ "tests/Foo.hs"
      , "tests/Foo/Bar.hs"
      , "tests/Foo/Bar/Baz.hs"
      ])

  -- Backslash escaping
  , ("\\[hello\\]",
      [ "[hello]"
      ])

  -- Backslash followed by a non-special character (in terms of globbing)
  -- should be ok
  , ("he\\ll\\o",
      [ "hello"
      ])

  -- choices
  , ("{a,b,c}",
      [ "a"
      , "b"
      , "c"
      ])

  , ("hello{world,}",
      [ "helloworld"
      , "hello"
      ])
  ]

dataDoesNotMatch :: [(String, [String])]
dataDoesNotMatch =
  [ ("hello[!Ww]orld",
      [ "helloWorld"
      , "helloworld"
      ])

  , ("[a-z0-9]",
      [ "a3"
      , "m0"
      , "y9"
      , "z2"
      , "a4"
      , "b8"
      ])

  , ("[a-z][0-9]",
      [ "a"
      , "m"
      , "y"
      , "z"
      , "0"
      , "5"
      ])

  , ("*.hs",
      [ ".hs"
      , ".Foo.hs"
      , ".hso"
      , "Foo.hso"
      ])

  , ("**/*.hs",
      [ ".hs"
      , ".Foo.hs"
      , ".hso"
      , "Foo.hso"
      , "Foo/.Bar.hs"
      , "Foo/.hs"
      ])

  ]
