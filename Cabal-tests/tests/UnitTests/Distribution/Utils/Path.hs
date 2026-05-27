{-# LANGUAGE DataKinds #-}

module UnitTests.Distribution.Utils.Path
  ( tests
  ) where

import Distribution.Utils.Path
  ( (</>)
  , makeRelativePathEx, makeSymbolicPath, relativePathMaybe
  )

import Test.Tasty
import Test.Tasty.HUnit

tests :: [TestTree]
tests =
  [ testCase "relativePathMaybe: direct child" $
      relativePathMaybe
        (makeSymbolicPath $ "a" </> "b")
        (makeSymbolicPath $ "a" </> "b" </> "c")
        @?= Just (makeRelativePathEx "c")
  , testCase "relativePathMaybe: deeper nesting" $
      relativePathMaybe
        (makeSymbolicPath "a")
        (makeSymbolicPath $ "a" </> "b" </> "c")
        @?= Just (makeRelativePathEx $ "b" </> "c")
  , testCase "relativePathMaybe: unrelated path" $
      relativePathMaybe
        (makeSymbolicPath $ "a" </> "b")
        (makeSymbolicPath $ "x" </> "y")
        @?= Nothing
  , testCase "relativePathMaybe: partial prefix is not a match" $
      relativePathMaybe
        (makeSymbolicPath $ "a" </> "bc")
        (makeSymbolicPath $ "a" </> "bcd" </> "e")
        @?= Nothing
  ]
