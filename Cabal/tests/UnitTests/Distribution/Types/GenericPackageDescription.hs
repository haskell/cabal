{-# OPTIONS_GHC -fno-warn-deprecations #-}   -- for importing "Distribution.Compat.Prelude.Internal"

module UnitTests.Distribution.Types.GenericPackageDescription where

import Prelude ()
import Distribution.Compat.Prelude.Internal
import Distribution.Types.GenericPackageDescription

import qualified Control.Exception as C
import Control.Monad
import Test.Tasty
import Test.Tasty.HUnit

tests :: [TestTree]
tests =
  [ testCase "GenericPackageDescription deepseq" gpdDeepseq
  , testCase "findDuplicateFlagAssignments" testFindDuplicateFlagAssignments
  ]

----- Verify "NFData GenericPackageDescription" instance ----------------------

gpdFields :: [(String, GenericPackageDescription -> GenericPackageDescription)]
gpdFields =
  [ ("packageDescription", \gpd -> gpd { packageDescription = undefined })
  , ("genPackageFlags",    \gpd -> gpd { genPackageFlags    = undefined })
  , ("condLibrary",        \gpd -> gpd { condLibrary        = undefined })
  , ("condSubLibraries",   \gpd -> gpd { condSubLibraries   = undefined })
  , ("condForeignLibs",    \gpd -> gpd { condForeignLibs    = undefined })
  , ("condExecutables",    \gpd -> gpd { condExecutables    = undefined })
  , ("condTestSuites",     \gpd -> gpd { condTestSuites     = undefined })
  , ("condBenchmarks",     \gpd -> gpd { condBenchmarks     = undefined })
  ]

gpdDeepseq :: Assertion
gpdDeepseq = sequence_
  [ throwsUndefined msg (f emptyGenericPackageDescription) | (msg, f) <- gpdFields ]

throwsUndefined :: NFData a => String -> a -> Assertion
throwsUndefined field a =
  C.catch (C.evaluate (rnf a) >> assertFailure ("Deepseq failed to evaluate " ++ show field))
          (\(C.ErrorCall _) -> return ())

----- Verify duplicate flag recognition in "FlagAssignment" -------------------

testFindDuplicateFlagAssignments :: Assertion
testFindDuplicateFlagAssignments = do
  let flag n v = (mkFlagName n, v)
      findDuplicates = findDuplicateFlagAssignments . mkFlagAssignment
  forM_ [ (b1,b2) | b1 <- [True,False], b2 <- [True,False] ] $ \(v1,v2) -> do
    assertEqual "" [] (findDuplicates [flag "foo" v1, flag "bar" v2])
    assertEqual "" [mkFlagName "foo"] (findDuplicates [flag "foo" v1, flag "FOO" v2])
