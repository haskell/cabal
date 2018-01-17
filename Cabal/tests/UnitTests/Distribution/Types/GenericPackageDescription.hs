{-# OPTIONS_GHC -fno-warn-deprecations #-}   -- for importing "Distribution.Compat.Prelude.Internal"

module UnitTests.Distribution.Types.GenericPackageDescription where

import Prelude ()
import Distribution.Compat.Prelude.Internal
import Distribution.Types.GenericPackageDescription

import Test.Tasty
import Test.Tasty.HUnit
import Control.Exception

tests :: [TestTree]
tests =
  [ testCase "GenericPackageDescription deepseq" gpdDeepseq
  ]

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
  catch (evaluate (rnf a) >> assertFailure ("Deepseq failed to evaluate " ++ show field))
        (\(ErrorCall _) -> return ())
