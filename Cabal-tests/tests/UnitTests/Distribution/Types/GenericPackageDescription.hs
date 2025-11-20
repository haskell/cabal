{-# OPTIONS_GHC -Wno-deprecations #-}   -- for importing "Distribution.Compat.Prelude.Internal"

module UnitTests.Distribution.Types.GenericPackageDescription where

import Prelude ()
import Distribution.Compat.Prelude.Internal
import Distribution.Types.GenericPackageDescription

import Test.Tasty
import Test.Tasty.HUnit
import qualified Control.Exception as C

tests :: [TestTree]
tests =
  [ testCase "GenericPackageDescription deepseq" gpdDeepseq
  ]

gpdFields :: [(String, GenericPackageDescription -> GenericPackageDescription)]
gpdFields =
  [ ("packageDescriptionInternal", \gpd -> gpd { packageDescriptionInternal = undefined })
  , ("gpdScannedVersionInternal",  \gpd -> gpd { gpdScannedVersionInternal  = undefined })
  , ("genPackageFlagsInternal",    \gpd -> gpd { genPackageFlagsInternal    = undefined })
  , ("gpdCommonStanzas",           \gpd -> gpd { gpdCommonStanzas           = undefined })
  , ("condLibraryUnmerged",        \gpd -> gpd { condLibraryUnmerged        = undefined })
  , ("condSubLibrariesUnmerged",   \gpd -> gpd { condSubLibrariesUnmerged   = undefined })
  , ("condForeignLibsUnmerged",    \gpd -> gpd { condForeignLibsUnmerged    = undefined })
  , ("condExecutablesUnmerged",    \gpd -> gpd { condExecutablesUnmerged    = undefined })
  , ("condTestSuitesUnmerged",     \gpd -> gpd { condTestSuitesUnmerged     = undefined })
  , ("condBenchmarksUnmerged",     \gpd -> gpd { condBenchmarksUnmerged     = undefined })
  ]

gpdDeepseq :: Assertion
gpdDeepseq = sequence_
  [ throwsUndefined msg (f emptyGenericPackageDescription) | (msg, f) <- gpdFields ]

throwsUndefined :: NFData a => String -> a -> Assertion
throwsUndefined field a =
  C.catch (C.evaluate (rnf a) >> assertFailure ("Deepseq failed to evaluate " ++ show field))
          (\(C.ErrorCall _) -> return ())
