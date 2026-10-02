{-# LANGUAGE DeriveGeneric #-}
module UnitTests.Distribution.Utils.Structured (tests) where

import GHC.Generics                  (Generic)
import Data.Proxy                    (Proxy (..))
import Distribution.Utils.MD5        (md5FromInteger)
import Distribution.Utils.Structured (structureHash, Structured)
import Test.Tasty                    (TestTree, testGroup)
import Test.Tasty.HUnit              (testCase, (@?=), Assertion, assertBool)

import Distribution.SPDX.License       (License)
import Distribution.Types.VersionRange (VersionRange)

import Distribution.Types.GenericPackageDescription (GenericPackageDescription)
import Distribution.Types.LocalBuildInfo            (LocalBuildInfo)

tests :: TestTree
tests = testGroup "Distribution.Utils.Structured"
    -- This test also verifies that structureHash doesn't loop.
    [ testCase "VersionRange" $
      md5Check (Proxy :: Proxy VersionRange) 0x8b8b42a03d1d7956502eeb9d8394fa4d
    , testCase "SPDX.License" $
      md5Check (Proxy :: Proxy License) 0x402a3ee7d7da906779e1b0a64ff042e4
    -- The difference is in encoding of newtypes
    , testCase "GenericPackageDescription" $ md5CheckGenericPackageDescription (Proxy :: Proxy GenericPackageDescription)
    , testCase "LocalBuildInfo" $ md5CheckLocalBuildInfo (Proxy :: Proxy LocalBuildInfo)
    , testCase "Issue #12360: A + BC vs AB + C" $
      assertBool "T1_Prefix_A and T1_Prefix_ hashes must differ" (structureHash (Proxy :: Proxy T1_Prefix_A) /= structureHash (Proxy :: Proxy T1_Prefix_))
    , testCase "Issue #12360: 2 constructors vs 1 constructor boundary" $
      assertBool "T2_A and T2_AT2_ hashes must differ" (structureHash (Proxy :: Proxy T2_A) /= structureHash (Proxy :: Proxy T2_AT2_))
    , testCase "Issue #12360: Unicode boundary" $
      assertBool "Unicode boundary hashes must differ" (structureHash (Proxy :: Proxy T3_U_) /= structureHash (Proxy :: Proxy T3_U_Λ_))
    ]

-- Types for testing string boundary confusion (issue #12360)
data T1_Prefix_A = B_Suffix deriving (Generic)
data T1_Prefix_ = AB_Suffix deriving (Generic)
instance Structured T1_Prefix_A
instance Structured T1_Prefix_

data T2_A = T2_B | T2_C1 deriving (Generic)
data T2_AT2_ = BT2_C1 deriving (Generic)
instance Structured T2_A
instance Structured T2_AT2_

data T3_U_ = Λ_Suffix deriving (Generic)
data T3_U_Λ_ = Suffix deriving (Generic)
instance Structured T3_U_
instance Structured T3_U_Λ_

md5Check :: Structured a => Proxy a -> Integer -> Assertion
md5Check proxy md5Int = structureHash proxy @?= md5FromInteger md5Int

-- NB: if you need to update these values locally, you can run:
--
-- > cabal run Cabal-tests:unit-tests -- -p "/Structured/"

md5CheckGenericPackageDescription :: Proxy GenericPackageDescription -> Assertion
md5CheckGenericPackageDescription proxy = md5Check proxy
    0x9a954d83a0a8e766784c35279b08b277

md5CheckLocalBuildInfo :: Proxy LocalBuildInfo -> Assertion
md5CheckLocalBuildInfo proxy = md5Check proxy
    0xb0a6e46660b21eb5fb46d2e615e641a0
