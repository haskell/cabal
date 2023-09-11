{-# LANGUAGE CPP #-}
module UnitTests.Distribution.Utils.Structured (tests) where

import Data.Proxy                    (Proxy (..))
import Distribution.Utils.MD5        (md5FromInteger)
import Distribution.Utils.Structured (structureHash, Structured)
import Test.Tasty                    (TestTree, testGroup)
import Test.Tasty.HUnit              (testCase, (@?=), Assertion)

import Distribution.SPDX.License       (License)
import Distribution.Types.VersionRange (VersionRange)

#if MIN_VERSION_base(4,7,0)
import Distribution.Types.GenericPackageDescription (GenericPackageDescription)
import Distribution.Types.LocalBuildInfo            (LocalBuildInfo)
#endif

import UnitTests.Orphans ()

tests :: TestTree
tests = testGroup "Distribution.Utils.Structured"
    -- This test also verifies that structureHash doesn't loop.
    [ testCase "VersionRange" $
      md5Check (Proxy :: Proxy VersionRange) 0x39396fc4f2d751aaa1f94e6d843f03bd
    , testCase "SPDX.License" $
      md5Check (Proxy :: Proxy License) 0xd3d4a09f517f9f75bc3d16370d5a853a
    -- The difference is in encoding of newtypes
#if MIN_VERSION_base(4,7,0)
    , testCase "GenericPackageDescription" $
      md5Check (Proxy :: Proxy GenericPackageDescription) 0x6ad1e12c6f88291e9b8c131d239eda70
    , testCase "LocalBuildInfo" $
      md5Check (Proxy :: Proxy LocalBuildInfo) 0xbc7ac84a9bc43345c812af222c3e5ba0
#endif
    ]

-- -------------------------------------------------------------------- --
-- utils

md5Check :: Structured a => Proxy a -> Integer -> Assertion
md5Check proxy md5Int = structureHash proxy @?= md5FromInteger md5Int
