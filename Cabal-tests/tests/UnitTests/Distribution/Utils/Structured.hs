{-# LANGUAGE CPP #-}
module UnitTests.Distribution.Utils.Structured (tests) where

import Data.Proxy                    (Proxy (..))
import Distribution.Utils.MD5        (md5FromInteger)
import Distribution.Utils.Structured (structureHash, Structured)
import Test.Tasty                    (TestTree, testGroup)
import Test.Tasty.HUnit              (testCase, (@?=), Assertion)

import Distribution.SPDX.License       (License)
import Distribution.Types.VersionRange (VersionRange)

import Distribution.Types.GenericPackageDescription (GenericPackageDescription)
import Distribution.Types.LocalBuildInfo            (LocalBuildInfo)

import UnitTests.Orphans ()

tests :: TestTree
tests = testGroup "Distribution.Utils.Structured"
    -- This test also verifies that structureHash doesn't loop.
    [ testCase "VersionRange" $
      md5Check (Proxy :: Proxy VersionRange) 0x39396fc4f2d751aaa1f94e6d843f03bd
    , testCase "SPDX.License" $
      md5Check (Proxy :: Proxy License) 0xd3d4a09f517f9f75bc3d16370d5a853a
    -- The difference is in encoding of newtypes
    , testCase "GenericPackageDescription" $ md5CheckGenericPackageDescription (Proxy :: Proxy GenericPackageDescription)
    , testCase "LocalBuildInfo" $ md5CheckLocalBuildInfo (Proxy :: Proxy LocalBuildInfo)
    ]

md5Check :: Structured a => Proxy a -> Integer -> Assertion
md5Check proxy md5Int = structureHash proxy @?= md5FromInteger md5Int

md5CheckGenericPackageDescription :: Proxy GenericPackageDescription -> Assertion
md5CheckGenericPackageDescription proxy = md5Check proxy
#if MIN_VERSION_base(4,19,0)
    0x4acd7857947385180d814f36dc1a759e
#else
    0x3ff3fa6c3c570bcafa10b457b1208cc8
#endif

md5CheckLocalBuildInfo :: Proxy LocalBuildInfo -> Assertion
md5CheckLocalBuildInfo proxy = md5Check proxy
#if MIN_VERSION_base(4,19,0)
    0x552eca9ce2e4a34e74deff571f279fc4
#else
    0x48497d6b3f15df06f1107b81b98febe1
#endif
