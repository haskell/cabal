module UnitTests.Distribution.Utils.Structured (tests) where

import Data.Proxy                    (Proxy (..))
import Distribution.Utils.MD5        (md5FromInteger)
import Distribution.Utils.Structured (structureHash, Structured)
import Test.Tasty                    (TestTree, testGroup)
import Test.Tasty.HUnit              (testCase, (@?=), Assertion)

import Distribution.SPDX.License       (License)
import Distribution.Types.VersionRange (VersionRange)

import Distribution.Types.GenericPackageDescription (GenericPackageDescription)
import Distribution.Types.LocalBuildInfo            (PersistedBuildInfo)

tests :: TestTree
tests = testGroup "Distribution.Utils.Structured"
    -- This test also verifies that structureHash doesn't loop.
    [ testCase "VersionRange" $
      md5Check (Proxy :: Proxy VersionRange) 0x39396fc4f2d751aaa1f94e6d843f03bd
    , testCase "SPDX.License" $
      md5Check (Proxy :: Proxy License) 0x2804cd5d2137508059c16ac2449741d2
    -- The difference is in encoding of newtypes
    , testCase "GenericPackageDescription" $ md5CheckGenericPackageDescription (Proxy :: Proxy GenericPackageDescription)
    , testCase "PersistedBuildInfo" $ md5CheckPersistedBuildInfo (Proxy :: Proxy PersistedBuildInfo)
    ]

md5Check :: Structured a => Proxy a -> Integer -> Assertion
md5Check proxy md5Int = structureHash proxy @?= md5FromInteger md5Int

-- NB: if you need to update these values locally, you can run:
--
-- > cabal run Cabal-tests:unit-tests -- -p "/Structured/"

md5CheckGenericPackageDescription :: Proxy GenericPackageDescription -> Assertion
md5CheckGenericPackageDescription proxy = md5Check proxy
    0x007316e7fd40543810a54a4d35883343

md5CheckPersistedBuildInfo :: Proxy PersistedBuildInfo -> Assertion
md5CheckPersistedBuildInfo proxy = md5Check proxy
    0x0563d730a1425415e5fd9f9c46268d0b
