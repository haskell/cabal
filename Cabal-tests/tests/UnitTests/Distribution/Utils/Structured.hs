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
<<<<<<< HEAD
    0xe40d8d67b85712f245354657d7a80165

md5CheckLocalBuildInfo :: Proxy LocalBuildInfo -> Assertion
md5CheckLocalBuildInfo proxy = md5Check proxy
    0x94827844fdb1afedee525061749fb16f
=======
#if MIN_VERSION_base(4,19,0)
    0x6639f65b143830a97e9c4f448b9cabb0
#else
    0x855933700dccfbcc1d642e3470c3702c
#endif

md5CheckLocalBuildInfo :: Proxy LocalBuildInfo -> Assertion
md5CheckLocalBuildInfo proxy = md5Check proxy
#if MIN_VERSION_base(4,19,0)
    0x2ae73730f60c7c947e2cb63c4aac1e54
#else
    0x906cbfdef0bcdfe5734499cfabc615f5
#endif
>>>>>>> 00835c075 (Registered the NamedDefaults language extension (#9740))
