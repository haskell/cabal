{-# LANGUAGE CPP #-}
module UnitTests.Distribution.Utils.Structured (tests) where

import Data.Proxy                    (Proxy (..))
import Distribution.Utils.Structured (structureHash)
import GHC.Fingerprint               (Fingerprint (..))
import Test.Tasty                    (TestTree, testGroup)
import Test.Tasty.HUnit              (testCase, (@?=))

import Distribution.SPDX.License         (License)
import Distribution.Types.VersionRange   (VersionRange)

import Distribution.Types.LocalBuildInfo (LocalBuildInfo)

import UnitTests.Orphans ()

tests :: TestTree
tests = testGroup "Distribution.Utils.Structured"
    -- This test also verifies that structureHash doesn't loop.
    [ testCase "VersionRange"   $ structureHash (Proxy :: Proxy VersionRange)   @?= Fingerprint 0x3827faffd22242bf 0xfd0c337e60fc808b
    , testCase "SPDX.License"   $ structureHash (Proxy :: Proxy License)        @?= Fingerprint 0xd3d4a09f517f9f75 0xbc3d16370d5a853a
    -- The difference is in encoding of newtypes
#if MIN_VERSION_base(4,7,0)
    , testCase "LocalBuildInfo" $ structureHash (Proxy :: Proxy LocalBuildInfo) @?= Fingerprint 0xbe2fe6db8173c3ca 0xc9961555f459ba95
#else
    , testCase "LocalBuildInfo" $ structureHash (Proxy :: Proxy LocalBuildInfo) @?= Fingerprint 3881475429727599201 16282833558811085076
#endif
    ]
