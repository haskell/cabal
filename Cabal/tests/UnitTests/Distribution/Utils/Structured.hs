{-# LANGUAGE CPP #-}
module UnitTests.Distribution.Utils.Structured (tests) where

import Data.Proxy                    (Proxy (..))
import Distribution.Utils.Structured (structureHash)
import GHC.Fingerprint               (Fingerprint (..))
import Test.Tasty                    (TestTree, testGroup)
import Test.Tasty.HUnit              (testCase, (@?=))

import Distribution.SPDX.License       (License)
import Distribution.Types.VersionRange (VersionRange)

#if MIN_VERSION_base(4,7,0)
import Distribution.Types.LocalBuildInfo (LocalBuildInfo)
#endif

import UnitTests.Orphans ()

tests :: TestTree
tests = testGroup "Distribution.Utils.Structured"
    -- This test also verifies that structureHash doesn't loop.
    [ testCase "VersionRange"   $ structureHash (Proxy :: Proxy VersionRange)   @?= Fingerprint 0x6a33c568c9307696 0xe383268b2389a958
    , testCase "SPDX.License"   $ structureHash (Proxy :: Proxy License)        @?= Fingerprint 0xd3d4a09f517f9f75 0xbc3d16370d5a853a
    -- The difference is in encoding of newtypes
#if MIN_VERSION_base(4,7,0)
    , testCase "LocalBuildInfo" $ structureHash (Proxy :: Proxy LocalBuildInfo) @?= Fingerprint 0xe0f5a6119058a02c 0x703cb61a72aa4564
#endif
    ]
