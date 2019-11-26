{-# LANGUAGE OverloadedStrings #-}

module UnitTests.Distribution.C2Hs ( tests ) where

import Distribution.C2Hs
import Distribution.Verbosity (normal)

import Test.Tasty
import Test.Tasty.HUnit

tests :: [TestTree]
tests = [ testCase "libarchive" $
            reorderC2Hs normal ["tests/c2hsTestModules/src"] ["Codec.Archive.Foreign.Archive", "Codec.Archive.Types.Foreign"] 
                >>= (@?= ["Codec.Archive.Types.Foreign", "Codec.Archive.Foreign.Archive"])
        , testCase "libarchive" $
            reorderC2Hs normal ["tests/c2hsTestModules/src"] ["Codec.Archive.Types.Foreign", "Codec.Archive.Foreign.Archive"] 
                >>= (@?= ["Codec.Archive.Types.Foreign", "Codec.Archive.Foreign.Archive"])
        ]
