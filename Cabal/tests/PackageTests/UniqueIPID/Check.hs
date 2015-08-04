module PackageTests.UniqueIPID.Check (suite) where

import System.FilePath ((</>))

import PackageTests.PackageTester
    ( PackageSpec(..), SuiteConfig, cabal_register
    , assertRegisterSucceeded, outputText )
import Test.Tasty.HUnit (Assertion, assertFailure)
import Data.List

import Control.Monad ( when )

this :: String
this = "UniqueIPID"

suite :: SuiteConfig -> Assertion
suite config = do
    let dir = "PackageTests" </> this
        spec1 = PackageSpec
                { directory = dir </> "P1"
                , configOpts = []
                , distPref = Nothing
                }
        spec2 = PackageSpec
                { directory = dir </> "P2"
                , configOpts = []
                , distPref = Nothing
                }
    hResult1 <- cabal_register config spec1 ["--print-ipid", "--inplace"]
    assertRegisterSucceeded hResult1
    hResult2 <- cabal_register config spec2 ["--print-ipid", "--inplace"]
    assertRegisterSucceeded hResult2
    when ((exIPID $ outputText hResult1) == (exIPID $ outputText hResult2)) $
      assertFailure $ "cabal has not calculated different Installed " ++
        "package ID when source is changed."
  where
    exIPID s = takeWhile (/= '\n') $
                 head . filter (isPrefixOf $ this ++ "-0.1-") $ (tails s)

