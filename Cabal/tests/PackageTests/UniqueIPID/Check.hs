module PackageTests.UniqueIPID.Check (suite) where

import System.FilePath ((</>))

import PackageTests.PackageTester
import Test.Tasty.HUnit (Assertion, assertFailure)
import Data.List
import Distribution.Compat.Exception

import Control.Monad ( when )
import System.Directory

this :: String
this = "UniqueIPID"

suite :: SuiteConfig -> Assertion
suite config = do
    let dir = "PackageTests" </> this
        db = "tmp.package.conf"
        spec1 = PackageSpec
                { directory = dir </> "P1"
                , configOpts = ["--package-db", ".." </> db]
                , distPref = Nothing
                }
        spec2 = PackageSpec
                { directory = dir </> "P2"
                , configOpts = ["--package-db", ".." </> db]
                , distPref = Nothing
                }
    removeDirectoryRecursive (dir </> db) `catchIO` const (return ())
    _ <- run Nothing (ghcPkgPath config) [] ["init", dir </> db]
    _ <- cabal_configure config spec1
    _ <- cabal_configure config spec2
    _ <- cabal_build config spec1
    _ <- cabal_build config spec1 -- test rebuild cycle works
    hResult1 <- cabal_register config spec1 ["--print-ipid", "--inplace"]
    assertRegisterSucceeded hResult1
    _ <- cabal_build config spec2
    hResult2 <- cabal_register config spec2 ["--print-ipid", "--inplace"]
    assertRegisterSucceeded hResult2
    when ((exIPID $ outputText hResult1) == (exIPID $ outputText hResult2)) $
      assertFailure $ "cabal has not calculated different Installed " ++
        "package ID when source is changed."
  where
    exIPID s = takeWhile (/= '\n') $
                 head . filter (isPrefixOf $ this ++ "-0.1-") $ (tails s)

