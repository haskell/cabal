module PackageTests.MultipleSource.Check
       ( tests
       ) where


import PackageTests.PackageTester

import Test.Framework                 as TF (Test)
import Test.Framework.Providers.HUnit (testCase)

import System.FilePath ((</>))

dir :: FilePath
dir = "PackageTests" </> "MultipleSource"

tests :: FilePath -> [TF.Test]
tests cabalPath =
    [ testCase "finds second source of multiple source" $ do
          -- can fail if there is no pre-existing sandbox
          _ <- cabal_sandbox dir ["delete"] cabalPath
          assertSandboxSucceeded =<< cabal_sandbox dir ["init"] cabalPath
          assertSandboxSucceeded =<< cabal_sandbox dir ["add-source", "p"] cabalPath
          assertSandboxSucceeded =<< cabal_sandbox dir ["add-source", "q"] cabalPath
          assertInstallSucceeded =<< cabal_install dir ["q"] cabalPath
    ]
