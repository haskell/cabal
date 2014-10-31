module PackageTests.MultipleSource.Check
       ( tests
       ) where


import PackageTests.PackageTester

import Test.Framework                 as TF (Test)
import Test.Framework.Providers.HUnit (testCase)

import Control.Monad    (void, when)
import System.Directory (doesDirectoryExist)
import System.FilePath  ((</>))

dir :: FilePath
dir = packageTestsDirectory </> "MultipleSource"

tests :: FilePath -> [TF.Test]
tests cabalPath =
    [ testCase "finds second source of multiple source" $ do
          sandboxExists <- doesDirectoryExist $ dir </> ".cabal-sandbox"
          let execute cmd params = cmd dir
                                       params
                                       cabalPath
                                       (".." </> packageTestsConfigFile)
          when sandboxExists $
            void $ execute cabal_sandbox ["delete"]
          assertSandboxSucceeded =<< execute cabal_sandbox ["init"]
          assertSandboxSucceeded =<< execute cabal_sandbox ["add-source", "p"]
          assertSandboxSucceeded =<< execute cabal_sandbox ["add-source", "q"]
          assertInstallSucceeded =<< execute cabal_install ["q"]
    ]
