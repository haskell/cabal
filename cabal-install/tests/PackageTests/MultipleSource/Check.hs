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

tests :: TestsPaths -> [TF.Test]
tests paths =
    [ testCase "finds second source of multiple source" $ do
          sandboxExists <- doesDirectoryExist $ dir </> ".cabal-sandbox"
          when sandboxExists $
            void $ cabal_sandbox paths dir ["delete"]
          assertSandboxSucceeded =<< cabal_sandbox paths dir ["init"]
          assertSandboxSucceeded =<< cabal_sandbox paths dir ["add-source", "p"]
          assertSandboxSucceeded =<< cabal_sandbox paths dir ["add-source", "q"]
          assertInstallSucceeded =<< cabal_install paths dir ["q"]
    ]
