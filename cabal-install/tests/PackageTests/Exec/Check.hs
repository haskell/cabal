{-# LANGUAGE CPP #-}
module PackageTests.Exec.Check
       ( tests
       ) where


import PackageTests.PackageTester

import Test.Framework                 as TF (Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit                     (assertBool)

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Monad (when)
import Data.List (intercalate, isInfixOf)
import System.FilePath ((</>))
import System.Directory (getDirectoryContents)

dir :: FilePath
dir = packageTestsDirectory </> "Exec"

tests :: TestsPaths -> [TF.Test]
tests paths =
    [ testCase "exits with failure if given no argument" $ do
          result <- cabal_exec paths dir []
          assertExecFailed result

    , testCase "prints error message if given no argument" $ do
          result <- cabal_exec paths dir []
          assertExecFailed result
          let output = outputText result
              expected = "specify an executable to run"
              errMsg = "should have requested an executable be specified\n" ++
                       output
          assertBool errMsg $
              expected `isInfixOf` (intercalate " " . lines $ output)

    , testCase "runs the given command" $ do
          result <- cabal_exec paths dir ["echo", "this", "string"]
          assertExecSucceeded result
          let output = outputText result
              expected = "this string"
              errMsg = "should have ran the given command\n" ++ output
          assertBool errMsg $
              expected `isInfixOf` (intercalate " " . lines $ output)

    , testCase "can run executables installed in the sandbox" $ do
          -- Test that an executable installed into the sandbox can be found.
          -- We do this by removing any existing sandbox. Checking that the
          -- executable cannot be found. Creating a new sandbox. Installing
          -- the executable and checking it can be run.

          cleanPreviousBuilds paths
          assertMyExecutableNotFound paths
          assertPackageInstall paths

          result <- cabal_exec paths dir ["my-executable"]
          assertExecSucceeded result
          let output = outputText result
              expected = "This is my-executable"
              errMsg = "should have found a my-executable\n" ++ output
          assertBool errMsg $
              expected `isInfixOf` (intercalate " " . lines $ output)

    , testCase "adds the sandbox bin directory to the PATH" $ do
          cleanPreviousBuilds paths
          assertMyExecutableNotFound paths
          assertPackageInstall paths

          result <- cabal_exec paths dir ["bash", "--", "-c", "my-executable"]
          assertExecSucceeded result
          let output = outputText result
              expected = "This is my-executable"
              errMsg = "should have found a my-executable\n" ++ output
          assertBool errMsg $
              expected `isInfixOf` (intercalate " " . lines $ output)

    , testCase "configures GHC to use the sandbox" $ do
          let libNameAndVersion = "my-0.1"

          cleanPreviousBuilds paths
          assertPackageInstall paths

          assertMyLibIsNotAvailableOutsideofSandbox paths libNameAndVersion

          result <- cabal_exec paths dir ["ghc-pkg", "list"]
          assertExecSucceeded result
          let output = outputText result
              errMsg = "my library should have been found"
          assertBool errMsg $
              libNameAndVersion `isInfixOf` (intercalate " " . lines $ output)
          

    -- , testCase "can find executables built from the package" $ do

    , testCase "configures cabal to use the sandbox" $ do
          let libNameAndVersion = "my-0.1"

          cleanPreviousBuilds paths
          assertPackageInstall paths

          assertMyLibIsNotAvailableOutsideofSandbox paths libNameAndVersion

          result <- cabal_exec paths dir ["bash", "--", "-c", "cd subdir ; cabal sandbox hc-pkg list"]
          assertExecSucceeded result
          let output = outputText result
              errMsg = "my library should have been found"
          assertBool errMsg $
              libNameAndVersion `isInfixOf` (intercalate " " . lines $ output)
    ]

cleanPreviousBuilds :: TestsPaths -> IO ()
cleanPreviousBuilds paths = do
    sandboxExists <- not . null . filter (== "cabal.sandbox.config") <$>
                         getDirectoryContents dir
    assertCleanSucceeded   =<< cabal_clean paths dir []
    when sandboxExists $ do
        assertSandboxSucceeded =<< cabal_sandbox paths dir ["delete"]


assertPackageInstall :: TestsPaths -> IO ()
assertPackageInstall paths = do
    assertSandboxSucceeded =<< cabal_sandbox paths dir ["init"]
    assertInstallSucceeded =<< cabal_install paths dir []


assertMyExecutableNotFound :: TestsPaths -> IO ()
assertMyExecutableNotFound paths = do
    result <- cabal_exec paths dir ["my-executable"]
    assertExecFailed result
    let output = outputText result
        expected = "cabal: The program 'my-executable' is required but it " ++
                   "could not be found"
        errMsg = "should not have found a my-executable\n" ++ output
    assertBool errMsg $
        expected `isInfixOf` (intercalate " " . lines $ output)



assertMyLibIsNotAvailableOutsideofSandbox :: TestsPaths -> String -> IO ()
assertMyLibIsNotAvailableOutsideofSandbox paths libNameAndVersion = do
    (_, _, output) <- run (Just $ dir) (ghcPkgPath paths) ["list"]
    assertBool "my library should not have been found" $ not $
        libNameAndVersion `isInfixOf` (intercalate " " . lines $ output)
