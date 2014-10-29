module PackageTests.Exec.Check
       ( tests
       ) where


import PackageTests.PackageTester

import Test.Framework                 as TF (Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit                     (assertBool)

import Control.Applicative ((<$>))
import Control.Monad (when)
import Data.List (intercalate, isInfixOf)
import System.FilePath ((</>))
import System.Directory (getDirectoryContents)

dir :: FilePath
dir = checkBasePath </> "Exec"

configPath :: FilePath
configPath = checkDefaultConfigRelativePath

tests :: FilePath -> FilePath -> [TF.Test]
tests cabalPath ghcPkgPath =
    [ testCase "exits with failure if given no argument" $ do
          result <- cabal_exec dir [] cabalPath configPath
          assertExecFailed result

    , testCase "prints error message if given no argument" $ do
          result <- cabal_exec dir [] cabalPath configPath
          assertExecFailed result
          let output = outputText result
              expected = "specify an executable to run"
              errMsg = "should have requested an executable be specified\n" ++
                       output
          assertBool errMsg $
              expected `isInfixOf` (intercalate " " . lines $ output)

    , testCase "runs the given command" $ do
          result <- cabal_exec dir ["echo", "this", "string"] cabalPath configPath
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

          cleanPreviousBuilds cabalPath
          assertMyExecutableNotFound cabalPath
          assertPackageInstall cabalPath

          result <- cabal_exec dir ["my-executable"] cabalPath configPath
          assertExecSucceeded result
          let output = outputText result
              expected = "This is my-executable"
              errMsg = "should have found a my-executable\n" ++ output
          assertBool errMsg $
              expected `isInfixOf` (intercalate " " . lines $ output)

    , testCase "adds the sandbox bin directory to the PATH" $ do
          cleanPreviousBuilds cabalPath
          assertMyExecutableNotFound cabalPath
          assertPackageInstall cabalPath

          result <- cabal_exec dir ["bash", "--", "-c", "my-executable"] cabalPath configPath
          assertExecSucceeded result
          let output = outputText result
              expected = "This is my-executable"
              errMsg = "should have found a my-executable\n" ++ output
          assertBool errMsg $
              expected `isInfixOf` (intercalate " " . lines $ output)

    , testCase "configures GHC to use the sandbox" $ do
          let libNameAndVersion = "my-0.1"

          cleanPreviousBuilds cabalPath
          assertPackageInstall cabalPath

          assertMyLibIsNotAvailableOutsideofSandbox ghcPkgPath libNameAndVersion

          result <- cabal_exec dir ["ghc-pkg", "list"] cabalPath configPath
          assertExecSucceeded result
          let output = outputText result
              errMsg = "my library should have been found"
          assertBool errMsg $
              libNameAndVersion `isInfixOf` (intercalate " " . lines $ output)
          

    -- , testCase "can find executables built from the package" $ do
    -- , testCase "configures cabal to use the sandbox" $ do
    ]

cleanPreviousBuilds :: FilePath -> IO ()
cleanPreviousBuilds cabalPath = do
    sandboxExists <- not . null . filter (== "cabal.sandbox.config") <$>
                         getDirectoryContents dir
    assertCleanSucceeded   =<< cabal_clean dir [] cabalPath configPath
    when sandboxExists $ do
        assertSandboxSucceeded =<< cabal_sandbox dir ["delete"] cabalPath configPath


assertPackageInstall :: FilePath -> IO ()
assertPackageInstall cabalPath = do
    assertSandboxSucceeded =<< cabal_sandbox dir ["init"] cabalPath configPath
    assertInstallSucceeded =<< cabal_install dir [] cabalPath configPath


assertMyExecutableNotFound :: FilePath -> IO ()
assertMyExecutableNotFound cabalPath = do
    result <- cabal_exec dir ["my-executable"] cabalPath configPath
    assertExecFailed result
    let output = outputText result
        expected = "cabal: The program 'my-executable' is required but it " ++
                   "could not be found"
        errMsg = "should not have found a my-executable\n" ++ output
    assertBool errMsg $
        expected `isInfixOf` (intercalate " " . lines $ output)



assertMyLibIsNotAvailableOutsideofSandbox :: FilePath -> String -> IO ()
assertMyLibIsNotAvailableOutsideofSandbox ghcPkgPath libNameAndVersion = do
    (_, _, output) <- run (Just $ dir) ghcPkgPath ["list"]
    assertBool "my library should not have been found" $ not $
        libNameAndVersion `isInfixOf` (intercalate " " . lines $ output)
