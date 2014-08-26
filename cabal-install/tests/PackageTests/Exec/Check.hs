module PackageTests.Exec.Check
       ( tests
       ) where


import PackageTests.PackageTester

import Test.Framework                 as TF (Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit                     (assertBool)

import Data.List (intercalate, isInfixOf)
import System.FilePath ((</>))

dir :: FilePath
dir = "PackageTests" </> "Exec"

tests :: FilePath -> FilePath -> [TF.Test]
tests cabalPath ghcPkgPath =
    [ testCase "exits with failure if given no argument" $ do
          result <- cabal_exec dir [] cabalPath
          assertExecFailed result

    , testCase "prints error message if given no argument" $ do
          result <- cabal_exec dir [] cabalPath
          assertExecFailed result
          let output = outputText result
              expected = "specify an executable to run"
              errMsg = "should have requested an executable be specified\n" ++
                       output
          assertBool errMsg $
              expected `isInfixOf` (intercalate " " . lines $ output)

    , testCase "runs the given command" $ do
          result <- cabal_exec dir ["echo", "this", "string"] cabalPath
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

          assertCleanSucceeded   =<< cabal_clean dir [] cabalPath
          assertSandboxSucceeded =<< cabal_sandbox dir ["delete"] cabalPath
          assertMyExecutableNotFound cabalPath
          assertSandboxSucceeded =<< cabal_sandbox dir ["init"] cabalPath
          assertInstallSucceeded =<< cabal_install dir [] cabalPath

          result <- cabal_exec dir ["my-executable"] cabalPath
          assertExecSucceeded result
          let output = outputText result
              expected = "This is my-executable"
              errMsg = "should have found a my-executable\n" ++ output
          assertBool errMsg $
              expected `isInfixOf` (intercalate " " . lines $ output)

    , testCase "adds the sandbox bin directory to the PATH" $ do
          assertCleanSucceeded   =<< cabal_clean dir [] cabalPath
          assertSandboxSucceeded =<< cabal_sandbox dir ["delete"] cabalPath
          assertMyExecutableNotFound cabalPath
          assertSandboxSucceeded =<< cabal_sandbox dir ["init"] cabalPath
          assertInstallSucceeded =<< cabal_install dir [] cabalPath

          result <- cabal_exec dir ["bash", "--", "-c", "my-executable"] cabalPath
          assertExecSucceeded result
          let output = outputText result
              expected = "This is my-executable"
              errMsg = "should have found a my-executable\n" ++ output
          assertBool errMsg $
              expected `isInfixOf` (intercalate " " . lines $ output)

    , testCase "configures GHC to use the sandbox" $ do
          let libNameAndVersion = "my-0.1"

          assertCleanSucceeded   =<< cabal_clean dir [] cabalPath
          assertSandboxSucceeded =<< cabal_sandbox dir ["delete"] cabalPath
          assertSandboxSucceeded =<< cabal_sandbox dir ["init"] cabalPath
          assertInstallSucceeded =<< cabal_install dir [] cabalPath

          assertMyLibIsNotAvailableOutsideofSandbox ghcPkgPath libNameAndVersion

          result <- cabal_exec dir ["ghc-pkg", "list"] cabalPath
          assertExecSucceeded result
          let output = outputText result
              errMsg = "my library should have been found"
          assertBool errMsg $
              libNameAndVersion `isInfixOf` (intercalate " " . lines $ output)
          

    -- , testCase "can find executables built from the package" $ do
    -- , testCase "configures cabal to use the sandbox" $ do
    ]



assertMyExecutableNotFound :: FilePath -> IO ()
assertMyExecutableNotFound cabalPath = do
    result <- cabal_exec dir ["my-executable"] cabalPath
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
