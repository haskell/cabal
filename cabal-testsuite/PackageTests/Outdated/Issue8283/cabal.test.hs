import Test.Cabal.Prelude
import System.Exit(ExitCode(..))


main = cabalTest $ withRepo "repo" $ do
  -- Test 1: Run from project root with no target specifier
  result1 <- cabal' "outdated" ["all"]
  assertOutputContains "package-a" result1
  assertOutputContains "package-b" result1

  -- Test 2: Run from project root with specific package target
  result2 <- cabal' "v2-outdated" ["package-a"]
  assertOutputContains "package-a" result2
  assertOutputDoesNotContain "package-b" result2

  -- Test 3: Run from project root with --simple-output flag
  result3 <- cabal' "v2-outdated" ["all", "--simple-output"]
  assertOutputContains "base" result3
  assertOutputContains "template-haskell" result3

  -- Test 4: Run from subdirectory
  result4 <- cabal' "v2-outdated" ["--project-context"]
  assertOutputContains "base" result4

  -- Test 6: Test exit code behavior
  result6 <- fails $ cabal' "v2-outdated" ["all", "--exit-code"]
  assertExitCode (ExitFailure 1) result6
