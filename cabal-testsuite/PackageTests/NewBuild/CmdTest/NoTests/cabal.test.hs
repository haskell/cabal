import Test.Cabal.Prelude

-- Package p has a test suite, package q does not.
--
-- Every scenario is run as is and, in the mode with the
-- "_+failflag" suffix, with --test-fail-when-no-test-suites.
-- The flag turns a skipped target into an error and is otherwise inert.

flag :: String
flag = "--test-fail-when-no-test-suites"

-- The X:tests target variants of the X targets must behave the same as
-- without the :tests filter, so the assertions are shared. Only the rendering
-- of the target in the notice or error message differs.

-- q is skipped with a notice, p is not, and the tests of p run.
assertSkippedQRanP :: String -> String -> Result -> TestM ()
assertSkippedQRanP q p res = do
  assertOutputContains ("No tests to run for " ++ q) res
  assertOutputDoesNotContain ("No tests to run for " ++ p) res
  assertOutputContains "Test suite p-tests: PASS" res

-- q having no tests is an error and the tests of p do not run.
assertFailedQNotRanP :: String -> Result -> TestM ()
assertFailedQNotRanP q res = do
  assertOutputContains ("Cannot run tests for the target '" ++ q ++ "'") res
  assertOutputDoesNotContain "Test suite p-tests: PASS" res

-- Nothing is skipped and the tests of p run.
assertSkippedNoneRanP :: Result -> TestM ()
assertSkippedNoneRanP res = do
  assertOutputDoesNotContain "No tests to run" res
  assertOutputContains "Test suite p-tests: PASS" res

-- q is skipped with a notice and the tests of p do not run.
assertSkippedQNotRanP :: String -> Result -> TestM ()
assertSkippedQNotRanP q res = do
  assertOutputContains ("No tests to run for " ++ q) res
  assertOutputDoesNotContain "Test suite p-tests" res

-- q having no tests is an error.
assertFailedQ :: String -> Result -> TestM ()
assertFailedQ q res =
  assertOutputContains ("Cannot run tests for the target '" ++ q ++ "'") res

main = do
  -- Requesting both must skip q with a notice and still run the tests of p.
  cabalTest' "mixed" $ do
    res <- cabal' "v2-test" ["p", "q"]
    assertSkippedQRanP "the package q-0.1" "the package p-0.1" res

  -- Same with :tests.
  cabalTest' "mixed-tests" $ do
    res <- cabal' "v2-test" ["p:tests", "q:tests"]
    assertSkippedQRanP "the test suites in the package q-0.1" "the test suites in the package p-0.1" res

  -- With the flag q having no tests triggers the error.
  cabalTest' "mixed_+failflag" $ do
    res <- fails $ cabal' "v2-test" ["p", "q", flag]
    assertFailedQNotRanP "q" res

  -- Same with :tests.
  cabalTest' "mixed-tests_+failflag" $ do
    res <- fails $ cabal' "v2-test" ["p:tests", "q:tests", flag]
    assertFailedQNotRanP "q:tests" res

  -- With "all" as target, the tests of p are found.
  cabalTest' "all" $ do
    res <- cabal' "v2-test" ["all"]
    assertSkippedNoneRanP res

  -- Same thing even with the fail flag.
  cabalTest' "all_+failflag" $ do
    res <- cabal' "v2-test" ["all", flag]
    assertSkippedNoneRanP res

  -- Same again with the :tests filter.
  cabalTest' "all-tests" $ do
    res <- cabal' "v2-test" ["all:tests"]
    assertSkippedNoneRanP res

  -- No difference even with the fail flag.
  cabalTest' "all-tests_+failflag" $ do
    res <- cabal' "v2-test" ["all:tests", flag]
    assertSkippedNoneRanP res

  -- When no target has tests, the command succeeds and reports skipped targets.
  cabalTest' "only-no-tests" $ do
    res <- cabal' "v2-test" ["q"]
    assertSkippedQNotRanP "the package q-0.1" res

  -- Same with :tests.
  cabalTest' "only-no-tests-tests" $ do
    res <- cabal' "v2-test" ["q:tests"]
    assertSkippedQNotRanP "the test suites in the package q-0.1" res

  -- The fail flag causes the command to fail when no tests are found.
  cabalTest' "only-no-tests_+failflag" $ do
    res <- fails $ cabal' "v2-test" ["q", flag]
    assertFailedQ "q" res

  -- Same with :tests.
  cabalTest' "only-no-tests-tests_+failflag" $ do
    res <- fails $ cabal' "v2-test" ["q:tests", flag]
    assertFailedQ "q:tests" res
