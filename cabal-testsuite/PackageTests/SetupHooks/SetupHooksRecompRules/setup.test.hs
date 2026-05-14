import Test.Cabal.Prelude

main :: IO ()
main = setupTest $ recordMode DoNotRecord $ do
  setup "configure" []

  -- First build: should run rules for A, B and C.
  build1 <- setup' "build" ["--verbose=1"]
  assertOutputContains "Running myPP preprocessor for A" build1
  assertOutputContains "Running myPP preprocessor for B" build1
  assertOutputContains "Running myPP preprocessor for C" build1

  -- Modify A.myPP, leaving other files alone.
  writeSourceFile "A.myPP" "a = 42\n"

  -- Check we only re-run the preprocessor for A (file dependency changed).
  build2 <- setup' "build" ["--verbose=1"]
  assertOutputContains       "Running myPP preprocessor for A" build2
  assertOutputDoesNotContain "Running myPP preprocessor for B" build2
  assertOutputDoesNotContain "Running myPP preprocessor for C" build2

  -- Change verbosity. C's rule stores the actual verbosity, while A and B
  -- bake in a constant verbosity. Thus we should only re-run the rule for C.
  build3 <- setup' "build" ["--verbose=2"]
  assertOutputDoesNotContain "Running myPP preprocessor for A" build3
  assertOutputDoesNotContain "Running myPP preprocessor for B" build3
  assertOutputContains       "Running myPP preprocessor for C" build3
