import Test.Cabal.Prelude

-- Test that "cabal haddock" preserves temporary files
-- We use haddock-keep-temp-file: True in the cabal.project.
main = cabalTest $ recordMode DoNotRecord $ withProjectFile "cabal.project" $ do
  cabal "haddock" []

  -- From the docs for `System.IO.openTempFile`:
  --
  --   On Windows, the template prefix may be truncated to 3 chars, e.g.
  --   "foobar.ext" will be "fooXXX.ext".
  let glob =
        if isWindows
          then "had*.txt"
          else "haddock-response*.txt"

  -- Check that there is a response file.
  responseFiles <- assertGlobMatchesTestDir testTmpDir glob

  -- Check that the matched response file is not empty, and is indeed a Haddock
  -- response file.
  assertAnyFileContains responseFiles "--package-name"
