import Test.Cabal.Prelude

-- Test that unqualified command line constraints do not constrain setup
-- dependencies. cabal should be able to install the local time-99999 by
-- building its setup script with the installed Cabal, which depends on the
-- installed time, even though the installed time doesn't fit the constraint.
main = cabalTest $ do
  -- TODO: Run this test on Windows once #5187 is resolved.
  skipIf =<< isWindows

  cabal "new-build" ["time", "--constraint=time==99999", "--dry-run"]

  -- Temporarily disabled recording here because output is not stable
  recordMode DoNotRecord $ do
    -- Constraining all uses of 'time' fails because the installed 'time'
    -- doesn't fit the constraint.
    r <- fails $ cabal' "new-build" ["time", "--constraint=any.time==99999", "--dry-run"]
    assertRegex "Expected cabal to reject the setup dependency on the installed time"
                ("rejecting: time:setup.time-[0-9.]*/installed-[^[:space:]]* "
                  ++ "\\(constraint from command line flag requires ==99999\\)")
                r
