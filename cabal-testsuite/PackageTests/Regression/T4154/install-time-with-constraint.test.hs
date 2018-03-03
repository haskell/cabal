import Test.Cabal.Prelude

-- Test that unqualified command line constraints do not constrain setup
-- dependencies. cabal should be able to install the local time-99999 by
-- building its setup script with the installed time, even though the installed
-- time doesn't fit the constraint.
main = cabalTest $ withRepo "repo" $ do
  cabal "new-build" ["time", "--constraint=time==99999", "--dry-run"]

  -- Temporarily disabled recording here because output is not stable
  recordMode DoNotRecord $ do
      r <- fails $ cabal' "new-build" ["time", "--constraint=any.time==99999", "--constraint=setup.Cabal installed", "--dry-run"]
      -- Constraining all uses of 'time' originally resulted in a cyclic dependency
      -- between 'Cabal' and the new 'time':
      -- assertOutputContains "cyclic dependencies; conflict set: time:setup.Cabal, time:setup.time" r
      -- However, this doesn't work anymore, so instead we more directly look for:
      assertOutputContains "time:setup.time~>time-99999 (conflict: time:setup.Cabal" r
