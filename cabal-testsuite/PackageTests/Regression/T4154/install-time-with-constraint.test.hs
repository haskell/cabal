import Test.Cabal.Prelude

-- Test that unqualified command line constraints do not constrain setup
-- dependencies. cabal should be able to install the local time-99999 by
-- building its setup script with the installed time, even though the installed
-- time doesn't fit the constraint.
main = cabalTest $ withRepo "repo" $
       cabal "new-build" ["time", "--constraint=time==99999", "--dry-run"]
