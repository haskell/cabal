-- 2021-10-07, issue #7705
--
-- Non-existent config file given explicitly (via option or environment variable)
-- should not be filled with default content.
-- Rather, an error should be raised.
--
-- See also @PackageTests/UserConfig/cabal.test.hs@ for testing of command
-- @user-config@.
--
-- We use the @info@ command as it does not have side-effects.
-- Does not really matter which command we use for this test,
-- except that it should accept option @--config-file@.
--
-- This is a golden value test that reports the produced error message.
-- Needs to be checked manually whether it meets expectations.

import Test.Cabal.Prelude

main = cabalTest $ do
  fails $
    cabalG [ "--config-file", "does.not.exist" ] "info" [ "happy" ]
  fails $ withEnv [("CABAL_CONFIG", Just "absent.file")] $
    cabal "info" [ "alex" ]
