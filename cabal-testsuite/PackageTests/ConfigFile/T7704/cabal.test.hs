-- 2021-10-06, issue #7704
--
-- A global option given after the command (e.g. @cabal get --config-file=foo@)
-- is rejected by the command parser as an unrecognized option. The error
-- message should point out that global options have to be given before the
-- command.

import Test.Cabal.Prelude

main = cabalTest $ do
  res <- fails $ cabalG' [] "get" ["--config-file=foo"]
  assertOutputContains "unrecognized 'get' option `--config-file=foo'" res
  assertOutputContains "the option `--config-file=foo' is global" res
  assertOutputContains "must be specified before" res
