-- 2021-10-06, issue #7248
--
-- Purpose of this test:
-- Make sure that ordinary user communication does not contain 'Show'ed internal structures.
--
-- This is a golden value test that reports the produced error message.
-- Needs to be checked manually whether it meets expectations.

import Test.Cabal.Prelude
main = cabalTest $
  fails $
  cabalG
    [ "--config-file", "cabal.config" ]
    "get"
    [ "a-b-s-e-n-t" ]
