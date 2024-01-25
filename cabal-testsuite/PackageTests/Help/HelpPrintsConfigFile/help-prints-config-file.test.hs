-- Andreas Abel, 2024-01-13
--
-- Ensure that the last line of the help text is the name of the config file.
-- This invariant is used by clients such as the Haskell setup github action.
-- See: https://github.com/haskell-actions/setup/pull/63

import Distribution.Utils.String (trim)
import Test.Cabal.Prelude

main = cabalTest $ do
  env <- getTestEnv
  res <- cabal' "--help" []

  -- The end of the help text should be something like:
  --
  -- > You can edit the cabal configuration file to set defaults:
  -- >   <<HOME>>/.cabal/config
  --
  -- So trimming the last line will give us the name of the config file.
  let configFile = trim . last . lines . resultOutput $ res

  -- Verify that this is indeed the config file.
  assertEqual "Last line of help text should be name of the config file"
    (testUserCabalConfigFile env)
    configFile
