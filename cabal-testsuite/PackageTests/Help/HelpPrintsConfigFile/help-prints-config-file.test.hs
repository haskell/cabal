-- Andreas Abel, 2024-01-13, 2024-01-28
--
-- Ensure that the help text prints the name of the config file, following a fixed text.
-- This invariant is used by clients such as the Haskell setup github action.
-- See: https://github.com/haskell-actions/setup/pull/63

-- The end of the help text should look like:
--
-- > You can edit the cabal configuration file to set defaults:
-- >   <<HOME>>/.cabal/config
-- > This file will be generated with sensible defaults if you run 'cabal update'.
--
-- The last line is only present when the file does *not* exist.
--
-- So while usually the last line is the name of the config file,
-- the correct way is to take the line after the fixed text "You can edit...".

import Distribution.Utils.String (trim)
import System.Directory (removeFile)
import Test.Cabal.Prelude

main = cabalTest $ do
  env <- getTestEnv
  let configFile = testUserCabalConfigFile env

  -- Test 1: with config file present.
  test configFile "Case: config file exists."

  -- Test 2: with config file absent.
  liftIO $ removeFile configFile
  test configFile "Case: config file does not exist."

test configFile info = do
  res <- cabal' "--help" []
  assertEqual (unwords ["Help text contains name of the config file after the marker.", info])
    configFile
    (configFileFromHelpText $ resultOutput res)

-- | The config file is the line following the fixed text:
--   "You can edit the cabal configuration file to set defaults:"
--
--   If this marker is not found, return the empty string.
configFileFromHelpText :: String -> FilePath
configFileFromHelpText txt =
    case found of
      _marker : l : _ -> l
      _ -> ""
  where
    marker = "You can edit the cabal configuration file to set defaults:"
    (_before, found) = break (marker ==) $ map trim $ lines txt
       -- Note: 'trim' lines to get rid of CR on Windows;
       -- 'lines' does not seem to remove it.
