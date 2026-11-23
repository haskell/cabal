import Test.Cabal.Prelude

import Control.Exception (evaluate)
import System.Directory (createDirectoryIfMissing)

-- Which configuration file is used. The rules are numbered as they are under
-- "Configuration file discovery" in doc/config.rst. Keep the two in step.
--
--  +---+--------------------------------------+-------------------------------+
--  | # | When                                 | Configuration file            |
--  +---+--------------------------------------+-------------------------------+
--  | 1 | --config-file is given               | That file                     |
--  | 2 | CABAL_CONFIG is set                  | That file                     |
--  | 3 | CABAL_DIR is set                     | $CABAL_DIR/config             |
--  | 4 | $XDG_CONFIG_HOME/cabal/config exists | $XDG_CONFIG_HOME/cabal/config |
--  | 5 | ~/.cabal/config exists               | ~/.cabal/config               |
--  | 6 | ~/.cabal is a directory              | ~/.cabal/config               |
--  | 7 | Otherwise                            | $XDG_CONFIG_HOME/cabal/config |
--  +---+--------------------------------------+-------------------------------+
--
-- The first rule that applies is used. When rule 4 applies and ~/.cabal also
-- exists, cabal warns that it is ignoring ~/.cabal.
--
-- When rule 3 picks a file that does not exist, cabal creates it. Rules 6 and 7
-- always create their file, as an earlier rule would have applied had the file
-- existed.
--
-- Rules 1 and 2 are tested by UserConfig and ConfigFile/T7705, the rest here.
--
-- On Windows the old location is %APPDATA%\cabal, which Windows gives by a
-- system call and not from an environment variable, so a test cannot put it
-- somewhere of its own. Only the XDG location can be moved, with
-- XDG_CONFIG_HOME. That leaves rule 4 as the only one of rules 4 to 7 that can
-- be tested there without touching the user's real %APPDATA%\cabal. Rule 6 in
-- particular would write a config file into it.
--
-- The test harness sets CABAL_CONFIG and CABAL_DIR for every test, so each case
-- unsets them and points HOME and the XDG variables at a directory of its own.
-- @cabal path --config-file@ prints the file chosen.
main = do
  discovery "cabal-dir" "Rule 3: CABAL_DIR is set, so its config is used, not the XDG one" $ \home -> do
    let cabalDir = home </> "chosen-dir"
    writeConfig cabalDir
    writeConfig (home </> "xdg-config" </> "cabal")
    r <- discover home (Just cabalDir)
    assertOutputContains "chosen-dir" r

  discovery "created" "Rule 3: a missing CABAL_DIR and its config are created by a command that reads the configuration" $ \home -> do
    let cabalDir = home </> "missing" </> "dir"
    shouldDirectoryNotExist cabalDir
    _ <- discover home (Just cabalDir)
    shouldExist (cabalDir </> "config")

  -- On Windows the output is not recorded, as it has a warning about the real
  -- %APPDATA%\cabal on a machine that has one.
  when isWindows $
    discoveryUnrecorded "xdg-only-windows" $ \home -> do
      writeConfig (home </> "xdg-config" </> "cabal")
      r <- discover home Nothing
      assertOutputContains ("xdg-config" </> "cabal" </> "config") r

  unless isWindows $ do
    discovery "xdg-only" "Rule 4: only the XDG config exists, so it is used" $ \home -> do
      writeConfig (home </> "xdg-config" </> "cabal")
      r <- discover home Nothing
      assertOutputContains ("xdg-config" </> "cabal" </> "config") r

    discovery "both" "Rule 4: both exist, so the XDG config is used and ~/.cabal is ignored with a warning" $ \home -> do
      writeConfig (home </> ".cabal")
      writeConfig (home </> "xdg-config" </> "cabal")
      r <- discover home Nothing
      assertOutputContains ("xdg-config" </> "cabal" </> "config") r
      assertOutputContains "ignoring the former" r

    discovery "old-only" "Rule 5: only ~/.cabal/config exists, so it is used" $ \home -> do
      writeConfig (home </> ".cabal")
      r <- discover home Nothing
      assertOutputContains (".cabal" </> "config") r

    discovery "old-empty" "Rule 6: only an empty ~/.cabal exists, so ~/.cabal/config is created and used" $ \home -> do
      liftIO $ createDirectoryIfMissing True (home </> ".cabal")
      r <- discover home Nothing
      assertOutputContains (".cabal" </> "config") r
      shouldExist (home </> ".cabal" </> "config")

    discovery "xdg-empty" "Rule 6: an XDG directory with no config in it does not stop ~/.cabal being used" $ \home -> do
      liftIO $ createDirectoryIfMissing True (home </> ".cabal")
      liftIO $ createDirectoryIfMissing True (home </> "xdg-config" </> "cabal")
      r <- discover home Nothing
      assertOutputContains (".cabal" </> "config") r
      shouldExist (home </> ".cabal" </> "config")
      shouldNotExist (home </> "xdg-config" </> "cabal" </> "config")

    discovery "neither" "Rule 7: neither exists, so the XDG config is created and used" $ \home -> do
      r <- discover home Nothing
      assertOutputContains ("xdg-config" </> "cabal" </> "config") r
      shouldExist (home </> "xdg-config" </> "cabal" </> "config")

    discovery "old-is-file" "Rule 7: a ~/.cabal that is a file and not a directory does not count" $ \home -> do
      liftIO $ createDirectoryIfMissing True home
      liftIO $ writeFile (home </> ".cabal") ""
      r <- discover home Nothing
      assertOutputContains ("xdg-config" </> "cabal" </> "config") r
      shouldExist (home </> "xdg-config" </> "cabal" </> "config")

-- A named case with expected output of its own, introduced there by its rule
-- and given a home directory of its own. The harness has a home directory
-- already, with a .cabal in it, so this one is somewhere else.
discovery :: String -> String -> (FilePath -> TestM ()) -> IO ()
discovery name rule m = cabalTest' name . recordMode RecordMarked $ do
  env <- getTestEnv
  recordHeader [rule]
  m (testWorkDir env </> "discovery")

-- A named case that is checked by its assertions alone.
discoveryUnrecorded :: String -> (FilePath -> TestM ()) -> IO ()
discoveryUnrecorded name m = cabalTest' name . recordMode DoNotRecord $ do
  env <- getTestEnv
  m (testWorkDir env </> "discovery")

-- Copy the configuration the harness wrote, with the path to the compiler, to
-- the config file of a directory.
writeConfig :: FilePath -> TestM ()
writeConfig dir = do
  env <- getTestEnv
  liftIO $ do
    config <- readFile (testUserCabalConfigFile env)
    _ <- evaluate (length config)
    createDirectoryIfMissing True dir
    writeFile (dir </> "config") config

-- Run with the given home directory and CABAL_DIR set as given.
discover :: FilePath -> Maybe FilePath -> TestM Result
discover home cabalDir =
  withEnv
    [ ("CABAL_CONFIG", Nothing)
    , ("CABAL_DIR", cabalDir)
    , ("HOME", Just home)
    , ("XDG_CONFIG_HOME", Just (home </> "xdg-config"))
    , ("XDG_CACHE_HOME", Just (home </> "xdg-cache"))
    , ("XDG_STATE_HOME", Just (home </> "xdg-state"))
    ]
    $ cabal' "path" ["-z", "--config-file"]
