import Test.Cabal.Prelude

import Control.Exception (evaluate)

-- The older and removed fields listed in doc/configuration-history.rst.
--
-- Each field is put in the configuration file or in the project and @cabal
-- path@ is run, as it reads both and does nothing else. With
-- @--remote-repo-cache@ it prints one line, which shows what @cachedir@ sets.
--
-- Each group of fields has expected output of its own, in which each field is
-- introduced by name.
main = do
  fields "newer-name" $ \inConfig _ ->
    forM_
      [ "remote-repo: packages.example.org:http://packages.example.org/"
      , "repos: packages.example.org:http://packages.example.org/"
      , "hackage-username: someone"
      , "hackage-password: secret"
      , "hackage-password-command: echo secret"
      , "hackage-token: secret"
      , "user-prefix: older-prefix"
      , "user-bindir: older-bindir"
      , "global-prefix: older-prefix"
      , "global-bindir: older-bindir"
      , "library-coverage: True"
      , "prefer-oldest: True"
      , "symlink-bindir: older-bindir"
      ]
      $ \field -> do
        introduce (name field ++ " has a newer name but is accepted in the configuration file")
        accepted =<< inConfig field

  fields "cachedir" $ \inConfig _ -> do
    env <- getTestEnv
    let olderCache = testCurrentDir env </> "older-cache"

    introduce "cachedir sets a directory"
    byOlderName <- inConfig ("cachedir: " ++ olderCache)
    accepted byOlderName
    assertOutputContains "older-cache" byOlderName

    introduce "remote-repo-cache sets the same directory"
    byNewerName <- inConfig ("remote-repo-cache: " ++ olderCache)
    assertOutputContains "older-cache" byNewerName

  fields "removed" $ \inConfig _ -> do
    forM_
      [ "local-repo"
      , "require-sandbox"
      , "ignore-sandbox"
      , "project-file"
      , "ignore-project"
      , "one-shot"
      , "nix"
      ]
      $ \field -> do
        introduce (field ++ " was removed and is ignored with a warning")
        assertOutputContains ("Unrecognized field " ++ field)
          =<< inConfig (field ++ ": removed")

    introduce "world-file was removed and is ignored without a warning"
    accepted =<< inConfig "world-file: removed"

    introduce "project-dir has never been a field of the configuration file"
    assertOutputContains "Unrecognized field project-dir"
      =<< inConfig "project-dir: other-dir"

  fields "in-project" $ \_ inProject -> do
    forM_
      [ "library-coverage: True"
      , "prefer-oldest: True"
      , "symlink-bindir: older-bindir"
      ]
      $ \field -> do
        introduce (name field ++ " has a newer name but is accepted in a project")
        accepted =<< inProject field

    forM_
      [ "project-file: other.project"
      , "ignore-project: True"
      , "project-dir: other-dir"
      ]
      $ \field -> do
        introduce (name field ++ " is an option that is accepted in a project")
        accepted =<< inProject field

  fields "not-in-project" $ \_ inProject -> do
    forM_
      [ "remote-repo"
      , "repos"
      , "cachedir"
      , "hackage-username"
      , "hackage-password"
      , "hackage-password-command"
      , "hackage-token"
      , "user-prefix"
      , "user-bindir"
      , "global-prefix"
      , "global-bindir"
      ]
      $ \field -> do
        introduce (field ++ " is for the configuration file only")
        unknownIn inProject field

    forM_
      [ "local-repo"
      , "require-sandbox"
      , "ignore-sandbox"
      , "one-shot"
      , "world-file"
      , "nix"
      ]
      $ \field -> do
        introduce (field ++ " was removed and is unknown in a project")
        unknownIn inProject field

  fields "remote-repo-section" $ \_ _ -> do
    env <- getTestEnv
    let configFile = testUserCabalConfigFile env
    introduce "user-config update writes a remote-repo field back as a repository section with a url field"
    liftIO $
      appendFile configFile "remote-repo: packages.example.org:http://packages.example.org/\n"
    cabal "user-config" ["update", "-f"]
    assertFileDoesContain configFile "repository packages.example.org"
    assertFileDoesContain configFile "url: http://packages.example.org/"
    assertFileDoesNotContain configFile "remote-repo:"

  fields "configure-writes" $ \_ _ -> do
    env <- getTestEnv
    let dir = testCurrentDir env
    liftIO $ writeFile (dir </> "other.project") "packages: .\n"
    introduce "configure writes project-file and ignore-project to the .local file"
    -- Not recorded, as it lists what would be built.
    recordMode DoNotRecord $ cabal "v2-configure" ["--project-file=other.project"]
    assertFileDoesContain (dir </> "other.project.local") "project-file: other.project"
    assertFileDoesContain (dir </> "other.project.local") "ignore-project: False"

  -- The project selects other-package. Were any of these fields to have an
  -- effect, the package listed would be older-fields, from this directory or
  -- from other.project, or there would be no project to list from.
  fields "no-effect" $ \_ _ -> do
    env <- getTestEnv
    let dir = testCurrentDir env
        listed file field = do
          introduce (name field ++ " has no effect in " ++ file)
          liftIO $ writeFile (dir </> "cabal.project") "packages: other-package\n"
          liftIO $ writeFile (dir </> "cabal.project.local") ""
          liftIO $ appendFile (dir </> file) (field ++ "\n")
          r <- cabal' "v2-sdist" ["--list-only", "all"]
          assertOutputContains "other-package.cabal" r
          assertOutputDoesNotContain "older-fields.cabal" r
    liftIO $ writeFile (dir </> "other.project") "packages: .\n"
    forM_ ["cabal.project", "cabal.project.local"] $ \file ->
      forM_
        [ "project-file: other.project"
        , "ignore-project: True"
        , "project-dir: no-such-directory"
        ]
        (listed file)
    liftIO $ writeFile (dir </> "cabal.project.local") ""

-- A named group of cases with expected output of its own. The group is given
-- two ways of running @cabal path@, with a field added to the configuration
-- file and with a field added to the project.
fields
  :: String
  -> ((String -> TestM Result) -> (String -> TestM Result) -> TestM ())
  -> IO ()
fields group m = cabalTest' group . recordMode RecordMarked $ do
  env <- getTestEnv
  let configFile = testUserCabalConfigFile env
      projectFile = testCurrentDir env </> "cabal.project"

  -- The configuration the harness wrote, with the path to the compiler.
  config <- liftIO $ do
    s <- readFile configFile
    _ <- evaluate (length s)
    return s

  let inConfig field = do
        liftIO $ writeFile configFile (config ++ field ++ "\n")
        cabal' "path" ["-z", "--remote-repo-cache"]

      inProject field = do
        liftIO $ writeFile configFile config
        liftIO $ writeFile projectFile (unlines ["packages: .", field])
        cabal' "path" ["--remote-repo-cache"]

  m inConfig inProject

-- Introduce a case in the expected output.
introduce :: String -> TestM ()
introduce = recordHeader . pure

-- The name of a field, from a line that sets it.
name :: String -> String
name = takeWhile (/= ':')

-- The field was read without complaint.
accepted :: Result -> TestM ()
accepted r = do
  assertOutputDoesNotContain "Unrecognized field" r
  assertOutputDoesNotContain "Unknown field" r

-- The field is not one a project has.
unknownIn :: (String -> TestM Result) -> String -> TestM ()
unknownIn inProject field =
  assertOutputContains ("Unknown field: \"" ++ field ++ "\"")
    =<< inProject (field ++ ": unknown")
