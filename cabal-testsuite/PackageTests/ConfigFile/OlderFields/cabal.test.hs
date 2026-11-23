import Test.Cabal.Prelude

import Control.Exception (evaluate)

-- The older and removed fields listed in doc/configuration-history.rst.
--
-- Each field is put in the configuration file or in the project and @cabal
-- path@ is run, as it reads both and does nothing else. With
-- @--remote-repo-cache@ it prints one line, which shows what @cachedir@ sets.
main = cabalTest $ do
  env <- getTestEnv
  let configFile = testUserCabalConfigFile env
      projectFile = testCurrentDir env </> "cabal.project"
      olderCache = testCurrentDir env </> "older-cache"

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

      -- Not recorded, as each of these prints the same one line.
      accepted m = recordMode DoNotRecord $ do
        r <- m
        assertOutputDoesNotContain "Unrecognized field" r
        assertOutputDoesNotContain "Unknown field" r

  -- Fields with a newer name are accepted in the configuration file without a
  -- warning.
  mapM_
    (accepted . inConfig)
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

  -- The cachedir field sets the same directory as remote-repo-cache.
  byOlderName <- inConfig ("cachedir: " ++ olderCache)
  assertOutputDoesNotContain "Unrecognized field" byOlderName
  assertOutputContains "older-cache" byOlderName
  byNewerName <- inConfig ("remote-repo-cache: " ++ olderCache)
  assertOutputContains "older-cache" byNewerName

  -- Removed fields are ignored with a warning.
  let removed =
        [ "local-repo"
        , "require-sandbox"
        , "ignore-sandbox"
        , "project-file"
        , "ignore-project"
        , "one-shot"
        , "nix"
        ]
  forM_ removed $ \field ->
    assertOutputContains ("Unrecognized field " ++ field)
      =<< inConfig (field ++ ": removed")

  -- Except for world-file, which is ignored without one.
  accepted $ inConfig "world-file: removed"

  -- The project-dir option has never been a field of the configuration file.
  assertOutputContains "Unrecognized field project-dir"
    =<< inConfig "project-dir: other-dir"

  -- Three of the fields with a newer name are accepted in a project too.
  mapM_
    (accepted . inProject)
    [ "library-coverage: True"
    , "prefer-oldest: True"
    , "symlink-bindir: older-bindir"
    ]

  -- So are three options to do with projects, though none is a field of the
  -- configuration file.
  mapM_
    (accepted . inProject)
    [ "project-file: other.project"
    , "ignore-project: True"
    , "project-dir: other-dir"
    ]

  -- The rest are for the configuration file only.
  assertOutputContains "Unknown field: \"remote-repo\""
    =<< inProject "remote-repo: packages.example.org:http://packages.example.org/"

  -- Not recorded, as each of these prints the same warning but for the name.
  let unknownInProject field =
        recordMode DoNotRecord $
          assertOutputContains ("Unknown field: \"" ++ field ++ "\"")
            =<< inProject (field ++ ": unknown")
  mapM_
    unknownInProject
    [ "repos"
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

  -- Removed fields are unknown in a project too, world-file among them.
  mapM_
    unknownInProject
    ["local-repo", "require-sandbox", "ignore-sandbox", "one-shot", "world-file", "nix"]

  -- The remote-repo field is equivalent to a repository section with a url
  -- field: user-config update writes it back as one.
  liftIO $
    writeFile configFile $
      config ++ "remote-repo: packages.example.org:http://packages.example.org/\n"
  recordMode DoNotRecord $ cabal "user-config" ["update", "-f"]
  assertFileDoesContain configFile "repository packages.example.org"
  assertFileDoesContain configFile "url: http://packages.example.org/"
  assertFileDoesNotContain configFile "remote-repo:"
