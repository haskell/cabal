import Test.Cabal.Prelude

-- A project is closed under reject-unconstrained-dependencies=eq once it is
-- frozen. The freeze file constrains every dependency to one version, the
-- build-tool dependency some-exe and the installed base included.
main = cabalTest . recordMode DoNotRecord . withRepo "repo" $ do
  cwd <- fmap testCurrentDir getTestEnv
  let freezeFile = cwd </> "cabal.project.freeze"
      build = cabal' "v2-build" ["all", "--dry-run", "--reject-unconstrained-dependencies=eq"]

  -- Before the freeze no dependency has a version constraint.
  shouldNotExist freezeFile
  res <- fails build
  assertOutputContains "not a user-provided goal nor mentioned as a constraint when reject-unconstrained-dependencies=eq" res

  cabal "v2-freeze" []
  shouldExist freezeFile
  mapM_
    (assertFileDoesContain freezeFile)
    [ "any.base =="
    , "any.some-lib ==1.0"
    , "any.other-lib ==1.0"
    , "any.some-exe ==1.0"
    ]

  -- After the freeze every dependency has an equality constraint.
  res <- build
  assertOutputDoesNotContain "not a user-provided goal" res
