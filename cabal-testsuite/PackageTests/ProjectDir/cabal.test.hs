import Test.Cabal.Prelude

main :: IO ()
main = cabalTest $ recordMode DoNotRecord $ do
  env <- getTestEnv

  let cwd = testCurrentDir env

  -- Relative directory
  cabal "v2-build" [ "--project-dir=proj", "all" ]

  -- Absolute directory
  cabal "v2-build" [ "--project-dir", (cwd </> "proj"), "all" ]

  cabal "v2-clean" [ "--project-dir=proj" ]

  withProjectFile "nix/cabal.project" $ do
    cabal "v2-build" [ "--project-dir=proj", "extra" ]

    cabal "v2-clean" [ "--project-dir=proj" ]

  -- App with no cabal.project
  void $ cabal_raw' [ "run", "--project-dir=app", "app" ] Nothing
