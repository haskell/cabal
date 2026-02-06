import Test.Cabal.Prelude

-- See #4332, dep solving output is not deterministic
-- other-lib is a dependency of b, but it's not listed in cabal.project
-- and some-exe is a build-tool dependency of b, again not listed

assertErr x = assertOutputContains ("not a user-provided goal nor mentioned as a constraint, but reject-unconstrained-dependencies=" ++ x ++ " was set")
constraint x = "--constraint=" ++ x
opts = [ "all", "--dry-run" ]

main = do
  skipIfOSX "macOS has a different solver output format"
  -- - - other-lib-1.0 (lib) (requires build)
  -- - - some-exe-1.0 (exe:some-exe) (requires build)
  --   - some-lib-1.0 (lib) (requires build)
  -- + - some-exe-1.0 (exe:some-exe) (requires build)
  -- + - other-lib-1.0 (lib) (requires build)
  --   - b-0 (lib) (first run)
  --   - a-0 (lib) (first run)
  cabalTest' "multipkg-all" . withRepo "repo" $ do
    let proj n = "--project-file=cabal.all" <.> n <.> ".project" : opts
    let no n = assertErr "all" =<< fails (cabal' "build" $ proj n)
    mapM_ (no . ("no" <.>)) ["exe-any", "lib-any", "lib-exe-any"]

  cabalTest' "multipkg-eq" . withRepo "repo" $ do
    let proj n = "--project-file=cabal.eq" <.> n <.> ".project" : opts
    let no n = assertErr "eq" =<< fails (cabal' "build" $ proj n)
    let go n = cabal' "build" $ proj n
    mapM_ (go . ("go" <.>)) ["eq-and-range", "eq"]
    mapM_ (no . ("no" <.>)) ["eq-and-range", "exe-eq", "lib-eq", "lib-exe-eq", "lib-range"]
