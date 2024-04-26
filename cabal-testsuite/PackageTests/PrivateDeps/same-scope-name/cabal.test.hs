import Test.Cabal.Prelude

{-
Checks that:

(a) If there are two scopes with the same name in two separate packages, the same
name shouldn't matter and should the scopes should be solved independently
because they are in different packages

(b) If the scopes with the same name are in same package but in different
components, they should be merged together and fail because of the conflict

(c) If the scopes are in the same component, then they should be merged and fail
because of the conflict too
-}

main =
  cabalTest $ recordMode DoNotRecord $ do
    -- withProjectFile "cabal.project.scenea" $ -- For some reason this doesn't work...

      withRepo "repo" $ do
        -- Succeeds because SameName from pkgA and SameName from pkgB do not collide.
        out <- cabal' "build" ["pkgA", "--project-file=cabal.project.scenea"]
        -- We build the two versions, one for each package's private deps.
        assertOutputContains "- libA-0.1.0.0 (lib) (requires build)" out
        assertOutputContains "- libA-0.2.0.0 (lib) (requires build)" out

      withRepo "repo" $ do
        -- Fails because SameName from pkgC in its two separate components
        fails (cabal' "build" ["pkgC", "--project-file=cabal.project.sceneb"])
        >>= assertOutputContains "rejecting: pkgC:SameName.libA-0.2.0.0 (conflict: pkgC => pkgC:SameName.libA==0.1.0.0)"

      withRepo "repo" $ do
        -- Fails because SameName from pkgD in the same component
        fails (cabal'  "build" ["pkgD", "--project-file=cabal.project.scenec"])
        >>= assertOutputContains "rejecting: pkgD:SameName.libA-0.2.0.0 (conflict: pkgD => pkgD:SameName.libA==0.1.0.0)"

