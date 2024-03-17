import Test.Cabal.Prelude

-- The local package, pkg-1.0, depends on build-tool-pkg-1 as a library and
-- build-tool-pkg-2 as a build-tool.  This test checks that cabal uses the
-- correct version of build-tool-pkg for each purpose.  pkg imports a version
-- number from the build-tool-pkg library and uses the build-tool-pkg executable
-- as a preprocessor to insert the executable's version number into the source
-- code.  Then the pkg executable prints out both versions.
--
-- Issue #5409 caused v2-build to use the same instance of build-tool-pkg for
-- the build-depends and build-tool-depends dependencies, even though it
-- violated the version constraints.
main = cabalTest $ withShorterPathForNewBuildStore $ do
    skipUnless "not v2-build compatible boot Cabal" =<< hasNewBuildCompatBootCabal
    withRepo "repo" $ do
      r1 <- recordMode DoNotRecord $
            cabal' "v2-build" ["pkg:my-exe"]

      let msg = concat
              [ "In order, the following will be built:"
              , "  - build-tool-pkg-1 (lib) (requires build)"
              , "  - build-tool-pkg-2 (lib) (requires build)"
              , "  - build-tool-pkg-2 (exe:build-tool-exe) (requires build)"
              , "  - pkg-1.0 (exe:my-exe) (first run)"
              ]

      assertOutputContains msg r1
      withPlan $ do
        r2 <- runPlanExe' "pkg" "my-exe" []
        assertOutputContains
            "build-tool library version: 1, build-tool exe version: 2" r2
