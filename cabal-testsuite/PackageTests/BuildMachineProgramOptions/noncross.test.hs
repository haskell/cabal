import Test.Cabal.Prelude

-- Without cross-compilation there is no build machine, so the build-machine
-- program settings have no effect, and they do not fall back to the host
-- machine either. cabal says so rather than ignoring them silently.
main :: IO ()
main = cabalTest . recordMode DoNotRecord $ do
  r <-
    cabal'
      "v2-build"
      [ "--ghc-options=-DHOST_MACHINE"
      , "--build-ghc-options=-DBUILD_MACHINE"
      , -- would break any build that used it
        "--with-build-gcc=/nonexistent/gcc"
      , "app"
      ]
  assertOutputContains "build-machine program settings" r
  assertOutputContains "are ignored" r
  withPlan $ do
    app <- runPlanExe' "app" "app" []
    assertOutputContains "app-machine: host" app
    assertOutputContains "app-lib-machine: host" app
