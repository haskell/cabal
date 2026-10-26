import Test.Cabal.Prelude

-- Without cross-compilation there is no build stage, so the build-stage
-- program settings have no effect, and they do not fall back to the host
-- stage either. cabal says so rather than ignoring them silently.
main :: IO ()
main = cabalTest . recordMode DoNotRecord $ do
  r <-
    cabal'
      "v2-build"
      [ "--ghc-options=-DHOST_STAGE"
      , "--build-ghc-options=-DBUILD_STAGE"
      , -- would break any build that used it
        "--with-build-gcc=/nonexistent/gcc"
      , "app"
      ]
  assertOutputContains "build-stage program settings" r
  assertOutputContains "are ignored" r
  withPlan $ do
    app <- runPlanExe' "app" "app" []
    assertOutputContains "app-stage: host" app
    assertOutputContains "app-lib-stage: host" app
