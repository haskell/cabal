import Test.Cabal.Prelude

main :: IO ()
main = setupTest . recordMode DoNotRecord . withPackageDb $ do
  -- This test exposes a recompilation bug in ghc versions 9.0.2 and 9.2.8
  skipIfGhcVersion "== 9.0.2 || == 9.2.8 || < 8.0 "
  setup_install ["-fgenerate"]
  r1 <- runInstalledExe' "autogen-toggle-test" []
  setup_install ["-f-generate"]
  r2 <- runInstalledExe' "autogen-toggle-test" []
  assertOutputContains "Real module, ship to production" r1
  assertOutputContains "Prebuilt module, don't use in production" r2
