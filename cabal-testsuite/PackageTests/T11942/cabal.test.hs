import Test.Cabal.Prelude

-- Setup.hs recompilation test for ticket #11942.
-- Before fixing, this test erroneously failed with an error of the form:
--
--  Saved package config file is outdated:
--    • the Cabal version changed from Cabal-3.16.0.0 to Cabal-3.17.0.0

main = cabalTest $ recordMode DoNotRecord $ do
  env <- getTestEnv

  -- Do a first build...
  cabal "v2-build" [ ]

  -- ... then modify Setup.hs and rebuild
  liftIO $ appendFile (testCurrentDir env </> "Setup.hs") "  -- some comment"
  cabal "v2-build" [ ]
