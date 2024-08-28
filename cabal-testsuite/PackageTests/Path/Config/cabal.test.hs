import Test.Cabal.Prelude

cabalPathFlags =
  [ "--cache-home"
  , "--remote-repo-cache"
  , "--logs-dir"
  , "--store-dir"
  , "--config-file"
  , "--installdir"
  ]

main = cabalTest $ do
  forM_ cabalPathFlags $ \flag -> do
    -- Basic output
    cabal "path" ["-z", "--output-format=key-value", flag]
    -- Works for json, too
    cabal "path" ["-z", "--output-format=json", flag]
    -- defaults to key-value
    cabal "path" ["-z", flag]
  -- Honours cli overwrites
  cabalG ["--store-dir=test-dir"] "path" ["-z", "--output-format=key-value", "--store-dir"]
  cabalG ["--store-dir=test-dir"] "path" ["-z", "--output-format=json", "--store-dir"]
  cabalG ["--store-dir=test-dir"] "path" ["-z", "--store-dir"]
  forM_ cabalPathFlags $ \flag -> do
    -- Honour config file overwrites:
    cabalG ["--config-file=fake-cabal.config"] "path" ["-z", "--output-format=key-value", flag]
    cabalG ["--config-file=fake-cabal.config"] "path" ["-z", "--output-format=json", flag]
    cabalG ["--config-file=fake-cabal.config"] "path" ["-z", flag]
    -- Honour cabal.project file
    cabal "path" ["--output-format=key-value", flag]
    cabal "path" ["--output-format=json", flag]
    cabal "path" [flag]
    -- Honour config file and project file overwrites:
    cabalG ["--config-file=fake-cabal.config"] "path" ["--project-file=fake.cabal.project", "--output-format=key-value", flag]
    cabalG ["--config-file=fake-cabal.config"] "path" ["--project-file=fake.cabal.project", "--output-format=json", flag]
    cabalG ["--config-file=fake-cabal.config"] "path" ["--project-file=fake.cabal.project", flag]
