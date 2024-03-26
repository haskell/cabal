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
    void $ cabal "path" ["-z", "--output-format=key-value", flag]
    -- Works for json, too
    void $ cabal "path" ["-z", "--output-format=json", "--remote-repo-cache"]
  -- Honours cli overwrites
  void $ cabalG ["--store-dir=test-dir"] "path" ["-z", "--output-format=key-value", "--store-dir"]
  void $ cabalG ["--store-dir=test-dir"] "path" ["-z", "--output-format=json", "--store-dir"]
  forM_ cabalPathFlags $ \flag -> do
    -- Honour config file overwrites:
    void $ cabalG ["--config-file=fake-cabal.config"] "path" ["-z", "--output-format=key-value", flag]
    void $ cabalG ["--config-file=fake-cabal.config"] "path" ["-z", "--output-format=json", flag]
    -- Honour cabal.project file
    void $ cabal "path" ["--output-format=key-value", flag]
    void $ cabal "path" ["--output-format=json", flag]
    -- Honour config file and project file overwrites:
    void $ cabalG ["--config-file=fake-cabal.config"] "path" ["--project-file=fake.cabal.project", "--output-format=key-value", flag]
    void $ cabalG ["--config-file=fake-cabal.config"] "path" ["--project-file=fake.cabal.project", "--output-format=json", flag]
