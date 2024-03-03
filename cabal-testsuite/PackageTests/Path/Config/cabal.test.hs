import Test.Cabal.Prelude

main = cabalTest $ do
  -- Basic output
  void $ cabal "path" ["-z", "--output-format=key-value", "--cache-dir"]
  void $ cabal "path" ["-z", "--output-format=key-value", "--logs-dir"]
  void $ cabal "path" ["-z", "--output-format=key-value", "--store-dir"]
  void $ cabal "path" ["-z", "--output-format=key-value", "--config-file"]
  void $ cabal "path" ["-z", "--output-format=key-value", "--installdir"]
  -- Works for json, too
  void $ cabal "path" ["-z", "--output-format=json", "--cache-dir"]
  void $ cabal "path" ["-z", "--output-format=json", "--logs-dir"]
  void $ cabal "path" ["-z", "--output-format=json", "--store-dir"]
  void $ cabal "path" ["-z", "--output-format=json", "--config-file"]
  void $ cabal "path" ["-z", "--output-format=json", "--installdir"]
  -- Honours cli overwrites
  void $ cabalG ["--store-dir=test-dir"] "path" ["-z", "--output-format=key-value", "--store-dir"]
  void $ cabalG ["--store-dir=test-dir"] "path" ["-z", "--output-format=json", "--store-dir"]
  -- Honour config file overwrites:
  void $ cabalG ["--config-file=fake-cabal.config"] "path" ["-z", "--output-format=key-value", "--cache-dir"]
  void $ cabalG ["--config-file=fake-cabal.config"] "path" ["-z", "--output-format=key-value", "--logs-dir"]
  void $ cabalG ["--config-file=fake-cabal.config"] "path" ["-z", "--output-format=key-value", "--store-dir"]
  void $ cabalG ["--config-file=fake-cabal.config"] "path" ["-z", "--output-format=key-value", "--config-file"]
  void $ cabalG ["--config-file=fake-cabal.config"] "path" ["-z", "--output-format=key-value", "--installdir"]

  void $ cabalG ["--config-file=fake-cabal.config"] "path" ["-z", "--output-format=json", "--cache-dir"]
  void $ cabalG ["--config-file=fake-cabal.config"] "path" ["-z", "--output-format=json", "--logs-dir"]
  void $ cabalG ["--config-file=fake-cabal.config"] "path" ["-z", "--output-format=json", "--store-dir"]
  void $ cabalG ["--config-file=fake-cabal.config"] "path" ["-z", "--output-format=json", "--config-file"]
  void $ cabalG ["--config-file=fake-cabal.config"] "path" ["-z", "--output-format=json", "--installdir"]

  -- Honour cabal.project file
  void $ cabal "path" ["--project-file=fake.cabal.project", "--output-format=key-value", "--cache-dir"]
  void $ cabal "path" ["--project-file=fake.cabal.project", "--output-format=key-value", "--logs-dir"]
  void $ cabal "path" ["--project-file=fake.cabal.project", "--output-format=key-value", "--store-dir"]
  void $ cabal "path" ["--project-file=fake.cabal.project", "--output-format=key-value", "--config-file"]
  void $ cabal "path" ["--project-file=fake.cabal.project", "--output-format=key-value", "--installdir"]
  -- Works for json, too
  void $ cabal "path" ["--project-file=fake.cabal.project", "--output-format=json", "--cache-dir"]
  void $ cabal "path" ["--project-file=fake.cabal.project", "--output-format=json", "--logs-dir"]
  void $ cabal "path" ["--project-file=fake.cabal.project", "--output-format=json", "--store-dir"]
  void $ cabal "path" ["--project-file=fake.cabal.project", "--output-format=json", "--config-file"]
  void $ cabal "path" ["--project-file=fake.cabal.project", "--output-format=json", "--installdir"]
