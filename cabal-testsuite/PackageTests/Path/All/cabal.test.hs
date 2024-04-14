import Test.Cabal.Prelude
import Data.List (subsequences)

allOutputFormats =
  [ ["--output-format", "json"]
  , ["--output-format", "key-value"]
  , [] -- no specific output format
  ]

cabalPathPathFlags =
  [ "--cache-home"
  , "--remote-repo-cache"
  , "--logs-dir"
  , "--store-dir"
  , "--config-file"
  , "--installdir"
  ]

main = cabalTest $ do
  forM_ allOutputFormats $ \outputFormat -> do
    -- Mix and match with some flags
    cabal "path" $ outputFormat <> ["--compiler-info", "--logs-dir", "--installdir"]
    cabal "path" $ outputFormat <> ["--store-dir", "--compiler-info", "--config-file"]
    cabal "path" $ outputFormat <> ["--remote-repo-cache", "--compiler-info"]
    cabal "path" $ outputFormat <> []
    -- 'cabal path' works when the compiler is unknown but no compiler info is asked.
    -- requires '-z' flag.
    forM_ cabalPathPathFlags $ \pathFlag -> do
      cabal "path" $ ["-w", "unknown-compiler", "-z"] <> outputFormat <> [pathFlag]
