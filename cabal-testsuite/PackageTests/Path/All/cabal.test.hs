import Test.Cabal.Prelude
import Data.List (subsequences)

allOutputFormats =
  [ "json"
  , "key-value"
  ]

allFlags =
  [ "--compiler-info"
  , "--cache-dir"
  , "--logs-dir"
  , "--store-dir"
  , "--config-file"
  , "--installdir"
  ]

main = cabalTest $ do
  forM_ allOutputFormats $ \outputFormat -> do
    -- Order of flags should not matter, neither does any flag depend on the
    -- existence of any other flag.
    --
    -- 'subsequences' generated "n over k" for k in (1 .. n)
    forM_ (subsequences allFlags) $ \flags -> do
      cabal "path" $ ["--output-format", outputFormat] <> flags


