import Test.Cabal.Prelude

main = withShorterPathForNewBuildStore $ \storeDir -> cabalTest $ withRepo "repo" $ do
  -- Checks if hoogle txt files are generated.
  -- Logs contain something like "Documentation created: dist/doc/html/indef/indef.txt", so we don't need
  -- to do extra check
  cabalG ["--store-dir=" ++ storeDir] "v2-build"
    [ "example"
    , "--enable-documentation"
    , "--haddock-hoogle"
    ]
