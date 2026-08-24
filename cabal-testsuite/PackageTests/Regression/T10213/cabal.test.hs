import Test.Cabal.Prelude

-- Regression test for #10213: parsing of @extra-lib-dirs@ (and other
-- comma-separated list fields) must accept a trailing comma. Before the
-- fix, a trailing comma caused a parse error for @cabal-version < 3.0@.
main = cabalTest $ recordMode DoNotRecord $ do
  env <- getTestEnv
  let dir = testCurrentDir env
      lib1 = dir </> "foo-lib"
      lib2 = dir </> "bar-extra"

  -- @extra-lib-dirs@ requires absolute paths, so we have to write the
  -- @.cabal@ file at runtime rather than committing absolute paths.
  liftIO $ writeFile (dir </> "t10213.cabal") $ unlines
    [ "cabal-version: 2.4"
    , "name:          t10213"
    , "version:       0"
    , "build-type:    Simple"
    , ""
    , "library"
    , "  build-depends:    base"
    , "  default-language: Haskell2010"
    , "  exposed-modules:  T10213"
    , "  extra-lib-dirs:   " ++ lib1 ++ ","
    , "                    " ++ lib2 ++ ","
    ]

  res <- cabalG' [] "build" ["-v3"]

  -- The trailing commas must not be merged into the paths: the two
  -- directories should be parsed as two separate entries.
  assertOutputContains "foo-lib" res
  assertOutputContains "bar-extra" res
  assertOutputDoesNotContain "foo-lib," res
  assertOutputDoesNotContain "bar-extra," res
