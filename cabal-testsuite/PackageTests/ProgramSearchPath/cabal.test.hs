import Test.Cabal.Prelude

-- Check we consistently use the B>E>P order for program search paths.
--
-- See Note [Program search paths] in Distribution.Simple.Configure.
main = do
  skipIfWindows "the fake programs are shell scripts"
  cabalTest $ do
    tmpDir <- testTmpDir <$> getTestEnv
    require_unique <- anyCabalVersion ">= 3.19"
    addToPath (tmpDir </> "scripts-path") $
      cabal "v2-build" $
        ["all", "--extra-prog-path=" ++ tmpDir </> "scripts-extra"]
          ++ ["--constraint=custom +unique-path" | require_unique]
