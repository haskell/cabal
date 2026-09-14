import Test.Cabal.Prelude

-- --unpack puts a vendored package's source under vendor/src so that it can
-- be worked on as a local package, which then takes precedence over the
-- vendored tarball.
main = cabalTest $ withShorterPathForNewBuildStore $ withRepo "repo" $ do
  env <- getTestEnv
  let cwd = testCurrentDir env
      vendorDir = cwd </> "vendor"
      srcDir = vendorDir </> "src" </> "my-lib-1.0"

  -- Unpacking everything is refused.
  fails $ cabal "v2-vendor" ["--unpack"]

  r <- cabal' "v2-vendor" ["my-lib", "--unpack"]
  shouldExist (vendorDir </> "my-lib-1.0.tar.gz")
  shouldExist (srcDir </> "my-lib.cabal")
  shouldExist (srcDir </> "MyLib.hs")
  assertOutputContains "vendor/src/my-lib-1.0" r

  -- Edit the unpacked copy; the project now builds from it, not from the
  -- tarball, and a second --unpack leaves the edits alone.
  liftIO $ writeFile (srcDir </> "MyLib.hs") $
    unlines
      [ "module MyLib (message) where"
      , "message :: String"
      , "message = \"hello from the local copy\""
      ]
  cabal' "v2-vendor" ["my-lib", "--unpack"] >>= assertOutputContains "already exists"
  assertFileDoesContain (srcDir </> "MyLib.hs") "hello from the local copy"
  withProjectFile "cabal.local.project" $
    cabal' "v2-run" ["main"] >>= assertOutputContains "hello from the local copy"
