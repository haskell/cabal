import Test.Cabal.Prelude

-- Install an executable from a tarball URI, outside of a project.
main = cabalTest $ do
  env <- getTestEnv
  skipIf "Windows: uses a file URI" isWindows
  let sdistDir = testWorkDir env </> "sdist"
      tarball = sdistDir </> "p-1.0.tar.gz"
      installdir = testPrefixDir env </> "bin"
  recordMode DoNotRecord $ do
    cabal "v2-sdist" ["p", "--output-directory", sdistDir]
    cabal "install"
      [ "--ignore-project"
      , "file://" ++ tarball
      , "--installdir", installdir
      , "--install-method=copy"
      , "--overwrite-policy=always"
      ]
    runInstalledExe' "p" [] >>= assertOutputContains "installed from a tarball"
