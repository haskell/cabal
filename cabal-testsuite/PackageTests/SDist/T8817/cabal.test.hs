import Test.Cabal.Prelude

main = cabalTest $ do
  tmpdir <- fmap testTmpDir getTestEnv

  cabal "v2-sdist" ["--output-directory", tmpdir, "all"]

  let distTarPath = tmpdir </> "t8817-0.tar.gz"
      distExtractedPath = tmpdir </> "t8817-0"

  shouldExist distTarPath

  tar ["-xzf", distTarPath]

  shouldExist (distExtractedPath </> "f1")
  shouldExist (distExtractedPath </> "d1" </> "f2.txt")
  shouldExist (distExtractedPath </> "d1" </> "f3.txt")
