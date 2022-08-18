import Test.Cabal.Prelude

-- Issue 7124
-- when we sdist, error should tell which package caused the failure

main :: IO ()
main = cabalTest $ do
  tmpdir <- fmap testTmpDir getTestEnv
  fails $ cabal "v2-sdist" ["--list-only", "--output-directory", tmpdir, "all"]
