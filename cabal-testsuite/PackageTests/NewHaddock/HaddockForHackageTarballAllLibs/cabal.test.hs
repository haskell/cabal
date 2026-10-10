import Test.Cabal.Prelude

-- The tarball should contain the haddocks of all the libraries of the
-- package.
main = cabalTest $ do
  env <- getTestEnv
  _ <- cabal' "haddock" ["--haddock-for-hackage"]
  let tarball = testDistDir env </> "mypkg-0.1-docs.tar.gz"
  tarballContents <- tar' ["-tzf", tarball]
  -- Docs for the main library...
  assertOutputContains "mypkg-0.1-docs/MyPkg.html" tarballContents
  -- ... as well as for the internal library.
  assertOutputContains "mypkg-0.1-docs/inner/MyPkg-Inner.html" tarballContents
