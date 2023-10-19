import Test.Cabal.Prelude

main = cabalTest . void $ do
  res <- cabal_raw' ["path", "--installdir"] Nothing

  assertOutputDoesNotContain "installdir:" res
  assertOutputContains "cabal/cabal-testsuite/PackageTests/Path/Single/cabal.dist/home/.cabal" res
