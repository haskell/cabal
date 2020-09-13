import Data.List (isPrefixOf)
import Test.Cabal.Prelude

-- Test that checkRelocate doesn't fail when library directory of dependee
-- contains '..'
main = setupAndCabalTest $ withPackageDb $ do
  skipIfWindows
  skipUnlessGhcVersion ">= 7.6"
  env <- getTestEnv
  let pkgroot = takeDirectory $ testPackageDbDir env
      prefix = testTmpDir env </> "prefix"
  assertBool "we need a prefix that is not under pkgroot for this test" $
    not $ pkgroot `isPrefixOf` prefix
  withDirectory "dependee" $
    setup_install ["--enable-relocatable", "--prefix", prefix]
  withDirectory "depender" $
    setup_install ["--enable-relocatable", "--prefix", prefix]
