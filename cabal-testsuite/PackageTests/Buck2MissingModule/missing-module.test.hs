import Test.Cabal.Prelude

-- A component whose sources can't all be found is skipped with a warning
-- saying why (emitting a rule that names a missing file would take down
-- the whole buck2 build), and so is every component of the same package
-- that depends on it. Everything else is still generated.
main = cabalTest $ do
    cwd <- fmap testCurrentDir getTestEnv
    r <- recordMode DoNotRecord $ cabal' "buck2" []

    assertOutputContains "for module Absent" r
    assertOutputContains "skipping library broken-pkg" r
    assertOutputContains "skipping executable uses-lib" r

    let bzl = cwd </> "broken-pkg" </> "BUCK.cabal.bzl"
    assertFileDoesContain bzl "'name': 'standalone'"
    assertFileDoesNotContain bzl "'name': 'uses-lib'"
    assertFileDoesNotContain bzl "'kind': 'library'"
