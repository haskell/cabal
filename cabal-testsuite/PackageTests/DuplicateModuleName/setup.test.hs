import Test.Cabal.Prelude
-- Test that if two components have the same module name, they do not
-- clobber each other.
main = setupAndCabalTest $ do
    skipIfAllCabalVersion "< 2.2"
    setup_build ["--enable-tests"]
    r1 <- fails $ setup' "test" ["foo"]
    assertOutputContains "test B" r1
    assertOutputContains "test A" r1
    r2 <- fails $ setup' "test" ["foo2"]
    assertOutputContains "test C" r2
    assertOutputContains "test A" r2
