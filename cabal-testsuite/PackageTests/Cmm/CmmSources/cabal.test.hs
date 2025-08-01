import Test.Cabal.Prelude

main = cabalTest $ do
    res <- cabal' "v2-run" ["demo"]
    assertOutputContains "= Post common block elimination =" res
    assertOutputContains "In Box we have 0x" res
