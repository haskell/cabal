import Test.Cabal.Prelude
main = setupAndCabalTest $ do
    skipUnless =<< ghcVersionIs (>= mkVersion [8,1])
    setup "configure" []
    r <- fails $ setup' "build" []
    assertOutputContains "Foobar" r
    assertRegex
      "error should be about not being able to find a module"
      "Could not (find|load) module"
      r
    return ()
