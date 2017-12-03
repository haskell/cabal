import Test.Cabal.Prelude
main = cabalTest $ do

    -- This package has explicit setup dependencies that do not include Cabal.
    -- new-build should try to build it, but configure should fail because
    -- Setup.hs just prints an error message and exits.
    r <- fails $ cabal' "new-build" ["custom-setup-without-cabal"]
    assertOutputContains "My custom Setup" r
