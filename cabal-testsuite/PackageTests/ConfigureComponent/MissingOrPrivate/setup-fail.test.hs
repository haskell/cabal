
import Test.Cabal.Prelude
main = setupTest $ do
    withPackageDb $ do
        base_id <- getIPID "base"
        setup_install ["foo-internal", "--cid", "foo-internal-0.1-abc"]
        r <- fails $ setup' "configure" [ "exe" ]
        assertOutputContains "Lib" r
