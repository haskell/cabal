import Test.Cabal.Prelude

-- Test for issue #12573;
-- If the test-directory or cabal file contain a utf-8 symbol (文档), the multi-repl shouldn't crash.
main = do
  cabalTest' "all" $ do
    skipUnlessGhcVersion ">= 9.4"
    liftIO $ skipIfWindows "doesn't use UTF-8 for FilePaths, thus this test directory doesn't work."
    res <- cabalWithStdin "v2-repl" ["--enable-multi-repl", "all"] "print 0xdeadbeef"
    -- The multi-repl session is functional (0xdeadbeef = 3735928559)
    assertOutputContains "3735928559" res
