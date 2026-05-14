import Test.Cabal.Prelude

main = do
    skipUnlessWindows
    cabalTest $ recordMode DoNotRecord $ fails $ do
      res <- cabal' "build" ["--enable-executable-dynamic", "--disable-shared"]
      assertOutputContains "does not support shared executables" res

