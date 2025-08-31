import Test.Cabal.Prelude

main = do
    skipUnlessWindows
    cabalTest $ recordMode DoNotRecord $ fails $ do
      res <- cabal' "build" ["--enable-relocatable"]
      assertOutputContains "windows, does not support relocatable builds" res
