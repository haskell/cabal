import Test.Cabal.Prelude

main = setupTest $ recordMode DoNotRecord $ do
  withDirectory "WD_NO_MENTION" $ setup_build []
