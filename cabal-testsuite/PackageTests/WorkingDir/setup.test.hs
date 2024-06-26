import Test.Cabal.Prelude

main = cabalTest $ recordMode DoNotRecord $ do
  skipUnlessAnyCabalVersion "< 3.13"
  cabalWithStdin "v2-build" [] ""
  return ()
