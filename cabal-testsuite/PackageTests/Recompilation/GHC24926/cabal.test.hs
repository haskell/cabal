import Test.Cabal.Prelude

-- See ghc#24926
main = cabalTest $ do
  recordMode DoNotRecord $ do

    root <- testTmpDir <$> getTestEnv

    writeInternalOrig root
    cabal "test" []

    liftIO $ writeFile (root ++ "/src/Internal.hs")
      " module Internal where;\

      \ data Unused = Unused;"
    fails $ cabal "test" [] -- broken module on purpose

    writeInternalOrig root
    out <- cabal' "test" [] -- shouldn't fail!

    assertOutputDoesNotContain
      "<no location info>: error:" out
    assertOutputDoesNotContain
      "Cannot continue after interface file error" out

  where

    writeInternalOrig r = liftIO $ do
      writeFile (r ++ "/src/Internal.hs")
        " module Internal where;\

        \ data Unused = Unused;\

        \ b :: IO (); \
        \ b = pure ();"

