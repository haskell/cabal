import Test.Cabal.Prelude

import System.IO (appendFile)

main :: IO ()
main =
  cabalTest $ do
    env <- getTestEnv
    let conf = testUserCabalConfigFile env
    liftIO $
      appendFile conf $
        unlines
          [ "program-default-options"
          , "  ghc-options: -fno-full-laziness"
          , "  ar-options: -baz"
          ]
    res <- recordMode DoNotRecord $ cabal' "v2-build" []
    assertOutputDoesNotContain "-fno-full-laziness -fno-full-laziness" res
