import Test.Cabal.Prelude
main = cabalTest $ do
  env <- getTestEnv

  liftIO $ appendFile (testUserCabalConfigFile env)
    $ unlines [ "repository hackage.haskell.org"
    , "  url:  http://hackage.haskell.org/"
    ]
  liftIO $ print $ testUserCabalConfigFile env
  liftIO $ print =<< readFile (testUserCabalConfigFile env)

  cabal "update" ["hackage.haskell.org,2022-01-28T02:36:41Z"]
  cabal "update" ["hackage.haskell.org,2016-09-24T17:47:48Z"]
  cabal "update" ["hackage.haskell.org,2022-01-28T02:36:41Z"]
