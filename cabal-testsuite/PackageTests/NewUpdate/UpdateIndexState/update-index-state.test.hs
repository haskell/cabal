import Test.Cabal.Prelude

main = cabalTest $ withRemoteRepo "repo" $ do
  -- This test causes a warning about missing mirrors, the warning is
  -- included in the expected output to make the test pass but it's not
  -- part of the test expectations.
  cabal "update" ["repository.localhost,2022-01-28T02:36:41Z"]
  cabal "update" ["repository.localhost,2016-09-24T17:47:48Z"]
  cabal "update" ["repository.localhost,2022-01-28T02:36:41Z"]
