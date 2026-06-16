import Test.Cabal.Prelude

-- Test for #11411: cabal should re-invoke build tools (in this case, alex)
-- whose version changes in between builds.
main :: IO ()
main = cabalTest $ withRepo "repo" $ do
  -- Expect failure on first build: alex 3.4 outputs invalid Haskell.
  fails $ cabal "v2-build" ["all", "-c", "any.alex==3.4.*"]
  -- Second build should succeed: switching to alex 3.5 should cause cabal
  -- to re-invoke alex, which will output valid Haskell this time around.
  -- If this fails, it's because cabal didn't re-run the preprocessing step.
  cabal "v2-build" ["all", "-c", "any.alex==3.5.*"]
