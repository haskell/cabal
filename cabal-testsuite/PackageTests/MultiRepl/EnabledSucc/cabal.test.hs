import Test.Cabal.Prelude

main = do
  cabalTest $ do
    skipUnlessGhcVersion ">= 9.4"
    -- the package order is non-deterministic.
    -- add Bar.Bar input to test that packages are trully loaded
    -- when GHC gets support for switching active units
    res <- cabalWithStdin "v2-repl" ["--enable-multi-repl","pkg-a", "pkg-b"] ""
    -- assertOutputContains "3735929054" res
    return ()
