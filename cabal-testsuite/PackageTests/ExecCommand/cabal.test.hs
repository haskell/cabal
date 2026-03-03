
import Test.Cabal.Prelude

main :: IO ()
main = cabalTest $ do
  cabal "v2-build" ["my:exe:my-executable"]

  withPlan $ do
    exePath <- planExePath "my" "my-executable"

    -- The test is to check that we’re not acceidentally passing more
    -- than expected.
    --
    -- Target executable may not accept any extra option cabal may pass.
    -- Or it may and it would lead to incorrect results and lengthy
    -- debugging, which is worse.
    cabal "v2-exec" [exePath, "--", "--required", "foo"]

