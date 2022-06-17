import Test.Cabal.Prelude

-- Do not output warning when an -O2 is behind a cabal flag.
main = cabalTest . expectBroken 7423 $ do
         res <- cabal' "check" []
         assertOutputContains "No errors or warnings could be found in the package." res

       -- once the test is not broken, replace main with:
       -- main = cabalTest $ cabal "check" []

