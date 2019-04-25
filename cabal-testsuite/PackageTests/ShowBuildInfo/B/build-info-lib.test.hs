import           Test.Cabal.Prelude

main = cabalTest $ do
    r <- cabal' "new-show-build-info" ["lib:B"]
    assertOutputContains "\"cabal-version\":\"3.0.0.0\""     r
    assertOutputContains "\"compiler\":{"                    r
    assertOutputContains "\"flavour\":\"ghc\""               r
    assertOutputContains "\"compiler-id\":"                  r
    assertOutputContains "\"path\":"                         r
    assertOutputContains "\"type\":\"lib\""                  r
    assertOutputContains "\"name\":\"lib\""                  r
    assertOutputContains "\"unit-id\":\"B-0.1.0.0-inplace\"" r
    assertOutputContains "\"compiler-args\":["               r
    assertOutputContains "\"modules\":[\"A\"]"               r
    assertOutputContains "\"src-files\":[]"                  r
    assertOutputContains "\"src-dirs\":[\"src\"]"            r
