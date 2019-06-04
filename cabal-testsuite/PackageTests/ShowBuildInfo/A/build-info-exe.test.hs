import           Test.Cabal.Prelude

main = cabalTest $
  recordMode DoNotRecord $ do
    r <- cabal' "new-show-build-info" ["exe:A"]
    assertOutputContains "\"cabal-version\":\"3.0.0.0\""       r
    assertOutputContains "\"compiler\":{"                      r
    assertOutputContains "\"flavour\":\"ghc\""                 r
    assertOutputContains "\"compiler-id\":"                    r
    assertOutputContains "\"path\":"                           r
    assertOutputContains "\"type\":\"exe\""                    r
    assertOutputContains "\"name\":\"exe:A\""                  r
    assertOutputContains "\"unit-id\":\"A-0.1.0.0-inplace-A\"" r
    assertOutputContains "\"compiler-args\":["                 r
    assertOutputContains "\"modules\":[]"                      r
    assertOutputContains "\"src-files\":[\"Main.hs\"]"         r
    assertOutputContains "\"src-dirs\":[\"src\"]"              r

