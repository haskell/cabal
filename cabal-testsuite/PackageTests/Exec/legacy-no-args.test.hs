import Test.Cabal.Prelude
main = cabalTest $ fails (cabal' "v1-exec" []) >>= assertOutputContains "Please specify an executable to run"
