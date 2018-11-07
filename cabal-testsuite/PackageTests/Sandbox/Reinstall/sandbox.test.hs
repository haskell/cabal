import Test.Cabal.Prelude
main = cabalTest $ do
    withSourceCopy . withDelay . withDirectory "p" . withSandbox $ do
        cabal_sandbox "add-source" ["../q"]
        cabal "v1-install" ["--only-dependencies"]
        recordMode RecordMarked $ cabal "v1-run" ["p", "-v0"]
        delay
        copySourceFileTo "../q/Q.hs.in2" "../q/Q.hs"
        recordMode RecordMarked $ cabal "v1-run" ["p", "-v0"]
