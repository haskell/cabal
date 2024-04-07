import Test.Cabal.Prelude
main = do
    skipIfWindows "uninteresting"
    cabalTest $ do
        cabal' "clean" []
        res <- cabal' "build" ["-v2"]
        -- we want a single GHC invocation
        assertOutputMatches "X\\.c.*Y\\.c" res
        cabal' "clean" []
        -- `--semaphore` should translate to `-jsem` here but only if GHC
        -- supports `-j` options for oneshot mode, which is not true of any
        -- released version yet
        res <- cabalG' ["--semaphore"] "build" ["-v2"]
        assertOutputDoesNotMatch "-c .*-jsem.*X\\.c.*Y\\.c" res
        assertOutputMatches "-c .*X\\.c.*Y\\.c" res
        cabal' "clean" []
        -- cabal's `-j4` isn't supposed to leak into GHC's `-j4`
        res <- cabalG' ["-j4"] "build" ["-v2"]
        assertOutputDoesNotMatch "-c .*-j4.*X\\.c.*Y\\.c" res
        assertOutputMatches "-c .*X\\.c.*Y\\.c" res
