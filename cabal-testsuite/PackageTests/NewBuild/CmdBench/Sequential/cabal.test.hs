import Test.Cabal.Prelude

-- #7557: benchmarks must not run concurrently, even when building in
-- parallel. Each benchmark fails if another one is running at the same time,
-- see Bench.hs.
main = cabalTest $
    -- Parallel flag means output of this test is nondeterministic
    recordMode DoNotRecord $
        cabal "v2-bench" ["-j3", "all"]
