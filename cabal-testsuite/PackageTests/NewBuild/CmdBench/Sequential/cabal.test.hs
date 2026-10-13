import Test.Cabal.Prelude

import Data.List (isInfixOf, isPrefixOf, isSuffixOf)

-- #7557: benchmarks are only run once everything is built, and one at a time,
-- even when building in parallel.
main = cabalTest $
    -- Parallel flag means output of this test is nondeterministic
    recordMode DoNotRecord $ do
        -- Each benchmark fails if another one is running at the same time,
        -- see Bench.hs.
        res <- cabal' "v2-bench" ["-j3", "all"]
        let isBuildStep l =
                any (`isPrefixOf` l) ["Configuring ", "Preprocessing ", "Building ", "Linking "]
                    || any (`isInfixOf` l) ["] Compiling ", "] Linking "]
            isBenchmarkRun l = "Benchmark " `isPrefixOf` l && ": RUNNING..." `isSuffixOf` l
            output = lines (filter (/= '\r') (resultOutput res))
            afterFirstRun = dropWhile (not . isBenchmarkRun) output
        assertEqual "Number of benchmarks run" 3 (length (filter isBenchmarkRun output))
        assertBool "Something was built after the first benchmark started" $
            not (any isBuildStep afterFirstRun)
