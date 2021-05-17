-- When Module.f is changed, with cabal <= 3.2 this non-deterministically fails
-- to compile and, if it doesn't fail, it also non-deterministically gives
-- a wrong answer (ignoring the change to Module.f in the output, despite
-- recompiling, so probably the wrong library is linked in); when running
-- manually on my machine, three changes to Module.hs are enough to trigger
-- the error, often two are enough, even with cabal 3.2, even to get
-- compilation error
--   "Ambiguous module name `Module': it was found in multiple packages: issue5782-0.1 issue5782-0.1"
-- not only the wrong result from exe run.
--
-- The dummy "--installdir=." is needed for cabal <= 3.2
-- and also to match cabal output on different OSes
-- (default installdir is different on various OSes).

import Test.Cabal.Prelude
main = withShorterPathForNewBuildStore $ \storeDir ->
  cabalTest $
    withSourceCopy . withDelay $ do
        writeSourceFile "issue5782/src/Module.hs" "module Module where\nf = \"AAA\""
        cabalG ["--store-dir=" ++ storeDir, "--installdir=" ++ storeDir, "--overwrite-policy=always"] "v2-install" ["issue5782"]
        withPlan $
            runPlanExe' "issue5782" "E" []
                >>= assertOutputContains "AAA"
        delay
        writeSourceFile "issue5782/src/Module.hs" "module Module where\nf = \"BBB\""
        cabalG ["--store-dir=" ++ storeDir, "--installdir=" ++ storeDir, "--overwrite-policy=always"] "v2-install" ["issue5782"]
        withPlan $
            runPlanExe' "issue5782" "E" []
                >>= assertOutputContains "BBB"
        writeSourceFile "issue5782/src/Module.hs" "module Module where\nf = \"CCC\""
        delay  -- different spot to try another scenario
        cabalG ["--store-dir=" ++ storeDir, "--installdir=" ++ storeDir, "--overwrite-policy=always"] "v2-install" ["issue5782"]
        withPlan $
            runPlanExe' "issue5782" "E" []
                >>= assertOutputContains "CCC"
