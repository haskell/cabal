import Test.Cabal.Prelude
main = cabalTest $ withDelay $ do
        writeSourceFile ("p/P.hs") "module P where\np = \"AAA\""
        cabal "v2-build" ["p","q"]
        delay
        writeSourceFile ("p/P.hs") "module P where\np = \"BBB\""
        cabal "v2-build" ["p"]
        cabal "v2-build" ["q"]
        withPlan $
            runPlanExe' "q" "qexe" []
                >>= assertOutputContains "BBB"
