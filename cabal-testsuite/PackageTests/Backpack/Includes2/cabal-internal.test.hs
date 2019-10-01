import Test.Cabal.Prelude

main = cabalTest $ do
    skipUnless =<< ghcVersionIs (>= mkVersion [8,1])
    skipIf =<< isWindows -- TODO: https://github.com/haskell/cabal/issues/6271
    withProjectFile "cabal.internal.project" $ do
        cabal "v2-build" ["exe"]
        withPlan $ do
            r <- runPlanExe' "Includes2" "exe" []
            assertOutputContains "minemysql minepostgresql" r
