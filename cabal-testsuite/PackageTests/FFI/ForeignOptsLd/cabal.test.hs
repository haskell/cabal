import Test.Cabal.Prelude

main = do
    skipIfOSX "Apple linker does not support --wrap"
    skipIfWindows "Windows linker does not support --wrap"
    cabalTest $ do
        cabal "v2-build" ["foreign-opts-ld-exe"]
        withPlan $ runPlanExe "foreign-opts-ld" "foreign-opts-ld-exe" []
