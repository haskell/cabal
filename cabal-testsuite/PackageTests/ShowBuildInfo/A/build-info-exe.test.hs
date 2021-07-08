import           Test.Cabal.Prelude
import           Test.Cabal.DecodeShowBuildInfo

main = cabalTest $ do
    buildInfo <- runShowBuildInfo ["exe:A"]
    assertCommonBuildInfo buildInfo
    assertEqual "Number of Components" 1 (length $ components buildInfo)
    let [exeComp] = components buildInfo
    assertExeComponent exeComp "exe:A" ["Main.hs"] ["src"]

    -- Must not have library as a dependency as "exe:A" does not depend on it.
    assertBool "Does not contain library as dependency"
        (all (/= "A-0.1.0.0-inplace") $ componentCompilerArgs exeComp)