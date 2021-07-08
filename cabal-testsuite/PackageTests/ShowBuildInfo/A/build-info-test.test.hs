import           Test.Cabal.Prelude
import           Test.Cabal.DecodeShowBuildInfo

main = cabalTest $ do
    buildInfo <- runShowBuildInfo ["test:A-tests"]
    assertCommonBuildInfo buildInfo
    assertEqual "Number of Components" 1 (length $ components buildInfo)
    let [testComp] = components buildInfo
    assertTestComponent testComp "test:A-tests" ["Test.hs"] ["src"]

    -- Must have library as a dependency as "test:A-tests" depends on it.
    assertBool "Contains internal dependency"
        (any (== "A-0.1.0.0-inplace") $ componentCompilerArgs testComp)
