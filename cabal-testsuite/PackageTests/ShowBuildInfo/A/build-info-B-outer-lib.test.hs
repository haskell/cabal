import           Test.Cabal.Prelude
import           Test.Cabal.DecodeShowBuildInfo

main = cabalTest $ do
    buildInfo <- runShowBuildInfo ["lib:B", "lib:A"]
    assertCommonBuildInfo buildInfo
    assertEqual "Number of Components" 2 (length $ components buildInfo)
    let [libAComp, libBComp] = components buildInfo
    assertLibComponent libAComp "lib" ["A"] ["src"]
    assertLibComponent libBComp "lib" ["B"] ["lib"]
