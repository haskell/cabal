import           Test.Cabal.Prelude
import           Test.Cabal.DecodeShowBuildInfo

main = cabalTest $ do
    buildInfo <- runShowBuildInfo ["lib:B"]
    assertCommonBuildInfo buildInfo
    assertEqual "Number of Components" 1 (length $ components buildInfo)
    let [libComp] = components buildInfo
    assertLibComponent libComp "lib" ["B"] ["lib"]