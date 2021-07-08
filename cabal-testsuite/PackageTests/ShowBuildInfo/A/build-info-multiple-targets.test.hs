import           Test.Cabal.Prelude
import           Test.Cabal.DecodeShowBuildInfo

main = cabalTest $ do
    buildInfo <- runShowBuildInfo ["exe:A", "lib:A"]
    assertCommonBuildInfo buildInfo
    let [libBuildInfo, exeBuildInfo] = components buildInfo
    assertExeComponent exeBuildInfo "exe:A" ["Main.hs"] ["src"]
    assertLibComponent libBuildInfo "lib" ["A"] ["src"]