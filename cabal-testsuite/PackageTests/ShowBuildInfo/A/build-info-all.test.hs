import           Test.Cabal.Prelude
import           Test.Cabal.DecodeShowBuildInfo

main = cabalTest $ do
    buildInfo <- runShowBuildInfo ["all", "--enable-tests"]
    assertCommonBuildInfo buildInfo
    assertEqual "Number of Components" 4 (length $ components buildInfo)
    let [libAComp, exeComp, testComp, libBComp] = components buildInfo
    assertExeComponent exeComp "exe:A" ["Main.hs"] ["src"]
    assertLibComponent libAComp "lib" ["A"] ["src"]
    assertLibComponent libBComp "lib" ["B"] ["lib"]
    assertTestComponent testComp "test:A-tests" ["Test.hs"] ["src"]
