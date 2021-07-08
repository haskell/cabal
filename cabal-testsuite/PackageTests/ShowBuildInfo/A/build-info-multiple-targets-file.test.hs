import           Test.Cabal.Prelude
import           Test.Cabal.DecodeShowBuildInfo

main = cabalTest $ withSourceCopy $ do
    cwd <- fmap testCurrentDir getTestEnv
    let fp = cwd </> "unit.json"
    _ <- cabal' "show-build-info" ["--buildinfo-json-output=" ++ fp, "exe:A", "lib:A"]
    buildInfo <- decodeBuildInfoFile fp
    assertCommonBuildInfo buildInfo
    assertEqual "Number of Components" 2 (length $ components buildInfo)
    let [libBuildInfo, exeBuildInfo] = components buildInfo
    assertExeComponent exeBuildInfo "exe:A" ["Main.hs"] ["src"]
    assertLibComponent libBuildInfo "lib" ["A"] ["src"]
