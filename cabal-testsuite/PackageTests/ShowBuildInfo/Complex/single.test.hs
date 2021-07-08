import           Test.Cabal.Prelude
import           Test.Cabal.DecodeShowBuildInfo

main = cabalTest $ withRepo "repo" $ do
  runShowBuildInfoWithMarker ["exe:Complex"] >>=
    (\buildInfo -> do
        assertCommonBuildInfo buildInfo
        let [exeComp] = components buildInfo
        assertExeComponent' exeComp "exe:Complex" ["Other", "Paths_Complex"] ["Main.lhs"] ["app"])

  runShowBuildInfoWithMarker ["lib:Complex"] >>=
    (\buildInfo -> do
        assertCommonBuildInfo buildInfo
        let [libComp] = components buildInfo
        assertLibComponent libComp "lib" ["A", "B", "C", "D", "Paths_Complex"] ["src", "doesnt-exist"])

  runShowBuildInfoWithMarker ["benchmark:complex-benchmarks"] >>=
    (\buildInfo -> do
        assertCommonBuildInfo buildInfo
        let [benchComp] = components buildInfo
        assertBenchComponent' benchComp "bench:complex-benchmarks" ["Paths_Complex"] ["Main.hs"] ["benchmark"])

  runShowBuildInfoWithMarker ["test:func-test"] >>=
    (\buildInfo -> do
        assertCommonBuildInfo buildInfo
        let [testComp] = components buildInfo
        assertTestComponent testComp "test:func-test" ["FuncMain.hs"] ["test"])

  runShowBuildInfoWithMarker ["test:unit-test"] >>=
    (\buildInfo -> do
        assertCommonBuildInfo buildInfo
        let [testComp] = components buildInfo
        assertTestComponent testComp "test:unit-test" ["UnitMain.hs"] ["test"])
