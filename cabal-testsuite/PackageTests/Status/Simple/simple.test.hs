import           Test.Cabal.Prelude
import           Test.Cabal.DecodeShowBuildInfo
import           Data.Maybe (isJust)
import           Data.List (sort, nub)

main = cabalTest $ do
  -- output-format flag is missing but required, must fail
  r <- fails $ cabal' "status" ["--target", "Main.hs"]
  assertOutputContains "The status command requires the flag '--output-format'." r

  -- Simple file target tests
  runStatusWithTargets ["exe/Main.hs"]$ \si -> do
    resolveOnce "exe/Main.hs" si
  runStatusWithTargets ["exe/Main.hs", "exe/Main2.hs"] $ \si -> do
    resolveOnce "exe/Main.hs" si
    resolveOnce "exe/Main2.hs" si
  runStatusWithTargets ["src/MyLib.hs"] $ \si -> do
    rts <- resolve "src/MyLib.hs" si
    assertEqual "Ambiguous component" 3 (length rts)
  runStatusWithTargets ["src/MyLib2.hs"] $ \si -> do
    resolveOnce "src/MyLib2.hs" si
  runStatusWithTargets ["bench/Bench.hs"] $ \si -> do
    resolveOnce "bench/Bench.hs" si
  runStatusWithTargets ["test/Main.hs"]$ \si -> do
    resolveOnce "test/Main.hs" si
  runStatusWithTargets ["flibsrc/MyForeignLib/AnotherVal.hs",
                        "flibsrc/MyForeignLib/Hello.hs",
                        "flibsrc/MyForeignLib/SomeBindings.hsc",
                        "csrc/MyForeignLibWrapper.c"
                        ]$ \si -> do
    resolveOnce "flibsrc/MyForeignLib/AnotherVal.hs" si
    resolveOnce "flibsrc/MyForeignLib/Hello.hs" si
    resolveOnce "flibsrc/MyForeignLib/SomeBindings.hsc" si
    unresolvable "csrc/MyForeignLibWrapper.c" si
  runStatusWithTargets ["lib:Simple", "exe:Simple", "Simple:exe:Simple"] $ \si -> do
    resolveOnce "lib:Simple" si
    resolveOnce "exe:Simple" si
    resolveOnce "Simple:exe:Simple" si
  -- pkgs syntax tests
  runStatusWithTargets ["Benchs"] $ \si -> do
    resolveOnce "Benchs" si
  -- meta targets
  runStatusWithTargets ["all"] $ \si -> do
    rts <- resolve "all" si
    assertEqual "Seven components" 7 (length rts)
  runStatusWithTargets ["exes"] $ \si -> do
    rts <- resolve "exes" si
    assertEqual "Two executables" 2 (length rts)
  runStatusWithTargets ["tests"] $ \si -> do
    resolveOnce "tests" si
  runStatusWithTargets ["benchmarks"] $ \si -> do
    resolveOnce "benchmarks" si

  -- unknown target selectors
  runStatusWithTargets ["executables"] $ \si ->
    unresolvable "executables" si
  runStatusWithTargets ["Main2.hs"] $ \si ->
    unresolvable "Main2.hs" si

  -- partially works, Main3.hs isn't known while `src/MyLib2.hs` is.
  runStatusWithTargets ["Main3.hs", "src/MyLib2.hs"] $ \si -> do
    unresolvable "Main3.hs" si
    resolveOnce "src/MyLib2.hs" si

  -- component fails to compile, still works
  runStatusWithTargets ["src/Fails.hs"] $ \si -> do
    resolveOnce "src/Fails.hs" si
  cabal "status" ["--output-format=json", "--compiler", "--target", "Fails.hs"]
  -- unbuildable target, resolves to 'null'
  runStatusWithTargets ["src/Unbuildable.hs"] $ \si -> do
    unresolvable "src/Unbuildable.hs" si
  cabal "status" ["--output-format=json", "--compiler", "--target", "src/Unbuildable"]
  where
    runStatusWithTargets :: [String] -> (StatusInformation -> TestM a) -> TestM  a
    runStatusWithTargets targets act = do
      r <- cabal' "status" $ ["--output-format=json"] ++ concatMap (\t -> ["--target", t]) targets
      statusInfo <- withJsonOutput r
      act statusInfo
