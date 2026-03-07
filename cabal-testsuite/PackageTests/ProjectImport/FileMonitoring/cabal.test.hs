import Test.Cabal.Prelude
import System.Exit (ExitCode(ExitFailure))
import System.IO

main = cabalTest (withProjectFile "cabal.project" $ do
  result <- fails $ recordMode DoNotRecord $ cabal' "build" []
  assertExitCode (ExitFailure 1) result
  assertOutputContains "Failed to build cabal-project-repro-0.1.0.0-inplace-cabal-project-repro-test." result

  -- change the imported project file
  test_dir <- fmap testTmpDir getTestEnv
  liftIO $ writeFile (test_dir </> "test" </> "tests-toggle.config") "package *\n  Tests: False"
  result' <- recordMode DoNotRecord $ cabal' "build" []
  assertOutputDoesNotContain "Test suite not yet implement" result'
  assertOutputDoesNotContain "Failed to build cabal-project-repro-0.1.0.0-inplace-cabal-project-repro-test." result'
  )
