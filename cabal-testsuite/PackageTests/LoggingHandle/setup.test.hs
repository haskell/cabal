import Distribution.Simple
import Distribution.Simple.InstallDirs
import Distribution.Simple.Setup
  hiding ( testVerbosity )
import Distribution.Utils.Path
  ( interpretSymbolicPath )
import Distribution.Verbosity

import Test.Cabal.Prelude

import System.IO
  ( IOMode(ReadWriteMode), withFile )

import System.Directory
  ( createDirectoryIfMissing )

main = setupTest $ do
  skipUnlessAnyCabalVersion ">= 3.17"
  cwd <- fmap testCurrentDir getTestEnv
  let
    outFile = cwd </> "stdout_log.txt"
    errFile = cwd </> "stderr_log.txt"

  confRes <- setup' "configure" ["-v1", "--enable-tests", "--enable-benchmarks"]
  assertOutputDoesNotContain "Configuring" confRes
  assertFileDoesContain outFile "Configuring"

  buildRes <- setup' "build" ["-v1"]
  assertOutputDoesNotContain "unusedLib"   buildRes
  assertOutputDoesNotContain "unusedExe"   buildRes
  assertOutputDoesNotContain "unusedTest"  buildRes
  assertOutputDoesNotContain "unusedBench" buildRes
  assertFileDoesContain errFile "unusedLib"
  assertFileDoesContain errFile "unusedExe"
  assertFileDoesContain errFile "unusedTest"
  assertFileDoesContain errFile "unusedBench"

  assertOutputDoesNotContain "Compiling Lib" buildRes
  assertFileDoesContain outFile "Compiling Lib"

  testRes <- setup' "test" ["-v1"]
  assertOutputDoesNotContain "Test suite pkg-test: RUNNING..." testRes
  assertFileDoesContain outFile "Test suite pkg-test: RUNNING..."
  assertOutputDoesNotContain "I am the test-suite!" testRes
  assertFileDoesContain outFile "I am the test-suite!"

  benchRes <- setup' "bench" ["-v1"]
  assertOutputDoesNotContain "Benchmark pkg-bench: RUNNING..." benchRes
  assertFileDoesContain outFile "Benchmark pkg-bench: RUNNING..."
  assertOutputDoesNotContain "I am the benchmark!" benchRes
  assertFileDoesContain outFile "I am the benchmark!"
