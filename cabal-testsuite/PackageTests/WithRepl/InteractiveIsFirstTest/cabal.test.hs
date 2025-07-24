{-# LANGUAGE OverloadedStrings #-}
import Distribution.Simple.Program
import Distribution.Simple.Program.GHC
import Distribution.Simple.Utils
import Test.Cabal.Prelude

-- On windows this test passed but then fails in CI with
--
-- D:\a\_temp\cabal-testsuite-12064\cabal-multi.dist\work\.\dist\multi-out-63884\paths\cabal-with-repl-exe-0.1.0.0-inplace-test-exe: removeDirectoryRecursive:removeContentsRecursive:removePathRecursive:removeContentsRecursive:removePathRecursive:DeleteFile "\\\\?\\D:\\a\\_temp\\cabal-testsuite-12064\\cabal-multi.dist\\work\\dist\\multi-out-63884\\paths\\cabal-with-repl-exe-0.1.0.0-inplace-test-exe": permission denied (The process cannot access the file because it is being used by another process.)
--

main = do
  -- Test that '--with-repl' with one target lists '--interactive' as the first argument
  -- This test should be deleted, once we reach Cabal 3.18, and enough HLS
  -- binaries have been released that support response file syntax.
  -- See Note [Make --interactive the first argument to GHC]
  mkTest "repl-single-target" $ \exePath -> do
    res <- cabalWithStdin "v2-repl" ["--with-repl=" ++ exePath, "test-exe"] ""
    assertOutputMatches "^--interactive" res
  mkTest "repl-multi-target" $ \exePath -> do
    requireGhcSupportsMultiRepl
    res <- cabalWithStdin "v2-repl" ["--enable-multi-repl", "--with-repl=" ++ exePath, "internal1", "internal2"] ""
    assertOutputMatches "^--interactive" res
  mkTest "repl-multi-target-all" $ \exePath -> do
    requireGhcSupportsMultiRepl
    res <- cabalWithStdin "v2-repl" ["--enable-multi-repl", "--with-repl=" ++ exePath, "all"] ""
    assertOutputMatches "^--interactive" res


mkTest name act = do
  skipIfCIAndWindows 11026
  cabalTest' name $ recordMode DoNotRecord $ do
    -- Build the executable
    cabal' "v2-build" ["test-exe"]
    -- Get the path to the built executable
    withPlan $ do
      exePath <- planExePath "cabal-with-repl-exe" "test-exe"
      act exePath

