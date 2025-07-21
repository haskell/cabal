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
  mkTest "normal-repl" $ \exePath -> do
    -- Try using the executable with --with-repl
    res <- cabalWithStdin "v2-repl" ["--with-repl=" ++ exePath, "test-exe"] ""
    assertOutputContains "My specific executable" res
  mkTest "multi-repl" $ \exePath -> do
    requireGhcSupportsMultiRepl
    res <- cabalWithStdin "v2-repl" ["--enable-multi-repl", "--with-repl=" ++ exePath, "all"] ""
    assertOutputContains "My specific executable" res


mkTest name act = do
  skipIfCIAndWindows 11026
  cabalTest' name $ recordMode DoNotRecord $ do
    -- Build the executable
    cabal' "v2-build" ["test-exe"]
    -- Get the path to the built executable
    withPlan $ do
      exePath <- planExePath "cabal-with-repl-exe" "test-exe"
      act exePath

