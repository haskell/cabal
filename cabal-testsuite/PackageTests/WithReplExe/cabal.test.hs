{-# LANGUAGE OverloadedStrings #-}
import Distribution.Simple.Program
import Distribution.Simple.Program.GHC
import Distribution.Simple.Utils
import Test.Cabal.Prelude

main = do
  mkTest $ \exePath -> do
    -- Try using the executable with --with-repl
    res <- cabalWithStdin "v2-repl" ["--with-repl=" ++ exePath, "test-exe"] ""
    assertOutputContains "My specific executable" res
  mkTest $ \exePath -> do
    requireGhcSupportsMultiRepl
    res <- cabalWithStdin "v2-repl" ["--enable-multi-repl", "--with-repl=" ++ exePath, "all"] ""
    assertOutputContains "My specific executable" res


mkTest act = cabalTest $ do
  -- Build the executable
  cabal' "v2-build" ["test-exe"]
  -- Get the path to the built executable
  withPlan $ do
    exePath <- planExePath "cabal-with-repl-exe" "test-exe"
    act exePath

