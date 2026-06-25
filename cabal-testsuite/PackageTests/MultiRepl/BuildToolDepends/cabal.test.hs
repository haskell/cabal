import Test.Cabal.Prelude

-- Test for issue #11850: in a multi-repl session, an executable which is a
-- build-tool-depends of another repl target must be compiled to disk together
-- with its library dependencies (not just loaded in memory), so the build
-- tool can be linked and run.
main = do
  -- The library is itself a repl target here, as in the original report of
  -- issue #11850: before the fix it was loaded in memory only, so building
  -- the build tool on disk failed with
  -- "cannot satisfy -package-id foo-0.1.0.0-inplace".
  cabalTest' "all" $ do
    skipUnlessGhcVersion ">= 9.4"
    res <- cabalWithStdin "v2-repl" ["--enable-multi-repl", "all"] "print 0xdeadbeef"
    -- The tool and its library dep are built on disk, not merely loaded in memory
    assertOutputContains "Building executable 'foo' for foo-0.1.0.0" res
    -- The multi-repl session is functional (0xdeadbeef = 3735928559)
    assertOutputContains "3735928559" res
    -- The tool binary exists on disk and was linked against the on-disk library
    -- (0xdeadc0de = 3735929054)
    withPlan $ do
      r <- runPlanExe' "foo" "foo" []
      assertOutputContains "3735929054" r

  -- The repl targets are the two executables; the library is not a repl
  -- target but is still pulled onto disk as a dependency of the build tool.
  cabalTest' "exes" $ do
    skipUnlessGhcVersion ">= 9.4"
    res <- cabalWithStdin "v2-repl" ["--enable-multi-repl", "exe:foo", "exe:bar"] "print 0xdeadbeef"
    assertOutputContains "Building executable 'foo' for foo-0.1.0.0" res
    -- Both executables are loaded into the multi-repl session
    assertOutputContains "Ok, two modules loaded." res
    assertOutputContains "3735928559" res
    withPlan $ do
      r <- runPlanExe' "foo" "foo" []
      assertOutputContains "3735929054" r
