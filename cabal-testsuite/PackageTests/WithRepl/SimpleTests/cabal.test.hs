import Test.Cabal.Prelude
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))

main = do
  -- Test that --with-repl works with a valid GHC path
  cabalTest' "with-repl-valid-path" $ do
    cabal' "clean" []
    -- Get the path to the system GHC
    ghc_prog <- requireProgramM ghcProgram
    res <- cabalWithStdin "v2-repl" ["--with-repl=" ++ programPath ghc_prog] ""
    assertOutputContains "Ok, one module loaded." res
    assertOutputContains "GHCi, version" res

  -- Test that --with-repl fails with an invalid path
  cabalTest' "with-repl-invalid-path" $ do
    cabal' "clean" []
    res <- fails $ cabalWithStdin "v2-repl" ["--with-repl=/nonexistent/path/to/ghc"] ""
    assertOutputContains "does not exist" res
