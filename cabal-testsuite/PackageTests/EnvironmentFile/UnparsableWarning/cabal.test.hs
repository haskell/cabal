import Test.Cabal.Prelude
import System.FilePath

main = cabalTest $ recordMode DoNotRecord $ do
    env <- getTestEnv
    let envFile = testCurrentDir env </> "bad.environment"
    -- Pointing cabal at an existing, malformed environment file should produce
    -- a warning naming the location (line and column) of the parse error, not
    -- merely state that the file is unparsable.
    -- See https://github.com/haskell/cabal/issues/11963
    res <- cabal' "install" ["--lib", "base", "--package-env=" ++ envFile]
    assertOutputContains "line 1, column 1" res
