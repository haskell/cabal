import Test.Cabal.Prelude
import System.FilePath

main = cabalTest $ do
    env <- getTestEnv
    let envFile = testCurrentDir env </> "bad.environment"
    -- See https://github.com/haskell/cabal/issues/11963
    cabal "install" ["--lib", "base", "--package-env=" ++ envFile]
