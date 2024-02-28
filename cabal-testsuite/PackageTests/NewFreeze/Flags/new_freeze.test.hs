import Test.Cabal.Prelude
import Control.Monad.IO.Class
import Data.Char
import System.Directory

-- Test that 'cabal v2-freeze' freezes flag choices. my-local-package depends
-- on my-library-dep. my-library-dep has a flag, my-flag, which defaults to
-- true.
main = cabalTest $
  withRepo "repo" $ do
    cabal' "v2-build" ["--dry-run"] >>= assertDependencyFlagChoice True

    cabal "v2-freeze" ["--constraint=my-library-dep -my-flag"]

    cwd <- fmap testCurrentDir getTestEnv
    let freezeFile = cwd </> "cabal.project.freeze"

    -- The freeze file should constrain the version and the flag.
    -- TODO: The flag constraint should be qualified. See
    -- https://github.com/haskell/cabal/issues/5134.
    assertFileDoesContain freezeFile "any.my-library-dep ==1.0"
    assertFileDoesContain freezeFile "my-library-dep -my-flag"

    -- cabal should be able to find an install plan that fits the constraints
    -- from the freeze file.
    cabal' "v2-build" ["--dry-run"] >>= assertDependencyFlagChoice False
  where
    -- my-library-dep's flag controls whether it depends on true-dep or
    -- false-dep, so this function uses the dependency to infer the flag choice.
    assertDependencyFlagChoice True out = do
        assertOutputContains "true-dep-1.0 (lib)" out
        assertOutputDoesNotContain "false-dep" out
    assertDependencyFlagChoice False out = do
        assertOutputContains "false-dep-1.0 (lib)" out
        assertOutputDoesNotContain "true-dep" out
