import Test.Cabal.Prelude

import Data.List (isSuffixOf)
import Distribution.Simple.Utils (getDirectoryContentsRecursive)
import System.FilePath ((</>))

-- https://github.com/haskell/cabal/issues/12212
-- Test that documentation is not lost for identifiers imported from an
-- internal sub-library (single-hop re-export).
--
--   Sub (declares 'subFn' with docs, in internal sub-library)
--     -> Mini (imports 'subFn' from Sub)
--
-- This is the minimal reproduction from
-- https://github.com/haskell/cabal/issues/12212#issuecomment-5231508298
main = cabalTest $ do
  res <- cabal' "haddock" ["--haddock-for-hackage"]
  -- The internal library's .haddock interface should be found, so the
  -- imported identifier's docs should not be lost.
  assertOutputDoesNotContain "Couldn't find .haddock for export subFn" res

  -- Verify that the documentation for 'subFn' actually appears in the
  -- generated HTML for the 'Mini' module.
  env <- getTestEnv
  files <- liftIO $ getDirectoryContentsRecursive (testDistDir env)
  case filter (isSuffixOf "Mini.html") files of
    (path : _) -> assertFindInFile "Documentation of subFn" (testDistDir env </> path)
    [] ->
      assertFailure "Mini.html was not found in the dist directory"
