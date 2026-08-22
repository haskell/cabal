import Test.Cabal.Prelude

import Data.List (isSuffixOf)
import Distribution.Simple.Utils (getDirectoryContentsRecursive)
import System.FilePath ((</>))

-- https://github.com/haskell/cabal/issues/12212
-- Test that documentation is not lost for identifiers re-exported through
-- more than one hop across internal sub-libraries.
--
--   Internal.Leaf (declares 'foo' with docs)
--     -> Internal.Aggregate (re-exports 'module Internal.Leaf')
--       -> Public (imports 'foo' from Internal.Aggregate)
main = cabalTest $ do
  res <- cabal' "haddock" ["--haddock-for-hackage"]
  -- The internal library's .haddock interface should be found, so the
  -- re-exported identifier's docs should not be lost.
  assertOutputDoesNotContain "Couldn't find .haddock for export foo" res

  -- Verify that the documentation for 'foo' actually appears in the
  -- generated HTML for the 'Public' module.
  env <- getTestEnv
  files <- liftIO $ getDirectoryContentsRecursive (testDistDir env)
  case filter (isSuffixOf "Public.html") files of
    (path : _) -> assertFindInFile "Docs for foo" (testDistDir env </> path)
    [] ->
      assertFailure "Public.html was not found in the dist directory"
