import Test.Cabal.Prelude

import Data.List (isSuffixOf)
import Distribution.Simple.Utils (getDirectoryContentsRecursive)
import System.FilePath ((</>))

-- https://github.com/haskell/cabal/issues/12212
-- The same single-hop re-export as 'HaddockReexportSingleHop', but for a
-- package whose name also occurs elsewhere in the haddock interface path.
--
-- The package name is load bearing: haddock output always lives under
-- @doc/html/<dir>@, so for a package named @html@ (a real Hackage package)
-- the interface path contains @html@ twice -- once as the fixed structural
-- component and once as the 'ForDevelopment' 'haddockDirName'. Recovering
-- the 'ForHackage' path must rewrite only the latter; rewriting every
-- matching component yields @doc/html-0.1.0.0-docs/html-0.1.0.0-docs@,
-- which does not exist, and the documentation is lost again.
main = cabalTest $ do
  res <- cabal' "haddock" ["--haddock-for-hackage"]
  assertOutputDoesNotContain "Couldn't find .haddock for export subFn" res

  env <- getTestEnv
  files <- liftIO $ getDirectoryContentsRecursive (testDistDir env)
  case filter (isSuffixOf "Mini.html") files of
    (path : _) -> assertFindInFile "Documentation of subFn" (testDistDir env </> path)
    [] ->
      assertFailure "Mini.html was not found in the dist directory"
