import Test.Cabal.Prelude

import Data.List (isPrefixOf, isSuffixOf)
import System.Directory (listDirectory)

-- With --enable-library-for-ghci the library's object files are merged into
-- a single GHCi object. GHC >= 9.4 does the merging for us via
-- `ghc --merge-objs`; with older GHCs Cabal runs `ld -r` itself.
main = do
  skipIfWindows "GHCi libraries are not built with GHC's Windows toolchain"
  setupTest $ recordMode DoNotRecord $ do
    opts <- resultOutput <$> ghc' ["--show-options"]
    let mergeObjs = "--merge-objs" `elem` lines opts
    setup "configure" ["--enable-library-for-ghci"]
    r <- setup' "build" []
    -- `ld -r -o <object>` is the invocation Cabal makes when it merges by itself.
    (if mergeObjs then assertOutputDoesNotContain else assertOutputContains) " -r -o " r
    dist <- testDistDir <$> getTestEnv
    objs <- liftIO $ filter isGhciObject <$> listDirectory (dist </> "build")
    assertBool ("expected one GHCi object in dist/build, found " ++ show objs) (length objs == 1)
  where
    isGhciObject f = "HS" `isPrefixOf` f && ".o" `isSuffixOf` f
