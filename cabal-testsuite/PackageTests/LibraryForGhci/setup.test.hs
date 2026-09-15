import Test.Cabal.Prelude

import Data.Char (isSpace)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import System.Directory (listDirectory)

-- With --enable-library-for-ghci the library's object files are merged into
-- a single GHCi object. GHC >= 9.4 does the merging for us via
-- `ghc --merge-objs`, provided it has a merge tool configured ("Merge objects
-- command" in its settings; GHC's Windows bindists have none, and GHCi
-- libraries are then turned off). With older GHCs Cabal runs `ld -r` itself.
main = setupTest $ recordMode DoNotRecord $ do
  opts <- resultOutput <$> ghc' ["--show-options"]
  info <- resultOutput <$> ghc' ["--info"]
  let mergeObjs = "--merge-objs" `elem` lines opts
      mergeTool = parseGhcInfo info >>= lookup "Merge objects command"

  rConf <- setup' "configure" ["--enable-library-for-ghci"]
  let ghciLibs = not ("--enable-library-for-ghci is not supported" `isInfixOf` resultOutput rConf)
  when mergeObjs $
    assertBool
      ("GHCi libraries should be built iff GHC has a merge tool; merge tool = " ++ show mergeTool)
      (ghciLibs == (mergeTool /= Just ""))

  rBuild <- setup' "build" []
  dist <- testDistDir <$> getTestEnv
  objs <- liftIO $ filter isGhciObject <$> listDirectory (dist </> "build")
  if ghciLibs
    then do
      -- `ld -r -o <object>` is the invocation Cabal makes when it merges by itself.
      (if mergeObjs then assertOutputDoesNotContain else assertOutputContains) " -r -o " rBuild
      assertBool ("expected one GHCi object in dist/build, found " ++ show objs) (length objs == 1)
    else assertBool ("expected no GHCi object in dist/build, found " ++ show objs) (null objs)
  where
    isGhciObject f = "HS" `isPrefixOf` f && ".o" `isSuffixOf` f

parseGhcInfo :: String -> Maybe [(String, String)]
parseGhcInfo str = case reads str of
  [(xs, rest)] | all isSpace rest -> Just xs
  _ -> Nothing
