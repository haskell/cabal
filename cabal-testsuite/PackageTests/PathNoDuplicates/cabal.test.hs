import Test.Cabal.Prelude

import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)
import System.FilePath (searchPathSeparator)

-- The PATH that cabal gives to ghc has each directory once; it used to list
-- the whole search path twice.
main = cabalTest $ recordMode DoNotRecord $ do
  path <- fromMaybe "" <$> liftIO (lookupEnv "PATH")
  let marker = "/path-no-duplicates-marker"
  withEnv [("PATH", Just (marker ++ searchPathSeparator : path))] $ do
    r <-
      cabalWithStdin "repl" ["lib:PathNoDuplicates"] $
        "System.Environment.getEnv \"PATH\" >>= \\p -> putStrLn (\"marker entries: \" ++ show (length (filter (Data.List.isPrefixOf "
          ++ show marker
          ++ ") (Data.List.tails p))))"
    assertOutputContains "marker entries: 1" r
