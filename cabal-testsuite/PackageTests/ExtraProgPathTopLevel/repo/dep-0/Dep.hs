module Dep (inPATH) where

import Language.Haskell.TH (Q, runIO)
import System.Environment (getEnv)
import System.FilePath (dropTrailingPathSeparator, splitSearchPath, takeFileName)

inPATH :: Bool -> Q ()
inPATH expected = runIO $ do
  path <- map dropTrailingPathSeparator . splitSearchPath <$> getEnv "PATH"
  let found = any ((== "scripts-top") . takeFileName) path
  if found == expected
    then return ()
    else fail ("scripts-top in PATH: " ++ show found ++ " in " ++ show path)
