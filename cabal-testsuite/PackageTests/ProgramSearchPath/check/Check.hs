module Check (checkProgramSearchPath) where

import Language.Haskell.TH (Q, runIO)
import System.Environment (getEnv)
import System.FilePath (dropTrailingPathSeparator, splitSearchPath, takeFileName)
import System.Process (readProcess)

-- | Check the program search path for TH splices is in the B<E<P order.
checkProgramSearchPath
  :: Bool -- ^ require uniqueness (no duplicates) in program search paths?
  -> Q ()
checkProgramSearchPath require_uniqueness = runIO $ do
  path <- map dropTrailingPathSeparator . splitSearchPath <$> getEnv "PATH"
  let indices dir = [i | (i, d) <- zip [0 :: Int ..] path, takeFileName d == dir]
      ok = case (indices "scripts-extra", indices "scripts-path") of
        (e : es, p : ps) -> e < p && (not require_uniqueness || (null es && null ps))
        _ -> False
  if ok
    then return ()
    else fail ("unexpected PATH: " ++ show path)
  out <- readProcess "tool" [] ""
  if out == "build-tool-depends\n"
    then return ()
    else fail ("ran the wrong tool: " ++ out)
