{-# OPTIONS -cpp #-}
module Compat.RawSystem (rawSystem) where

#if (!(defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ > 600))
import Data.List (intersperse)
import System.Cmd (system)
import System.Exit (ExitCode)
#else
import System.Cmd (rawSystem)
#endif


#if (!(defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ > 600))
rawSystem :: String -> [String] -> IO ExitCode
rawSystem p args = system $ concat $ intersperse " " (p : map esc args)
  where esc arg = "\"" ++ arg ++ "\"" -- this is hideously broken, actually
#endif
