{-# OPTIONS -cpp #-}
module Compat.RawSystem (rawSystem) where

#ifndef __GLASGOW_HASKELL__
import Data.List (intersperse)
import System.Cmd (system)
import System.Exit (ExitCode)
#else
import System.Cmd (rawSystem)
#endif


#ifndef __GLASGOW_HASKELL__
rawSystem :: String -> [String] -> IO ExitCode
rawSystem p args = system $ concat $ intersperse " " (p : map esc args)
  where esc arg = "\"" ++ arg ++ "\"" -- this is hideously broken, actually
#endif
