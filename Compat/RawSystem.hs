module Compat.RawSystem (rawSystem) where

import Data.List (intersperse)
import System.Cmd (system)
import System.Exit (ExitCode)

rawSystem :: String -> [String] -> IO ExitCode
rawSystem p args = system $ concat $ intersperse " " (p : map esc args)
  where esc arg = "\"" ++ arg ++ "\"" -- this is hideously broken, actually
