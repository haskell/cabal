{-# OPTIONS_GHC -cpp #-}
-- #hide
module Distribution.Compat.RawSystem (rawSystem) where

#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ < 602
import Data.List (intersperse)
import System.Cmd (system)
import System.Exit (ExitCode)
#else
import System.Cmd (rawSystem)
#endif

#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ < 602
rawSystem :: String -> [String] -> IO ExitCode
rawSystem p args = system $ concat $ intersperse " " (p : map esc args)
  where esc arg = "'" ++ arg ++ "'" -- this is hideously broken, actually
#endif
