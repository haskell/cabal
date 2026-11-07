{-# LANGUAGE CPP #-}
module Main (main) where

import MyLib (libMachine)

main :: IO ()
main = do
  putStrLn ("app-machine: " ++ machine)
  putStrLn ("app-lib-machine: " ++ libMachine)

machine :: String
#if defined(HOST_MACHINE) && defined(BUILD_MACHINE)
machine = "both"
#elif defined(HOST_MACHINE)
machine = "host"
#elif defined(BUILD_MACHINE)
machine = "build"
#else
machine = "none"
#endif
