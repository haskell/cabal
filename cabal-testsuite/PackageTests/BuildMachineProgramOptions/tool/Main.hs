{-# LANGUAGE CPP #-}
module Main (main) where

import MyLib (libMachine)

-- A build tool: under cross-compilation it is built for the build machine, so
-- it (and its copy of mylib) must see the build-machine options only.
main :: IO ()
main = do
  putStrLn ("tool-machine: " ++ machine)
  putStrLn ("tool-lib-machine: " ++ libMachine)

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
