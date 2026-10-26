{-# LANGUAGE CPP #-}
module Main (main) where

import MyLib (libStage)

main :: IO ()
main = do
  putStrLn ("app-stage: " ++ stage)
  putStrLn ("app-lib-stage: " ++ libStage)

stage :: String
#if defined(HOST_STAGE) && defined(BUILD_STAGE)
stage = "both"
#elif defined(HOST_STAGE)
stage = "host"
#elif defined(BUILD_STAGE)
stage = "build"
#else
stage = "none"
#endif
