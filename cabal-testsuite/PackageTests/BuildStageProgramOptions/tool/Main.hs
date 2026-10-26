{-# LANGUAGE CPP #-}
module Main (main) where

import MyLib (libStage)

-- A build tool: under cross-compilation it is built for the build stage, so
-- it (and its copy of mylib) must see the build-stage options only.
main :: IO ()
main = do
  putStrLn ("tool-stage: " ++ stage)
  putStrLn ("tool-lib-stage: " ++ libStage)

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
