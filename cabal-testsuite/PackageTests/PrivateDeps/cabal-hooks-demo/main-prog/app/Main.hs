-- This is linked directly against lib, but can communicate with older versions of
-- lib via the hooks executable.
module Main where

import System.Directory
import Lib
import HooksLib
import Control.Monad.IO.Class
import System.Environment

main :: IO ()
main = do
  -- Receive the path to the hooks_exe as an argument
  [hooks_exe] <- getArgs
  withHooksExe hooks_exe $ do
    liftIO $ putStr "hooks_show: "
    rendered <- hooks_show (A 5)
    liftIO $ putStrLn rendered
    liftIO $ putStr "hooks_inc: "
    a' <- hooks_inc (A 5)
    liftIO $ print a'
