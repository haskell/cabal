-- This is linked directly against lib, but can communicate with older versions of
-- lib via the hooks executable.
module Main where

import System.Directory
import Lib
import HooksLib
import Control.Monad.IO.Class

main :: IO ()
main = do
  Just hooks_exe <- findExecutable "hooks-exe"
  withHooksExe hooks_exe $ do
    liftIO $ putStr "hooks_show: "
    rendered <- hooks_show (A 5)
    liftIO $ putStrLn rendered
    liftIO $ putStr "hooks_inc: "
    a' <- hooks_inc (A 5)
    liftIO $ print a'
