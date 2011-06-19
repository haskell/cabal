module Main where

import Foreign

type Action = IO ()

foreign import ccall "wrapper"
    mkAction :: Action -> IO (FunPtr Action)

main :: IO ()
main = return ()
