module TestFFI where

import Foreign

type Action = IO ()

foreign import ccall "wrapper"
    mkAction :: Action -> IO (FunPtr Action)
