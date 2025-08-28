module LibB where

import LibA (libAValue)

-- Use both C function and imported Haskell function
libBValue :: Int
libBValue = fromIntegral $ (fromIntegral libAValue) + 1

