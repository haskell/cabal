module MyLib (someFunc) where

#ifdef FOO
someFunc :: IO ()
someFunc = putStrLn "someFunc"
#endif
