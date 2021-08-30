module MyLib2 (someFunc2) where

someFunc2 :: IO ()
-- Intentional typo, should fail to compile
someFunc2 = putStrn "someFunc"
--          ^^------- missing 'L'
