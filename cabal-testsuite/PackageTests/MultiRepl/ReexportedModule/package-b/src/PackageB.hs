{-# LANGUAGE NoImplicitPrelude #-}
module PackageB (someFunc) where

-- reexport from package-a
import PreludeA
-- Normal import from package-a
import PackageA ()

someFunc :: IO ()
someFunc = putStrLn "someFunc"
