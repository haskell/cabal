{-# LANGUAGE CPP #-}

module Main where

#ifndef __TESTOPT_CPP__
#error "Did not get required __TESTOPT_CPP__ from cpp-options"
#endif

main :: IO ()
main = do
    -- The value 44 comes from __TESTOPT_CPP__ - see the cabal file.
    let secret = __TESTOPT_CPP__ :: Int
    if (secret == 44)
        then putStrLn ("The secret is " ++ show secret)
        else error ("Expected value 44, got " ++ show secret)
