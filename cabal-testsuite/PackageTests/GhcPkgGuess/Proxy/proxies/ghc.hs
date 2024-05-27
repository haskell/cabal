{-# LANGUAGE CPP, TypeApplications #-}

module Main where

import Control.Monad (unless)
import System.Environment (getArgs)
import System.Exit
import System.IO
import System.Process

main :: IO ()
main = getArgs >>= run

run :: [String] -> IO ()
run args
    | "--info" `elem` args = do
        ghcInfo <- read @Info <$> readProcess GHC_PATH ["--info"] ""
        print (ghcInfo ++ [("ghc", GHC_PATH)])
    | otherwise = do
        (exitCode, out, err) <- readProcessWithExitCode GHC_PATH args ""
        unless (null out) $ do
            putStr out
            hFlush stdout
        unless (null err) $ do
            hPutStr stderr err
            hFlush stderr
        exitWith exitCode

type Info = [(String, String)]
