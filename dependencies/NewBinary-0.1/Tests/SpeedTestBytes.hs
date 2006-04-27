module Main
    where

import Binary
import System.CPUTime
import System
import IO

main = do
  [n] <- getArgs
--  h   <- openFile "tmp" WriteMode
--  bin <- openBinIO_ h
  bin <- openBinMem 1 undefined
  pos <- tellBin bin
  t0  <- getCPUTime
  writeStuff bin (read n)
  t1  <- getCPUTime
  putStrLn $ "Write time: " ++ show (t1 - t0)
  flushByte bin
--  hClose h
  t0  <- getCPUTime
--  h   <- openFile "tmp" ReadMode
--  bin <- openBinIO_ h
  seekBin bin pos
  readStuff bin (read n)  
  t1  <- getCPUTime
--  hClose h
  putStrLn $ " Read time: " ++ show (t1 - t0)

writeStuff bin (n :: Int)
    | n <= 0         = return ()
    | n `mod` 5 == 0 = do writeNothing bin
                          writeStuff bin (n-1)
    | otherwise      = do writeJust bin n
                          writeStuff bin (n-1)

writeNothing bin = putByte bin 0
writeJust bin n  = putByte bin 1 >> put_ bin n


readStuff bin n
    | n <= 0         = return ()
    | otherwise      = do v <- readMaybe bin
                          readStuff bin (n-1)

readMaybe bin = do
  v <- getByte bin
  case v of
    0 -> return Nothing
    _ -> do (n :: Int) <- get bin
            return (Just n)
