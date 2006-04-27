-- A simple client that calls the methods in test_server.hs

import System (getArgs, exitFailure)
import IO (hPutStrLn, stderr)
import Time

import Network.XmlRpc.Client

time :: String -> IO CalendarTime
time url = remote url "examples.time"

add :: String -> Int -> Int -> IO Int
add url = remote url "examples.add"

fault :: String -> IO Int
fault url = remote url "echo.fault"


parseArgs :: IO (String, Int, Int)
parseArgs = do
            args <- getArgs
            case args of
                      [url,x,y] -> return (url, read x, read y)
                      _ -> do
                           hPutStrLn stderr "Usage: test_client url x y"
                           exitFailure

main = do
       (url, x, y) <- parseArgs
       t <- time url
       putStrLn ("The server's current time is " ++ calendarTimeToString t) 
       z <- add url x y
       putStrLn (show x ++ " + " ++ show y ++ " = " ++ show z)
       putStrLn "And now for an error:"
       fault url
       return ()

