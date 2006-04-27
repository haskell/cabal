-- A client for simple_server

import Network.XmlRpc.Client

server = "http://localhost/~bjorn/cgi-bin/simple_server"

add :: String -> Int -> Int -> IO Int
add url = remote url "examples.add"

main = do
       let x = 4
	   y = 7
       z <- add server x y
       putStrLn (show x ++ " + " ++ show y ++ " = " ++ show z)
