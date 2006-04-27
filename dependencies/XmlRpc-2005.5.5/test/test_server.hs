-- A simple server

import Time
import Network.XmlRpc.Server

add :: Int -> Int -> IO Int
add x y = return (x + y)

time :: IO CalendarTime
time = getClockTime >>= toCalendarTime

fault :: IO Int -- dummy
fault = fail "blaha"

main = cgiXmlRpcServer [
     ("examples.add", fun add),
     ("echo.fault", fun fault),
     ("examples.time", fun time)]
