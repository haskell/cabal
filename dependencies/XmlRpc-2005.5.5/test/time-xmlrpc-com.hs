import Network.XmlRpc.Client
import System.Time

server = "http://time.xmlrpc.com/RPC2"

currentTime :: IO CalendarTime
currentTime = remote server "currentTime.getCurrentTime"

main = do
       t <- currentTime
       putStrLn (calendarTimeToString t)
