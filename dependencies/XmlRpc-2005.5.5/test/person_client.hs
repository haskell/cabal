-- | Example client using a heterogeneous struct.

import Network.XmlRpc.Client
import Person

server = "http://localhost/~bjorn/cgi-bin/person_server"

listPeople :: IO [Person]
listPeople = remote server "listPeople"

main = do
       people <- listPeople
       mapM_ print people
