-- | Example server using a heterogeneous struct.

import Network.XmlRpc.Server
import Person

listPeople :: IO [Person]
listPeople = return [
		     Person { name = "Homer Simpson", age = 38 },
		     Person { name = "Lisa Simpson", age = 8}
		    ]

main = cgiXmlRpcServer [("listPeople", fun listPeople)]