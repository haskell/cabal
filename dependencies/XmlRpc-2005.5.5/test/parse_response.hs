-- Reads a method response in XML from standard input and prints its
-- internal representation to standard output.

import Network.XmlRpc.Internals

main = do
       c <- getContents
       r <- handleError fail (parseResponse c)
       putStrLn (show r)
