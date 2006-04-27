-- | This module demonstrates how to handle heterogeneous structs.
--   See person_server.hs and person_client.hs for examples.
module Person where

import Network.XmlRpc.Internals

-- | Record type used to represent the struct in Haskell.
data Person = Person { name :: String, age :: Int } deriving Show

-- | Converts a Person to and from a heterogeneous struct.
--   Uses the existing instance of XmlRpcType for [(String,Value)]
instance XmlRpcType Person where
    toValue p = toValue [("name",toValue (name p)),
			 ("age", toValue (age p))]
    fromValue v = do
		  t <- fromValue v
		  n <- getField "name" t
		  a <- getField "age" t
		  return Person { name = n, age = a }
    getType _ = TStruct
