-- | This module demonstrates how to handle heterogeneous structs
--   using Template Haskell.
--   See person_server.hs and person_client.hs for examples.
module PersonTH where

import Network.XmlRpc.Internals
import Network.XmlRpc.THDeriveXmlRpcType

-- | Record type used to represent the struct in Haskell.
data Person = Person { name :: String, age :: Int, spouse :: Maybe String } deriving Show

$(asXmlRpcStruct (reifyDecl Person))