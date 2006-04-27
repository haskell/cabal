module Network.XmlRpc.Introspect where

import Network.XmlRpc.Internals
import Network.XmlRpc.Client

type Signature = ([Type],Type)
type Help = String
type MethodInfo = (String,[Signature],Help)

-- Primitive introspection functions

listMethods :: String -> IO [String]
listMethods url = remote url "system.listMethods"

methodSignature :: String -> String -> IO [[String]]
methodSignature url = remote url "system.methodSignature"

methodHelp :: String -> String -> IO String
methodHelp url = remote url "system.methodHelp"


signatures :: String -> String -> IO [Signature]
signatures url name = do
		      sigs <- methodSignature url name
		      return [ (map read as,read r) | (r:as) <- sigs ]

methodInfo :: String -> String -> IO MethodInfo
methodInfo url name = do
		      sigs <- signatures url name
		      help <- methodHelp url name
		      return (name, sigs, help)
