-- Implements the validation suite from http://validator.xmlrpc.com/
-- This has not been tested as the XML-RPC validator does not seem to 
-- be working at the moment.

import System.Time
import Network.XmlRpc.Internals
import Network.XmlRpc.Server

get :: String -> [(String,a)] -> IO a
get f xs = maybeToM ("No such field: '" ++ f ++ "'") (lookup f xs)

arrayOfStructsTest :: [[(String,Int)]] -> IO Int 
arrayOfStructsTest xs = return $ sum [ i | Just i <- map (lookup "curly") xs]

countTheEntities :: String -> IO [(String,Int)]
countTheEntities xs = return [
			      ("ctLeftAngleBrackets", count '<'),
			      ("ctRightAngleBrackets", count '>'),
			      ("ctAmpersands", count '&'),
			      ("ctApostrophes", count '\''),
			      ("ctQuotes", count '"')
			     ]
    where count c = length (filter (==c) xs)

easyStructTest :: [(String,Int)] -> IO Int 
easyStructTest xs = do
		    m <- get "moe" xs
		    l <- get "larry" xs
		    c <- get "curly" xs
		    return (m+l+c)

-- FIXME: should be able to get it as a struct
echoStructTest :: Value -> IO Value
echoStructTest xs = return xs

manyTypesTest :: Int -> Bool -> String -> Double -> CalendarTime -> String
		 -> IO [Value]
manyTypesTest i b s d t b64 = return [toValue i, toValue b, toValue s,
				      toValue d, toValue t, toValue b64]

moderateSizeArrayCheck :: [String] -> IO String
moderateSizeArrayCheck [] = fail "empty array"
moderateSizeArrayCheck xs = return (head xs ++ last xs)

nestedStructTest :: [(String,[(String,[(String,[(String,Int)])])])] -> IO Int
nestedStructTest c = do
		      y <- get "2000" c
		      m <- get "04" y
		      d <- get "01" m
		      easyStructTest d


simpleStructReturnTest :: Int -> IO [(String, Int)]
simpleStructReturnTest x = return [
				   ("times10",10*x),
				   ("times100",100*x),
				   ("times1000",1000*x)
				  ]

main = cgiXmlRpcServer 
       [
	("validator1.arrayOfStructsTest", fun arrayOfStructsTest),
	("validator1.countTheEntities", fun countTheEntities),
	("validator1.easyStructTest", fun easyStructTest),
	("validator1.echoStructTest", fun echoStructTest),
	("validator1.manyTypesTest", fun manyTypesTest),
	("validator1.moderateSizeArrayCheck", fun moderateSizeArrayCheck),
	("validator1.nestedStructTest", fun nestedStructTest),
	("validator1.simpleStructReturnTest", fun simpleStructReturnTest)
       ]

