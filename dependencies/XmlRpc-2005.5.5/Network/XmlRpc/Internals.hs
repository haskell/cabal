-----------------------------------------------------------------------------
-- |
-- Module      :  Network.XmlRpc.Internals
-- Copyright   :  (c) Bjorn Bringert 2003
-- License     :  BSD-style
-- 
-- Maintainer  :  bjorn@bringert.net
-- Stability   :  experimental
-- Portability :  non-portable (requires extensions and non-portable libraries)
--
-- This module contains the core functionality of the XML-RPC library. 
-- Most applications should not need to use this module. Client
-- applications should use "XmlRpcClient" and server applications should
-- use "XmlRpcServer".
--
-- The XML-RPC specifcation is available at <http://www.xmlrpc.com/spec>.
--
-----------------------------------------------------------------------------

module Network.XmlRpc.Internals (
-- * Method calls and repsonses
MethodCall(..), MethodResponse(..),
-- * XML-RPC types
Value(..), Type(..), XmlRpcType(..),
-- * Converting from XML 
parseResponse, parseCall, getField, getFieldMaybe,
-- * Converting to XML 
renderCall, renderResponse,
-- * Error monad
Err, maybeToM, handleError, ioErrorToErr
) where

import Prelude hiding (showString, catch)
import Control.Monad
import Data.Maybe
import Data.List
import Numeric (showFFloat)
import Data.Char
import System.Time
import System.Locale
import Control.Exception
import Control.Monad.Error
import Control.Monad.Identity
import System.IO.Unsafe (unsafePerformIO)

import Text.XML.HaXml.Xml2Haskell

import Codec.Binary.Base64 as Base64
import Codec.Utils (Octet)

import Network.XmlRpc.DTD_XMLRPC

--
-- General utilities
--

-- | Replaces all occurances of a sublist in a list with another list.
--   If the list to replace is the empty list, does nothing.
replace :: Eq a => 
	[a] -- ^ The sublist to replace when found
	-> [a] -- ^ The list to replace it with
	-> [a] -- ^ The list to replace in
	-> [a] -- ^ The result
replace [] _ xs = xs
replace _ _ [] = []
replace ys zs xs@(x:xs')
    | isPrefixOf ys xs = zs ++ replace ys zs (drop (length ys) xs)
    | otherwise = x : replace ys zs xs'

-- | Convert a 'Maybe' value to a value in any monad
maybeToM :: Monad m =>  
		String -- ^ Error message to fail with for 'Nothing'
	     -> Maybe a -- ^ The 'Maybe' value.
	     -> m a -- ^ The resulting value in the monad.
maybeToM err Nothing = fail err
maybeToM _ (Just x) = return x

--
-- Error monad stuff
--

type Err m a = ErrorT String m a

-- | Evaluate the argument and catch error call exceptions
errorToErr :: Monad m => a -> Err m a
errorToErr x = let e = unsafePerformIO (tryJust errorCalls (evaluate x))
		   in ErrorT (return e)

-- | Catch IO errors in the error monad.
ioErrorToErr :: IO a -> Err IO a
ioErrorToErr = ErrorT . liftM (either (Left . show) Right) . tryJust ioErrors


-- | Handle errors from the error monad.
handleError :: Monad m => (String -> m a) -> Err m a -> m a
handleError h m = do 
		  Right x <- runErrorT (catchError m (lift . h))
		  return x

errorRead :: (Monad m, Read a) => 
	     ReadS a -- ^ Parser
	  -> String -- ^ Error message
	  -> String -- ^ String to parse
	  -> Err m a
errorRead r err s = case [x | (x,t) <- r s, ("","") <- lex t] of
		          [x] -> return x
		          _   -> fail (err ++ ": '" ++ s ++ "'")

-- | Convert an 'Int' to some instance of 'Enum', and fail if the 
--   'Int' is out of range.
errorToEnum :: (Monad m, Bounded a, Enum a) => 
	       String -- ^ Error message
	    -> Int
	    -> Err m a
errorToEnum err x | x < fromEnum (minBound `asTypeOf` r) = fail err
		  | x > fromEnum (maxBound `asTypeOf` r) = fail err
		  | otherwise = return r
    where r = toEnum x

--
-- Types for methods calls and responses
--

-- | An XML-RPC method call. Consists of a method name and a list of 
--   parameters.
data MethodCall = MethodCall String [Value]
		  deriving (Eq, Show) -- for debugging 

-- | An XML-RPC response.
data MethodResponse = Return Value -- ^ A method response returning a value
		    | Fault Int String -- ^ A fault response
		      deriving (Eq, Show) -- for debugging 

-- | An XML-RPC value.
data Value = 
      ValueInt Int -- ^ int or i4
    | ValueBool Bool -- ^ bool
    | ValueString String -- ^ string
    | ValueDouble Double -- ^ double
    | ValueDateTime CalendarTime -- ^ dateTime.iso8601
    | ValueBase64 String -- ^ base 64
    | ValueStruct [(String,Value)] -- ^ struct
    | ValueArray [Value]  -- ^ array
      deriving (Eq, Show) -- for debugging 

-- | An XML-RPC value. Use for error messages and introspection.
data Type = 
	  TInt
	  | TBool
	  | TString
	  | TDouble
	  | TDateTime
	  | TBase64
	  | TStruct
	  | TArray
	  | TUnknown
      deriving (Eq)

instance Show Type where
    show TInt = "int"
    show TBool = "bool"
    show TString = "string"
    show TDouble = "double"
    show TDateTime = "dateTime.iso8601"
    show TBase64 = "base64"
    show TStruct = "struct"
    show TArray = "array"
    show TUnknown = "unknown"

instance Read Type where
    readsPrec _ s = case break isSpace (dropWhile isSpace s) of
		    ("int",r) -> [(TInt,r)]
		    ("bool",r) -> [(TBool,r)]
		    ("string",r) -> [(TString,r)]
		    ("double",r) -> [(TDouble,r)]
		    ("dateTime.iso8601",r) -> [(TDateTime,r)]
		    ("base64",r) -> [(TBase64,r)]
		    ("struct",r) -> [(TStruct,r)]
		    ("array",r) -> [(TArray,r)]

-- | Gets the value of a struct member
structGetValue :: Monad m => String -> Value -> Err m Value
structGetValue n (ValueStruct t) = 
    maybeToM ("Unknown member '" ++ n ++ "'") (lookup n t)
structGetValue _ _ = fail "Value is not a struct"

-- | Builds a fault struct 
faultStruct :: Int -> String -> Value
faultStruct code str = ValueStruct [("faultCode",ValueInt code),
				    ("faultString",ValueString str)]

-- XML-RPC specification:
-- "The body of the response is a single XML structure, a
-- <methodResponse>, which can contain a single <params> which contains a
-- single <param> which contains a single <value>."
onlyOneResult :: Monad m => [Value] -> Err m Value 
onlyOneResult [] = fail "Method returned no result" 
onlyOneResult [x] = return x 
onlyOneResult _ = fail "Method returned more than one result"

--
-- Converting to and from XML-RPC types
--

-- | A class for mapping Haskell types to XML-RPC types.
class XmlRpcType a where
    -- | Convert from this type to a 'Value'
    toValue :: a -> Value
    -- | Convert from a 'Value' to this type. May fail if
    --   if there is a type error.
    fromValue :: Monad m => Value -> Err m a
    getType :: a -> Type

typeError :: Monad m => Type -> Value -> Err m a
typeError t v = fail ("Wanted: "  
		     -- ++ show (getType t)
		      ++ "', got: '" 
		      ++ showXml (toXRValue v) ++ "'")


simpleFromValue :: (Monad m, XmlRpcType a) => (Value -> Maybe a) 
		-> Value -> Err m a
simpleFromValue f v = 
    maybe (typeError (getType (fromJust (f v))) v) return (f v)


-- | Exists to allow explicit type conversions.
instance XmlRpcType Value where
    toValue = id
    fromValue = return . id
    getType _ = TUnknown

-- FIXME: instance for ()?


instance XmlRpcType Int where
    toValue = ValueInt
    fromValue = simpleFromValue f
	where f (ValueInt x) = Just x
	      f _ = Nothing
    getType _ = TInt

-- FIXME: true / false or 1 / 0 ?
instance XmlRpcType Bool where
    toValue = ValueBool
    fromValue = simpleFromValue f
	where f (ValueBool x) = Just x
	      f _ = Nothing
    getType _ = TBool

instance XmlRpcType String where
    toValue = ValueString
    fromValue = simpleFromValue f
	where f (ValueString x) = Just x
	      f (ValueBase64 x) = Just x
	      f _ = Nothing
    getType _ = TString

instance XmlRpcType Double where
    toValue = ValueDouble
    fromValue = simpleFromValue f
	where f (ValueDouble x) = Just x
	      f _ = Nothing
    getType _ = TDouble

instance XmlRpcType CalendarTime where
    toValue = ValueDateTime
    fromValue = simpleFromValue f
	where f (ValueDateTime x) = Just x
	      f _ = Nothing
    getType _ = TDateTime

-- FIXME: array elements may have different types
instance XmlRpcType a => XmlRpcType [a] where
    toValue = ValueArray . map toValue 
    fromValue v = case v of
			 ValueArray _ -> r
			 -- FIXME: I am using TArray here because for
			 -- some reason Hugs Nov 2003 requires a XmlRpcRpc [a]
			 -- constraint on fromValue (which is not 
			 -- legal haskell) to do (getType t) here.
			 _ -> r >>= \t -> typeError TArray v
	where ValueArray xs = v
	      r = mapM fromValue xs
    getType _ = TArray

-- FIXME: struct elements may have different types
instance XmlRpcType a => XmlRpcType [(String,a)] where
    toValue xs = ValueStruct [(n, toValue v) | (n,v) <- xs]

    fromValue v = case v of
		  ValueStruct _ -> r
		  _ -> r >>= \t -> typeError (getType t) v
	where ValueStruct xs = v
	      r = mapM (\ (n,v) -> liftM ((,) n) (fromValue v)) xs
    getType _ = TStruct

-- | Get a field value from a (possibly heterogeneous) struct.
getField :: (Monad m, XmlRpcType a) => 
	    String           -- ^ Field name
	 -> [(String,Value)] -- ^ Struct
	 -> Err m a
getField x xs = maybeToM ("struct member " ++ show x ++ " not found") 
		(lookup x xs) >>= fromValue

-- | Get a field value from a (possibly heterogeneous) struct.
getFieldMaybe :: (Monad m, XmlRpcType a) => 
	    String           -- ^ Field name
	 -> [(String,Value)] -- ^ Struct
	 -> Err m (Maybe a)
getFieldMaybe x xs = case lookup x xs of 
				      Nothing -> return Nothing
				      Just v -> liftM Just (fromValue v)

--
-- Converting to XR types
--

toXRValue :: Value -> XRValue
toXRValue (ValueInt x) = XRValueXRInt (XRInt (showInt x))
toXRValue (ValueBool b) = XRValueXRBoolean (XRBoolean (showBool b))
toXRValue (ValueString s) = XRValueXRString (XRString (showString s))
toXRValue (ValueDouble d) = XRValueXRDouble (XRDouble (showDouble d))
toXRValue (ValueDateTime t) = 
    XRValueXRDateTime_iso8601 (XRDateTime_iso8601 (showDateTime t))
toXRValue (ValueBase64 s) = XRValueXRBase64 (XRBase64 (showBase64 s))
toXRValue (ValueStruct xs) = XRValueXRStruct (XRStruct (map toXRMember xs))
toXRValue (ValueArray xs) = 
    XRValueXRArray (XRArray (XRData (map toXRValue xs)))

showInt :: Int -> String
showInt = show

showBool :: Bool -> String
showBool b = if b then "true" else "false"

-- escapes & and <
showString :: String -> String
showString s = replace "<" "&lt;" (replace "&" "&amp;" s)

-- | Shows a double in signed decimal point notation.
showDouble :: Double -> String
showDouble d = showFFloat Nothing d ""

-- | Shows a date and time on the format: YYYYMMDDTHH:mm:SS
showDateTime :: CalendarTime -> String
showDateTime t = formatCalendarTime defaultTimeLocale xmlRpcDateFormat t
    where xmlRpcDateFormat = "%Y%m%dT%H:%M:%S"

showBase64 :: String -> String
showBase64 = encode . stringToOctets
    where
        -- FIXME: this probably only works right for latin-1 strings
	stringToOctets :: String -> [Octet]
	stringToOctets = map (fromIntegral . fromEnum)


toXRMethodCall :: MethodCall -> XRMethodCall
toXRMethodCall (MethodCall name vs) = 
    XRMethodCall (XRMethodName name) (Just (toXRParams vs))

toXRMethodResponse :: MethodResponse -> XRMethodResponse
toXRMethodResponse (Return val) = XRMethodResponseXRParams (toXRParams [val])
toXRMethodResponse (Fault code str) = 
    XRMethodResponseXRFault (XRFault (toXRValue (faultStruct code str)))

toXRParams :: [Value] -> XRParams 
toXRParams vs = XRParams (map (XRParam . toXRValue) vs)

toXRMember :: (String, Value) -> XRMember
toXRMember (n, v) = XRMember (XRName n) (toXRValue v)

--
-- Converting from XR types
--

fromXRValue :: Monad m => XRValue -> Err m Value
fromXRValue (XRValueXRI4 (XRI4 x)) = liftM ValueInt (readInt x)
fromXRValue (XRValueXRInt (XRInt x)) = liftM ValueInt (readInt x)
fromXRValue (XRValueXRBoolean (XRBoolean x)) = liftM ValueBool (readBool x)
fromXRValue (XRValueXRDouble (XRDouble x)) = liftM ValueDouble (readDouble x)
fromXRValue (XRValueXRString (XRString x)) = liftM ValueString (readString x)
fromXRValue (XRValueXRDateTime_iso8601 (XRDateTime_iso8601 x)) =
    liftM ValueDateTime (readDateTime x)
fromXRValue (XRValueXRBase64 (XRBase64 x)) = liftM ValueBase64 (readBase64 x)
fromXRValue (XRValueXRStruct (XRStruct ms)) = 
    liftM ValueStruct (mapM fromXRMember ms) 
fromXRValue (XRValueXRArray (XRArray (XRData xs))) = 
    liftM ValueArray (mapM fromXRValue xs) 

fromXRMember :: Monad m => XRMember -> Err m (String,Value)
fromXRMember (XRMember (XRName n) xv) = liftM (\v -> (n,v)) (fromXRValue xv)

-- | From the XML-RPC specification:
--
-- \"An integer is a 32-bit signed number. You can include a plus or
-- minus at the beginning of a string of numeric characters. Leading
-- zeros are collapsed. Whitespace is not permitted. Just numeric
-- characters preceeded by a plus or minus.\"
readInt :: Monad m => String -> Err m Int
readInt s = errorRead reads "Error parsing integer" s


-- | From the XML-RPC specification:
--
-- \"0 (false) or 1 (true)\"
readBool :: Monad m => String -> Err m Bool
readBool s = errorRead readsBool "Error parsing boolean" s
    where readsBool "true" = [(True,"")]
	  readsBool "false" = [(False,"")]
	  readsBool "1" = [(True,"")]
	  readsBool "0" = [(False,"")]
	  readsBool _ = []

-- | From the XML-RPC specification:
--
-- \"Any characters are allowed in a string except \< and &, which are
-- encoded as &lt; and &amp;. A string can be used to encode binary data.\"
--
-- To work with implementations (such as some Python bindings for example) 
-- which also escape \>, &gt; is unescaped. This is non-standard, but
-- seems unlikely to cause problems.
readString :: Monad m => String -> Err m String
readString = return . replace "&amp;" "&" . replace "&lt;" "<"
	     . replace "&gt;" ">"


-- | From the XML-RPC specification:
-- 
-- There is no representation for infinity or negative infinity or \"not
-- a number\". At this time, only decimal point notation is allowed, a
-- plus or a minus, followed by any number of numeric characters,
-- followed by a period and any number of numeric
-- characters. Whitespace is not allowed. The range of allowable values
-- is implementation-dependent, is not specified.
--
-- FIXME: accepts more than decimal point notation
readDouble :: Monad m => String -> Err m Double
readDouble s = errorRead reads "Error parsing double" s

-- | From <http://groups.yahoo.com/group/xml-rpc/message/4733>:
--
--   \"Essentially \"dateTime.iso8601\" is a misnomer and the format of the
--   content of this element should not be assumed to comply with the
--   variants of the ISO8601 standard. Only assume YYYYMMDDTHH:mm:SS\"
-- FIXME: make more robust
readDateTime :: Monad m => String -> Err m CalendarTime
readDateTime (x:xs) | isSpace x = readDateTime xs
readDateTime (y1:y2:y3:y4:m1:m2:d1:d2:'T':h1:h2:':':mi1:mi2:':':s1:s2:xs) 
	     | all isSpace xs =
    do
    y <- errorRead reads "Error parsing year" [y1,y2,y3,y4] 
    m' <- errorRead reads "Error parsing month" [m1,m2] 
    m <-  errorToEnum ("Bad month number: " ++ show m') (m'-1)
    d <- errorRead reads "Error parsing day" [d1,d2]
    h <- errorRead reads "Error parsing hour" [h1,h2]
    mi <- errorRead reads "Error parsing minute" [mi1,mi2]
    s <- errorRead reads "Error parsing second" [s1,s2]
    return (mkUTCTime y m d h mi s)
readDateTime s = fail ("Error parsing dateTime '" ++ s ++ "'")

-- | Hack to avoid having to fill in all CalendarTime fields
mkUTCTime :: Int -- ^ Year
	  -> Month -- ^ Month
	  -> Int -- ^ Day
	  -> Int -- ^ Hour
	  -> Int -- ^ Minute
	  -> Int -- ^ Second
	  -> CalendarTime 
mkUTCTime y m d h mi s = 
    toUTCTime $ toClockTime $ 
	      CalendarTime { ctYear = y, ctMonth = m, ctDay = d, 
			     ctHour = h, ctMin = mi, ctSec = s, 
			     ctPicosec = 0, ctWDay = undefined, 
			     ctYDay = undefined, ctTZName = undefined,
			     ctTZ = 0, ctIsDST = undefined }


-- FIXME: what if data contains non-base64 characters?
readBase64 :: Monad m => String -> Err m String
readBase64 = return . octetsToString . decode
    where
        -- FIXME: this probably only works right for latin-1 strings
	octetsToString :: [Octet] -> String
	octetsToString = map (toEnum . fromIntegral)


fromXRParams :: Monad m => XRParams -> Err m [Value]
fromXRParams (XRParams xps) = mapM (\(XRParam v) -> fromXRValue v) xps

fromXRMethodCall :: Monad m => XRMethodCall -> Err m MethodCall
fromXRMethodCall (XRMethodCall (XRMethodName name) params) = 
    liftM (MethodCall name) (fromXRParams (fromMaybe (XRParams []) params))

fromXRMethodResponse :: Monad m => XRMethodResponse -> Err m MethodResponse
fromXRMethodResponse (XRMethodResponseXRParams xps) = 
    liftM Return (fromXRParams xps >>= onlyOneResult)
fromXRMethodResponse (XRMethodResponseXRFault (XRFault v)) =
    do
    struct <- fromXRValue v
    vcode <- structGetValue "faultCode" struct
    code <- fromValue vcode
    vstr <- structGetValue "faultString" struct
    str <- fromValue vstr
    return (Fault code str)

--
-- Parsing calls and reponses from XML
--     

-- | Parses a method call from XML.
parseCall :: Monad m => String -> Err m MethodCall
parseCall c = 
    do
    mxc <- errorToErr (readXml c)
    xc <- maybeToM "Error parsing method call" mxc
    fromXRMethodCall xc

-- | Parses a method response from XML.
parseResponse :: Monad m => String -> Err m MethodResponse
parseResponse c = 
    do
    mxr <- errorToErr (readXml c)
    xr <- maybeToM "Error parsing method response" mxr
    fromXRMethodResponse xr

--
-- Rendering calls and reponses to XML
--

-- | Makes an XML-representation of a method call.
-- FIXME: pretty prints ugly XML
renderCall :: MethodCall -> String
renderCall = showXml . toXRMethodCall

-- | Makes an XML-representation of a method response.
-- FIXME: pretty prints ugly XML
renderResponse :: MethodResponse -> String
renderResponse  = showXml . toXRMethodResponse
