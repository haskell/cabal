-----------------------------------------------------------------------------
-- |
-- Module      :  Network.HTTP
-- Copyright   :  (c) Warrick Gray 2002, Bjorn Bringert 2003-2005
-- License     :  BSD
-- 
-- Maintainer  :  bjorn@bringert.net
-- Stability   :  experimental
-- Portability :  non-portable (not tested)
--
-- An easy HTTP interface enjoy.
--
-- * Changes by Simon Foster:
--      - Split module up into to sepearate Network.[Stream,TCP,HTTP] modules
--      - Created functions receiveHTTP and responseHTTP to allow server side interactions
--        (although 100-continue is unsupported and I haven't checked for standard compliancy).
--      - Pulled the transfer functions from sendHTTP to global scope to allow access by
--        above functions.
--
-- * Changes by Graham Klyne:
--      - export httpVersion
--      - use new URI module (similar to old, but uses revised URI datatype)
--
-- * Changes by Bjorn Bringert:
--
--      - handle URIs with a port number
--      - added debugging toggle
--      - disabled 100-continue transfers to get HTTP\/1.0 compatibility
--      - change 'ioError' to 'throw'
--      - Added simpleHTTP_, which takes a stream argument.
--
-- * Changes from 0.1
--      - change 'openHTTP' to 'openTCP', removed 'closeTCP' - use 'close' from 'Stream' class.
--      - added use of inet_addr to openHTTP, allowing use of IP "dot" notation addresses.
--      - reworking of the use of Stream, including alterations to make 'sendHTTP' generic
--        and the addition of a debugging stream.
--      - simplified error handling.
-- 
-- * TODO
--     - request pipelining
--     - https upgrade (includes full TLS, i.e. SSL, implementation)
--         - use of Stream classes will pay off
--         - consider C implementation of encryption\/decryption
--     - comm timeouts
--     - MIME & entity stuff (happening in separate module)
--     - support \"*\" uri-request-string for OPTIONS request method
-- 
-- 
-- * Header notes:
--
--     [@Host@]
--                  Required by HTTP\/1.1, if not supplied as part
--                  of a request a default Host value is extracted
--                  from the request-uri.
-- 
--     [@Connection@] 
--                  If this header is present in any request or
--                  response, and it's value is "close", then
--                  the current request\/response is the last 
--                  to be allowed on that connection.
-- 
--     [@Expect@]
--                  Should a request contain a body, an Expect
--                  header will be added to the request.  The added
--                  header has the value \"100-continue\".  After
--                  a 417 \"Expectation Failed\" response the request
--                  is attempted again without this added Expect
--                  header.
--                  
--     [@TransferEncoding,ContentLength,...@]
--                  if request is inconsistent with any of these
--                  header values then you may not receive any response
--                  or will generate an error response (probably 4xx).
--
--
-- * Response code notes
-- Some response codes induce special behaviour:
--
--   [@1xx@]   \"100 Continue\" will cause any unsent request body to be sent.
--             \"101 Upgrade\" will be returned.
--             Other 1xx responses are ignored.
-- 
--   [@417@]   The reason for this code is \"Expectation failed\", indicating
--             that the server did not like the Expect \"100-continue\" header
--             added to a request.  Receipt of 417 will induce another
--             request attempt (without Expect header), unless no Expect header
--             had been added (in which case 417 response is returned).
--
-----------------------------------------------------------------------------
module Network.HTTP (
    module Network.Stream,
    module Network.TCP,

    -- ** Constants
    httpVersion,
    
    -- ** HTTP 
    Request(..),
    Response(..),
    RequestMethod(..),
    simpleHTTP, simpleHTTP_,
    sendHTTP,
    receiveHTTP,
    respondHTTP,

    -- ** Header Functions
    HasHeaders,
    Header(..),
    HeaderName(..),
    insertHeader,
    insertHeaderIfMissing,
    insertHeaders,
    retrieveHeaders,
    replaceHeader,
    findHeader,

    -- ** URL Encoding
    urlEncode,
    urlDecode,
    urlEncodeVars,

    -- ** URI authority parsing
    URIAuthority(..),
    parseURIAuthority
) where



-----------------------------------------------------------------
------------------ Imports --------------------------------------
-----------------------------------------------------------------

import Control.Exception as Exception

-- Networking
import Network (withSocketsDo)
import Network.BSD
import Network.URI
import Network.Socket
import Network.Stream
import Network.TCP


-- Util
import Data.Bits ((.&.))
import Data.Char
import Data.List (isPrefixOf,partition,elemIndex)
import Data.Maybe
import Data.Array.MArray
import Data.IORef
import Control.Concurrent
import Control.Monad (when,liftM,guard)
import Control.Monad.ST (ST,stToIO)
import Numeric (readHex)
import Text.ParserCombinators.ReadP
import Text.Read.Lex 
import System.IO
import System.IO.Error (isEOFError)
import qualified System.IO.Error

import Foreign.C.Error


-- Turn on to enable HTTP traffic logging
debug :: Bool
debug = False

-- File that HTTP traffic logs go to
httpLogFile :: String
httpLogFile = "http-debug.log"

-----------------------------------------------------------------
------------------ Misc -----------------------------------------
-----------------------------------------------------------------

-- remove leading and trailing whitespace.
trim :: String -> String
trim = let dropspace = dropWhile isSpace in
       reverse . dropspace . reverse . dropspace


-- Split a list into two parts, the delimiter occurs
-- at the head of the second list.  Nothing is returned
-- when no occurance of the delimiter is found.
split :: Eq a => a -> [a] -> Maybe ([a],[a])
split delim list = case delim `elemIndex` list of
    Nothing -> Nothing
    Just x  -> Just $ splitAt x list
    


crlf = "\r\n"
sp   = " "

-----------------------------------------------------------------
------------------ URI Authority parsing ------------------------
-----------------------------------------------------------------

data URIAuthority = URIAuthority { user :: Maybe String, 
				   password :: Maybe String,
				   host :: String,
				   port :: Maybe Int
				 } deriving (Eq,Show)

-- | Parse the authority part of a URL.
--
-- > RFC 1732, section 3.1:
-- >
-- >       //<user>:<password>@<host>:<port>/<url-path>
-- >  Some or all of the parts "<user>:<password>@", ":<password>",
-- >  ":<port>", and "/<url-path>" may be excluded.
parseURIAuthority :: String -> Maybe URIAuthority
parseURIAuthority s = listToMaybe (map fst (readP_to_S pURIAuthority s))


pURIAuthority :: ReadP URIAuthority
pURIAuthority = do
		(u,pw) <- (pUserInfo `before` char '@') 
			  <++ return (Nothing, Nothing)
		h <- munch (/=':')
		p <- orNothing (char ':' >> readDecP)
		look >>= guard . null 
		return URIAuthority{ user=u, password=pw, host=h, port=p }

pUserInfo :: ReadP (Maybe String, Maybe String)
pUserInfo = do
	    u <- orNothing (munch (`notElem` ":@"))
	    p <- orNothing (char ':' >> munch (/='@'))
	    return (u,p)

before :: Monad m => m a -> m b -> m a
before a b = a >>= \x -> b >> return x

orNothing :: ReadP a -> ReadP (Maybe a)
orNothing p = fmap Just p <++ return Nothing

-----------------------------------------------------------------
------------------ Header Data ----------------------------------
-----------------------------------------------------------------


-- | The Header data type pairs header names & values.
data Header = Header HeaderName String


instance Show Header where
    show (Header key value) = show key ++ ": " ++ value ++ crlf


-- | HTTP Header Name type:
--  Why include this at all?  I have some reasons
--   1) prevent spelling errors of header names,
--   2) remind everyone of what headers are available,
--   3) might speed up searches for specific headers.
--
--  Arguments against:
--   1) makes customising header names laborious
--   2) increases code volume.
--
data HeaderName = 
                 -- Generic Headers --
                  HdrCacheControl
                | HdrConnection
                | HdrDate
                | HdrPragma
                | HdrTransferEncoding        
                | HdrUpgrade                
                | HdrVia

                -- Request Headers --
                | HdrAccept
                | HdrAcceptCharset
                | HdrAcceptEncoding
                | HdrAcceptLanguage
                | HdrAuthorization
                | HdrCookie
                | HdrExpect
                | HdrFrom
                | HdrHost
                | HdrIfModifiedSince
                | HdrIfMatch
                | HdrIfNoneMatch
                | HdrIfRange
                | HdrIfUnmodifiedSince
                | HdrMaxForwards
                | HdrProxyAuthorization
                | HdrRange
                | HdrReferer
                | HdrUserAgent

                -- Response Headers
                | HdrAge
                | HdrLocation
                | HdrProxyAuthenticate
                | HdrPublic
                | HdrRetryAfter
                | HdrServer
                | HdrSetCookie
                | HdrVary
                | HdrWarning
                | HdrWWWAuthenticate

                -- Entity Headers
                | HdrAllow
                | HdrContentBase
                | HdrContentEncoding
                | HdrContentLanguage
                | HdrContentLength
                | HdrContentLocation
                | HdrContentMD5
                | HdrContentRange
                | HdrContentType
                | HdrETag
                | HdrExpires
                | HdrLastModified

                -- Mime entity headers (for sub-parts)
                | HdrContentTransferEncoding

                -- | Allows for unrecognised or experimental headers.
                | HdrCustom String -- not in header map below.
    deriving(Eq)


-- Translation between header names and values,
-- good candidate for improvement.
headerMap :: [ (String,HeaderName) ]
headerMap 
 = [  ("Cache-Control"        ,HdrCacheControl      )
	, ("Connection"           ,HdrConnection        )
	, ("Date"                 ,HdrDate              )    
	, ("Pragma"               ,HdrPragma            )
	, ("Transfer-Encoding"    ,HdrTransferEncoding  )        
	, ("Upgrade"              ,HdrUpgrade           )                
	, ("Via"                  ,HdrVia               )
	, ("Accept"               ,HdrAccept            )
	, ("Accept-Charset"       ,HdrAcceptCharset     )
	, ("Accept-Encoding"      ,HdrAcceptEncoding    )
	, ("Accept-Language"      ,HdrAcceptLanguage    )
	, ("Authorization"        ,HdrAuthorization     )
	, ("From"                 ,HdrFrom              )
	, ("Host"                 ,HdrHost              )
	, ("If-Modified-Since"    ,HdrIfModifiedSince   )
	, ("If-Match"             ,HdrIfMatch           )
	, ("If-None-Match"        ,HdrIfNoneMatch       )
	, ("If-Range"             ,HdrIfRange           ) 
	, ("If-Unmodified-Since"  ,HdrIfUnmodifiedSince )
	, ("Max-Forwards"         ,HdrMaxForwards       )
	, ("Proxy-Authorization"  ,HdrProxyAuthorization)
	, ("Range"                ,HdrRange             )   
	, ("Referer"              ,HdrReferer           )
	, ("User-Agent"           ,HdrUserAgent         )
	, ("Age"                  ,HdrAge               )
	, ("Location"             ,HdrLocation          )
	, ("Proxy-Authenticate"   ,HdrProxyAuthenticate )
	, ("Public"               ,HdrPublic            )
	, ("Retry-After"          ,HdrRetryAfter        )
	, ("Server"               ,HdrServer            )
	, ("Vary"                 ,HdrVary              )
	, ("Warning"              ,HdrWarning           )
	, ("WWW-Authenticate"     ,HdrWWWAuthenticate   )
	, ("Allow"                ,HdrAllow             )
	, ("Content-Base"         ,HdrContentBase       )
	, ("Content-Encoding"     ,HdrContentEncoding   )
	, ("Content-Language"     ,HdrContentLanguage   )
	, ("Content-Length"       ,HdrContentLength     )
	, ("Content-Location"     ,HdrContentLocation   )
	, ("Content-MD5"          ,HdrContentMD5        )
	, ("Content-Range"        ,HdrContentRange      )
	, ("Content-Type"         ,HdrContentType       )
	, ("ETag"                 ,HdrETag              )
	, ("Expires"              ,HdrExpires           )
	, ("Last-Modified"        ,HdrLastModified      )
   	, ("Set-Cookie"           ,HdrSetCookie         )
	, ("Cookie"               ,HdrCookie            )
    , ("Expect"               ,HdrExpect            ) ]


instance Show HeaderName where
    show (HdrCustom s) = s
    show x = case filter ((==x).snd) headerMap of
                [] -> error "headerMap incomplete"
                (h:_) -> fst h





-- | This class allows us to write generic header manipulation functions
-- for both 'Request' and 'Response' data types.
class HasHeaders x where
    getHeaders :: x -> [Header]
    setHeaders :: x -> [Header] -> x



-- Header manipulation functions
insertHeader, replaceHeader, insertHeaderIfMissing
    :: HasHeaders a => HeaderName -> String -> a -> a


-- | Inserts a header with the given name and value.
-- Allows duplicate header names.
insertHeader name value x = setHeaders x newHeaders
    where
        newHeaders = (Header name value) : getHeaders x


-- | Adds the new header only if no previous header shares
-- the same name.
insertHeaderIfMissing name value x = setHeaders x (newHeaders $ getHeaders x)
    where
        newHeaders list@(h@(Header n _): rest)
            | n == name  = list
            | otherwise  = h : newHeaders rest
        newHeaders [] = [Header name value]

            

-- | Removes old headers with duplicate name.
replaceHeader name value x = setHeaders x newHeaders
    where
        newHeaders = Header name value : [ x | x@(Header n v) <- getHeaders x, name /= n ]
          

-- | Inserts multiple headers.
insertHeaders :: HasHeaders a => [Header] -> a -> a
insertHeaders hdrs x = setHeaders x (getHeaders x ++ hdrs)


-- | Gets a list of headers with a particular 'HeaderName'.
retrieveHeaders :: HasHeaders a => HeaderName -> a -> [Header]
retrieveHeaders name x = filter matchname (getHeaders x)
    where
        matchname (Header n _)  |  n == name  =  True
        matchname _ = False


-- | Lookup presence of specific HeaderName in a list of Headers
-- Returns the value from the first matching header.
findHeader :: HasHeaders a => HeaderName -> a -> Maybe String
findHeader n x = lookupHeader n (getHeaders x)

-- An anomally really:
lookupHeader :: HeaderName -> [Header] -> Maybe String
lookupHeader v (Header n s:t)  |  v == n   =  Just s
                               | otherwise =  lookupHeader v t
lookupHeader _ _  =  Nothing




{-
instance HasHeaders [Header]
...requires -fglasgow-exts, and is not really necessary anyway...
-}



-----------------------------------------------------------------
------------------ HTTP Messages --------------------------------
-----------------------------------------------------------------


-- Protocol version
httpVersion :: String
httpVersion = "HTTP/1.1"


-- | The HTTP request method, to be used in the 'Request' object.
-- We are missing a few of the stranger methods, but these are
-- not really necessary until we add full TLS.
data RequestMethod = HEAD | PUT | GET | POST | OPTIONS | TRACE
    deriving(Show,Eq)

rqMethodMap = [("HEAD",    HEAD),
	       ("PUT",     PUT),
	       ("GET",     GET),
	       ("POST",    POST),
	       ("OPTIONS", OPTIONS),
	       ("TRACE",   TRACE)]

-- | An HTTP Request.
-- The 'Show' instance of this type is used for message serialisation,
-- which means no body data is output.
data Request =
     Request { rqURI       :: URI   -- ^ might need changing in future
                                    --  1) to support '*' uri in OPTIONS request
                                    --  2) transparent support for both relative
                                    --     & absolute uris, although this should
                                    --     already work (leave scheme & host parts empty).
             , rqMethod    :: RequestMethod             
             , rqHeaders   :: [Header]
             , rqBody      :: String
             }




-- Notice that request body is not included,
-- this show function is used to serialise
-- a request for the transport link, we send
-- the body separately where possible.
instance Show Request where
    show (Request u m h _) =
        show m ++ sp ++ alt_uri ++ sp ++ httpVersion ++ crlf
        ++ foldr (++) [] (map show h) ++ crlf
        where
            alt_uri = show $ if null (uriPath u) || head (uriPath u) /= '/' 
                        then u { uriPath = '/' : uriPath u } 
                        else u


instance HasHeaders Request where
    getHeaders = rqHeaders
    setHeaders rq hdrs = rq { rqHeaders=hdrs }






type ResponseCode  = (Int,Int,Int)
type ResponseData  = (ResponseCode,String,[Header])
type RequestData   = (RequestMethod,URI,[Header])

-- | An HTTP Response.
-- The 'Show' instance of this type is used for message serialisation,
-- which means no body data is output, additionally the output will
-- show an HTTP version of 1.1 instead of the actual version returned
-- by a server.
data Response =
    Response { rspCode     :: ResponseCode
             , rspReason   :: String
             , rspHeaders  :: [Header]
             , rspBody     :: String
             }
                   


-- This is an invalid representation of a received response, 
-- since we have made the assumption that all responses are HTTP/1.1
instance Show Response where
    show (Response (a,b,c) reason headers _) =
        httpVersion ++ ' ' : map intToDigit [a,b,c] ++ ' ' : reason ++ crlf
        ++ foldr (++) [] (map show headers) ++ crlf



instance HasHeaders Response where
    getHeaders = rspHeaders
    setHeaders rsp hdrs = rsp { rspHeaders=hdrs }

-----------------------------------------------------------------
------------------ Parsing --------------------------------------
-----------------------------------------------------------------

parseHeader :: String -> Result Header
parseHeader str =
    case split ':' str of
        Nothing -> Left (ErrorParse $ "Unable to parse header: " ++ str)
        Just (k,v) -> Right $ Header (fn k) (trim $ drop 1 v)
    where
        fn k = case map snd $ filter (match k . fst) headerMap of
                 [] -> (HdrCustom k)
                 (h:_) -> h

        match :: String -> String -> Bool
        match s1 s2 = map toLower s1 == map toLower s2
    

parseHeaders :: [String] -> Result [Header]
parseHeaders = catRslts [] . map (parseHeader . clean) . joinExtended ""
    where
        -- Joins consecutive lines where the second line
        -- begins with ' ' or '\t'.
        joinExtended old (h : t)
            | not (null h) && (head h == ' ' || head h == '\t')
                = joinExtended (old ++ ' ' : tail h) t
            | otherwise = old : joinExtended h t
        joinExtended old [] = [old]

        clean [] = []
        clean (h:t) | h `elem` "\t\r\n" = ' ' : clean t
                    | otherwise = h : clean t

        -- tollerant of errors?  should parse
        -- errors here be reported or ignored?
        -- currently ignored.
        catRslts :: [a] -> [Result a] -> Result [a]
        catRslts list (h:t) = 
            case h of
                Left _ -> catRslts list t
                Right v -> catRslts (v:list) t
        catRslts list [] = Right $ reverse list            
        

-- Parsing a request
parseRequestHead :: [String] -> Result RequestData
parseRequestHead [] = Left ErrorClosed
parseRequestHead (com:hdrs) =
    requestCommand com `bindE` \(version,rqm,uri) ->
    parseHeaders hdrs `bindE` \hdrs' ->
    Right (rqm,uri,hdrs')
    where
        requestCommand line
	    =  case words line of
                yes@(rqm:uri:version) -> case (parseURIReference uri, lookup rqm rqMethodMap) of
					  (Just u, Just r) -> Right (version,r,u)
					  _                -> Left (ErrorParse $ "Request command line parse failure: " ++ line)
		no -> if null line
			       then Left ErrorClosed
			       else Left (ErrorParse $ "Request command line parse failure: " ++ line)  

-- Parsing a response
parseResponseHead :: [String] -> Result ResponseData
parseResponseHead [] = Left ErrorClosed
parseResponseHead (sts:hdrs) = 
    responseStatus sts `bindE` \(version,code,reason) ->
    parseHeaders hdrs `bindE` \hdrs' ->
    Right (code,reason,hdrs')
    where

        responseStatus line
            =  case words line of
                yes@(version:code:reason) -> Right (version,match code,concatMap (++" ") reason)
                no -> if null line 
                    then Left ErrorClosed  -- an assumption
                    else Left (ErrorParse $ "Response status line parse failure: " ++ line)


        match [a,b,c] = (digitToInt a,
                         digitToInt b,
                         digitToInt c)
        match _ = (-1,-1,-1)  -- will create appropriate behaviour


        

-----------------------------------------------------------------
------------------ HTTP Send / Recv ----------------------------------
-----------------------------------------------------------------

data Behaviour = Continue
               | Retry
               | Done
               | ExpectEntity
               | DieHorribly String





matchResponse :: RequestMethod -> ResponseCode -> Behaviour
matchResponse rqst rsp =
    case rsp of
        (1,0,0) -> Continue
        (1,0,1) -> Done        -- upgrade to TLS
        (1,_,_) -> Continue    -- default
        (2,0,4) -> Done
        (2,0,5) -> Done
        (2,_,_) -> ans
        (3,0,4) -> Done
        (3,0,5) -> Done
        (3,_,_) -> ans
        (4,1,7) -> Retry       -- Expectation failed
        (4,_,_) -> ans
        (5,_,_) -> ans
        (a,b,c) -> DieHorribly ("Response code " ++ map intToDigit [a,b,c] ++ " not recognised")
    where
        ans | rqst == HEAD = Done
            | otherwise    = ExpectEntity
        

-- | Simple way to get a resource across a non-persistant connection.
-- Headers that may be altered:
--  Host        Altered only if no Host header is supplied, HTTP\/1.1
--              requires a Host header.
--  Connection  Where no allowance is made for persistant connections
--              the Connection header will be set to "close"
simpleHTTP :: Request -> IO (Result Response)
simpleHTTP r = 
    do 
       auth <- getAuth r
       c <- openTCPPort (host auth) (fromMaybe 80 (port auth))
       simpleHTTP_ c r

-- | Like 'simpleHTTP', but acting on an already opened stream.
simpleHTTP_ :: Stream s => s -> Request -> IO (Result Response)
simpleHTTP_ s r =
    do 
       auth <- getAuth r
       let r' = fixReq auth r 
       rsp <- if debug then do
	        s' <- debugStream httpLogFile s
	        sendHTTP s' r'
	       else
	        sendHTTP s r'
       -- already done by sendHTTP because of "Connection: close" header
       --; close s 
       return rsp
       where
  {- RFC 2616, section 5.1.2:
     "The most common form of Request-URI is that used to identify a
      resource on an origin server or gateway. In this case the absolute
      path of the URI MUST be transmitted (see section 3.2.1, abs_path) as
      the Request-URI, and the network location of the URI (authority) MUST
      be transmitted in a Host header field." -}
  -- we assume that this is the case, so we take the host name from
  -- the Host header if there is one, otherwise from the request-URI.
  -- Then we make the request-URI an abs_path and make sure that there
  -- is a Host header.
             fixReq :: URIAuthority -> Request -> Request
	     fixReq URIAuthority{host=h} r = 
		 replaceHeader HdrConnection "close" $
		 insertHeaderIfMissing HdrHost h $
		 r { rqURI = (rqURI r){ uriScheme = "", 
					uriAuthority = Nothing } }	       

getAuth :: Monad m => Request -> m URIAuthority
getAuth r = case parseURIAuthority auth of
			 Just x -> return x 
			 Nothing -> fail $ "Error parsing URI authority '"
				           ++ auth ++ "'"
		 where auth = case findHeader HdrHost r of
			      Just h -> h
			      Nothing -> authority (rqURI r)

sendHTTP :: Stream s => s -> Request -> IO (Result Response)
sendHTTP conn rq = 
    do { let a_rq = fixHostHeader rq
       ; rsp <- Exception.catch (main a_rq)
                      (\e -> do { close conn; throw e })
       ; let fn list = when (or $ map findConnClose list)
                            (close conn)
       ; either (\_ -> fn [rqHeaders rq])
                (\r -> fn [rqHeaders rq,rspHeaders r])
                rsp
       ; return rsp
       }
    where       
-- From RFC 2616, section 8.2.3:
-- 'Because of the presence of older implementations, the protocol allows
-- ambiguous situations in which a client may send "Expect: 100-
-- continue" without receiving either a 417 (Expectation Failed) status
-- or a 100 (Continue) status. Therefore, when a client sends this
-- header field to an origin server (possibly via a proxy) from which it
-- has never seen a 100 (Continue) status, the client SHOULD NOT wait
-- for an indefinite period before sending the request body.'
--
-- Since we would wait forever, I have disabled use of 100-continue for now.
        main :: Request -> IO (Result Response)
        main rqst =
            do 
	       --let str = if null (rqBody rqst)
               --              then show rqst
               --              else show (insertHeader HdrExpect "100-continue" rqst)
               writeBlock conn (show rqst)
	       -- write body immediately, don't wait for 100 CONTINUE
	       writeBlock conn (rqBody rqst)
               rsp <- getResponseHead               
               switchResponse True False rsp rqst
        
        -- reads and parses headers
        getResponseHead :: IO (Result ResponseData)
        getResponseHead =
            do { lor <- readTillEmpty1 conn
               ; return $ lor `bindE` parseResponseHead
               }

        -- Hmmm, this could go bad if we keep getting "100 Continue"
        -- responses...  Except this should never happen according
        -- to the RFC.
        switchResponse :: Bool {- allow retry? -}
                       -> Bool {- is body sent? -}
                       -> Result ResponseData
                       -> Request
                       -> IO (Result Response)
            
        switchResponse _ _ (Left e) _ = return (Left e)
                -- retry on connreset?
                -- if we attempt to use the same socket then there is an excellent
                -- chance that the socket is not in a completely closed state.

        switchResponse allow_retry bdy_sent (Right (cd,rn,hdrs)) rqst =
            case matchResponse (rqMethod rqst) cd of
                Continue
                    | not bdy_sent -> {- Time to send the body -}
                        do { val <- writeBlock conn (rqBody rqst)
                           ; case val of
                                Left e -> return (Left e)
                                Right _ ->
                                    do { rsp <- getResponseHead
                                       ; switchResponse allow_retry True rsp rqst
                                       }
                           }
                    | otherwise -> {- keep waiting -}
                        do { rsp <- getResponseHead
                           ; switchResponse allow_retry bdy_sent rsp rqst                           
                           }

                Retry -> {- Request with "Expect" header failed.
                                Trouble is the request contains Expects
                                other than "100-Continue" -}
                    do { writeBlock conn (show rqst ++ rqBody rqst)
                       ; rsp <- getResponseHead
                       ; switchResponse False bdy_sent rsp rqst
                       }   
                     
                Done ->
                    return (Right $ Response cd rn hdrs "")

                DieHorribly str ->
                    return $ Left $ ErrorParse ("Invalid response: " ++ str)

                ExpectEntity ->
                    let tc = lookupHeader HdrTransferEncoding hdrs
                        cl = lookupHeader HdrContentLength hdrs
                    in
                    do { rslt <- case tc of
                          Nothing -> 
                              case cl of
                                  Just x  -> linearTransfer conn (read x :: Int)
                                  Nothing -> hopefulTransfer conn ""
                          Just x  -> 
                              case map toLower (trim x) of
                                  "chunked" -> chunkedTransfer conn
                                  _         -> uglyDeathTransfer conn
                       ; return $ rslt `bindE` \(ftrs,bdy) -> Right (Response cd rn (hdrs++ftrs) bdy) 
                       }

        
        -- Adds a Host header if one is NOT ALREADY PRESENT
        fixHostHeader :: Request -> Request
        fixHostHeader rq =
            let uri = rqURI rq
                host = authority uri
            in insertHeaderIfMissing HdrHost host rq
                                     
        -- Looks for a "Connection" header with the value "close".
        -- Returns True when this is found.
        findConnClose :: [Header] -> Bool
        findConnClose hdrs =
            case lookupHeader HdrConnection hdrs of
                Nothing -> False
                Just x  -> map toLower (trim x) == "close"

-- | Receive and parse a HTTP request from the given Stream. Should be used 
--   for server side interactions.
receiveHTTP :: Stream s => s -> IO (Result Request)
receiveHTTP conn = do rq <- getRequestHead
		      processRequest rq	    
    where
        -- reads and parses headers
        getRequestHead :: IO (Result RequestData)
        getRequestHead =
            do { lor <- readTillEmpty1 conn
               ; return $ lor `bindE` parseRequestHead
               }
	
        processRequest (Left e) = return $ Left e
	processRequest (Right (rm,uri,hdrs)) = 
	    do -- FIXME : Also handle 100-continue.
               let tc = lookupHeader HdrTransferEncoding hdrs
                   cl = lookupHeader HdrContentLength hdrs
	       rslt <- case tc of
                          Nothing ->
                              case cl of
                                  Just x  -> linearTransfer conn (read x :: Int)
                                  Nothing -> return (Right ([], "")) -- hopefulTransfer ""
                          Just x  ->
                              case map toLower (trim x) of
                                  "chunked" -> chunkedTransfer conn
                                  _         -> uglyDeathTransfer conn
               
               return $ rslt `bindE` \(ftrs,bdy) -> Right (Request uri rm (hdrs++ftrs) bdy)


-- | Very simple function, send a HTTP response over the given stream. This 
--   could be improved on to use different transfer types.
respondHTTP :: Stream s => s -> Response -> IO ()
respondHTTP conn rsp = do writeBlock conn (show rsp)
                          -- write body immediately, don't wait for 100 CONTINUE
                          writeBlock conn (rspBody rsp)
			  return ()

-- The following functions were in the where clause of sendHTTP, they have
-- been moved to global scope so other functions can access them.		       

-- | Used when we know exactly how many bytes to expect.
linearTransfer :: Stream s => s -> Int -> IO (Result ([Header],String))
linearTransfer conn n
    = do info <- readBlock conn n
         return $ info `bindE` \str -> Right ([],str)

-- | Used when nothing about data is known,
--   Unfortunately waiting for a socket closure
--   causes bad behaviour.  Here we just
--   take data once and give up the rest.
hopefulTransfer :: Stream s => s -> String -> IO (Result ([Header],String))
hopefulTransfer conn str
    = readLine conn >>= 
      either (\v -> return $ Left v)
             (\more -> if null more 
                         then return (Right ([],str)) 
                         else hopefulTransfer conn (str++more))
-- | A necessary feature of HTTP\/1.1
--   Also the only transfer variety likely to
--   return any footers.
chunkedTransfer :: Stream s => s -> IO (Result ([Header],String))
chunkedTransfer conn
    =  chunkedTransferC conn 0 >>= \v ->
       return $ v `bindE` \(ftrs,count,info) ->
                let myftrs = Header HdrContentLength (show count) : ftrs              
                in Right (myftrs,info)

chunkedTransferC :: Stream s => s -> Int -> IO (Result ([Header],Int,String))
chunkedTransferC conn n
    =  readLine conn >>= \v -> case v of
                  Left e -> return (Left e)
                  Right line ->
                      let size = ( if null line || (head line) == '0'
                                     then 0
                                     else case readHex line of
                                        (n,_):_ -> n
                                        _       -> 0
                                     )
                      in if size == 0
                           then do { rs <- readTillEmpty2 conn []
                                   ; return $
                                        rs `bindE` \strs ->
                                        parseHeaders strs `bindE` \ftrs ->
                                        Right (ftrs,n,"")
                                   }
                           else do { some <- readBlock conn size
                                   ; readLine conn
                                   ; more <- chunkedTransferC conn (n+size)
                                   ; return $ 
                                        some `bindE` \cdata ->
                                        more `bindE` \(ftrs,m,mdata) -> 
                                        Right (ftrs,m,cdata++mdata) 
                                   }                   

-- | Maybe in the future we will have a sensible thing
--   to do here, at that time we might want to change
--   the name.
uglyDeathTransfer :: Stream s => s -> IO (Result ([Header],String))
uglyDeathTransfer conn
    = return $ Left $ ErrorParse "Unknown Transfer-Encoding"

-- | Remove leading crlfs then call readTillEmpty2 (not required by RFC)
readTillEmpty1 :: Stream s => s -> IO (Result [String])
readTillEmpty1 conn =
    do { line <- readLine conn
       ; case line of
           Left e -> return $ Left e
           Right s ->
               if s == crlf
                 then readTillEmpty1 conn
                 else readTillEmpty2 conn [s]
       }

-- | Read lines until an empty line (CRLF),
--   also accepts a connection close as end of
--   input, which is not an HTTP\/1.1 compliant
--   thing to do - so probably indicates an
--   error condition.
readTillEmpty2 :: Stream s => s -> [String] -> IO (Result [String])
readTillEmpty2 conn list =
    do { line <- readLine conn
       ; case line of
           Left e -> return $ Left e
           Right s ->
               if s == crlf || null s
                 then return (Right $ reverse (s:list))
                 else readTillEmpty2 conn (s:list)
       }

        
-----------------------------------------------------------------
------------------ A little friendly funtionality ---------------
-----------------------------------------------------------------


{-
    I had a quick look around but couldn't find any RFC about
    the encoding of data on the query string.  I did find an
    IETF memo, however, so this is how I justify the urlEncode
    and urlDecode methods.

    Doc name: draft-tiwari-appl-wxxx-forms-01.txt  (look on www.ietf.org)

    Reserved chars:  ";", "/", "?", ":", "@", "&", "=", "+", ",", and "$" are reserved.
    Unwise: "{" | "}" | "|" | "\" | "^" | "[" | "]" | "`"
    URI delims: "<" | ">" | "#" | "%" | <">
    Unallowed ASCII: <US-ASCII coded characters 00-1F and 7F hexadecimal>
                     <US-ASCII coded character 20 hexadecimal>
    Also unallowed:  any non-us-ascii character

    Escape method: char -> '%' a b  where a, b :: Hex digits
-}

urlEncode, urlDecode :: String -> String

urlDecode ('%':a:b:rest) = chr (16 * digitToInt a + digitToInt b)
                         : urlDecode rest
urlDecode (h:t) = h : urlDecode t
urlDecode [] = []

urlEncode (h:t) =
    let str = if reserved (ord h) then escape h else [h]
    in str ++ urlEncode t
    where
        reserved x
            | x >= ord 'a' && x <= ord 'z' = False
            | x >= ord 'A' && x <= ord 'Z' = False
            | x >= ord '0' && x <= ord '9' = False
            | x <= 0x20 || x >= 0x7F = True
            | otherwise = x `elem` map ord [';','/','?',':','@','&'
                                           ,'=','+',',','$','{','}'
                                           ,'|','\\','^','[',']','`'
                                           ,'<','>','#','%','"']
        -- wouldn't it be nice if the compiler
        -- optimised the above for us?

        escape x = 
            let y = ord x 
            in [ '%', intToDigit ((y `div` 16) .&. 0xf), intToDigit (y .&. 0xf) ]

urlEncode [] = []
            


-- Encode form variables, useable in either the
-- query part of a URI, or the body of a POST request.
-- I have no source for this information except experience,
-- this sort of encoding worked fine in CGI programming.
urlEncodeVars :: [(String,String)] -> String
urlEncodeVars ((n,v):t) =
    let (same,diff) = partition ((==n) . fst) t
    in urlEncode n ++ '=' : foldl (\x y -> x ++ ',' : urlEncode y) (urlEncode $ v) (map snd same)
       ++ urlEncodeRest diff
       where urlEncodeRest [] = []
             urlEncodeRest diff = '&' : urlEncodeVars diff
urlEncodeVars [] = []
