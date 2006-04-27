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
    httpVersion,

    -- * Type declarations

    -- ** Streams
    Debug,
    Stream(..),
    debugStream,

    -- ** Connections
    Connection,
    ConnError(..),
    openTCP,
    isConnectedTo,

    
    -- ** HTTP 
    Request(..),
    Response(..),
    RequestMethod(..),
    simpleHTTP,
    sendHTTP,

    -- ** Header Functions
    HasHeaders,
    Header(..),
    HeaderName(..),
    insertHeader,
    insertHeaderIfMissing,
    insertHeaders,
    retrieveHeaders,
    replaceHeader,

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
------------------ TCP Connections ------------------------------
-----------------------------------------------------------------

-- | The 'Connection' newtype is a wrapper that allows us to make
-- connections an instance of the StreamIn\/Out classes, without ghc extensions.
-- While this looks sort of like a generic reference to the transport
-- layer it is actually TCP specific, which can be seen in the
-- implementation of the 'Stream Connection' instance.
newtype Connection = ConnRef {getRef :: IORef Conn}


-- | The 'Conn' object allows input buffering, and maintenance of 
-- some admin-type data.
data Conn = MkConn { connSock :: ! Socket
                   , connAddr :: ! SockAddr 
                   , connBffr :: ! String 
                   , connHost :: String
                   }
          | ConnClosed
    deriving(Eq)



data ConnError = ErrorReset 
               | ErrorClosed
               | ErrorParse String
               | ErrorMisc String
    deriving(Show,Eq)



-- | This is the type returned by many exported network functions.
type Result a = Either ConnError   {- error  -}
                       a           {- result -}


-- error propagating:
-- we could've used a monad, but that would lead us
-- into using the "-fglasgow-exts" compile flag.
bindE :: Either ConnError a -> (a -> Either ConnError b) -> Either ConnError b
bindE (Left e)  _ = Left e
bindE (Right v) f = f v


-- | Open a connection to port 80 on a remote host.
openTCP :: String -> IO Connection
openTCP host = openTCPPort host 80


-- This function establishes a connection to a remote
-- host, it uses "getHostByName" which interrogates the
-- DNS system, hence may trigger a network connection.
--
-- Add a "persistant" option?  Current persistant is default.
-- Use "Result" type for synchronous exception reporting?
openTCPPort :: String -> Int -> IO Connection
openTCPPort uri port = 
    do { s <- socket AF_INET Stream 6
       ; setSocketOption s KeepAlive 1
       ; host <- Exception.catch (inet_addr uri)    -- handles ascii IP numbers
                       (\_ -> getHostByName uri >>= \host ->
                            case hostAddresses host of
                                [] -> return (error "no addresses in host entry")
                                (h:_) -> return h)
       ; let a = SockAddrInet (toEnum port) host
       ; connect s a
       ; v <- newIORef (MkConn s a [] uri)
       ; return (ConnRef v)
       }



-----------------------------------------------------------------
------------------ Gentle Art of Socket Sucking -----------------
-----------------------------------------------------------------

-- | Streams should make layering of TLS protocol easier in future,
-- they allow reading/writing to files etc for debugging,
-- they allow use of protocols other than TCP/IP
-- and they allow customisation.
--
-- Instances of this class should not trim
-- the input in any way, e.g. leave LF on line
-- endings etc. Unless that is exactly the behaviour
-- you want from your twisted instances ;)
class Stream x where 
    readLine   :: x -> IO (Result String)
    readBlock  :: x -> Int -> IO (Result String)
    writeBlock :: x -> String -> IO (Result ())
    close      :: x -> IO ()





-- Exception handler for socket operations
handleSocketError :: Socket -> Exception -> IO (Result a)
handleSocketError sk e =
    do { se <- getSocketOption sk SoError
       ; if se == 0
            then throw e
            else return $ if se == 10054       -- reset
                then Left ErrorReset
                else Left $ ErrorMisc $ show se
       }




instance Stream Socket where
    readBlock sk n = (liftM Right $ fn n) `Exception.catch` (handleSocketError sk)
        where
            fn x = do { str <- myrecv sk x
                      ; let len = length str
                      ; if len < x
                          then ( fn (x-len) >>= \more -> return (str++more) )                        
                          else return str
                      }

    -- Use of the following function is discouraged.
    -- The function reads in one character at a time, 
    -- which causes many calls to the kernel recv()
    -- hence causes many context switches.
    readLine sk = (liftM Right $ fn "") `Exception.catch` (handleSocketError sk)
            where
                fn str =
                    do { c <- myrecv sk 1 -- like eating through a straw.
                       ; if null c || c == "\n"
                           then return (reverse str++c)
                           else fn (head c:str)
                       }
    
    writeBlock sk str = (liftM Right $ fn str) `Exception.catch` (handleSocketError sk)
        where
            fn [] = return ()
            fn x  = send sk x >>= \i -> fn (drop i x)

    -- This slams closed the connection (which is considered rude for TCP\/IP)
    close sk = shutdown sk ShutdownBoth >> sClose sk




instance Stream Connection where
    readBlock ref n = 
        readIORef (getRef ref) >>= \conn -> case conn of
            ConnClosed -> return (Left ErrorClosed)
            (MkConn sk addr bfr hst)
                | length bfr >= n ->
                    do { modifyIORef (getRef ref) (\c -> c { connBffr=(drop n bfr) })
                       ; return (Right $ take n bfr)
                       }
                | otherwise ->
                    do { modifyIORef (getRef ref) (\c -> c { connBffr=[] })
                       ; more <- readBlock sk (n - length bfr)
                       ; return $ case more of
                            Left _ -> more
                            Right s -> (Right $ bfr ++ s)
                       }

    -- This function uses a buffer, at this time the buffer is just 1000 characters.
    -- (however many bytes this is is left to the user to decypher)
    readLine ref =
        readIORef (getRef ref) >>= \conn -> case conn of
             ConnClosed -> return (Left ErrorClosed)
             (MkConn sk addr bfr _)
                 | null bfr ->  {- read in buffer -}
                      do { str <- myrecv sk 1000  -- DON'T use "readBlock sk 1000" !!
                                                -- ... since that call will loop.
                         ; let len = length str
                         ; if len == 0   {- indicates a closed connection -}
                              then return (Right "")
                              else modifyIORef (getRef ref) (\c -> c { connBffr=str })
                                   >> readLine ref  -- recursion
                         }
                 | otherwise ->
                      case elemIndex '\n' bfr of
                          Nothing -> {- need recursion to finish line -}
                              do { modifyIORef (getRef ref) (\c -> c { connBffr=[] })
                                 ; more <- readLine ref -- contains extra recursion                      
                                 ; return $ more `bindE` \str -> Right (bfr++str)
                                 }
                          Just i ->    {- end of line found -}
                              let (bgn,end) = splitAt i bfr in
                              do { modifyIORef (getRef ref) (\c -> c { connBffr=(drop 1 end) })
                                 ; return (Right (bgn++['\n']))
                                 }



    -- The 'Connection' object allows no outward buffering, 
    -- since in general messages are serialised in their entirety.
    writeBlock ref str =
        readIORef (getRef ref) >>= \conn -> case conn of
            ConnClosed -> return (Left ErrorClosed)
            (MkConn sk addr _ _) -> fn sk addr str `Exception.catch` (handleSocketError sk)
        where
            fn sk addr s
                | null s    = return (Right ())  -- done
                | otherwise =
                    getSocketOption sk SoError >>= \se ->
                    if se == 0
                        then sendTo sk s addr >>= \i -> fn sk addr (drop i s)
                        else writeIORef (getRef ref) ConnClosed >>
                             if se == 10054
                                 then return (Left ErrorReset)
                                 else return (Left $ ErrorMisc $ show se)


    -- Closes a Connection.  Connection will no longer
    -- allow any of the other Stream functions.  Notice that a Connection may close
    -- at any time before a call to this function.  This function is idempotent.
    -- (I think the behaviour here is TCP specific)
    close ref = 
        do { c <- readIORef (getRef ref)
           ; closeConn c `Exception.catch` (\_ -> return ())
           ; writeIORef (getRef ref) ConnClosed
           }
        where
            -- Be kind to peer & close gracefully.
            closeConn (ConnClosed) = return ()
            closeConn (MkConn sk addr [] _) =
                do { shutdown sk ShutdownSend
                   ; suck ref
                   ; shutdown sk ShutdownReceive
                   ; sClose sk
                   }

            suck :: Connection -> IO ()
            suck cn = readLine cn >>= 
                      either (\_ -> return ()) -- catch errors & ignore
                             (\x -> if null x then return () else suck cn)

 





-- | Allows stream logging.
-- Refer to 'debugStream' below.
data Debug x = Dbg Handle x


instance (Stream x) => Stream (Debug x) where
    readBlock (Dbg h c) n =
        do { val <- readBlock c n
           ; hPutStrLn h ("readBlock " ++ show n ++ ' ' : show val)
           ; return val
           }

    readLine (Dbg h c) =
        do { val <- readLine c
           ; hPutStrLn h ("readLine " ++ show val)
           ; return val
           }

    writeBlock (Dbg h c) str =
        do { val <- writeBlock c str
           ; hPutStrLn h ("writeBlock " ++ show val ++ ' ' : show str)
           ; return val
           }

    close (Dbg h c) =
        do { hPutStrLn h "closing..."
           ; hFlush h
           ; close c
           ; hPutStrLn h "...closed"
           ; hClose h
           }


-- | Wraps a stream with logging I\/O, the first
-- argument is a filename which is opened in AppendMode.
debugStream :: (Stream a) => String -> a -> IO (Debug a)
debugStream file stm = 
    do { h <- openFile file AppendMode
       ; hPutStrLn h "File opened for appending."
       ; return (Dbg h stm)
       }


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
        

-- Checks both that the underlying Socket is connected
-- and that the connection peer matches the given
-- host name (which is recorded locally).
isConnectedTo :: Connection -> String -> IO Bool
isConnectedTo conn name =
    do { v <- readIORef (getRef conn)
       ; case v of
            ConnClosed -> return False
            (MkConn sk _ _ h) ->
                if (map toLower h == map toLower name)
                then sIsConnected sk
                else return False
       }




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
       let r' = fixReq auth r 
       c <- openTCPPort (host auth) (fromMaybe 80 (port auth))
       rsp <- if debug then do
	        c' <- debugStream httpLogFile c
	        sendHTTP c' r'
	       else
	        sendHTTP c r'
       -- already done by sendHTTP because of "Connection: close" header
       --; close c 
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
        
        -- remove leading crlfs then call readTillEmpty2 (not required by RFC)
        readTillEmpty1 :: IO (Result [String])
        readTillEmpty1 =
            do { line <- readLine conn
               ; case line of
                    Left e -> return $ Left e
                    Right s ->
                        if s == crlf
                          then readTillEmpty1
                          else readTillEmpty2 [s]
               }
               
               
        -- read lines until an empty line (CRLF),
        -- also accepts a connection close as end of
        -- input, which is not an HTTP/1.1 compliant
        -- thing to do - so probably indicates an
        -- error condition.
        readTillEmpty2 :: [String] -> IO (Result [String])
        readTillEmpty2 list =
            do { line <- readLine conn
               ; case line of
                    Left e -> return $ Left e
                    Right s ->
                        if s == crlf || null s
                          then return (Right $ reverse (s:list))
                          else readTillEmpty2 (s:list)
               }
               

        -- reads and parses headers
        getResponseHead :: IO (Result ResponseData)
        getResponseHead =
            do { lor <- readTillEmpty1
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
                                  Just x  -> linearTransfer (read x :: Int)
                                  Nothing -> hopefulTransfer ""
                          Just x  -> 
                              case map toLower (trim x) of
                                  "chunked" -> chunkedTransfer
                                  _         -> uglyDeathTransfer
                       ; return $ rslt `bindE` \(ftrs,bdy) -> Right (Response cd rn (hdrs++ftrs) bdy) 
                       }

        
        -- Adds a Host header if one is NOT ALREADY PRESENT
        fixHostHeader :: Request -> Request
        fixHostHeader rq =
            let uri = rqURI rq
                host = authority uri
            in insertHeaderIfMissing HdrHost host rq

        
        -- Used when we know exactly how many bytes to expect.
        linearTransfer :: Int -> IO (Result ([Header],String))
        linearTransfer n
            = do info <- readBlock conn n
                 return $ info `bindE` \str -> Right ([],str)

        -- Used when nothing about data is known,
        -- Unfortunately waiting for a socket closure
        -- causes bad behaviour.  Here we just
        -- take data once and give up the rest.
        hopefulTransfer :: String -> IO (Result ([Header],String))
        hopefulTransfer str
            = readLine conn >>= 
              either (\v -> return $ Left v)
                     (\more -> if null more 
                        then return (Right ([],str)) 
                        else hopefulTransfer (str++more))

                    
        -- A necessary feature of HTTP/1.1
        -- Also the only transfer variety likely to
        -- return any footers.
        chunkedTransfer :: IO (Result ([Header],String))
        chunkedTransfer
            =  chunkedTransferC 0 >>= \v ->
               return $ v `bindE` \(ftrs,count,info) ->
                        let myftrs = Header HdrContentLength (show count) : ftrs              
                        in Right (myftrs,info)

        chunkedTransferC :: Int -> IO (Result ([Header],Int,String))
        chunkedTransferC n
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
                           then do { rs <- readTillEmpty2 []
                                   ; return $
                                        rs `bindE` \strs ->
                                        parseHeaders strs `bindE` \ftrs ->
                                        Right (ftrs,n,"")
                                   }
                           else do { some <- readBlock conn size
                                   ; readLine conn
                                   ; more <- chunkedTransferC (n+size)
                                   ; return $ 
                                        some `bindE` \cdata ->
                                        more `bindE` \(ftrs,m,mdata) -> 
                                        Right (ftrs,m,cdata++mdata) 
                                   }                   
                         
             
        -- Maybe in the future we will have a sensible thing
        -- to do here, at that time we might want to change
        -- the name.
        uglyDeathTransfer :: IO (Result ([Header],String))
        uglyDeathTransfer
            = return $ Left $ ErrorParse "Unknown Transfer-Encoding"
          
        
        -- Looks for a "Connection" header with the value "close".
        -- Returns True when this is found.
        findConnClose :: [Header] -> Bool
        findConnClose hdrs =
            case lookupHeader HdrConnection hdrs of
                Nothing -> False
                Just x  -> map toLower (trim x) == "close"


        
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


myrecv :: Socket -> Int -> IO String
myrecv sock len =
    let handler e = if isEOFError e then return [] else ioError e
        in System.IO.Error.catch (recv sock len) handler
