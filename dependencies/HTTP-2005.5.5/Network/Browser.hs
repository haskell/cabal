-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Browser
-- Copyright   :  (c) Warrick Gray 2002
-- License     :  BSD
-- 
-- Maintainer  :  bjorn@bringert.net
-- Stability   :  experimental
-- Portability :  non-portable (not tested)
--
-- An HTTP\/1.1 compatible wrapper for the HTTP module.
-----------------------------------------------------------------------------
 
{-

  Change Log:
   - altered 'closeTCP' to 'close', for consistency with altered HTTP
   - added debugging settings to browser.

  To Do: 
   - testing!!!
   - remove BrowserAction type? Possibly replace with IORef?
   - (more todo's in the HTTP mod)

-}

module Network.Browser (
    BrowserState,
    BrowserAction,      -- browser monad, effectively a state monad.
    Cookie,
    Form(..),
    Proxy(..),
    
    browse,             -- BrowserAction a -> IO a
    request,            -- Request -> BrowserAction Response
    
    setAllowRedirects,
    getAllowRedirects,
    
    setCookieFilter,
    defaultCookieFilter,
    userCookieFilter,
    
    getCookies,
    setCookies,
    addCookie,

    setErrHandler,
    setOutHandler,

    setProxy,

    setDebugLog,

    out,
    err,
    ioAction,           -- :: IO a -> BrowserAction a

    defaultGETRequest,
    formToRequest,
    uriDefaultTo,
    uriTrimHost
) where

import Network.HTTP

import Data.Char (toLower,isAlphaNum,isSpace)
import Data.List (isPrefixOf,isSuffixOf,elemIndex,elemIndices)
import Data.Maybe
import Control.Monad (foldM,filterM,liftM,when)
import Text.ParserCombinators.Parsec
import Network.URI

import qualified System.IO
import qualified Data.Digest.MD5 as MD5
import qualified Codec.Binary.Base64 as Base64
import Codec.Utils (Octet)




------------------------------------------------------------------
----------------------- Miscellaneous ----------------------------
------------------------------------------------------------------

word, quotedstring :: Parser String
quotedstring =
    do { char '"'
       ; str <- many (satisfy $ not . (=='"'))
       ; char '"'
       ; return str
       }

word = many1 (satisfy (\x -> isAlphaNum x || x=='_' || x=='.'))


-- misc string fns
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
    


-- removes delimiters
splitMany :: Eq a => a -> [a] -> [[a]]
splitMany delim str = fn str ixs
    where
        ixs = elemIndices delim str
        fn _ [] = []
        fn ls (h:t) = let (a,b) = splitAt h ls in a : fn b t


-- Returns a URI that is consistent with the first
-- argument uri when read in the context of a second.
-- If second argument is not sufficient context for
-- determining a full URI then anarchy reins.
uriDefaultTo :: URI -> URI -> URI
uriDefaultTo a b =
    case a `relativeTo` b of
        Nothing -> a
        Just x  -> x


uriTrimHost :: URI -> URI
uriTrimHost uri = uri { uriScheme="", uriAuthority=Nothing }


------------------------------------------------------------------
----------------------- Cookie Stuff -----------------------------
------------------------------------------------------------------

-- Some conventions: 
--     assume ckDomain is lowercase
--
data Cookie = MkCookie { ckDomain
                       , ckName
                       , ckValue :: String
                       , ckPath
                       , ckComment
                       , ckVersion :: Maybe String
                       }
    deriving(Show,Read)


instance Eq Cookie where
    a == b  =  ckDomain a == ckDomain b 
            && ckName a == ckName b 
            && ckPath a == ckPath b




defaultCookieFilter url cky = return True
userCookieFilter url cky =
    do putStrLn ("Set-Cookie received when requesting: " ++ show url)
       case ckComment cky of
          Nothing -> return ()
          Just x  -> putStrLn ("Cookie Comment:\n" ++ x)
       putStrLn ("Domain/Path: " ++ ckDomain cky ++ 
            case ckPath cky of
                Nothing -> ""
                Just x  -> "/" ++ x)
       putStrLn (ckName cky ++ '=' : ckValue cky)
       System.IO.hSetBuffering System.IO.stdout System.IO.NoBuffering
       System.IO.hSetBuffering System.IO.stdin System.IO.NoBuffering
       System.IO.hPutStr System.IO.stdout "Accept [y/n]? "
       x <- System.IO.hGetChar System.IO.stdin
       System.IO.hSetBuffering System.IO.stdin System.IO.LineBuffering
       System.IO.hSetBuffering System.IO.stdout System.IO.LineBuffering
       return (toLower x == 'y')
       


-- Serialise a Cookie for inclusion in a request.
cookieToHeader :: Cookie -> Header
cookieToHeader ck = Header HdrCookie text
    where
        text = "$Version=" ++ fromMaybe "0" (ckVersion ck)
             ++ ';' : ckName ck ++ "=" ++ ckValue ck
             ++ (case ckPath ck of
                     Nothing -> ""
                     Just x  -> ";$Path=" ++ x)
             ++ ";$Domain=" ++ ckDomain ck



{- replace "error" call with [] in final version? -}
headerToCookies :: String -> Header -> [Cookie]
headerToCookies dom (Header HdrSetCookie val) = 
    case parse cookies "" val of
        Left e  -> error ("Cookie parse failure on: " ++ val ++ " " ++ show e)
        Right x  -> x
    where
        cookies :: Parser [Cookie]
        cookies = sepBy1 cookie (char ',')

        cookie :: Parser Cookie
        cookie =
            do { name <- word
               ; spaces
               ; char '='
               ; spaces
               ; val <- cvalue
               ; args <- cdetail
               ; return $ mkCookie name val args
               }

        cvalue :: Parser String
        
        spaces = many (satisfy isSpace)

        cvalue = quotedstring <|> many1 (satisfy $ not . (==';'))
       
        -- all keys in the result list MUST be in lower case
        cdetail :: Parser [(String,String)]
        cdetail = many $
            do { spaces
               ; char ';'
               ; spaces
               ; s1 <- word
               ; spaces
               ; s2 <- option "" (do { char '=' ; spaces ; v <- cvalue ; return v })
               ; return (map toLower s1,s2)
               }

        mkCookie :: String -> String -> [(String,String)] -> Cookie
        mkCookie nm val more = MkCookie { ckName=nm
                                        , ckValue=val
                                        , ckDomain=map toLower (fromMaybe dom (lookup "domain" more))
                                        , ckPath=lookup "path" more
                                        , ckVersion=lookup "version" more
                                        , ckComment=lookup "comment" more
                                        }

      

-- Adds a cookie to the browser state, removing duplicates.
addCookie :: Cookie -> BrowserAction ()
addCookie c = alterBS (\b -> b { bsCookies=c : fn (bsCookies b) })
    where
        fn = filter (not . (==c))

setCookies cs = alterBS (\b -> b { bsCookies=cs })
getCookies = getBS bsCookies

-- ...get domain specific cookies...
-- ... this needs changing for consistency with rfc2109...
-- ... currently too broad.
getCookiesFor :: String -> String -> BrowserAction [Cookie]
getCookiesFor dom path =
    do cks <- getCookies
       return (filter cookiematch cks)
    where
        cookiematch :: Cookie -> Bool
        cookiematch ck = ckDomain ck `isSuffixOf` dom
                      && case ckPath ck of
                             Nothing -> True
                             Just p  -> p `isPrefixOf` path
      

setCookieFilter :: (URI -> Cookie -> IO Bool) -> BrowserAction ()
setCookieFilter f = alterBS (\b -> b { bsCookieFilter=f })

getCookieFilter :: BrowserAction (URI -> Cookie -> IO Bool)
getCookieFilter = getBS bsCookieFilter

------------------------------------------------------------------
----------------------- Authorisation Stuff ----------------------
------------------------------------------------------------------

{-

The browser handles 401 responses in the following manner:
  1) extract all WWW-Authenticate headers from a 401 response
  2) rewrite each as a Challenge object, using "headerToChallenge"
  3) pick a challenge to respond to, usually the strongest
     challenge understood by the client, using "pickChallenge"
  4) generate a username/password combination using the browsers
     "bsAuthorityGen" function (the default behaviour is to ask
     the user)
  5) build an Authority object based upon the challenge and user
     data, store this new Authority in the browser state
  6) convert the Authority to a request header and add this
     to a request using "withAuthority"
  7) send the amended request

Note that by default requests are annotated with authority headers
before the first sending, based upon previously generated Authority
objects (which contain domain information).  Once a specific authority
is added to a rejected request this predictive annotation is suppressed.

407 responses are handled in a similar manner, except
   a) Authorities are not collected, only a single proxy authority
      is kept by the browser
   b) If the proxy used by the browser (type Proxy) is NoProxy, then
      a 407 response will generate output on the "err" stream and
      the response will be returned.


Notes:
 - digest authentication so far ignores qop, so fails to authenticate 
   properly with qop=auth-int challenges
 - calculates a1 more than necessary
 - doesn't reverse authenticate
 - doesn't properly receive AuthenticationInfo headers, so fails
   to use next-nonce etc

-}


data Algorithm = AlgMD5 | AlgMD5sess
    deriving(Eq)

instance Show Algorithm where
    show AlgMD5 = "md5"
    show AlgMD5sess = "md5-sess"


data Qop = QopAuth | QopAuthInt
    deriving(Eq,Show)


data Challenge = ChalBasic  { chRealm   :: String }
               | ChalDigest { chRealm   :: String
                            , chDomain  :: [URI]
                            , chNonce   :: String
                            , chOpaque  :: Maybe String
                            , chStale   :: Bool
                            , chAlgorithm ::Maybe Algorithm
                            , chQop     :: [Qop]
                            }

-- Convert WWW-Authenticate header into a Challenge object
headerToChallenge :: URI -> Header -> Maybe Challenge
headerToChallenge baseURI (Header _ str) =
    case parse challenge "" str of
        Left e -> Nothing
        Right (name,props) -> case name of
            "basic"  -> mkBasic props
            "digest" -> mkDigest props
            _        -> Nothing
    where
        challenge :: Parser (String,[(String,String)])
        challenge =
            do { nme <- word
               ; pps <- cprops
               ; return (map toLower nme,pps)
               }

        cprops = sepBy1 cprop comma

        comma = do { spaces ; char ',' ; spaces }

        cprop =
            do { nm <- word
               ; char '='
               ; val <- quotedstring
               ; return (map toLower nm,val)
               }

        quotedstring =
            do { char '"'
               ; str <- many (satisfy (not.(=='"')))
               ; char '"'
               ; return str
               }        
        
        mkBasic, mkDigest :: [(String,String)] -> Maybe Challenge

        mkBasic params = fmap ChalBasic (lookup "realm" params)

        mkDigest params =
            -- with Maybe monad
            do { r <- lookup "realm" params
               ; n <- lookup "nonce" params
               ; return $ 
                    ChalDigest { chRealm  = r
                               , chDomain = (annotateURIs 
                                            $ map parseURI
                                            $ words 
                                            $ fromMaybe [] 
                                            $ lookup "domain" params)
                               , chNonce  = n
                               , chOpaque = lookup "opaque" params
                               , chStale  = "true" == (map toLower
                                           $ fromMaybe "" (lookup "stale" params))
                               , chAlgorithm= readAlgorithm (fromMaybe "MD5" $ lookup "algorithm" params)
                               , chQop    = readQop (fromMaybe "" $ lookup "qop" params)
                               }
               }

        annotateURIs :: [Maybe URI] -> [URI]
        annotateURIs = (map (\u -> fromMaybe u (u `relativeTo` baseURI))) . catMaybes

        -- Change These:
        readQop :: String -> [Qop]
        readQop = catMaybes . (map strToQop) . (splitMany ',')

        strToQop str = case map toLower (trim str) of
            "auth"     -> Just QopAuth
            "auth-int" -> Just QopAuthInt
            _          -> Nothing

        readAlgorithm str = case map toLower (trim str) of
            "md5"      -> Just AlgMD5
            "md5-sess" -> Just AlgMD5sess
            _          -> Nothing


data Authority = AuthBasic { auRealm    :: String
                           , auUsername :: String
                           , auPassword :: String
                           , auSite     :: URI
                           }
               | AuthDigest { auRealm     :: String
                            , auUsername  :: String
                            , auPassword  :: String
                            , auNonce     :: String
                            , auAlgorithm :: Maybe Algorithm
                            , auDomain    :: [URI]
                            , auOpaque    :: Maybe String
                            , auQop       :: [Qop]
                            }


-- Return authorities for a given domain and path.
-- Assumes "dom" is lower case
getAuthFor :: String -> String -> BrowserAction [Authority]
getAuthFor dom pth =
    do { list <- getAuthorities
       ; return (filter match list)
       }
    where
        match :: Authority -> Bool
        match (AuthBasic _ _ _ s) = matchURI s
        match (AuthDigest _ _ _ _ _ ds _ _) = or (map matchURI ds)            

        matchURI :: URI -> Bool
        matchURI s = (authority s == dom) && (path s `isPrefixOf` pth)
    

-- Interacting with browser state:
getAuthorities :: BrowserAction [Authority]
getAuthorities = getBS bsAuthorities

setAuthorities :: [Authority] -> BrowserAction ()
setAuthorities as = alterBS (\b -> b { bsAuthorities=as })

addAuthority :: Authority -> BrowserAction ()
addAuthority a = alterBS (\b -> b { bsAuthorities=a:bsAuthorities b })

getAuthorityGen :: BrowserAction (URI -> String -> IO (Maybe (String,String)))
getAuthorityGen = getBS bsAuthorityGen

setAuthorityGen :: (URI -> String -> IO (Maybe (String,String))) -> BrowserAction ()
setAuthorityGen f = alterBS (\b -> b { bsAuthorityGen=f })

setAllowBasicAuth :: Bool -> BrowserAction ()
setAllowBasicAuth ba = alterBS (\b -> b { bsAllowBasicAuth=ba })




-- TO BE CHANGED!!!
pickChallenge :: [Challenge] -> Maybe Challenge
pickChallenge = listToMaybe



-- Retrieve a likely looking authority for a Request.
anticipateChallenge :: Request -> BrowserAction (Maybe Authority)
anticipateChallenge rq =
    let uri = rqURI rq in
    do { authlist <- getAuthFor (authority uri) (path uri)
       ; return (listToMaybe authlist)
       }


-- Asking the user to respond to a challenge
challengeToAuthority :: URI -> Challenge -> BrowserAction (Maybe Authority)
challengeToAuthority uri ch =
    -- prompt user for authority
    if answerable ch then
        do { prompt <- getAuthorityGen
           ; userdetails <- ioAction $ prompt uri (chRealm ch)
           ; case userdetails of
               Nothing    -> return Nothing
               Just (u,p) -> return (Just $ buildAuth ch u p)
           }
    else return Nothing
    where
        answerable :: Challenge -> Bool
        answerable (ChalBasic _) = True
        answerable ch            = (chAlgorithm ch) == Just AlgMD5

        buildAuth :: Challenge -> String -> String -> Authority
        buildAuth (ChalBasic r) u p = 
            AuthBasic { auSite=uri
                      , auRealm=r
                      , auUsername=u
                      , auPassword=p
                      }

         -- note to self: this is a pretty stupid operation
         -- to perform isn't it? ChalX and AuthX are so very
         -- similar.
        buildAuth (ChalDigest r d n o s a q) u p =
            AuthDigest { auRealm=r
                       , auUsername=u
                       , auPassword=p
                       , auDomain=d
                       , auNonce=n
                       , auOpaque=o
                       , auAlgorithm=a
                       , auQop=q
                       }


-- Generating a credentials value from an Authority, in
-- the context of a specific request.  If a client nonce
-- was to be used then this function might need to
-- be of type ... -> BrowserAction String
withAuthority :: Authority -> Request -> String
withAuthority a rq = case a of
        AuthBasic _ _ user pass ->
	    "basic " ++ base64encode (auUsername a ++ ':' : auPassword a)
        AuthDigest _ _ _ _ _ _ _ _ ->
            "digest username=\"" ++ auUsername a 
              ++ "\",realm=\"" ++ auRealm a
              ++ "\",nonce=\"" ++ auNonce a
              ++ "\",uri=\"" ++ digesturi
              ++ ",response=\"" ++ rspdigest 
              ++ "\""
              -- plus optional stuff:
              ++ ( if isJust (auAlgorithm a) then "" else ",algorithm=\"" ++ show (fromJust $ auAlgorithm a) ++ "\"" )
              ++ ( if isJust (auOpaque a) then "" else ",opaque=\"" ++ (fromJust $ auOpaque a) ++ "\"" )
              ++ ( if null (auQop a) then "" else ",qop=auth" )
    where
        rspdigest = "\"" 
                 ++ map toLower (kd (md5 a1) (noncevalue ++ ":" ++ md5 a2))
                 ++ "\""

        -- FIXME: these probably only work right for latin-1 strings
	stringToOctets :: String -> [Octet]
	stringToOctets = map (fromIntegral . fromEnum)
	octetsToString :: [Octet] -> String
	octetsToString = map (toEnum . fromIntegral)
	base64encode :: String -> String
	base64encode = Base64.encode . stringToOctets
	md5 :: String -> String
	md5 = octetsToString . MD5.hash . stringToOctets

        kd :: String -> String -> String
        kd a b = md5 (a ++ ":" ++ b)

        a1, a2 :: String
        a1 = auUsername a ++ ":" ++ auRealm a ++ ":" ++ auPassword a
        
        {-
        If the "qop" directive's value is "auth" or is unspecified, then A2
        is:
           A2  = Method ":" digest-uri-value
        If the "qop" value is "auth-int", then A2 is:
           A2  = Method ":" digest-uri-value ":" H(entity-body)
        -}
        a2 = show (rqMethod rq) ++ ":" ++ digesturi

        digesturi = show (rqURI rq)
        noncevalue = auNonce a


------------------------------------------------------------------
------------------ Proxy Stuff -----------------------------------
------------------------------------------------------------------

data Proxy = NoProxy
           | Proxy String (Maybe Authority)


------------------------------------------------------------------
------------------ Browser State Actions -------------------------
------------------------------------------------------------------


data BrowserState = BS { bsErr, bsOut     :: String -> IO ()
                       , bsCookies        :: [Cookie]
                       , bsCookieFilter   :: URI -> Cookie -> IO Bool
                       , bsAuthorityGen   :: URI -> String -> IO (Maybe (String,String))
                       , bsAuthorities    :: [Authority]
                       , bsAllowRedirects :: Bool
                       , bsAllowBasicAuth :: Bool
                       , bsConnectionPool :: [Connection]
                       , bsProxy          :: Proxy
                       , bsDebug          :: Maybe String
                       }

instance Show BrowserState where
    show bs =  "BrowserState { " 
            ++ show (bsCookies bs)  ++ "\n"
           {- ++ show (bsAuthorities bs) ++ "\n"-}
            ++ "AllowRedirects: " ++ show (bsAllowRedirects bs)
            ++ "} "


-- Simple DIY stateful behaviour, with IO
data BrowserAction a = BA { lift :: (BrowserState -> IO (BrowserState,a)) }

instance Monad BrowserAction where
    a >>= f  =  BA (\b -> do { (nb,v) <- lift a b ; lift (f v) nb})
    return x =  BA (\b -> return (b,x))


-- Apply a browser action to a state.
browse :: BrowserAction a -> IO a
browse act = do x <- lift act defaultBrowserState
                return (snd x)
    where
        defaultBrowserState :: BrowserState
        defaultBrowserState = 
            BS { bsErr              = putStrLn
               , bsOut              = putStrLn
               , bsCookies          = []
               , bsCookieFilter     = defaultCookieFilter
               , bsAuthorityGen     = (error "bsAuthGen wanted")
               , bsAuthorities      = []
               , bsAllowRedirects   = True
               , bsAllowBasicAuth   = False
               , bsConnectionPool   = []
               , bsProxy            = NoProxy
               , bsDebug            = Nothing 
               }

-- Alter browser state
alterBS :: (BrowserState -> BrowserState) -> BrowserAction ()
alterBS f = BA (\b -> return (f b,()))

getBS :: (BrowserState -> a) -> BrowserAction a
getBS f = BA (\b -> return (b,f b))

-- Do an io action
ioAction :: IO a -> BrowserAction a
ioAction a = BA (\b -> a >>= \v -> return (b,v))


-- Stream handlers
setErrHandler, setOutHandler :: (String -> IO ()) -> BrowserAction ()
setErrHandler h = alterBS (\b -> b { bsErr=h })
setOutHandler h = alterBS (\b -> b { bsOut=h })

out, err :: String -> BrowserAction ()
out s = do { f <- getBS bsOut ; ioAction $ f s }
err s = do { f <- getBS bsErr ; ioAction $ f s }

-- Redirects
setAllowRedirects :: Bool -> BrowserAction ()
setAllowRedirects bl = alterBS (\b -> b {bsAllowRedirects=bl})

getAllowRedirects :: BrowserAction Bool
getAllowRedirects = getBS bsAllowRedirects


-- Proxy
setProxy :: Proxy -> BrowserAction ()
setProxy p = alterBS (\b -> b {bsProxy = p})

getProxy :: BrowserAction Proxy
getProxy = getBS bsProxy


-- Debug
setDebugLog :: Maybe String -> BrowserAction ()
setDebugLog v = alterBS (\b -> b {bsDebug=v})


-- Page control
type RequestState = ( Int    -- number of 401 responses so far
                    , Int    -- number of redirects so far
                    , Int    -- number of retrys so far
                    , Bool   -- whether to pre-empt 401 response
                    )



-- Surely the most important bit:
request = request' initialState
    where
        initialState = (0,0,0,True)


request' :: RequestState -> Request -> BrowserAction (URI,Response)
request' (denycount,redirectcount,retrycount,preempt) rq =
    do -- add cookies to request
       let uri = rqURI rq
       cookies <- getCookiesFor (authority uri) (path uri)
       
       when (not $ null cookies) 
            (out $ "Adding cookies to request.  Cookie names: " 
                 ++ foldl spaceappend "" (map ckName cookies))
       
       -- add credentials to request
       rq' <- if not preempt then return rq else
              do { auth <- anticipateChallenge rq
                 ; case auth of
                     Just x  -> return (insertHeader HdrAuthorization (withAuthority x rq) rq)
                     Nothing -> return rq
                 }
               
       let rq'' = insertHeaders (map cookieToHeader cookies) rq'

       p <- getProxy

       out ("Sending:\n" ++ show rq'') 
       e_rsp <- case p of
            NoProxy -> dorequest (authority $ rqURI rq'') rq''
            Proxy str ath ->
                let rq''' = case ath of 
                                Nothing -> rq''
                                Just x  -> insertHeader HdrProxyAuthorization (withAuthority x rq'') rq''
                in dorequest str rq'''

       case e_rsp of
           Left v -> if (retrycount < 4) && (v == ErrorReset || v == ErrorClosed)
               then request' (denycount,redirectcount,retrycount+1,preempt) rq
               else error ("Exception raised in request: " ++ show v)
           Right rsp -> do 
               out ("Received:\n" ++ show rsp)

               -- add new cookies to browser state
               let cookieheaders = retrieveHeaders HdrSetCookie rsp
               let newcookies = concat (map (headerToCookies $ authority uri) cookieheaders)

               when (not $ null newcookies)
                    (out $ foldl (\x y -> x ++ "\n  " ++ show y) "Cookies received:" newcookies)
               
               filterfn <- getCookieFilter
               newcookies' <- ioAction (filterM (filterfn uri) newcookies)
               foldM (\_ -> addCookie) () newcookies'

               when (not $ null newcookies)
                    (out $ "Accepting cookies with names: " ++ foldl spaceappend "" (map ckName newcookies'))
       
               case rspCode rsp of
                   (4,0,1) ->  -- Credentials not sent or refused.
                       out "401 - credentials not sent or refused" >>
                       if denycount > 2 then return (uri,rsp) else
                       do { let hdrs = retrieveHeaders HdrWWWAuthenticate rsp
                          ; case pickChallenge (catMaybes $ map (headerToChallenge uri) hdrs) of
                                Just x  ->
                                    do { au <- challengeToAuthority uri x
                                       ; case au of
                                            Just au' ->
                                                out "Retrying request with new credentials" >>
                                                request' (denycount+1,redirectcount,retrycount,False)
                                                         (insertHeader HdrAuthorization (withAuthority au' rq) rq)
                                            Nothing  -> return (uri,rsp)   {- do nothing -}
                                       }
                                          
                                Nothing -> return (uri,rsp)   {- do nothing -}
                          }
                   

                   (4,0,7) ->  -- Proxy Authentication required
                       out "407 - proxy authentication required" >>
                       if denycount > 2 then return (uri,rsp) else
                       do { let hdrs = retrieveHeaders HdrProxyAuthenticate rsp
                          ; case pickChallenge (catMaybes $ map (headerToChallenge uri) hdrs) of
                                Just x  ->
                                    do { au <- challengeToAuthority uri x
                                       ; case au of
                                            Just au' ->
                                                do { pxy <- getBS bsProxy
                                                   ; case pxy of
                                                        NoProxy ->
                                                            do { err "Proxy authentication required without proxy!"
                                                               ; return (uri,rsp)
                                                               }
                                                        Proxy x y ->
                                                            do { out "Retrying with proxy authentication"
                                                               ; setProxy (Proxy x $ Just au')
                                                               ; request' (denycount+1,redirectcount,retrycount,False) rq
                                                               }
                                                   }                                                      
                                            Nothing  -> return (uri,rsp)   {- do nothing -}
                                       }
                                          
                                Nothing -> return (uri,rsp)   {- do nothing -}
                          }


                   (3,0,3) ->  -- Redirect using GET request method.
                       do { out "303 - redirect using GET"
                          ; rd <- getAllowRedirects
                          ; if not rd || redirectcount > 4 then return (uri,rsp) else
                            case retrieveHeaders HdrLocation rsp of
                                (Header _ u:_) -> case parseURI u of
                                    Just newuri ->
                                        let newuri' = case newuri `relativeTo` uri of
                                                        Nothing -> newuri
                                                        Just x  -> x
                                        in do { out ("Redirecting to " ++ show newuri' ++ " ...") 
                                              ; let rq = rq { rqMethod=GET, rqURI=newuri', rqBody="" }
                                              ; request' (0,redirectcount+1,retrycount,True)
                                                         (replaceHeader HdrContentLength "0" rq) 
                                              }
                                    Nothing ->
                                        do { err ("Parse of Location header in a redirect response failed: " ++ u)
                                           ; return (uri,rsp)
                                           }
                                [] -> do { err "No Location header in redirect response"
                                         ; return (uri,rsp)
                                         }
                          }
                        
                   (3,0,5) ->
                        case retrieveHeaders HdrLocation rsp of
                            (Header _ u:_) -> case parseURI u of
                                Just newuri ->
                                    do { out ("Retrying with proxy " ++ show newuri ++ "...")
                                       ; setProxy (Proxy (authority newuri) Nothing)
                                       ; request' (0,0,retrycount+1,True) rq
                                       }
                                Nothing ->
                                    do { err ("Parse of Location header in a proxy redirect response failed: " ++ u)
                                       ; return (uri,rsp)
                                       }
                            [] -> do { err "No Location header in proxy redirect response."
                                     ; return (uri,rsp)
                                     }
                   

                   (3,_,_) ->  -- Redirection
                       do { rd <- getAllowRedirects
                          ; if not rd || redirectcount > 4 then return (uri,rsp) else
                            case retrieveHeaders HdrLocation rsp of
                                (Header _ u:_) -> case parseURI u of
                                    Just newuri ->
                                        let newuri' = case newuri `relativeTo` uri of
                                                        Nothing -> newuri
                                                        Just x  -> x
                                        in do { out ("Redirecting to " ++ show newuri' ++ " ...") 
                                              ; request' (0,redirectcount+1,retrycount,True)
                                                         (rq { rqURI=newuri' }) 
                                              }
                                    Nothing ->
                                        do { err ("Parse of Location header in a redirect response failed: " ++ u)
                                           ; return (uri,rsp)
                                           }
                                [] -> do { err "No Location header in redirect response."
                                         ; return (uri,rsp)
                                         }
                          }

                   _      -> return (uri,rsp)

    where      
        spaceappend :: String -> String -> String
        spaceappend x y = x ++ ' ' : y

        dorequest :: String -> Request -> BrowserAction (Either ConnError Response)
        dorequest hst rqst = 
            do { pool <- getBS bsConnectionPool
               ; conn <- ioAction $ filterM (\c -> c `isConnectedTo` hst) pool
               ; rsp <- case conn of
                    [] -> do { out ("Creating new connection to " ++ hst)
                             ; c <- ioAction $ openTCP hst
                             ; let pool' = if length pool > 5
                                           then init pool
                                           else pool
                             ; when (length pool > 5)
                                    (ioAction $ close (last pool))
                             ; alterBS (\b -> b { bsConnectionPool=c:pool' })
                             ; dorequest2 hst c rqst 
                             }
                    (c:_) ->
                        do { out ("Recovering connection to " ++ hst)
                           ; dorequest2 hst c rqst
                           }
               ; 
               ; return rsp
               }

        dorequest2 hst c r =
            do { dbg <- getBS bsDebug
               ; ioAction $ case dbg of
                 Nothing -> sendHTTP c r
                 Just f  ->
                    debugStream (f++'-':hst) c 
                    >>= \c' -> sendHTTP c' r  
               }
 




------------------------------------------------------------------
------------------ Request Building ------------------------------
------------------------------------------------------------------


libUA = "haskell-libwww/0.1"

defaultGETRequest uri = 
    Request { rqURI=uri
            , rqBody=""
            , rqHeaders=[ Header HdrContentLength "0"
                        , Header HdrUserAgent libUA
                        ]
            , rqMethod=GET
            }



-- This form junk is completely untested...

type FormVar = (String,String)

data Form = Form RequestMethod URI [FormVar]


formToRequest :: Form -> Request
formToRequest (Form m u vs) =
    let enc = urlEncodeVars vs
    in case m of
        GET -> Request { rqMethod=GET
                       , rqHeaders=[ Header HdrContentLength "0" ]
                       , rqBody=""
                       , rqURI=u { uriQuery=enc }  -- What about old query?
                       }
        POST -> Request { rqMethod=POST
                        , rqHeaders=[ Header HdrContentLength (show $ length enc) ]
                        , rqBody=enc
                        , rqURI=u { uriQuery=enc }  -- What about old query?
                        }
