-----------------------------------------------------------------------------
-- |
-- Module      :  Network.TCP
-- Copyright   :  (c) Warrick Gray 2002, Bjorn Bringert 2003-2004, Simon Foster 2004
-- License     :  BSD
--
-- Maintainer  :  bjorn@bringert.net
-- Stability   :  experimental
-- Portability :  non-portable (not tested)
--
-- An easy access TCP library. Makes the use of TCP in Haskell much easier.
-- This was originally part of Gray's\/Bringert's HTTP module.
--
-- * Changes by Simon Foster:
--      - Split module up into to sepearate Network.[Stream,TCP,HTTP] modules
--      
-----------------------------------------------------------------------------
module Network.TCP (
    -- ** Connections
    Conn(..),
    Connection(..),
    openTCP,
    openTCPPort,
    isConnectedTo
) where

import Control.Exception as Exception

-- Networking
import Network (withSocketsDo)
import Network.BSD
import Network.URI
import Network.Socket
import Network.Stream

import Data.List (isPrefixOf,partition,elemIndex)
import Data.Char
import Data.IORef
import Control.Monad (when,liftM,guard)
import System.IO

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


-- | Open a connection to port 80 on a remote host.
openTCP :: String -> IO Connection
openTCP host = openTCPPort host 80


-- | This function establishes a connection to a remote
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
       ; Exception.catch (connect s a) (\e -> sClose s >> throw e)
       ; v <- newIORef (MkConn s a [] uri)
       ; return (ConnRef v)
       }

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

-- | Checks both that the underlying Socket is connected
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

