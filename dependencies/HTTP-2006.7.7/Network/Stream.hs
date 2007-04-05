-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Stream
-- Copyright   :  (c) Warrick Gray 2002, Bjorn Bringert 2003-2004, Simon Foster 2004
-- License     :  BSD
--
-- Maintainer  :  bjorn@bringert.net
-- Stability   :  experimental
-- Portability :  non-portable (not tested)
--
-- An library for creating abstract streams. Originally part of Gray's\/Bringert's
-- HTTP module.
--
-- * Changes by Simon Foster:
--      - Split module up into to sepearate Network.[Stream,TCP,HTTP] modules
--      
-----------------------------------------------------------------------------
module Network.Stream (
    -- ** Streams
    Debug,
    Stream(..),
    debugStream,
    
    -- ** Errors
    ConnError(..),
    Result,
    handleSocketError,
    bindE,
    myrecv

) where

import Control.Exception as Exception
import System.IO.Error

-- Networking
import Network (withSocketsDo)
import Network.BSD
import Network.URI
import Network.Socket

import Control.Monad (when,liftM,guard)
import System.IO

data ConnError = ErrorReset 
               | ErrorClosed
               | ErrorParse String
               | ErrorMisc String
    deriving(Show,Eq)

-- error propagating:
-- we could've used a monad, but that would lead us
-- into using the "-fglasgow-exts" compile flag.
bindE :: Either ConnError a -> (a -> Either ConnError b) -> Either ConnError b
bindE (Left e)  _ = Left e
bindE (Right v) f = f v

-- | This is the type returned by many exported network functions.
type Result a = Either ConnError   {- error  -}
                       a           {- result -}

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

myrecv :: Socket -> Int -> IO String
myrecv sock len =
    let handler e = if isEOFError e then return [] else ioError e
        in System.IO.Error.catch (recv sock len) handler

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
