-----------------------------------------------------------------------------------------
{-| Module      :  Database.HSQL.SQLite
    Copyright   :  (c) Krasimir Angelov 2003
    License     :  BSD-style

    Maintainer  :  ka2_mail@yahoo.com
    Stability   :  provisional
    Portability :  portable

    The module provides interface to SQLite
-}
-----------------------------------------------------------------------------------------

module Database.HSQL.SQLite(connect, module Database.HSQL) where

import Database.HSQL
import Database.HSQL.Types
import Foreign
import Foreign.C
import System.IO
import Control.Monad(when)
import Control.Exception(throwDyn)
import Control.Concurrent.MVar

#include <fcntl.h>
#include <sqlite.h>

type SQLite = Ptr ()

foreign import ccall sqlite_open :: CString -> Int -> Ptr CString -> IO SQLite
foreign import ccall sqlite_close :: SQLite -> IO ()
foreign import ccall sqlite_exec :: SQLite -> CString -> FunPtr () -> Ptr () -> Ptr CString -> IO Int
foreign import ccall sqlite_get_table ::   SQLite -> CString -> Ptr (Ptr CString) -> Ptr Int -> Ptr Int -> Ptr CString -> IO Int
foreign import ccall sqlite_free_table :: Ptr CString -> IO ()
foreign import ccall sqlite_freemem :: CString -> IO ()

foreign import ccall "strlen" strlen :: CString -> IO Int

-----------------------------------------------------------------------------------------
-- routines for handling exceptions
-----------------------------------------------------------------------------------------

handleSqlResult :: Int -> Ptr CString -> IO ()
handleSqlResult res ppMsg
	| res == (#const SQLITE_OK) = return ()
	| otherwise = do
		pMsg <- peek ppMsg
		msg <- peekCString pMsg
		sqlite_freemem pMsg
		throwDyn (SqlError "E" res msg)

-----------------------------------------------------------------------------------------
-- Connect
-----------------------------------------------------------------------------------------

connect :: FilePath -> IOMode -> IO Connection
connect fpath mode =
	alloca $ \ppMsg ->
	  withCString fpath $ \pFPath -> do
		sqlite <- sqlite_open pFPath 0 ppMsg
		when (sqlite == nullPtr) $ do
			pMsg <- peek ppMsg
			msg <- peekCString pMsg
			free pMsg
			throwDyn (SqlError
			            { seState = "C"
			            , seNativeError = 0
			            , seErrorMsg = msg
			            })
		refFalse <- newMVar False
		let connection = Connection
			{ connDisconnect = sqlite_close sqlite
			, connClosed     = refFalse
			, connExecute    = execute sqlite
			, connQuery      = query connection sqlite
			, connTables     = tables connection sqlite
			, connDescribe   = describe connection sqlite
			, connBeginTransaction = execute sqlite "BEGIN TRANSACTION"
			, connCommitTransaction = execute sqlite "COMMIT TRANSACTION"
			, connRollbackTransaction = execute sqlite "ROLLBACK TRANSACTION"
			}
		return connection
	where
		oflags1 = case mode of
	    	  ReadMode      -> (#const O_RDONLY)
	    	  WriteMode     -> (#const O_WRONLY)
	    	  ReadWriteMode -> (#const O_RDWR)
	    	  AppendMode    -> (#const O_APPEND)

		execute :: SQLite -> String -> IO ()
		execute sqlite query =
			withCString query $ \pQuery -> do
			alloca $ \ppMsg -> do
				res <- sqlite_exec sqlite pQuery nullFunPtr nullPtr ppMsg
				handleSqlResult res ppMsg

		query :: Connection -> SQLite -> String -> IO Statement
		query connection sqlite query = do
			withCString query $ \pQuery -> do
			alloca $ \ppResult -> do
			alloca $ \pnRow -> do
			alloca $ \pnColumn -> do
			alloca $ \ppMsg -> do
				res <- sqlite_get_table sqlite pQuery ppResult pnRow pnColumn ppMsg
				handleSqlResult res ppMsg
				pResult <- peek ppResult
				rows    <- peek pnRow
				columns <- peek pnColumn
				defs <- getFieldDefs pResult 0 columns
				refFalse <- newMVar False
				refIndex <- newMVar 0
				return (Statement
				              { stmtConn   = connection
				              , stmtClose  = sqlite_free_table pResult
				              , stmtFetch  = fetch refIndex rows
				              , stmtGetCol = getColValue pResult refIndex columns rows
				              , stmtFields = defs
				              , stmtClosed = refFalse
				              })
			where
				getFieldDefs :: Ptr CString -> Int -> Int -> IO [FieldDef]
				getFieldDefs pResult index count
					| index >= count = return []
					| otherwise         = do
						name <- peekElemOff pResult index >>= peekCString
						defs <- getFieldDefs pResult (index+1) count
						return ((name,SqlText,True):defs)

		tables :: Connection -> SQLite -> IO [String]
		tables connection sqlite = do
			stmt <- query connection sqlite "select tbl_name from sqlite_master"
			collectRows (\stmt -> getFieldValue stmt "tbl_name") stmt

		describe :: Connection -> SQLite -> String -> IO [FieldDef]
		describe connection sqlite table = do
			stmt <- query connection sqlite ("pragma table_info("++table++")")
			collectRows getRow stmt
			where
				getRow stmt = do
					name <- getFieldValue stmt "name"
					notnull <- getFieldValue stmt "notnull"
					return (name, SqlText, notnull=="0")

		fetch tupleIndex countTuples =
			modifyMVar tupleIndex (\index -> return (index+1,index < countTuples))

		getColValue pResult refIndex columns rows colNumber  (name,sqlType,nullable) f = do
			index <- readMVar refIndex
			when (index > rows) (throwDyn SqlNoData)
			pStr <- peekElemOff pResult (columns*index+colNumber)
			if pStr == nullPtr
			  then return Nothing
			  else do
				strLen <- strlen pStr
				mb_value <- f sqlType pStr strLen
				case mb_value of
					Just v   -> return (Just v)
					Nothing -> throwDyn (SqlBadTypeCast name sqlType)
