-----------------------------------------------------------------------------------------
{-| Module      :  Database.HSQL.MySQL
    Copyright   :  (c) Krasimir Angelov 2003
    License     :  BSD-style

    Maintainer  :  ka2_mail@yahoo.com
    Stability   :  provisional
    Portability :  portable

    The module provides interface to MySQL database
-}
-----------------------------------------------------------------------------------------

#include <config.h>

module Database.HSQL.MySQL(connect, module Database.HSQL) where

import Database.HSQL
import Database.HSQL.Types
import Data.Dynamic
import Data.Bits
import Data.Char
import Foreign
import Foreign.C
import Control.Monad(when,unless)
import Control.Exception (throwDyn, finally)
import Control.Concurrent.MVar
import System.Time
import System.IO.Unsafe
import Text.ParserCombinators.ReadP
import Text.Read

#include <HsMySQL.h>

type MYSQL = Ptr ()
type MYSQL_RES = Ptr ()
type MYSQL_FIELD = Ptr ()
type MYSQL_ROW = Ptr CString
type MYSQL_LENGTHS = Ptr CULong

#if defined(_WIN32_)
#let CALLCONV = "stdcall"
#else
#let CALLCONV = "ccall"
#endif

foreign import #{CALLCONV} "HsMySQL.h mysql_init" mysql_init :: MYSQL -> IO MYSQL
foreign import #{CALLCONV} "HsMySQL.h mysql_real_connect" mysql_real_connect :: MYSQL -> CString -> CString -> CString -> CString -> Int -> CString -> Int -> IO MYSQL
foreign import #{CALLCONV} "HsMySQL.h mysql_close" mysql_close :: MYSQL -> IO ()
foreign import #{CALLCONV} "HsMySQL.h mysql_errno" mysql_errno :: MYSQL -> IO Int
foreign import #{CALLCONV} "HsMySQL.h mysql_error" mysql_error :: MYSQL -> IO CString
foreign import #{CALLCONV} "HsMySQL.h mysql_query" mysql_query :: MYSQL -> CString -> IO Int
foreign import #{CALLCONV} "HsMySQL.h mysql_use_result" mysql_use_result :: MYSQL -> IO MYSQL_RES
foreign import #{CALLCONV} "HsMySQL.h mysql_fetch_field" mysql_fetch_field :: MYSQL_RES -> IO MYSQL_FIELD
foreign import #{CALLCONV} "HsMySQL.h mysql_free_result" mysql_free_result :: MYSQL_RES -> IO ()
foreign import #{CALLCONV} "HsMySQL.h mysql_fetch_row" mysql_fetch_row :: MYSQL_RES -> IO MYSQL_ROW
foreign import #{CALLCONV} "HsMySQL.h mysql_fetch_lengths" mysql_fetch_lengths :: MYSQL_RES -> IO MYSQL_LENGTHS
foreign import #{CALLCONV} "HsMySQL.h mysql_list_tables" mysql_list_tables :: MYSQL -> CString -> IO MYSQL_RES
foreign import #{CALLCONV} "HsMySQL.h mysql_list_fields" mysql_list_fields :: MYSQL -> CString -> CString -> IO MYSQL_RES

-----------------------------------------------------------------------------------------
-- routines for handling exceptions
-----------------------------------------------------------------------------------------

handleSqlError :: MYSQL -> IO a
handleSqlError pMYSQL = do
	errno <- mysql_errno pMYSQL
	errMsg <- mysql_error pMYSQL >>= peekCString
	throwDyn (SqlError "" errno errMsg)

-----------------------------------------------------------------------------------------
-- Connect/Disconnect
-----------------------------------------------------------------------------------------

-- | Makes a new connection to the database server.
connect :: String   -- ^ Server name
        -> String   -- ^ Database name
        -> String   -- ^ User identifier
        -> String   -- ^ Authentication string (password)
        -> IO Connection
connect server database user authentication = do
	pMYSQL <- mysql_init nullPtr
	pServer <- newCString server
	pDatabase <- newCString database
	pUser <- newCString user
	pAuthentication <- newCString authentication
	res <- mysql_real_connect pMYSQL pServer pUser pAuthentication pDatabase 0 nullPtr 0
	free pServer
	free pDatabase
	free pUser
	free pAuthentication
	when (res == nullPtr) (handleSqlError pMYSQL)
	refFalse <- newMVar False
	let connection = Connection
		{ connDisconnect = mysql_close pMYSQL
		, connExecute = execute pMYSQL
		, connQuery = query connection pMYSQL
		, connTables = tables connection pMYSQL
		, connDescribe = describe connection pMYSQL
		, connBeginTransaction = execute pMYSQL "begin"
		, connCommitTransaction = execute pMYSQL "commit"
		, connRollbackTransaction = execute pMYSQL "rollback"
		, connClosed = refFalse
		}
	return connection
	where
		execute :: MYSQL -> String -> IO ()
		execute pMYSQL query = do
			res <- withCString query (mysql_query pMYSQL)
			when (res /= 0) (handleSqlError pMYSQL)

		withStatement :: Connection -> MYSQL -> MYSQL_RES -> IO Statement
		withStatement conn pMYSQL pRes = do
			currRow  <- newMVar (nullPtr, nullPtr)
			refFalse <- newMVar False
			if (pRes == nullPtr)
			  then do
				errno <- mysql_errno pMYSQL
				when (errno /= 0) (handleSqlError pMYSQL)
				return (Statement
			              { stmtConn   = conn
			              , stmtClose  = return ()
			              , stmtFetch  = fetch pRes currRow
			              , stmtGetCol = getColValue currRow
			              , stmtFields = []
			              , stmtClosed = refFalse
			              })
			  else do
				fieldDefs <- getFieldDefs pRes
				return (Statement
			              { stmtConn   = conn
			              , stmtClose  = mysql_free_result pRes
			              , stmtFetch  = fetch pRes currRow
			              , stmtGetCol = getColValue currRow
			              , stmtFields = fieldDefs
			              , stmtClosed = refFalse
			              })
			where
				getFieldDefs pRes = do
					pField <- mysql_fetch_field pRes
					if pField == nullPtr
					  then return []
					  else do
						name <- (#peek MYSQL_FIELD, name) pField >>= peekCString
						(dataType :: Int) <-  (#peek MYSQL_FIELD, type) pField
						(columnSize :: Int)  <-  (#peek MYSQL_FIELD, length) pField
						(flags :: Int)  <-  (#peek MYSQL_FIELD, flags) pField
						(decimalDigits :: Int)  <-  (#peek MYSQL_FIELD, decimals) pField
						let sqlType = mkSqlType dataType columnSize decimalDigits
						defs <- getFieldDefs pRes
						return ((name,sqlType,(flags .&. (#const NOT_NULL_FLAG)) == 0):defs)

				mkSqlType :: Int -> Int -> Int -> SqlType
				mkSqlType (#const FIELD_TYPE_STRING)     size _	   = SqlChar size
				mkSqlType (#const FIELD_TYPE_VAR_STRING) size _    = SqlVarChar size
				mkSqlType (#const FIELD_TYPE_DECIMAL)    size prec = SqlNumeric size prec
				mkSqlType (#const FIELD_TYPE_SHORT)      _    _    = SqlSmallInt
				mkSqlType (#const FIELD_TYPE_INT24)      _    _    = SqlMedInt
				mkSqlType (#const FIELD_TYPE_LONG)       _    _    = SqlInteger
				mkSqlType (#const FIELD_TYPE_FLOAT)      _    _	   = SqlReal
				mkSqlType (#const FIELD_TYPE_DOUBLE)     _    _    = SqlDouble
				mkSqlType (#const FIELD_TYPE_TINY)       _    _    = SqlTinyInt
				mkSqlType (#const FIELD_TYPE_LONGLONG)   _    _    = SqlBigInt
				mkSqlType (#const FIELD_TYPE_DATE)       _    _    = SqlDate
				mkSqlType (#const FIELD_TYPE_TIME)       _    _    = SqlTime
				mkSqlType (#const FIELD_TYPE_TIMESTAMP)  _    _    = SqlTimeStamp
				mkSqlType (#const FIELD_TYPE_DATETIME)   _    _    = SqlDateTime
				mkSqlType (#const FIELD_TYPE_YEAR)       _    _    = SqlYear
				mkSqlType (#const FIELD_TYPE_BLOB)       _    _    = SqlBLOB
				mkSqlType (#const FIELD_TYPE_SET)        _    _    = SqlSET
				mkSqlType (#const FIELD_TYPE_ENUM)       _    _    = SqlENUM
				mkSqlType tp                             _    _    = SqlUnknown tp

		query :: Connection -> MYSQL -> String -> IO Statement
		query conn pMYSQL query = do
			res <- withCString query (mysql_query pMYSQL)
			when (res /= 0) (handleSqlError pMYSQL)
			pRes <- mysql_use_result pMYSQL
			withStatement conn pMYSQL pRes

		fetch :: MYSQL_RES -> MVar (MYSQL_ROW, MYSQL_LENGTHS) -> IO Bool
		fetch pRes currRow
			| pRes == nullPtr = return False
			| otherwise = modifyMVar currRow $ \(pRow, pLengths) -> do
				pRow <- mysql_fetch_row pRes
				pLengths <- mysql_fetch_lengths pRes
				return ((pRow, pLengths), pRow /= nullPtr)

		getColValue :: MVar (MYSQL_ROW, MYSQL_LENGTHS) -> Int -> FieldDef -> (SqlType -> CString -> Int -> IO (Maybe a)) -> IO (Maybe a)
		getColValue currRow colNumber (name,sqlType,nullable) f = do
			(row, lengths) <- readMVar currRow
			pValue <- peekElemOff row colNumber
			len <- fmap fromIntegral (peekElemOff lengths colNumber)
			if pValue == nullPtr
			  then return Nothing
			  else do
				mv <- f sqlType pValue len
				case mv of
					Just v  -> return (Just v)
					Nothing -> throwDyn (SqlBadTypeCast name sqlType)

		tables :: Connection -> MYSQL -> IO [String]
		tables conn pMYSQL = do
			pRes <- mysql_list_tables pMYSQL nullPtr
			stmt <- withStatement conn pMYSQL pRes
			-- SQLTables returns:
			-- Column name     #   Type
			-- Tables_in_xx      0   VARCHAR
			collectRows (\stmt -> do
				mb_v <- stmtGetCol stmt 0 ("Tables", SqlVarChar 0, False) fromNonNullSqlCStringLen
				return (case mb_v of { Nothing -> ""; Just a -> a })) stmt

		describe :: Connection -> MYSQL -> String -> IO [FieldDef]
		describe conn pMYSQL table = do
			pRes <- withCString table (\table -> mysql_list_fields pMYSQL table nullPtr)
			stmt <- withStatement conn pMYSQL pRes
			return (getFieldsTypes stmt)
