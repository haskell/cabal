-----------------------------------------------------------------------------------------
{-| Module      :  Database.HSQL.PostgreSQL
    Copyright   :  (c) Krasimir Angelov 2003
    License     :  BSD-style

    Maintainer  :  ka2_mail@yahoo.com
    Stability   :  provisional
    Portability :  portable

    The module provides interface to PostgreSQL database
-}
-----------------------------------------------------------------------------------------

module Database.HSQL.PostgreSQL(connect, module Database.HSQL) where

import Database.HSQL
import Database.HSQL.Types
import Data.Dynamic
import Data.Char
import Foreign
import Foreign.C
import Control.Exception (throwDyn, catchDyn, dynExceptions, Exception(..))
import Control.Monad(when,unless,mplus)
import Control.Concurrent.MVar
import System.Time
import System.IO.Unsafe
import Text.ParserCombinators.ReadP
import Text.Read
import Numeric

# include <time.h>
#include <libpq-fe.h>
#include <postgres.h>
#include <catalog/pg_type.h>

type PGconn = Ptr ()
type PGresult = Ptr ()
type ConnStatusType = #type ConnStatusType
type ExecStatusType = #type ExecStatusType
type Oid = #type Oid

foreign import ccall "libpq-fe.h PQsetdbLogin" pqSetdbLogin :: CString -> CString -> CString -> CString -> CString -> CString -> CString -> IO PGconn
foreign import ccall "libpq-fe.h PQstatus" pqStatus :: PGconn -> IO ConnStatusType
foreign import ccall "libpq-fe.h PQerrorMessage"  pqErrorMessage :: PGconn -> IO CString
foreign import ccall "libpq-fe.h PQfinish" pqFinish :: PGconn -> IO ()
foreign import ccall "libpq-fe.h PQexec" pqExec :: PGconn -> CString -> IO PGresult
foreign import ccall "libpq-fe.h PQresultStatus" pqResultStatus :: PGresult -> IO ExecStatusType
foreign import ccall "libpq-fe.h PQresStatus" pqResStatus :: ExecStatusType -> IO CString
foreign import ccall "libpq-fe.h PQresultErrorMessage" pqResultErrorMessage :: PGresult -> IO CString
foreign import ccall "libpq-fe.h PQnfields" pgNFields :: PGresult -> IO Int
foreign import ccall "libpq-fe.h PQntuples" pqNTuples :: PGresult -> IO Int
foreign import ccall "libpq-fe.h PQfname" pgFName :: PGresult -> Int -> IO CString
foreign import ccall "libpq-fe.h PQftype" pqFType :: PGresult -> Int -> IO Oid
foreign import ccall "libpq-fe.h PQfmod" pqFMod :: PGresult -> Int -> IO Int
foreign import ccall "libpq-fe.h PQfnumber" pqFNumber :: PGresult -> CString -> IO Int
foreign import ccall "libpq-fe.h PQgetvalue" pqGetvalue :: PGresult -> Int -> Int -> IO CString
foreign import ccall "libpq-fe.h PQgetisnull" pqGetisnull :: PGresult -> Int -> Int -> IO Int

foreign import ccall "strlen" strlen :: CString -> IO Int

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
	pServer <- newCString server
	pDatabase <- newCString database
	pUser <- newCString user
	pAuthentication <- newCString authentication
	pConn <- pqSetdbLogin pServer nullPtr nullPtr nullPtr pDatabase pUser pAuthentication
	free pServer
	free pUser
	free pAuthentication
	status <- pqStatus pConn
	unless (status == (#const CONNECTION_OK))  (do
		errMsg <- pqErrorMessage pConn >>= peekCString
		pqFinish pConn
		throwDyn (SqlError {seState="C", seNativeError=fromIntegral status, seErrorMsg=errMsg}))
	refFalse <- newMVar False
	let connection = Connection
		{ connDisconnect = pqFinish pConn
		, connExecute = execute pConn
		, connQuery = query connection pConn
		, connTables = tables connection pConn
		, connDescribe = describe connection pConn
		, connBeginTransaction = execute pConn "begin"
		, connCommitTransaction = execute pConn "commit"
		, connRollbackTransaction = execute pConn "rollback"
		, connClosed = refFalse
		}
	return connection
	where
		execute :: PGconn -> String -> IO ()
		execute pConn sqlExpr = do
			pRes <- withCString sqlExpr (pqExec pConn)
			when (pRes==nullPtr) (do
				errMsg <- pqErrorMessage pConn >>= peekCString
				throwDyn (SqlError {seState="E", seNativeError=(#const PGRES_FATAL_ERROR), seErrorMsg=errMsg}))
			status <- pqResultStatus pRes
			unless (status == (#const PGRES_COMMAND_OK) || status == (#const PGRES_TUPLES_OK)) (do
				errMsg <- pqResultErrorMessage pRes >>= peekCString
				throwDyn (SqlError {seState="E", seNativeError=fromIntegral status, seErrorMsg=errMsg}))
			return ()

		query :: Connection -> PGconn -> String -> IO Statement
		query conn pConn query = do
			pRes <- withCString query (pqExec pConn)
			when (pRes==nullPtr) (do
				errMsg <- pqErrorMessage pConn >>= peekCString
				throwDyn (SqlError {seState="E", seNativeError=(#const PGRES_FATAL_ERROR), seErrorMsg=errMsg}))
			status <- pqResultStatus pRes
			unless (status == (#const PGRES_COMMAND_OK) || status == (#const PGRES_TUPLES_OK)) (do
				errMsg <- pqResultErrorMessage pRes >>= peekCString
				throwDyn (SqlError {seState="E", seNativeError=fromIntegral status, seErrorMsg=errMsg}))
			defs <- if status ==  (#const PGRES_TUPLES_OK) then pgNFields pRes >>= getFieldDefs pRes 0 else return []
			countTuples <- pqNTuples pRes;
			tupleIndex <- newMVar (-1)
			refFalse <- newMVar False
			return (Statement
			              { stmtConn   = conn
			              , stmtClose  = return ()
			              , stmtFetch  = fetch tupleIndex countTuples
			              , stmtGetCol = getColValue pRes tupleIndex countTuples
			              , stmtFields = defs
			              , stmtClosed = refFalse
			              })
			where
				getFieldDefs pRes i n
					| i >= n = return []
					| otherwise = do
						name <- pgFName pRes i	>>= peekCString
						dataType <- pqFType pRes i
						modifier <- pqFMod pRes i
						defs <- getFieldDefs pRes (i+1) n
						return ((name,mkSqlType dataType modifier,True):defs)

		mkSqlType :: Oid -> Int -> SqlType
		mkSqlType (#const BPCHAROID)    size = SqlChar (size-4)
		mkSqlType (#const VARCHAROID)   size = SqlVarChar (size-4)
		mkSqlType (#const NAMEOID)      size = SqlVarChar 31
		mkSqlType (#const TEXTOID)      size = SqlText
		mkSqlType (#const NUMERICOID)   size = SqlNumeric ((size-4) `div` 0x10000) ((size-4) `mod` 0x10000)
		mkSqlType (#const INT2OID)      size = SqlSmallInt
		mkSqlType (#const INT4OID)      size = SqlInteger
		mkSqlType (#const FLOAT4OID)    size = SqlReal
		mkSqlType (#const FLOAT8OID)    size = SqlDouble
		mkSqlType (#const BOOLOID)      size = SqlBit
		mkSqlType (#const BITOID)       size = SqlBinary size
		mkSqlType (#const VARBITOID)    size = SqlVarBinary size
		mkSqlType (#const BYTEAOID)     size = SqlTinyInt
		mkSqlType (#const INT8OID)      size = SqlBigInt
		mkSqlType (#const DATEOID)      size = SqlDate
		mkSqlType (#const TIMEOID)      size = SqlTime
		mkSqlType (#const TIMETZOID)    size = SqlTimeTZ
		mkSqlType (#const ABSTIMEOID)   size = SqlAbsTime
		mkSqlType (#const RELTIMEOID)   size = SqlRelTime
		mkSqlType (#const INTERVALOID)  size = SqlTimeInterval
		mkSqlType (#const TINTERVALOID) size = SqlAbsTimeInterval
		mkSqlType (#const TIMESTAMPOID)	size = SqlDateTime
		mkSqlType (#const TIMESTAMPTZOID)	size = SqlDateTimeTZ
		mkSqlType (#const CASHOID)      size = SqlMoney
		mkSqlType (#const INETOID)      size = SqlINetAddr
		mkSqlType (#const 829)          size = SqlMacAddr		-- hack
		mkSqlType (#const CIDROID)      size = SqlCIDRAddr
		mkSqlType (#const POINTOID)     size = SqlPoint
		mkSqlType (#const LSEGOID)      size = SqlLSeg
		mkSqlType (#const PATHOID)      size = SqlPath
		mkSqlType (#const BOXOID)       size = SqlBox
		mkSqlType (#const POLYGONOID)   size = SqlPolygon
		mkSqlType (#const LINEOID)      size = SqlLine
		mkSqlType (#const CIRCLEOID)    size = SqlCircle
		mkSqlType tp   size = SqlUnknown (fromIntegral tp)

		getFieldValue stmt colNumber fieldDef v = do
			mb_v <- stmtGetCol stmt colNumber fieldDef fromNonNullSqlCStringLen
			return (case mb_v of { Nothing -> v; Just a -> a })

		tables :: Connection -> PGconn -> IO [String]
		tables connection pConn = do
			stmt <- query connection pConn "select relname from pg_class where relkind='r' and relname !~ '^pg_'"
			collectRows (\s -> getFieldValue  s 0 ("relname", SqlVarChar 0, False) "") stmt

		describe :: Connection -> PGconn -> String -> IO [FieldDef]
		describe connection pConn table = do
			stmt <- query connection pConn
					("select attname, atttypid, atttypmod, attnotnull " ++
					 "from pg_attribute as cols join pg_class as ts on cols.attrelid=ts.oid " ++
					 "where cols.attnum > 0 and ts.relname='"++table++"'")
			collectRows getColumnInfo stmt
			where
				getColumnInfo stmt = do
					column_name <- getFieldValue stmt 0 ("attname", SqlVarChar 0, False) ""
					(data_type::Int) <- getFieldValue stmt 1 ("atttypid", SqlInteger, False) 0
					(type_mod::Int) <- getFieldValue stmt 2 ("atttypmod", SqlInteger, False) 0
					(notnull::Bool) <- getFieldValue stmt 3 ("attnotnull", SqlBit, False) False
					let sqlType = mkSqlType (fromIntegral data_type) (fromIntegral type_mod)
					return (column_name, sqlType, not notnull)

		fetch :: MVar Int -> Int -> IO Bool
		fetch tupleIndex countTuples =
			modifyMVar tupleIndex (\index -> return (index+1,index < countTuples-1))

		getColValue :: PGresult -> MVar Int -> Int -> Int -> FieldDef -> (SqlType -> CString -> Int -> IO (Maybe a)) -> IO (Maybe a)
		getColValue pRes tupleIndex countTuples colNumber  (name,sqlType,nullable) f = do
			index <- readMVar tupleIndex
			when (index >= countTuples) (throwDyn SqlNoData)
			isnull <- pqGetisnull pRes index colNumber
			if isnull == 1
				then return Nothing
				else do
					pStr <- pqGetvalue pRes index colNumber
					strLen <- strlen pStr
					mb_value <- f sqlType pStr strLen
					case mb_value of
						Just v   -> return (Just v)
						Nothing -> throwDyn (SqlBadTypeCast name sqlType)
