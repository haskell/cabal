-----------------------------------------------------------------------------------------
{-| Module      :  Database.HSQL.ODBC
    Copyright   :  (c) Krasimir Angelov 2003
    License     :  BSD-style

    Maintainer  :  ka2_mail@yahoo.com
    Stability   :  provisional
    Portability :  portable

    The module provides interface to ODBC
-}
-----------------------------------------------------------------------------------------

module Database.HSQL.ODBC(connect, driverConnect, module Database.HSQL) where

import Database.HSQL
import Database.HSQL.Types
import Data.Word(Word32, Word16)
import Data.Int(Int32, Int16)
import Data.Maybe
import Foreign
import Foreign.C
import Control.Monad(unless)
import Control.Exception(throwDyn)
import Control.Concurrent.MVar
import System.IO.Unsafe
import System.Time

#include <time.h>
#include <HsODBC.h>

type SQLHANDLE = Ptr ()
type HENV = SQLHANDLE
type HDBC = SQLHANDLE
type HSTMT = SQLHANDLE
type HENVRef = ForeignPtr ()

type SQLSMALLINT  = #type SQLSMALLINT
type SQLUSMALLINT = #type SQLUSMALLINT
type SQLINTEGER   = #type SQLINTEGER
type SQLUINTEGER  = #type SQLUINTEGER
type SQLRETURN	  = SQLSMALLINT
type SQLLEN       = SQLINTEGER
type SQLULEN      = SQLINTEGER

#if defined(_WIN32_)
#let CALLCONV = "stdcall"
#else
#let CALLCONV = "ccall"
#endif

foreign import #{CALLCONV} "HsODBC.h SQLAllocEnv" sqlAllocEnv :: Ptr HENV -> IO SQLRETURN
#if defined(_WIN32_)
foreign import ccall "HsODBC.h &my_sqlFreeEnv" sqlFreeEnv_p :: FunPtr (HENV -> IO ())
#else
foreign import ccall "HsODBC.h &SQLFreeEnv" sqlFreeEnv_p :: FunPtr (HENV -> IO ())
#endif
foreign import #{CALLCONV} "HsODBC.h SQLAllocConnect" sqlAllocConnect :: HENV -> Ptr HDBC -> IO SQLRETURN
foreign import #{CALLCONV} "HsODBC.h SQLFreeConnect" sqlFreeConnect:: HDBC -> IO SQLRETURN
foreign import #{CALLCONV} "HsODBC.h SQLConnect" sqlConnect :: HDBC -> CString -> Int -> CString -> Int -> CString -> Int -> IO SQLRETURN
foreign import #{CALLCONV} "HsODBC.h SQLDriverConnect" sqlDriverConnect :: HDBC -> Ptr () -> CString -> SQLSMALLINT -> CString -> SQLSMALLINT -> Ptr SQLSMALLINT -> SQLUSMALLINT -> IO SQLRETURN
foreign import #{CALLCONV} "HsODBC.h SQLDisconnect" sqlDisconnect :: HDBC -> IO SQLRETURN
foreign import #{CALLCONV} "HsODBC.h SQLAllocStmt" sqlAllocStmt :: HDBC -> Ptr HSTMT -> IO SQLRETURN
foreign import #{CALLCONV} "HsODBC.h SQLFreeStmt" sqlFreeStmt :: HSTMT -> SQLUSMALLINT -> IO SQLRETURN
foreign import #{CALLCONV} "HsODBC.h SQLNumResultCols" sqlNumResultCols :: HSTMT -> Ptr SQLUSMALLINT -> IO SQLRETURN
foreign import #{CALLCONV} "HsODBC.h SQLDescribeCol" sqlDescribeCol :: HSTMT -> SQLUSMALLINT -> CString -> SQLSMALLINT -> Ptr SQLSMALLINT -> Ptr SQLSMALLINT -> Ptr SQLULEN -> Ptr SQLSMALLINT -> Ptr SQLSMALLINT -> IO SQLRETURN
foreign import #{CALLCONV} "HsODBC.h SQLBindCol" sqlBindCol :: HSTMT -> SQLUSMALLINT -> SQLSMALLINT -> Ptr a -> SQLLEN -> Ptr SQLINTEGER -> IO SQLRETURN
foreign import #{CALLCONV} "HsODBC.h SQLFetch" sqlFetch :: HSTMT -> IO SQLRETURN
foreign import #{CALLCONV} "HsODBC.h SQLGetDiagRec" sqlGetDiagRec :: SQLSMALLINT -> SQLHANDLE -> SQLSMALLINT -> CString -> Ptr SQLINTEGER -> CString -> SQLSMALLINT -> Ptr SQLSMALLINT -> IO SQLRETURN
foreign import #{CALLCONV} "HsODBC.h SQLExecDirect" sqlExecDirect :: HSTMT -> CString -> Int -> IO SQLRETURN
foreign import #{CALLCONV} "HsODBC.h SQLSetConnectOption" sqlSetConnectOption :: HDBC -> SQLUSMALLINT -> SQLULEN -> IO SQLRETURN
foreign import #{CALLCONV} "HsODBC.h SQLTransact" sqlTransact :: HENV -> HDBC -> SQLUSMALLINT -> IO SQLRETURN
foreign import #{CALLCONV} "HsODBC.h SQLGetData" sqlGetData :: HSTMT -> SQLUSMALLINT -> SQLSMALLINT -> Ptr () -> SQLINTEGER -> Ptr SQLINTEGER -> IO SQLRETURN
foreign import #{CALLCONV} "HsODBC.h SQLTables" sqlTables :: HSTMT -> CString -> SQLSMALLINT -> CString -> SQLSMALLINT -> CString -> SQLSMALLINT -> CString -> SQLSMALLINT -> IO SQLRETURN
foreign import #{CALLCONV} "HsODBC.h SQLColumns" sqlColumns :: HSTMT -> CString -> SQLSMALLINT -> CString -> SQLSMALLINT -> CString -> SQLSMALLINT -> CString -> SQLSMALLINT -> IO SQLRETURN

-----------------------------------------------------------------------------------------
-- routines for handling exceptions
-----------------------------------------------------------------------------------------

sqlSuccess :: SQLRETURN -> Bool
sqlSuccess res =
	(res == (#const SQL_SUCCESS)) || (res == (#const SQL_SUCCESS_WITH_INFO)) || (res == (#const SQL_NO_DATA))

handleSqlResult :: SQLSMALLINT -> SQLHANDLE -> SQLRETURN -> IO ()
handleSqlResult handleType handle res
	| sqlSuccess res = return ()
	| res == (#const SQL_INVALID_HANDLE) = throwDyn SqlInvalidHandle
	| res == (#const SQL_STILL_EXECUTING) = throwDyn SqlStillExecuting
	| res == (#const SQL_NEED_DATA) = throwDyn SqlNeedData
	| res == (#const SQL_ERROR) = 
	        allocaBytes 256 $ \pState ->
	        alloca $ \pNative ->
	        allocaBytes 256 $ \pMsg ->
	        alloca $ \pTextLen ->
		do
		res <- sqlGetDiagRec handleType handle 1 pState pNative pMsg 256 pTextLen
		e <- if res == (#const SQL_NO_DATA)
		       then return SqlNoData
		       else do
		          state  <- peekCString pState
		          native <- peek pNative
		          msg    <- peekCString pMsg
		          return (SqlError {seState=state, seNativeError=fromIntegral native, seErrorMsg=msg})
		throwDyn e
	| otherwise = error (show res)

-----------------------------------------------------------------------------------------
-- keeper of HENV
-----------------------------------------------------------------------------------------

{-# NOINLINE myEnvironment #-}
myEnvironment :: HENVRef
myEnvironment = unsafePerformIO $ alloca $ \ (phEnv :: Ptr HENV) -> do
	res <- sqlAllocEnv phEnv
	hEnv <- peek phEnv
	handleSqlResult 0 nullPtr res
	newForeignPtr sqlFreeEnv_p hEnv

-----------------------------------------------------------------------------------------
-- Connect/Disconnect
-----------------------------------------------------------------------------------------

-- | Makes a new connection to the ODBC data source
connect :: String               -- ^ Data source name
        -> String               -- ^ User identifier
        -> String               -- ^ Authentication string (password)
        -> IO Connection        -- ^ the returned value represents the new connection
connect server user authentication = connectHelper $ \hDBC ->
        withCString server $ \pServer ->
	withCString user $ \pUser ->
	withCString authentication $ \pAuthentication ->
	sqlConnect hDBC pServer (#const SQL_NTS) pUser (#const SQL_NTS) pAuthentication (#const SQL_NTS)

-- | 'driverConnect' is an alternative to 'connect'. It supports data sources that 
-- require more connection information than the three arguments in 'connect'
-- and data sources that are not defined in the system information.
driverConnect :: String               -- ^ Connection string
              -> IO Connection        -- ^ the returned value represents the new connection
driverConnect connString = connectHelper $ \hDBC -> 
        withCString connString $ \pConnString ->
	allocaBytes 1024 $ \pOutConnString ->
	alloca $ \pLen ->
	sqlDriverConnect hDBC nullPtr pConnString (#const SQL_NTS) pOutConnString 1024 pLen (#const SQL_DRIVER_NOPROMPT)

connectHelper :: (HDBC -> IO SQLRETURN) -> IO Connection
connectHelper connectFunction = withForeignPtr myEnvironment $ \hEnv -> do
        hDBC <- alloca $ \ (phDBC :: Ptr HDBC) -> do
	    res <- sqlAllocConnect hEnv phDBC
	    handleSqlResult (#const SQL_HANDLE_ENV) hEnv res
	    peek phDBC
	res <- connectFunction hDBC
	handleSqlResult (#const SQL_HANDLE_DBC) hDBC res
	refFalse <- newMVar False
	let connection = (Connection
			{ connDisconnect = disconnect hDBC
			, connExecute    = execute hDBC
			, connQuery      = query connection hDBC
			, connTables     = tables connection hDBC
			, connDescribe   = describe connection hDBC
			, connBeginTransaction = beginTransaction myEnvironment hDBC
			, connCommitTransaction = commitTransaction myEnvironment hDBC
			, connRollbackTransaction = rollbackTransaction myEnvironment hDBC
			, connClosed     = refFalse
			})
	return connection
	where
		disconnect :: HDBC -> IO ()
		disconnect hDBC = do
			sqlDisconnect hDBC >>= handleSqlResult (#const SQL_HANDLE_DBC) hDBC
			sqlFreeConnect hDBC >>= handleSqlResult (#const SQL_HANDLE_DBC) hDBC

		execute :: HDBC -> String -> IO ()
		execute hDBC query = allocaBytes (#const sizeof(HSTMT)) $
                  \pStmt -> do
			res <- sqlAllocStmt hDBC pStmt
			handleSqlResult (#const SQL_HANDLE_DBC) hDBC res
			hSTMT <- peek pStmt
			withCStringLen query $ \(pQuery,len) -> do
			     res <- sqlExecDirect hSTMT pQuery len
                             handleSqlResult (#const SQL_HANDLE_STMT) hSTMT res
			res <- sqlFreeStmt hSTMT (#const SQL_DROP)
			handleSqlResult (#const SQL_HANDLE_STMT) hSTMT res

		stmtBufferSize = 256

		withStatement :: Connection -> HDBC -> (HSTMT -> IO SQLRETURN) -> IO Statement
		withStatement connection hDBC f = 
		        allocaBytes (#const sizeof(FIELD)) $ \pFIELD -> do
			res <- sqlAllocStmt hDBC ((#ptr FIELD, hSTMT) pFIELD)
			handleSqlResult (#const SQL_HANDLE_DBC) hDBC res
			hSTMT <- (#peek FIELD, hSTMT) pFIELD
			let handleResult res = handleSqlResult (#const SQL_HANDLE_STMT) hSTMT res
			f hSTMT >>= handleResult
			sqlNumResultCols hSTMT ((#ptr FIELD, fieldsCount) pFIELD) >>= handleResult
			count <- (#peek FIELD, fieldsCount) pFIELD
			fields <- getFieldDefs hSTMT pFIELD 1 count
			buffer <- mallocBytes (fromIntegral stmtBufferSize)
			refFalse <- newMVar False
			let statement = Statement
				{ stmtConn   = connection
				, stmtClose  = closeStatement hSTMT buffer
				, stmtFetch  = fetch hSTMT
				, stmtGetCol = getColValue hSTMT buffer
				, stmtFields = fields
				, stmtClosed = refFalse
				}
			return statement
			where
				getFieldDefs :: HSTMT -> Ptr a -> SQLUSMALLINT -> SQLUSMALLINT -> IO [FieldDef]
				getFieldDefs hSTMT pFIELD n count
					| n > count  = return []
					| otherwise = do
						res <- sqlDescribeCol hSTMT n ((#ptr FIELD, fieldName) pFIELD) (#const FIELD_NAME_LENGTH) ((#ptr FIELD, NameLength) pFIELD) ((#ptr FIELD, DataType) pFIELD) ((#ptr FIELD, ColumnSize) pFIELD) ((#ptr FIELD, DecimalDigits) pFIELD) ((#ptr FIELD, Nullable) pFIELD)
						handleSqlResult (#const SQL_HANDLE_STMT) hSTMT res
						name <- peekCString ((#ptr FIELD, fieldName) pFIELD)
						dataType <- (#peek FIELD, DataType) pFIELD
						columnSize <- (#peek FIELD, ColumnSize) pFIELD
						decimalDigits <- (#peek FIELD, DecimalDigits) pFIELD
						(nullable :: SQLSMALLINT) <- (#peek FIELD, Nullable) pFIELD
						let sqlType = mkSqlType dataType columnSize decimalDigits
						fields <- getFieldDefs hSTMT pFIELD (n+1) count
						return ((name,sqlType,toBool nullable):fields)

		mkSqlType :: SQLSMALLINT -> SQLULEN -> SQLSMALLINT -> SqlType
		mkSqlType (#const SQL_CHAR)         size    _    = SqlChar (fromIntegral size)
		mkSqlType (#const SQL_VARCHAR)      size    _    = SqlVarChar (fromIntegral size)
		mkSqlType (#const SQL_LONGVARCHAR)  size    _    = SqlLongVarChar (fromIntegral size)
		mkSqlType (#const SQL_DECIMAL)      size    prec = SqlDecimal (fromIntegral size) (fromIntegral prec)
		mkSqlType (#const SQL_NUMERIC)      size    prec = SqlNumeric (fromIntegral size) (fromIntegral prec)
		mkSqlType (#const SQL_SMALLINT)     _       _    = SqlSmallInt
		mkSqlType (#const SQL_INTEGER)      _       _    = SqlInteger
		mkSqlType (#const SQL_REAL)         _       _    = SqlReal
		-- From: http://msdn.microsoft.com/library/en-us/odbc/htm/odappdpr_2.asp
		-- "Depending on the implementation, the precision of SQL_FLOAT can be either 24 or 53:
		-- if it is 24, the SQL_FLOAT data type is the same as SQL_REAL;
		-- if it is 53, the SQL_FLOAT data type is the same as SQL_DOUBLE."
		mkSqlType (#const SQL_FLOAT)        _        _    = SqlFloat
		mkSqlType (#const SQL_DOUBLE)		_    	_    = SqlDouble
		mkSqlType (#const SQL_BIT)          _       	_    = SqlBit
		mkSqlType (#const SQL_TINYINT)      _    	_    = SqlTinyInt
		mkSqlType (#const SQL_BIGINT)       _    	_    = SqlBigInt
		mkSqlType (#const SQL_BINARY)       size    _    = SqlBinary (fromIntegral size)
		mkSqlType (#const SQL_VARBINARY)    size    _    = SqlVarBinary (fromIntegral size)
		mkSqlType (#const SQL_LONGVARBINARY)size    _    = SqlLongVarBinary (fromIntegral size)
		mkSqlType (#const SQL_DATE)         _    	_    = SqlDate
		mkSqlType (#const SQL_TIME)         _    	_    = SqlTime
		mkSqlType (#const SQL_TIMESTAMP)	_    	_    = SqlDateTime
		mkSqlType (#const SQL_WCHAR)        size	_    = SqlWChar (fromIntegral size)
		mkSqlType (#const SQL_WVARCHAR)     size    _    = SqlWVarChar (fromIntegral size)
		mkSqlType (#const SQL_WLONGVARCHAR)	size    _    = SqlWLongVarChar (fromIntegral size)
		mkSqlType tp                        _       _    = SqlUnknown (fromIntegral tp)

		query :: Connection -> HDBC -> String -> IO Statement
		query connection hDBC q = withStatement connection hDBC doQuery
    			where doQuery hSTMT = withCStringLen q (uncurry (sqlExecDirect hSTMT))

		beginTransaction myEnvironment hDBC = do
			sqlSetConnectOption hDBC (#const SQL_AUTOCOMMIT) (#const SQL_AUTOCOMMIT_OFF)
			return ()

		commitTransaction myEnvironment hDBC = withForeignPtr myEnvironment $ \hEnv -> do
			sqlTransact hEnv hDBC (#const SQL_COMMIT)
			sqlSetConnectOption hDBC (#const SQL_AUTOCOMMIT) (#const SQL_AUTOCOMMIT_ON)
			return ()

		rollbackTransaction myEnvironment hDBC = withForeignPtr myEnvironment $ \hEnv -> do
			sqlTransact hEnv hDBC (#const SQL_ROLLBACK)
			sqlSetConnectOption hDBC (#const SQL_AUTOCOMMIT) (#const SQL_AUTOCOMMIT_ON)
			return ()

		tables :: Connection -> HDBC -> IO [String]
		tables connection hDBC = do
			stmt <- withStatement connection hDBC sqlTables'
			-- SQLTables returns (column names may vary):
			-- Column name     #   Type
			-- TABLE_NAME      3   VARCHAR
			collectRows (\s -> getFieldValue  s 3 ("TABLE_NAME", SqlVarChar 0, False) "") stmt
			where sqlTables' hSTMT = sqlTables hSTMT nullPtr 0 nullPtr 0 nullPtr 0 nullPtr 0

		describe :: Connection -> HDBC -> String -> IO [FieldDef]
		describe connection hDBC table = do
			stmt <- withStatement connection hDBC (sqlColumns' table)
			collectRows getColumnInfo stmt
			where
				sqlColumns' table hSTMT =
					withCStringLen table (\(pTable,len) ->
						sqlColumns hSTMT nullPtr 0 nullPtr 0 pTable (fromIntegral len) nullPtr 0)
					-- SQLColumns returns (column names may vary):
					-- Column name     #   Type
					-- COLUMN_NAME     4   Varchar not NULL
					-- DATA_TYPE       5   Smallint not NULL
					-- COLUMN_SIZE     7   Integer
					-- DECIMAL_DIGITS  9   Smallint
					-- NULLABLE       11   Smallint not NULL

				getColumnInfo stmt = do
					column_name <- getFieldValue stmt 4 ("COLUMN_NAME", SqlVarChar 0, False) ""
					(data_type::Int) <- getFieldValue stmt 5 ("DATA_TYPE", SqlSmallInt, False) 0
					(column_size::Int) <- getFieldValue stmt 7 ("COLUMN_SIZE", SqlInteger, True) 0
					(decimal_digits::Int) <- getFieldValue stmt 9 ("DECIMAL_DIGITS", SqlSmallInt, True) 0
					(nullable::Int) <- getFieldValue stmt 11 ("NULLABLE", SqlSmallInt, False) 0
					let sqlType = mkSqlType (fromIntegral data_type) (fromIntegral column_size) (fromIntegral decimal_digits)
					return (column_name, sqlType, toBool nullable)

		getFieldValue stmt colNumber fieldDef v = do
			mb_v <- stmtGetCol stmt (colNumber-1) fieldDef fromNonNullSqlCStringLen
			return (case mb_v of { Nothing -> v; Just a -> a })

		fetch :: HSTMT -> IO Bool
		fetch hSTMT = do
			res <- sqlFetch hSTMT
			handleSqlResult (#const SQL_HANDLE_STMT) hSTMT res
			return (res /= (#const SQL_NO_DATA))

		getColValue :: HSTMT -> CString -> Int -> FieldDef -> (SqlType -> CString -> Int -> IO (Maybe a)) -> IO (Maybe a)
		getColValue hSTMT buffer colNumber (name,sqlType,nullable) f = do
			(res,len_or_ind) <- getData buffer (fromIntegral stmtBufferSize)
			if len_or_ind == (#const SQL_NULL_DATA)
			  then return Nothing
			  else do
				mb_value <- (if res == (#const SQL_SUCCESS_WITH_INFO)
				                      then getLongData len_or_ind
				                      else f sqlType buffer (fromIntegral len_or_ind))
				case mb_value of
					Just value -> return (Just value)
					Nothing    -> throwDyn (SqlBadTypeCast name sqlType)
			where
				getData :: CString -> SQLINTEGER -> IO (SQLRETURN, SQLINTEGER)
				getData buffer size = alloca $ \lenP -> do
					res <- sqlGetData hSTMT (fromIntegral colNumber+1) (#const SQL_C_CHAR) (castPtr buffer) size lenP
					handleSqlResult (#const SQL_HANDLE_STMT) hSTMT res
					len_or_ind <- peek lenP
					return (res, len_or_ind)

				-- gets called only when there is more data than would
				-- fit in the normal buffer. This call to
				-- SQLGetData() will fetch the rest of the data.
				-- We create a new buffer big enough to hold the
				-- old and the new data, copy the old data into
				-- it and put the new data in buffer after the old.
				getLongData len = allocaBytes (fromIntegral newBufSize) $ \newBuf -> do
					copyBytes newBuf buffer stmtBufferSize
		        	-- The last byte of the old data with always be null,
					-- so it is overwritten with the first byte of  the new data.
					let newDataStart = newBuf `plusPtr` (stmtBufferSize - 1)
					    newDataLen = newBufSize - (fromIntegral stmtBufferSize - 1)
					(res,_) <- getData newDataStart newDataLen
					f sqlType newBuf (fromIntegral newBufSize-1)
					where
						newBufSize = len+1 -- to allow for terminating null character

		closeStatement :: HSTMT -> CString -> IO ()
		closeStatement hSTMT buffer = do
			free buffer
			sqlFreeStmt hSTMT (#const SQL_DROP) >>= handleSqlResult (#const SQL_HANDLE_STMT) hSTMT
