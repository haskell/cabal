-----------------------------------------------------------------------------------------
{-| Module      :  Database.HSQL.ODBC
    Copyright   :  (c) Krasimir Angelov 2003
    License     :  BSD-style

    Maintainer  :  ka2_mail@yahoo.com
    Stability   :  provisional
    Portability :  portable

    The module provides an abstract database interface
-}
-----------------------------------------------------------------------------------------

module Database.HSQL
	(
	-- * Connect\/Disconnect
	  Connection
	, disconnect        -- :: Connection -> IO ()
	
	-- * Command Execution Functions
	-- | Once a connection to a database has been successfully established, 
	-- the functions described here are used to perform SQL queries and commands.
	, execute           -- :: Connection -> String -> IO ()
	, Statement
	, query             -- :: Connection -> String -> IO Statement
	, closeStatement    -- :: Statement -> IO ()
	, fetch             -- :: Statement -> IO Bool
	
	-- * Retrieving Statement values and types
	, FieldDef, SqlType(..), SqlBind, toSqlValue
	, getFieldValueMB   -- :: SqlBind a => Statement -> String -> IO (Maybe a)
	, getFieldValue     -- :: SqlBind a => Statement -> String -> IO a
	, getFieldValue'    -- :: SqlBind a => Statement -> String -> a -> IO a
	, getFieldValueType -- :: Statement -> String -> (SqlType, Bool)
	, getFieldsTypes    -- :: Statement -> [(String, SqlType, Bool)]
	
	-- * Transactions
	, inTransaction     -- :: Connection -> (Connection -> IO a) -> IO a

	
	-- * SQL Exceptions handling
	, SqlError(..)
	, catchSql          -- :: IO a -> (SqlError -> IO a) -> IO a
	, handleSql         -- :: (SqlError -> IO a) -> IO a -> IO a
	, sqlExceptions     -- :: Exception -> Maybe SqlError
	
	-- * Utilities
	, forEachRow        -- :: (Statement -> s -> IO s) -- ^ an action
	, forEachRow'       -- :: (Statement -> IO ()) -> Statement -> IO ()
	, collectRows       -- :: (Statement -> IO a) -> Statement -> IO [a]
	
	-- * Metadata
	, tables            -- :: Connection -> IO [String]
	, describe          -- :: Connection -> String -> IO [FieldDef]

	-- * Extra types
	, Point(..), Line(..), Path(..), Box(..), Circle(..), Polygon(..)
	) where

import Prelude hiding (catch)
import Foreign
import Foreign.C
import Data.Int
import Data.Char
import Data.Dynamic
import System.Time
import System.IO.Unsafe(unsafePerformIO)
import Control.Monad(when,unless,mplus)
import Control.Exception (throwDyn, catchDyn, dynExceptions, Exception(..), finally, catch, throwIO)
import Control.Concurrent.MVar
import Text.ParserCombinators.ReadP
import Text.Read
import Text.Read.Lex
import Numeric
import Database.HSQL.Types

#include <time.h>

-----------------------------------------------------------------------------------------
-- routines for exception handling
-----------------------------------------------------------------------------------------

catchSql :: IO a -> (SqlError -> IO a) -> IO a
catchSql = catchDyn

handleSql :: (SqlError -> IO a) -> IO a -> IO a
handleSql h f = catchDyn f h

sqlExceptions :: Exception -> Maybe SqlError
sqlExceptions e = dynExceptions e >>= fromDynamic

checkHandle :: MVar Bool -> IO a -> IO a
checkHandle ref action =
	withMVar ref (\closed -> when closed (throwDyn SqlClosedHandle) >> action)

closeHandle :: MVar Bool -> IO () -> IO ()
closeHandle ref action =
	modifyMVar_ ref (\closed -> unless closed action >> return True)

-----------------------------------------------------------------------------------------
-- Operations on the connection
-----------------------------------------------------------------------------------------

-- | Closes the connection. Performing 'disconnect' on a connection that has already been 
-- closed has no effect. All other operations on a closed connection will fail.
disconnect :: Connection -> IO ()
disconnect conn = closeHandle (connClosed conn) (connDisconnect conn)
	
-- | Submits a command to the database.
execute :: Connection  -- ^ the database connection
        -> String      -- ^ the text of SQL command
        -> IO ()
execute conn query = checkHandle (connClosed conn) (connExecute conn query)

-- | Executes a query and returns a result set
query :: Connection    -- ^ the database connection
      -> String        -- ^ the text of SQL query
      -> IO Statement  -- ^ the associated statement. Must be closed with 
                       -- the 'closeStatement' function
query conn query = checkHandle (connClosed conn) (connQuery conn query)
	

-- | List all tables in the database.
tables :: Connection   -- ^ Database connection
       -> IO [String]  -- ^ The names of all tables in the database.
tables conn = checkHandle (connClosed conn) (connTables conn)

-- | List all columns in a table along with their types and @nullable@ flags
describe :: Connection    -- ^ Database connection
	 -> String        -- ^ Name of a database table
	 -> IO [FieldDef] -- ^ The list of fields in the table
describe conn table = checkHandle (connClosed conn) (connDescribe conn table)

-----------------------------------------------------------------------------------------
-- transactions
-----------------------------------------------------------------------------------------

-- | The 'inTransaction' function executes the specified action in transaction mode.
-- If the action completes successfully then the transaction will be commited.
-- If the action completes with an exception then the transaction will be rolled back
-- and the exception will be throw again.
inTransaction :: Connection
              -> (Connection -> IO a)  -- ^ an action
              -> IO a                  -- ^ the returned value is the result returned from action
inTransaction conn action = do
	checkHandle (connClosed conn) (connBeginTransaction conn)
	r <- catch (action conn) (\err -> do
			checkHandle (connClosed conn) (connRollbackTransaction conn)
			throwIO err)
	checkHandle (connClosed conn) (connCommitTransaction conn)
	return r

-----------------------------------------------------------------------------------------
-- Operations on the statements
-----------------------------------------------------------------------------------------

-- | 'fetch' fetches the next rowset of data from the result set.
-- The values from columns can be retrieved with 'getFieldValue' function.
fetch :: Statement -> IO Bool
fetch stmt = checkHandle (stmtClosed stmt) (stmtFetch stmt)

-- | 'closeStatement' stops processing associated with a specific statement, closes any open cursors
-- associated with the statement, discards pending results, and frees all resources associated with
-- the statement. Performing 'closeStatement' on a statement that has already been 
-- closed has no effect. All other operations on a closed statement will fail.
closeStatement :: Statement -> IO ()
closeStatement stmt = closeHandle (stmtClosed stmt) (stmtClose stmt)

-- | Returns the type and the @nullable@ flag for field with specified name
getFieldValueType :: Statement -> String -> (SqlType, Bool)
getFieldValueType stmt name = (sqlType, nullable)
	where
		(sqlType,nullable,colNumber) = findFieldInfo name (stmtFields stmt) 0

-- | Returns the list of fields with their types and @nullable@ flags
getFieldsTypes :: Statement -> [(String, SqlType, Bool)]
getFieldsTypes stmt = stmtFields stmt

findFieldInfo :: String -> [FieldDef] -> Int -> (SqlType,Bool,Int)
findFieldInfo name [] colNumber = throwDyn (SqlUnknownField name)
findFieldInfo name (fieldDef@(name',sqlType,nullable):fields) colNumber
	| name == name' = (sqlType,nullable,colNumber)
	| otherwise     = findFieldInfo name fields $! (colNumber+1)

-----------------------------------------------------------------------------------------
-- binding
-----------------------------------------------------------------------------------------

foreign import ccall "stdlib.h atoi" c_atoi :: CString -> IO Int
#ifdef WIN32
foreign import ccall "stdlib.h _atoi64" c_atoi64 :: CString -> IO Int64
#else
foreign import ccall "stdlib.h strtoll" c_strtoll :: CString -> Ptr CString -> Int -> IO Int64
#endif

instance SqlBind Int where
	fromNonNullSqlCStringLen sqlType cstr cstrLen = do
	    if sqlType==SqlInteger || sqlType==SqlMedInt 
	       || sqlType==SqlTinyInt || sqlType==SqlSmallInt
	       || sqlType==SqlBigInt
		then do
		    val <- c_atoi cstr
		    return (Just val)
		else
		    return Nothing

	fromSqlValue SqlInteger  s = Just (read s)
	fromSqlValue SqlMedInt s   = Just (read s)
	fromSqlValue SqlTinyInt s  = Just (read s)
	fromSqlValue SqlSmallInt s = Just (read s)
	fromSqlValue SqlBigInt s = Just (read s)
	fromSqlValue _ _           = Nothing

	toSqlValue s = show s

instance SqlBind Int64 where
	fromNonNullSqlCStringLen sqlType cstr cstrLen = do
	    if sqlType==SqlInteger || sqlType==SqlMedInt 
	       || sqlType==SqlTinyInt || sqlType==SqlSmallInt || sqlType==SqlBigInt
	      then do
#ifdef WIN32
	        val <- c_atoi64 cstr
#else
	        val <- c_strtoll cstr nullPtr 10
#endif
	        return (Just val)
	      else
	        return Nothing

	fromSqlValue SqlInteger s = Just (read s)
	fromSqlValue SqlMedInt s   = Just (read s)
	fromSqlValue SqlTinyInt s  = Just (read s)
	fromSqlValue SqlSmallInt s = Just (read s)
	fromSqlValue SqlBigInt s = Just (read s)
	fromSqlValue _ s = Nothing

	toSqlValue val = show val

instance SqlBind Integer where
	fromSqlValue SqlInteger  s = Just (read s)
	fromSqlValue SqlMedInt s   = Just (read s)
	fromSqlValue SqlTinyInt s  = Just (read s)
	fromSqlValue SqlSmallInt s = Just (read s)
	fromSqlValue SqlBigInt   s = Just (read s)
	fromSqlValue _ _           = Nothing

	toSqlValue s = show s
instance SqlBind String where
	fromSqlValue _ = Just

	toSqlValue s = '\'' : foldr mapChar "'" s
		where
			mapChar '\\'   s = '\\':'\\':s
			mapChar '\''   s = '\\':'\'':s
			mapChar '\n'   s = '\\':'n' :s
			mapChar '\r'   s = '\\':'r' :s
			mapChar '\t'   s = '\\':'t' :s
			mapChar '\NUL' s = '\\':'0' :s
			mapChar c      s = c        :s

instance SqlBind Bool where
	fromSqlValue SqlBit s = Just (s == "t")
	fromSqlValue _ _ = Nothing

	toSqlValue True  = "'t'"
	toSqlValue False = "'f'"

instance SqlBind Double where
	fromSqlValue (SqlDecimal _ _) s = Just (read s)
	fromSqlValue (SqlNumeric _ _) s = Just (read s)
	fromSqlValue SqlDouble  s = Just (read s)
	fromSqlValue SqlReal s = Just (read s)
	fromSqlValue SqlFloat s = Just (read s)
	fromSqlValue _ _ = Nothing

	toSqlValue d = show d

mkClockTime :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> ClockTime
mkClockTime year mon mday hour min sec tz =
	unsafePerformIO $ do
		allocaBytes (#const sizeof(struct tm)) $ \ p_tm -> do
			(#poke struct tm,tm_sec  ) p_tm	(fromIntegral sec  :: CInt)
			(#poke struct tm,tm_min  ) p_tm	(fromIntegral min  :: CInt)
			(#poke struct tm,tm_hour ) p_tm	(fromIntegral hour :: CInt)
			(#poke struct tm,tm_mday ) p_tm	(fromIntegral mday :: CInt)
			(#poke struct tm,tm_mon  ) p_tm	(fromIntegral (mon-1) :: CInt)
			(#poke struct tm,tm_year ) p_tm	(fromIntegral (year-1900) :: CInt)
			(#poke struct tm,tm_isdst) p_tm	(-1 :: CInt)
			t <- mktime p_tm
			return (TOD (fromIntegral t + fromIntegral (tz-currTZ)) 0)
foreign import ccall unsafe mktime :: Ptr () -> IO CTime

{-# NOINLINE currTZ #-}
currTZ :: Int
currTZ = ctTZ (unsafePerformIO (getClockTime >>= toCalendarTime))                  -- Hack

parseTZ :: ReadP Int
parseTZ =  (char '+' >> readDecP) `mplus` (char '-' >> fmap negate readDecP)

f_read :: ReadP a -> String -> Maybe a
f_read f s = case readP_to_S f s of {[(x,_)] -> Just x}

instance SqlBind ClockTime where
	fromSqlValue SqlTimeTZ s = f_read getTimeTZ s
		where
			getTimeTZ :: ReadP ClockTime
			getTimeTZ = do
				hour    <- readDecP
				char ':'
				minutes <- readDecP
				char ':'
				seconds <- readDecP
				(char '.' >> readDecP) `mplus` (return 0)
				tz <- parseTZ
				return (mkClockTime 1970 1 1 hour minutes seconds (tz*3600))

	fromSqlValue SqlTime s = f_read getTime s
		where
			getTime :: ReadP ClockTime
			getTime = do
				hour    <- readDecP
				char ':'
				minutes <- readDecP
				char ':'
				seconds <- readDecP
				return (mkClockTime 1970 1 1 hour minutes seconds currTZ)

	fromSqlValue SqlDate s = f_read getDate s
		where
			getDate :: ReadP ClockTime
			getDate = do
				year  <- readDecP
				char '-'
				month <- readDecP
				char '-'
				day   <- readDecP
				return (mkClockTime year month day 0 0 0 currTZ)

	fromSqlValue SqlDateTimeTZ s = f_read getDateTimeTZ s
		where
			getDateTimeTZ :: ReadP ClockTime
			getDateTimeTZ = do
				year  <- readDecP
				char '-'
				month <- readDecP
				char '-'
				day   <- readDecP
				skipSpaces
				hour  <- readDecP
				char ':'
				minutes <- readDecP
				char ':'
				seconds <- readDecP
				char '.' >> readDecP -- ) `mplus` (return 0)
				tz    <- parseTZ
				return (mkClockTime year month day hour minutes seconds (tz*3600))

	fromSqlValue SqlDateTime s = f_read getDateTime s
		where
			getDateTime :: ReadP ClockTime
			getDateTime = do
				year  <- readDecP
				char '-'
				month <- readDecP
				char '-'
				day   <- readDecP
				skipSpaces
				hour  <- readDecP
				char ':'
				minutes <- readDecP
				char ':'
				seconds <- readDecP
				return (mkClockTime year month day hour minutes seconds currTZ)

	fromSqlValue SqlTimeStamp s =
		let
			[year,month,day,hour,minutes,seconds] = parts [4,2,2,2,2,2] s

			parts [] xs = []
			parts (ix:ixs) xs = part ix 0 xs
				where
					part 0 n xs = n : parts ixs xs
					part k n (x:xs) = part (k-1) (n*10 + (ord x - ord '0')) xs
		in
			Just (mkClockTime year month day hour minutes seconds currTZ)

	fromSqlValue _ _ = Nothing

	toSqlValue ct = '\'' : (shows (ctYear t) .
                                score .
                                shows (ctMonth t) .
                                score .
                                shows (ctDay t) .
                                space .
                                shows (ctHour t) .
                                colon .
                                shows (ctMin t) .
                                colon .
                                shows (ctSec t)) "'"
                       where
                         t = toUTCTime ct
                         score = showChar '-'
                         space = showChar ' '
                         colon = showChar ':'

data Point = Point Double Double deriving (Eq, Show)
data Line   = Line Point Point deriving (Eq, Show)
data Path  = OpenPath [Point] | ClosedPath [Point] deriving (Eq, Show)
data Box   = Box Double Double Double Double deriving (Eq, Show)
data Circle = Circle Point Double deriving (Eq, Show)
data Polygon = Polygon [Point] deriving (Eq, Show)

instance SqlBind Point where
	fromSqlValue SqlPoint s = case read s of
		(x,y) -> Just (Point x y)
	fromSqlValue _ _ = Nothing

	toSqlValue (Point x y) = '\'' : shows (x,y) "'"

instance SqlBind Line where
	fromSqlValue SqlLSeg s = case read s of
		[(x1,y1),(x2,y2)] -> Just (Line (Point x1 y1) (Point x2 y2))
	fromSqlValue _ _ = Nothing

	toSqlValue (Line (Point x1 y1) (Point x2 y2)) = '\'' : shows [(x1,y1),(x2,y2)] "'"

instance SqlBind Path where
	fromSqlValue SqlPath ('(':s) = case read ("["++init s++"]") of   -- closed path
		ps -> Just (ClosedPath (map  (\(x,y) -> Point x y) ps))
	fromSqlValue SqlPath s = case read s of   -- closed path        -- open path
		ps -> Just (OpenPath (map  (\(x,y) -> Point x y) ps))
	fromSqlValue SqlLSeg s = case read s of
		[(x1,y1),(x2,y2)] -> Just (OpenPath [(Point x1 y1),  (Point x2 y2)])
	fromSqlValue SqlPoint s = case read s of
		(x,y) -> Just (ClosedPath [Point x y])
	fromSqlValue _ _ = Nothing

	toSqlValue (OpenPath ps) = '\'' : shows ps "'"
	toSqlValue (ClosedPath ps) = "'(" ++ init (tail (show ps)) ++ "')"

instance SqlBind Box where
	fromSqlValue SqlBox s = case read ("("++s++")") of
		((x1,y1),(x2,y2)) -> Just (Box x1 y1 x2 y2)
	fromSqlValue _ _ = Nothing

	toSqlValue (Box x1 y1 x2 y2) = '\'' : shows ((x1,y1),(x2,y2)) "'"

instance SqlBind Polygon where
	fromSqlValue SqlPolygon s = case read ("["++init (tail s)++"]") of
		ps -> Just (Polygon (map  (\(x,y) -> Point x y) ps))
	fromSqlValue _ _ = Nothing

	toSqlValue (Polygon ps) = "'(" ++ init (tail (show ps)) ++ "')"

instance SqlBind Circle where
	fromSqlValue SqlCircle s = case read ("("++init (tail s)++")") of
		((x,y),r) -> Just (Circle (Point x y) r)
	fromSqlValue _ _ = Nothing

	toSqlValue (Circle (Point x y) r) = "'<" ++ show (x,y) ++ "," ++ show r ++ "'>"

data INetAddr = INetAddr Int Int Int Int Int deriving (Eq,Show)

instance SqlBind INetAddr where
	fromSqlValue t s
		| t == SqlINetAddr || t == SqlCIDRAddr =
			case readNum s of
				(x1,s) -> case readNum s of
					(x2,s) -> case readNum s of
						(x3,s) -> case readNum s of
							(x4,s) -> case readNum s of
								(mask,_) -> Just (INetAddr x1 x2 x3 x4 mask)
		| otherwise = Nothing
		where
			readNum s = case readDec s of
				[(x,'.':s)] -> (x,s)
				[(x,'/':s)] -> (x,s)
				[(x,"")]    -> (x,"")
				_           -> (0,"")

	toSqlValue (INetAddr x1 x2 x3 x4 mask) = '\'' :
		(shows x1 .
		 dot .
		 shows x2.
		 dot .
		 shows x3 .
		 dot .
		 shows x4 .
		 slash .
		 shows mask) "'"
		where
			dot = showChar '.'
			slash = showChar '/'

data MacAddr = MacAddr Int Int Int Int Int Int deriving (Eq,Show)

instance SqlBind MacAddr where
	fromSqlValue SqlMacAddr s =
		case readHex s of
			[(x1,':':s)] -> case readHex s of
				[(x2,':':s)] -> case readHex s of
					[(x3,':':s)] -> case readHex s of
						[(x4,':':s)] -> case readHex s of
							[(x5,':':s)] -> case readHex s of
								[(x6,_)] -> Just (MacAddr x1 x2 x3 x4 x5 x6)
	fromSqlValue _ _ = Nothing

	toSqlValue (MacAddr x1 x2 x3 x4 x5 x6) = '\'' :
		(showHex x1 .
		 colon .
		 showHex x2 .
		 colon .
		 showHex x3 .
		 colon .
		 showHex x4 .
		 colon .
		 showHex x5 .
		 colon .
		 showHex x6) "'"
		where
			colon = showChar ':'
			showHex = showIntAtBase 16 intToDigit

-- | Retrieves the value of field with the specified name.
-- The returned value is Nothing if the field value is @null@.
getFieldValueMB :: SqlBind a => Statement
                             -> String         -- ^ Field name
                             -> IO (Maybe a)   -- ^ Field value or Nothing
getFieldValueMB stmt name = checkHandle (stmtClosed stmt) $
	stmtGetCol stmt colNumber (name,sqlType,nullable) fromNonNullSqlCStringLen
	where
		(sqlType,nullable,colNumber) = findFieldInfo name (stmtFields stmt) 0

-- | Retrieves the value of field with the specified name.
-- If the field value is @null@ then the function will throw 'SqlFetchNull' exception.
getFieldValue :: SqlBind a => Statement
                           -> String       -- ^ Field name
                           -> IO a         -- ^ Field value
getFieldValue stmt name = do
	mb_v <- getFieldValueMB stmt name
	case mb_v of
		Nothing -> throwDyn (SqlFetchNull name)
		Just a  -> return a

-- | Retrieves the value of field with the specified name.
-- If the field value is @null@ then the function will return the default value.
getFieldValue' :: SqlBind a => Statement
                            -> String     -- ^ Field name
                            -> a          -- ^ Default field value
                            -> IO a       -- ^ Field value
getFieldValue' stmt name def = do
	mb_v <- getFieldValueMB stmt name
	return (case mb_v of { Nothing -> def; Just a -> a })


-----------------------------------------------------------------------------------------
-- helpers
-----------------------------------------------------------------------------------------

-- | The 'forEachRow' function iterates through the result set in 'Statement' and
-- executes the given action for each row in the set. The function closes the 'Statement'
-- after the last row processing or if the given action raises an exception.
forEachRow :: (Statement -> s -> IO s) -- ^ an action
           -> Statement                -- ^ the statement
           -> s                        -- ^ initial state
           -> IO s                     -- ^ final state
forEachRow f stmt s = loop s `finally` closeStatement stmt
    where
        loop s = do
	        success <- fetch stmt
	        if success then f stmt s >>= loop else return s

-- | The 'forEachRow\'' function is analogous to 'forEachRow' but doesn't provide state.
-- The function closes the 'Statement' after the last row processing or if the given
-- action raises an exception.
forEachRow' :: (Statement -> IO ()) -> Statement -> IO ()
forEachRow' f stmt = loop `finally` closeStatement stmt
    where
        loop = do
	        success <- fetch stmt
	        when success (f stmt >> loop)

-- | The 'collectRows' function iterates through the result set in 'Statement' and
-- executes the given action for each row in the set. The values returned from action
-- are collected and returned as list. The function closes the 'Statement' after the
-- last row processing or if the given action raises an exception.
collectRows :: (Statement -> IO a) -> Statement -> IO [a]
collectRows f stmt = loop `finally` closeStatement stmt
	where
		loop = do
			success <- fetch stmt
			if success
				then do
					x <- f stmt
					xs <- loop
					return (x:xs)
				else return []
