module Queries where

import Database.HSQL

createTables :: Connection -> IO ()
createTables c = do
	execute c "create table Test(id integer not null, name varchar(255) not null)"

dropTables :: Connection -> IO ()
dropTables c = do
	execute c "drop table Test"
	
insertRecords :: Connection -> IO ()
insertRecords c = do
	execute c "insert into Test(id,name) values (1,'Test1')"
	execute c "insert into Test(id,name) values (2,'Test2')"
	execute c "insert into Test(id,name) values (3,'Test3')"
	execute c "insert into Test(id,name) values (4,'Test4')"

retrieveRecords :: Connection -> IO [(Int,String)]
retrieveRecords c = do
	query c "select id, name from Test" >>= collectRows getRow
	where
		getRow :: Statement -> IO (Int,String)
		getRow stmt = do
			id   <- getFieldValue stmt "id"
			name <- getFieldValue stmt "name"
			return (id,name)

getMetaInfo :: Connection -> IO [(String,[FieldDef])]
getMetaInfo c = do
	ts <- tables c
	mapM (\t -> describe c t >>= \cs -> return (t,cs)) ts
