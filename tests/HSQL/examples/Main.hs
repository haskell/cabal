module Main where

import Control.Exception
import Database.HSQL.ODBC
import Queries

-- Change the following definitions to connect to
-- another data source
datasource = "HSQL_Example"
user_id    = ""
password   = ""

main = handleSql print $ do
	bracket (connect datasource user_id password) disconnect $ \c ->
		inTransaction c $ \c -> do
			createTables c
			insertRecords c
			retrieveRecords c
			rs <- retrieveRecords c
			
			putStrLn " Records inserted in table Test are: "
			putStrLn "*************************************"
			mapM print rs
			putStrLn "*************************************"
			putStrLn ""
			
			putStrLn " The tables in your database are:    "
			putStrLn "*************************************"
			mi <- getMetaInfo c
			mapM print mi
			putStrLn "*************************************"
			putStrLn ""
			
			dropTables c
