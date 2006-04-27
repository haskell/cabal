-- Connects to an XML-RPC server that supports introspection
-- and prints a Haskell module to standard output that contains
-- stubs for all the methods available at the server.

import Network.XmlRpc.Internals
import Network.XmlRpc.Client
import Network.XmlRpc.Introspect

import Data.List
import System.Exit (exitFailure)
import System.Environment (getArgs)
import System.IO 
import Text.PrettyPrint.HughesPJ

showHaskellType :: Type -> String
showHaskellType TInt = "Int"
showHaskellType TBool = "Bool"
showHaskellType TString = "String"
showHaskellType TDouble = "Double"
showHaskellType TDateTime = "CalendarTime"
showHaskellType TBase64 = "String"
showHaskellType TStruct = "[(String,Value)]"
showHaskellType TArray = "[Value]"
showHaskellType TUnknown = error "unknown type"

showHdr :: String -> String -> Doc
showHdr mod url = text "module" <+> text mod <+> text "where" 
                   $$ text "import Network.XmlRpc.Client" 
                   $$ text "import Network.XmlRpc.Internals (Value)"
                   $$ text "import System.Time (CalendarTime)"
                   $$ text ""
                   $$ text "server :: String" 
                   $$ text "server =" <+> doubleQuotes (text url)

showStub :: MethodInfo -> Doc
showStub (name,[(as,ret)],help) = 
    text "" $$ text "{-" <+> text help <+> text "-}"
     $$ text hsname <+> text "::" <+> hsep (intersperse (text "->") ft)
     $$ text hsname <+> text "= remote server" <+> doubleQuotes (text name)
    where 
    hsname = mkname name
    ft = map (text . showHaskellType) as 
	 ++ [text "IO" <+> text (showHaskellType ret)]
    mkname [] = []
    mkname ('.':xs) = '_':mkname xs
    mkname (x:xs) = x:mkname xs
showStub (name, _, _) = error (name ++ " is overloaded")

printStub :: String -> String -> IO ()
printStub url method = methodInfo url method >>= putStrLn . show . showStub

printModule :: String -> String -> IO ()
printModule mod url = do
		      ms <- listMethods url
		      putStrLn $ show $ showHdr mod url
		      mapM_ (printStub url) ms

parseArgs :: IO (String,String)
parseArgs = do
	    args <- getArgs
	    case args of 
		      [mod,url] -> return (mod,url)
		      _ -> do
			   hPutStrLn stderr "Usage: make-stubs module-name url"
			   exitFailure

main :: IO ()
main = do
       hSetBuffering stdout NoBuffering
       (mod,url) <- parseArgs
       printModule mod url
