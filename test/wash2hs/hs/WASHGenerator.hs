module WASHGenerator (preprocess, preprocessPIPE) where {

import List;
import IO;

import WASHData ;
import Parsec hiding (try) ;
import qualified WASHParser ;
import qualified WASHExpression ;
import qualified WASHClean ;
import WASHFlags ;

-- import Trace;

preprocess :: FLAGS -> String -> String -> String -> IO ();
preprocess flags srcName dstName globalDefs =
  bracket (openFile srcName ReadMode)
    (\ srcHandle -> hClose srcHandle)
    (\ srcHandle -> 
       bracket (openFile dstName WriteMode)
	 (\ dstHandle -> hClose dstHandle)
	 (\ dstHandle -> 
	  preprocessPIPE flags srcName srcHandle dstHandle globalDefs));


preprocessPIPE :: FLAGS -> String -> Handle -> Handle -> String -> IO ();
preprocessPIPE flags srcName srcHandle dstHandle globalDefs = do {
    input <- hGetContents srcHandle;
    let { parsing = parse WASHParser.washfile srcName input };
    case parsing of {
        Left error -> ioError $ userError $ show error;
        Right washfile ->
        hPutStrLn dstHandle (postprocess $ file flags globalDefs washfile "");
    };
};

file :: FLAGS -> String -> [CodeFrag] -> ShowS ;
file flags globalDefs fcode = 
  WASHExpression.code flags (WASHClean.cleanCodeFragList fcode) .
  showString globalDefs .
  showString "\n"
  ;

imports ::  [String] -> String ;
imports is = concat $ map (\m -> "import " ++ m ++ ";\n") is ;

postprocess :: String -> String ;
postprocess = unlines . postprocess' . lines ;

postprocess' :: [String] -> [String] ;
postprocess' [] = [] ;
postprocess' xs'@(x:xs) = 
  if "import" `isPrefixOf` x
  then "import qualified CGI" : xs'
  else x : postprocess' xs ;

}
