module WASHUtil 
    ( normalize
    , outList
    , itemList
    , openFile
    ) where {

import IOExts ;
import WASHData ;

itemList :: (item -> ShowS) -> String -> String -> [item] -> ShowS ;
itemList showsItem empty glue items =
  f items 
  where { f []     = showString empty ;
	  f [item] = showsItem item ;
	  f (item:items) = showsItem item . showString glue . f items ;
	};


normalize :: [Content] -> [Content] ;
normalize xs = shortContent.dropEmpty $ xs ;

dropEmpty :: [Content] -> [Content] ;
dropEmpty [] = [] ;
dropEmpty (c:cs) = if isWhite c 
                   then dropEmpty cs 
                   else c:(dropEmpty cs);

isWhite :: Content -> Bool ;
isWhite (CText txt) = and $ map isWhite' (textString txt)
    where { isWhite' c =  c == ' '
                       || c == '\n'
                       || c == '\t'
                       || c == '\r' ;
    } ;
isWhite _ = False;

hasText :: Content -> Bool;
hasText (CText {}) = True ;
hasText (CReference {}) = True ;
hasText _ = False ;

textOf :: Content -> Text;
textOf (CText t) = t;
textOf (CReference t) = t;
textOf c = error ("textOf " ++ show c);

joinable :: Text -> Text -> Bool;
joinable t1 t2 = g (textMode t1) (textMode t2)
  where { g S F = True;
	  g S S = False;
	  g F F = True;
	  g V V = True;
	  g _ _ = False;
	};

joinText :: Text -> Text -> Text;
joinText t1 t2 = Text (textMode t1) (textString t1 ++ textString t2);

shortContent :: [Content] -> [Content] ;
shortContent [] = [] ;
shortContent (CPI pi:cs) = shortContent cs ;
shortContent (CComment c:cs) = shortContent cs ;
shortContent (c1:c2:cs) | hasText c1 && hasText c2 =
	     let { t1 = textOf c1 ; t2 = textOf c2 } in
	     if joinable t1 t2 then shortContent (CText (joinText t1 t2):cs)
			       else c1:shortContent (c2:cs);
shortContent (c:cs) = c:shortContent cs ;

outList :: [String] -> String ;
outList xs = "[" ++ (outList' $ filter (/= []) xs) ++ "]" ;

outList' ([])     = "" ;
outList' (x:y:[]) = x ++ ", " ++ y ;
outList' (x:y:xs) = x ++ ", " ++ outList' (y:xs) ;
outList' (x:[])   = x ;
outList' _        = error "ERROR: in processing showList" ;

openFile :: String -> String ;
openFile fname = unsafePerformIO (readFile fname) ;

}
