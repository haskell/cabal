module WASHExpression where

import Monad

import WASHFlags
import qualified WASHUtil
import WASHData
import WASHOut

code :: FLAGS -> [CodeFrag] -> ShowS
code flags [] = id
code flags (x:xs) = code' flags x . code flags xs

code' :: FLAGS -> CodeFrag -> ShowS
code' flags (HFrag h) = 
  showString h
code' flags (EFrag e) =
  runOut $ element flags e
code' flags (CFrag cnts) =
  showChar '(' .
  runOut (contents flags [] cnts) .
  showChar ')'
code' flags (AFrag attrs) =
  showChar '(' .
  WASHUtil.itemList (attribute flags) "CGI.empty" " >> " attrs .
  showChar ')'
code' flags (VFrag var) = 
  id
code' flags _ = error "Unknown type: code"

outMode :: Mode -> Out ()
outMode = outShowS . showMode

showMode :: Mode -> ShowS
showMode V = id
showMode S = showString "_T"
showMode F = showString "_S"

element :: FLAGS -> Element -> Out [String]
element flags (Element mode nm ats cnt et) =
  do outChar '('
     outString "CGI."
     outString nm
     when (generateBT flags) $ outMode mode
     outChar '('
     outShowS $ attributes flags ats
     rvs <- contents flags [] cnt
     outString "))"
     return rvs

outRVS :: [String] -> Out ()
outRVS [] = outString "()"
outRVS (x:xs) =
  do outChar '('
     outString x
     mapM_ g xs
     outChar ')'
  where g x = do { outChar ','; outString x; }

outRVSpat :: [String] -> Out ()
outRVSpat [] = outString "(_)"
outRVSpat xs = outRVS xs

contents :: FLAGS -> [String] -> [Content] -> Out [String]
contents flags inRVS cts =
  case cts of
    [] ->
      do outString "return"
	 outRVS inRVS
	 return inRVS
    ct:cts ->
      do rvs <- content flags ct
	 case rvs of
	   [] ->
             case (cts, inRVS) of
	       ([],[]) ->
	         return []
	       _ ->
		 do outString " >> "
		    contents flags inRVS cts
	   _ ->
	     case (cts, inRVS) of
	       ([],[]) ->
	         return rvs
	       _ ->
		 do outString " >>= \\ "
		    outRVSpat rvs
		    outString " -> "
		    contents flags (rvs ++ inRVS) cts

content :: FLAGS -> Content -> Out [String]
content flags (CElement elem)  = 
  element flags elem
content flags (CText txt) =
  do text flags txt
     return []
content flags (CCode (VFrag var:c)) =
  do outShowS $ (showChar '(' . code flags c . showChar ')')
     return [var]
content flags (CCode c) =
  do outShowS $ (showChar '(' . code flags c . showChar ')')
     return []
content flags (CComment cc) =
  do outShowS $ (showString "return (const () " . shows cc . showChar ')')
     return []
content flags (CReference txt) =
  do text flags txt
     return []
content flags c = 
  error $ "Unknown type: content -- " ++ (show c)

text :: FLAGS -> Text -> Out [String]
text flags txt =
  do outString "CGI.rawtext"
     when (generateBT flags) $ outMode (textMode txt)
     outChar ' '
     outs (textString txt)
     return []

attributes :: FLAGS -> [Attribute] -> ShowS
attributes flags atts = 
  f atts
    where
      f [] = id
      f (att:atts) = 
	attribute flags att .
	showString " >> " .
	f atts

attribute :: FLAGS -> Attribute -> ShowS
attribute flags (Attribute m n v) = 
  showString "(CGI.attr" .
  (if generateBT flags then (attrvalueBT m v) else id) .
  showChar ' ' .
  shows n . 
  showString " " .
  attrvalue v .
  showString ")"
attribute flags (AttrPattern pat) =
  showString "( " .
  showString pat .
  showString " )"
attribute flags a = error $ "Unknown type: attribute -- " ++ (show a)

attrvalue :: AttrValue -> ShowS
attrvalue (AText t) = 
  shows t
attrvalue (ACode c) =
  showString "( " .
  showString c .
  showString " )"
attrvalue a = error $ "Unknown type: attrvalue -- " ++ (show a)

attrvalueBT :: Mode -> AttrValue -> ShowS
attrvalueBT V _ = id
attrvalueBT m (AText _) = showMode m . showChar 'S'
attrvalueBT m (ACode _) = showMode m . showChar 'D'
attrvalueBT m a = error $ "Unknown type: attrvalueBT -- " ++ (show a)
