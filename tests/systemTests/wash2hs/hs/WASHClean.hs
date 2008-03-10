module WASHClean where

import Char

import WASHData

data CM a = CM ([String] -> a)
instance Monad CM where -- Reader monad
  return x = CM (const x)
  m >>= f  = CM (\strs ->
		 case m of
		   CM mfun -> 
		     case f (mfun strs) of
		       CM ffun ->
			 ffun strs)

class Clean n where
  clean :: n -> CM n

cleanCodeFragList :: [CodeFrag] -> [CodeFrag]
cleanCodeFragList = map g
  where g (EFrag el) = EFrag (cleanElement el)
	g (CFrag cs) = CFrag (cleanContentList cs)
	g cf         = cf

cleanElement :: Element -> Element
cleanElement e@Element{elemName = en, elemContent = ec} =
  if en == "pre"
  then e
  else let ec' = cleanContentList ec in
       e{elemContent = ec'}

cleanContentList :: [Content] -> [Content]
cleanContentList = remove . map g . combine
  where g c = case c of CElement{celem = el} -> CElement{celem = cleanElement el}
			CText{ctext = et}    -> CText{ctext = et { textString = cleanText (textString et) }}
			CCode{ccode = ec}    -> CCode{ccode = cleanCodeFragList ec}
			_ -> c
	combine (CText {ctext = t1} : CText {ctext = t2} : rest ) = 
		combine (CText {ctext = Text {textString = textString t1++ textString t2, textMode = textMode t1}} : rest)
	combine (x : xs) = x : combine xs
	combine [] = []
	remove  (CText{ctext = tt} : rest) | textString tt == " " = remove rest
	-- remove  (CText{ctext = tt} : rest@(CElement{} : _)) = CText{ctext = dropRight tt} : remove rest
	-- remove  (e@CElement{} : (CText{ctext = tt} : rest)) = e : remove (CText{ctext = dropLeft tt} : rest)
	remove  (x : rest) = x : remove rest
	remove  [] = []

cleanText "" = ""
cleanText xs@[x] | isSpace x = " "
		 | otherwise = xs
cleanText (x : ys@(y : _)) | isSpace x = if isSpace y 
					 then cleanText ys
					 else ' ' : cleanText ys
			   | otherwise = x : cleanText ys

dropRight tt = tt { textString = reverse (dropWhile isSpace (reverse (textString tt))) }
dropLeft  tt = tt { textString = dropWhile isSpace (textString tt) }
