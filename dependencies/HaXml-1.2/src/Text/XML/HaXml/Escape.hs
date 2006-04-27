{- This module contains code for escaping/unescaping text in attributes
   and elements in the HaXml Element type, replacing characters by character 
   references or vice-versa.  Two uses are envisaged for this:

   (1) stopping HaXml generating incorrect XML when a character is included
       which is also the appropriate XML terminating character, for example
       when an attribute includes a double quote.
   (2) representing XML which contains non-ASCII characters as ASCII.
   -}
module Text.XML.HaXml.Escape(
   xmlEscape,
      -- :: XmlEscaper -> Element -> Element
   xmlUnEscape,
      -- :: XmlEscaper -> Element -> Element

   XmlEscaper,
      -- Something describing a particular set of escapes.

   stdXmlEscaper,
      -- Standard boilerplate escaper, escaping everything that is 
      -- nonprintable, non-ASCII, or might conceivably cause problems by
      -- parsing XML, for example quotes, < signs, and ampersands.

   mkXmlEscaper,
      -- :: [(Char,String)] -> (Char -> Bool) -> XmlEscaper
      -- The first argument contains a list of characters, with their
      --    corresponding character reference names.
      --    For example [('\60',"lt"),('\62',"gt"),('\38',"amp"),
      --       ('\39',"apos"),('\34',"quot")] will give you the "standard"
      --       XML escapes listed in section 4.6 of the XML standard, so that
      --       "&quot;" will automatically get translated into a double
      --       quotation mark.
      --
      --       It's the caller's responsibility to see that the reference
      --       names ("lt","gt","amp","apos" and "quot" in the above example)
      --       are valid XML reference names.  A sequence of letters, digits,
      --       "." or ":" characters should be fine so long as the first one
      --       isn't a digit.
      --
      -- The second argument is a function applied to each text character.
      --    If it returns True, that means we should escape this character.

      -- Policy: on escaping, we expand all characters for which the
      -- (Char -> Bool) function returns True, either giving the corresponding
      -- character reference name if one was supplied, or else using a
      -- hexadecimal CharRef.
      --
      -- on unescaping, we translate all the references we understand
      --   (hexadecimal,decimal, and the ones in the [(Char,String)] list,
      --   and leave the others alone.

   ) where

import Data.Char
import Numeric

import Data.FiniteMap

import Text.XML.HaXml.Types

-- ------------------------------------------------------------------------
-- Data types
-- ------------------------------------------------------------------------

data XmlEscaper = XmlEscaper {
   toEscape :: FiniteMap Char String,
   fromEscape :: FiniteMap String Char,
   isEscape :: Char -> Bool
   }


-- ------------------------------------------------------------------------
-- Escaping
-- ------------------------------------------------------------------------



xmlEscape :: XmlEscaper -> Element -> Element
xmlEscape xmlEscaper element = 
   compressElement (escapeElement xmlEscaper element)

escapeElement :: XmlEscaper -> Element -> Element
escapeElement xmlEscaper (Elem name attributes content) =
   Elem name (escapeAttributes xmlEscaper attributes) 
      (escapeContent xmlEscaper content)

escapeAttributes :: XmlEscaper -> [Attribute] -> [Attribute]
escapeAttributes xmlEscaper atts =
   map
      (\ (name,av) -> (name,escapeAttValue xmlEscaper av))
      atts
   
escapeAttValue :: XmlEscaper -> AttValue -> AttValue
escapeAttValue xmlEscaper (AttValue attValList) =
   AttValue (
      concat ( 
         map
            (\ av -> case av of
               Right ref -> [av]
               Left s ->
                  map
                     (\ c -> if isEscape xmlEscaper c 
                        then
                           Right (mkEscape xmlEscaper c)
                        else
                           Left [c]
                        )
                     s 
               )
            attValList
         )
      )

escapeContent :: XmlEscaper -> [Content] -> [Content]
escapeContent xmlEscaper contents =
   concat
      (map
          (\ content -> case content of
             (CString b str) ->
                map
                   (\ c -> if isEscape xmlEscaper c
                      then
                         CRef (mkEscape xmlEscaper c)
                      else
                         CString b [c]
                      )
                   str
             (CElem elem) -> [CElem (escapeElement xmlEscaper elem)]
             _ -> [content]
             )
          contents
          )

mkEscape :: XmlEscaper -> Char -> Reference
mkEscape (XmlEscaper {toEscape = toEscape}) ch =
   case lookupFM toEscape ch of
      Nothing  -> RefChar (ord ch)
      Just str -> RefEntity str
   where
      showHex = showIntAtBase 16 intToDigit 
      -- It should be, but in GHC it isn't.

-- ------------------------------------------------------------------------
-- Unescaping
-- ------------------------------------------------------------------------

xmlUnEscape :: XmlEscaper -> Element -> Element
xmlUnEscape xmlEscaper element =
   compressElement (unEscapeElement xmlEscaper element)

unEscapeElement :: XmlEscaper -> Element -> Element
unEscapeElement xmlEscaper (Elem name attributes content) =
   Elem name (unEscapeAttributes xmlEscaper attributes)
      (unEscapeContent xmlEscaper content)

unEscapeAttributes :: XmlEscaper -> [Attribute] -> [Attribute]
unEscapeAttributes xmlEscaper atts =
   map
      (\ (name,av) -> (name,unEscapeAttValue xmlEscaper av))
      atts

unEscapeAttValue :: XmlEscaper -> AttValue -> AttValue
unEscapeAttValue xmlEscaper (AttValue attValList) =
   AttValue (
      map
         (\ av -> case av of
            Left s -> av
            Right ref -> case unEscapeChar xmlEscaper ref of
               Just c -> Left [c]
               Nothing -> av
            )
         attValList
      )

unEscapeContent :: XmlEscaper -> [Content] -> [Content]
unEscapeContent xmlEscaper content =
   map
      (\ content -> case content of
         CRef ref -> case unEscapeChar xmlEscaper ref of
            Just c -> CString True [c]
            Nothing -> content
         CElem elem -> CElem (unEscapeElement xmlEscaper elem)
         _ -> content
         )
      content

unEscapeChar :: XmlEscaper -> Reference -> Maybe Char
unEscapeChar xmlEscaper ref =
   case ref of
      RefChar i      -> Just (chr i)
      RefEntity name -> lookupFM (fromEscape xmlEscaper) name

-- ------------------------------------------------------------------------
-- After escaping and unescaping we rebuild the lists, compressing
-- adjacent identical character data.
-- ------------------------------------------------------------------------

compressElement :: Element -> Element
compressElement (Elem name attributes content) =
   Elem name (compressAttributes attributes) (compressContent content)

compressAttributes :: [(Name,AttValue)] -> [(Name,AttValue)]
compressAttributes atts =
   map
      (\ (name,av) -> (name,compressAttValue av))
      atts

compressAttValue :: AttValue -> AttValue
compressAttValue (AttValue l) = AttValue (compress l)
   where
      compress :: [Either String Reference] -> [Either String Reference]
      compress [] = []
      compress (Right ref : es) = Right ref : (compress es)
      compress ( (ls @ (Left s1)) : es) =
         case compress es of
            (Left s2 : es2) -> Left (s1 ++ s2) : es2
            es2 -> ls : es2

compressContent :: [Content] -> [Content]
compressContent [] = []
compressContent ((csb @ (CString b1 s1)) : cs) =
   case compressContent cs of
      (CString b2 s2) : cs2
          | b1 == b2
          -> CString b1 (s1 ++ s2) : cs2
      cs2 -> csb : cs2
compressContent (CElem element : cs) = 
   CElem (compressElement element) : compressContent cs
compressContent (c : cs) = c : compressContent cs


-- ------------------------------------------------------------------------
-- Making XmlEscaper values.
-- ------------------------------------------------------------------------

stdXmlEscaper :: XmlEscaper
stdXmlEscaper = mkXmlEscaper
   [('\60',"lt"),('\62',"gt"),('\38',"amp"),('\39',"apos"),('\34',"quot")]
   (\ ch -> 
      let
         i = ord ch
      in
         i < 32 || i >= 127 ||
            case ch of
               '\"' -> True
               '&' -> True
               '<' -> True
               '>' -> True
               _ -> False
      )
             
            
mkXmlEscaper :: [(Char,String)] -> (Char -> Bool) -> XmlEscaper
mkXmlEscaper escapes isEscape =
   XmlEscaper {
      toEscape = listToFM escapes,
      fromEscape = listToFM (map (\ (c,str) -> (str,c)) escapes),
      isEscape = isEscape
      }

