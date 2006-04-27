-- | This module provides the 'XmlContent' class and 'readXml' and 'writeXml'
--   functions that you will need if you generate a module of Haskell
--   datatype definitions from an XML DTD.  Use the DtdToHaskell
--   program to generate both datatypes and instances of this class,
--   then import this module to read and write values to and from XML files.

module Text.XML.HaXml.Xml2Haskell
  ( -- * Reading and writing XML data into a typed Haskell representation.
    readXml,  showXml
  , hGetXml,  hPutXml
  , fReadXml, fWriteXml
  -- * The enabling classes.
  , XmlContent(..)
  , XmlAttributes(..)
  , XmlAttrType(..)
  -- * Parsing and printing helper functions
  , choice, definite, many, fromText, toText
  , List1(..), ANYContent(..)
  , maybeToAttr, defaultToAttr
  , definiteA, defaultA, possibleA, fromAttrToStr, toAttrFrStr
  , Defaultable(..)
  , str2attr, attr2str
  -- * Re-exports
  , Element(..), Content(..)	-- from Text.Xml.HaXml.Types
  , catMaybes			-- from Maybe
  ) where

import System.IO
import Data.Maybe (catMaybes)
import Data.Char  (chr)
import Text.PrettyPrint.HughesPJ (render)
import Text.XML.HaXml.Types
import Text.XML.HaXml.Pretty (document)
import Text.XML.HaXml.Parse  (xmlParse)


-- | Read an XML document from a file and convert it to a fully-typed
--   Haskell value.
fReadXml  :: (Show a,XmlContent a) => FilePath -> IO a
fReadXml fp = do
    f <- ( if fp=="-" then return stdin
           else openFile fp ReadMode )
    x <- hGetContents f
    let (Document _ _ y) = xmlParse fp x
        result = fst (fromElem [CElem y]) :: (XmlContent a,Show a) => Maybe a
    return (maybe (error $ "XML value not found") id result)

-- | Write a fully-typed Haskell value to the given file as an XML
--   document.
fWriteXml :: XmlContent a => FilePath -> a -> IO ()
fWriteXml fp x = do
    f <- ( if fp=="-" then return stdout
           else openFile fp WriteMode )
    hPutXml f x
    hClose f

-- | Read a fully-typed XML document from a string.
readXml :: XmlContent a => String -> Maybe a
readXml s =
    let (Document _ _ y) = xmlParse "string input" s in
    fst (fromElem [CElem y])
-- | Convert a fully-typed XML document to a string.
showXml :: XmlContent a => a -> String
showXml x =
    case toElem x of
      [CElem y] ->
          (render . document
           . Document (Prolog (Just (XMLDecl "1.0" Nothing Nothing)) Nothing)
                      emptyST) y
      _ -> ""

-- | Read a fully-typed XML document from a file handle.
hGetXml :: XmlContent a => Handle -> IO a
hGetXml h = do
    x <- hGetContents h
    let (Document _ _ y) = xmlParse "file handle" x
    return (maybe (error "XML value not found") id (fst (fromElem [CElem y])))
-- | Write a fully-typed XML document to a file handle.
hPutXml :: XmlContent a => Handle -> a -> IO ()
hPutXml h x = do
    ( hPutStrLn h . render . document .
      Document (Prolog Nothing Nothing) emptyST . deCont . toElem) x
  where
    deCont [CElem x] = x
    deCont [] = error "no XML content generated"
    deCont _  = error "too much XML content generated"


---- Conversion operations on generated types ----

-- | The XmlContent class promises that an XML content element can be
--   converted to and from a Haskell value.
class XmlContent a where
    fromElem :: [Content] -> (Maybe a,[Content])
    toElem   :: a -> [Content]
-- | The XmlAttributes class promises that a list of XML tag attributes
--   can be converted to and from a Haskell value.
class XmlAttributes a where
    fromAttrs :: [Attribute] -> a
    toAttrs   :: a -> [Attribute]
-- | The XmlAttrType class promises that an attribute taking an XML enumerated
--   type can be converted to and from a Haskell value.
class XmlAttrType a where
    fromAttrToTyp :: String -> Attribute -> Maybe a
    toAttrFrTyp   :: String -> a -> Maybe Attribute



---- Useful variants of "fromElem" ----

choice :: XmlContent a
        => (a -> b)                             -- constructor
        -> ([Content]->(Maybe b,[Content]))     -- continuation
        -> [Content] -> (Maybe b,[Content])
choice cons other input =
    case fromElem input of
      (Just x, rest) -> (Just (cons x), rest)
      (Nothing,rest) -> other input


definite :: ([Content]->(Maybe a,[Content])) -> String -> String ->
             [Content] -> (a,[Content])
definite from inner tag cs =
    let (m,cs0) = from cs
    in case m of
         Nothing -> error ("content error: expected "++inner++" inside <"
                           ++tag++"> element\n")
         (Just a)-> (a,cs0)

many :: ([Content]->(Maybe a,[Content])) -> [Content] -> ([a], [Content])
many from [] = ([],[])
many from cs =
    let (m,cs0) = from cs
    in case m of
         Nothing -> ([],cs0)
         (Just a)-> let (as,cs1) = many from cs0
                    in (a:as, cs1)

fromText :: [Content] -> (Maybe String, [Content])
fromText c =
  case c of
    (CString _ s: cs)        -> more s cs
    (CRef (RefChar s): cs)   -> more ("&#"++show s++";") cs
    (CRef (RefEntity s): cs) -> more ('&':s++";") cs
    (CMisc _: cs)            -> more "" cs
    []                       -> (Just "",[])
    _                        -> (Nothing,c)
  where more s cs = case fromText cs of
                        (Nothing, _)   -> (Just s, cs)
                        (Just s', cs') -> (Just (s++s'), cs')

toText :: String -> [Content]
toText s = [CString False s]

---- Useful auxiliaries for "fromAttributes" ----

-- | If an attribute is defaultable, then it either takes the default
--   value (which is omitted from the output), or a non-default value
--   (which obviously must be printed).
data Defaultable a  = Default a    | NonDefault a    deriving (Eq,Show)

searchMaybe :: (a -> Maybe b) -> [a] -> Maybe b
searchMaybe f [] = Nothing
searchMaybe f (x:xs) =
    let fx = f x in
    case fx of
      Nothing  -> searchMaybe f xs
      (Just _) -> fx

maybeToAttr :: (String->a->Maybe Attribute) -> String -> Maybe a
               -> Maybe Attribute
maybeToAttr to n Nothing  = Nothing
maybeToAttr to n (Just v) = to n v

defaultToAttr :: (String->a->Maybe Attribute) -> String -> Defaultable a
                 -> Maybe Attribute
defaultToAttr to n (Default v)  = Nothing
defaultToAttr to n (NonDefault v) = to n v

definiteA :: (String->Attribute->Maybe a) -> String -> String
             -> [Attribute] -> a
definiteA from tag at as =
    case searchMaybe (from at) as of
      Nothing  -> error ("missing attribute "++at++" in tag <"++tag++">")
      (Just a) -> a

defaultA :: (String->Attribute->Maybe a) -> a -> String
            -> [Attribute] -> Defaultable a
defaultA from def at as =
    case searchMaybe (from at) as of
      Nothing  -> Default def
      (Just a) -> NonDefault a

possibleA :: (String->Attribute->Maybe a) -> String -> [Attribute] -> Maybe a
possibleA from at as = searchMaybe (from at) as

fromAttrToStr :: String -> Attribute -> Maybe String
fromAttrToStr n (n0,v)
        | n == n0   = Just (attr2str v)
        | otherwise = Nothing

toAttrFrStr   :: String -> String -> Maybe Attribute
toAttrFrStr n v = Just (n, str2attr v)

str2attr :: String -> AttValue
str2attr s =
    let f s = 
          let (l,r) = span (\c-> not (elem c "\"&<>'")) s
          in if null r then [Left l]
             else Left l: Right (g (head r)): f (tail r)
        g '"'  = RefEntity "quot"
        g '&'  = RefEntity "amp"
        g '<'  = RefEntity "lt"
        g '>'  = RefEntity "gt"
        g '\'' = RefEntity "apos"
    in AttValue (f s)

attr2str :: AttValue -> String		-- really needs symbol table
attr2str (AttValue xs) =
    let f (Left s) = s
        f (Right (RefChar i))        = [chr i]
        f (Right (RefEntity "quot")) = "\""
        f (Right (RefEntity "amp"))  = "&"
        f (Right (RefEntity "lt"))   = "<"
        f (Right (RefEntity "gt"))   = ">"
        f (Right (RefEntity "apos")) = "'"
        f (Right _)                  = "*"  -- Ooops, ST needed here.
    in concatMap f xs
        

---- New types ----
{-
data OneOf2 a b
data OneOf3 a b c
data OneOf4 a b c d
    ... etc are now defined (with instances) in module OneOfN.
-}

-- | A type corresponding to XML's ANY contentspec
--data ANYContent = forall a . XmlContent a => ANYContent a
data ANYContent = ANYContent  deriving (Eq,Show)


-- | The List1 type represents lists with at least one element.
--   It is required for DTD content models that use + as a modifier.
data List1 a = NonEmpty [a]  deriving (Eq, Show)


---- Needed instances ----

instance (XmlContent a, XmlContent b) => XmlContent (a,b) where
    fromElem c0 =
        case (\(a,ca)->
               (\(b,cb)->
                 (a,b,cb))
               (fromElem ca))
             (fromElem c0) of
        (Just x, Just y, cn) -> (Just (x,y), cn)
        (_,_,_) -> (Nothing,c0)
    toElem (x,y) = toElem x ++ toElem y

instance (XmlContent a, XmlContent b, XmlContent c) => XmlContent (a,b,c) where
    fromElem c0 =
        case (\(a,ca)->
               (\(b,cb)->
                 (\(c,cc)->
                   (a,b,c,cc))
                 (fromElem cb))
               (fromElem ca))
             (fromElem c0) of
        (Just x, Just y, Just z,cn) -> (Just (x,y,z), cn)
        (_,_,_,_) -> (Nothing,c0)
    toElem (x,y,z) = toElem x ++ toElem y ++ toElem z

instance (XmlContent a) => XmlContent [a] where
    fromElem c0 = (\(a,cn)-> (Just a,cn)) (many fromElem c0)
    toElem xs = concatMap toElem xs

instance (XmlContent a) => XmlContent (Maybe a) where
    fromElem c0 =
        case fromElem c0 of
        (Just x, cn) -> (Just (Just x), cn)
        (Nothing,cn) -> (Just Nothing, cn)
    toElem (Just x) = toElem x
    toElem Nothing  = []	-- this clause not actually required

instance (XmlContent a) => XmlContent (List1 a) where
    fromElem c0 =
        case many fromElem c0 of
        ([], _)  -> (Nothing, c0)
        (xs, cn) -> (Just (NonEmpty xs), cn)
    toElem (NonEmpty xs) = concatMap toElem xs

instance XmlContent ANYContent where
    fromElem c0 = (Just ANYContent, [])
    toElem ANYContent = []

{-
instance (XmlContent a, XmlContent b) => XmlContent (OneOf2 a b) where
    fromElem c0 =
        case fromElem c0 of
        (Just x, cn) -> (Just (OneOfTwo x), cn)
        (Nothing,cn) ->
            case fromElem c0 of
            (Just y, cn) -> (Just (TwoOfTwo y), cn)
            (Nothing,cn) -> (Nothing, c0)
    toElem (OneOfTwo x) = toElem x
    toElem (TwoOfTwo y) = toElem y

instance (XmlContent a, XmlContent b, XmlContent c) =>
         XmlContent (OneOf3 a b c) where
    fromElem c0 =
        case fromElem c0 of
        (Just x, cn) -> (Just (OneOfThree x), cn)
        (Nothing,cn) ->
            case fromElem c0 of
            (Just y, cn) -> (Just (TwoOfThree y), cn)
            (Nothing,cn) ->
                case fromElem c0 of
                (Just z, cn) -> (Just (ThreeOfThree z), cn)
                (Nothing,cn) -> (Nothing, c0)
    toElem (OneOfThree x)   = toElem x
    toElem (TwoOfThree y)   = toElem y
    toElem (ThreeOfThree z) = toElem z

instance (XmlContent a, XmlContent b, XmlContent c, XmlContent d) =>
         XmlContent (OneOf4 a b c d) where
    fromElem c0 =
        case fromElem c0 of
        (Just x, cn) -> (Just (OneOfFour x), cn)
        (Nothing,cn) ->
            case fromElem c0 of
            (Just y, cn) -> (Just (TwoOfFour y), cn)
            (Nothing,cn) ->
                case fromElem c0 of
                (Just z, cn) -> (Just (ThreeOfFour z), cn)
                (Nothing,cn) ->
                    case fromElem c0 of
                    (Just t, cn) -> (Just (FourOfFour t), cn)
                    (Nothing,cn) -> (Nothing, c0)
    toElem (OneOfFour x)   = toElem x
    toElem (TwoOfFour y)   = toElem y
    toElem (ThreeOfFour z) = toElem z
    toElem (FourOfFour t)  = toElem t
-}


{-
---- Example instances (e.g. generated with DtdToHaskell) ----

data X = X Y
instance XmlContent X where
    fromElem (CElem (Elem "x" as cs): rest) =
        let y = (\(a,c0)-> a) (definite fromElem "x" cs)
        in (Just (X y), rest)
    fromElem rest = (Nothing,rest)
    toElem (X y) = [CElem (Elem "x" [] (toElem y))]

data X = X [Y]
instance XmlContent X where
    fromElem (CElem (Elem "x" as cs): rest) =
        let ys = (\(a,c0)-> a) (many fromElem cs)
        in (Just (X ys), rest)
    fromElem rest = (Nothing, rest)
    toElem (X ys) = [CElem (Elem "x" [] (concatMap toElem ys))]

data X = X (Maybe Y)
instance XmlContent X where
    fromElem (CElem (Elem "x" as cs): rest) =
        let my = (\(a,c0)-> a) (fromElem cs)
        in (Just (X my), rest)
    fromElem rest = (Nothing, rest)
    toElem (X my) = [CElem (Elem "x" [] (maybe [] toElem my))]

data X = X [Y] (Maybe Z) T
instance XmlContent X where
    fromElem (CElem (Elem "x" as cs): rest) =
        let (ys,z,t) = (\(a,c0)->
                         (\(b,c1)->
                           (\(c,c2)-> (a,b,c))
                           (definite fromElem c1))
                         (fromElem c0))
                       (many fromElem cs)
        in (Just (X ys z t), rest)
    fromElem rest = (Nothing, rest)
    toElem (X ys z t) = [CElem (Elem "x" [] (concatMap toElem ys ++
                                             maybe [] toElem z ++
                                             toElem t))]

-- <!ELEMENT x ((p?,q) | (t?,u*) | (r+,s))*>
data X = X [X_]
data X_ = X_P_Q (Maybe P) Q
        | X_T_U (Maybe T) [U]
        | X_R_S [R] S
instance XmlContent X_ where
    fromElem c0 =
        case (\(a,ca)->
               (\(b,cb)->
                 (a,b,cb))
               (fromElem ca))
             (fromElem c0) of
        (Nothing,Nothing,_) ->
            case (\(a,ca)->
                   (\(b,cb)->
                     (a,b,cb))
                   (fromElem ca))
                 (many fromElem c0) of
            (Nothing,[],_) ->
                case (\(a,ca)->
                       (\(b,cb)->
                         (a,b,cb))
                       (many fromElem ca))
                     (fromElem c0) of
                ([],Nothing,rest) -> (Nothing, rest)
                (a, Just b, rest) -> (Just (X_R_S a b), rest)
            (a, b, rest) -> (Just (X_T_U a b), rest)
        (a, Just b, rest) -> (Just (X_P_Q a b), rest)
    toElem (X_P_Q mp q)  = maybe [] toElem mp ++ toElem q
    toElem (X_R_S mt us) = maybe [] toElem rs ++ concatMap toElem s
    toElem (X_R_S rs s)  = concatMap toElem rs ++ toElem s


---- Example instances with attributes ----

instance XmlAttributes Match_Attrs where
    fromAttrs as =
        Match_Attrs             -- note: defaults can be handled with fromMaybe
          { opposition = definiteA fromStrAttr "match" "opposition" as
          , date       = definiteA fromStrAttr "match" "date" as
          , location   = definiteA fromAttr)    "match" "location" as
          , ourgoals   = possibleA fromStrAttr "ourgoals" as
          , theirgoals = possibleA fromStrAttr "theirgoals" as
          , status     = definiteA fromStrAttr "match" "status" as
          }
    toAttrs v = catMaybes
          [ toAttrFrStr        "opposition"  (opposition v)
          , toAttrFrStr        "date"              (date v)
          , toAttrFrTyp        "location"      (location v)
          , maybeA toAttrFrStr "ourgoals"      (ourgoals v)
          , maybeA toAttrFrStr "theirgoals"  (theirgoals v)
          , toAttrFrTyp        "status"          (status v)
          ]
instance XmlAttrType Location where
    fromAttrToTyp n (n0,v)
        | n == n0   = translate (attr2str v)
        | otherwise = Nothing
        where translate "VarsityCentre" = Just VarsityCentre
              translate "KingsManor"    = Just KingsManor
              translate _               = Nothing
    toAttrFrTyp n VarsityCentre = Just (n, str2attr "VarsityCentre")
    toAttrFrTyp n KingsManor    = Just (n, str2attr "KingsManor")
instance XmlAttrType Status where
    fromAttrToTyp n (n0,v)
        | n == n0   = translate (attr2str v)
        | otherwise = Nothing
        where translate "planned"     = Just Planned
              translate "played"      = Just Played
              translate "rescheduled" = Just Rescheduled
              translate _             = Nothing
    toAttrFrTyp n Planned     = Just (n, str2attr "planned")
    toAttrFrTyp n Played      = Just (n, str2attr "played")
    toAttrFrTyp n Rescheduled = Just (n, str2attr "rescheduled")
 
-}
