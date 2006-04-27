-- | Validate a document against a dtd.
module Text.XML.HaXml.Validate
  ( validate
  , partialValidate
  ) where

import Text.XML.HaXml.Types
import Text.XML.HaXml.Combinators (multi,tag,iffind,literal,none,o)
import Text.XML.HaXml.Xml2Haskell (attr2str)
import Maybe (fromMaybe,isNothing,fromJust)
import List (intersperse,nub,(\\))

#if defined(__GLASGOW_HASKELL__) || defined(__HUGS__)
-- real finite map, if it is available
import Data.FiniteMap
#else
-- otherwise, a very simple and inefficient implementation of a finite map
type FiniteMap a b = [(a,b)]
listToFM :: Eq a => [(a,b)] -> FiniteMap a b
listToFM = id
lookupFM :: Eq a => FiniteMap a b -> a -> Maybe b
lookupFM fm k = lookup k fm
#endif

-- gather appropriate information out of the DTD
data SimpleDTD = SimpleDTD
    { elements   :: FiniteMap Name ContentSpec	-- content model of elem
    , attributes :: FiniteMap (Name,Name) AttType -- type of (elem,attr)
    , required   :: FiniteMap Name [Name]	-- required attributes of elem
    , ids        :: [(Name,Name)]	-- all (element,attr) with ID type
    , idrefs     :: [(Name,Name)]	-- all (element,attr) with IDREF type
    }

simplifyDTD :: DocTypeDecl -> SimpleDTD
simplifyDTD (DTD _ _ decls) =
    SimpleDTD
      { elements   = listToFM [ (name,content)
                              | Element (ElementDecl name content) <- decls ]
      , attributes = listToFM [ ((elem,attr),typ)
                              | AttList (AttListDecl elem attdefs) <- decls
                              , AttDef attr typ _ <- attdefs ]
      , required   = listToFM [ (elem, [ attr
                                       | AttDef attr _ REQUIRED <- attdefs ])
                              | AttList (AttListDecl elem attdefs) <- decls ]
      , ids        = [ (elem,attr)
                     | Element (ElementDecl elem _) <- decls
                     , AttList (AttListDecl name attdefs) <- decls
                     , elem == name
                     , AttDef attr (TokenizedType ID) _ <- attdefs ]
      , idrefs     = []	-- not implemented
      }

-- simple auxiliary to avoid lots of if-then-else with empty else clauses.
gives :: Bool -> a -> [a]
True `gives` x = [x]
False `gives` _ = []

-- | 'validate' takes a DTD and a tagged element, and returns a list of
--   errors in the document with respect to its DTD.
--
--   If you have several documents to validate against a single DTD,
--   then you will gain efficiency by freezing-in the DTD through partial
--   application, e.g. @checkMyDTD = validate myDTD@.
validate :: DocTypeDecl -> Element -> [String]
validate dtd' elem = root dtd' elem ++ partialValidate dtd' elem
  where
    root (DTD name _ _) (Elem name' _ _) =
        (name/=name') `gives` ("Document type should be <"++name
                               ++"> but appears to be <"++name'++">.")

-- | 'partialValidate' is like validate, except that it does not check that
--   the element type matches that of the DTD's root element.
partialValidate :: DocTypeDecl -> Element -> [String]
partialValidate dtd' elem = valid elem ++ checkIDs elem
  where
    dtd = simplifyDTD dtd'

    valid (Elem name attrs contents) =
        -- is the element defined in the DTD?
        let spec = lookupFM (elements dtd) name in 
        (isNothing spec) `gives` ("Element <"++name++"> not known.")
        -- is each attribute mentioned only once?
        ++ (let dups = duplicates (map fst attrs) in
            not (null dups) `gives`
               ("Element <"++name++"> has duplicate attributes: "
                ++concat (intersperse "," dups)++"."))
        -- does each attribute belong to this element?  value is in range?
        ++ concatMap (checkAttr name) attrs
        -- are all required attributes present?
        ++ concatMap (checkRequired name attrs)
                     (fromMaybe [] (lookupFM (required dtd) name))
        -- are its children in a permissible sequence?
        ++ checkContentSpec name (fromMaybe ANY spec) contents
        -- now recursively check the element children
        ++ concatMap valid [ elem | CElem elem <- contents ]

    checkAttr elem (attr, val) =
        let typ = lookupFM (attributes dtd) (elem,attr)
            attval = attr2str val in
        if isNothing typ then ["Attribute \""++attr
                               ++"\" not known for element <"++elem++">."]
        else
          case fromJust typ of
            EnumeratedType e ->
              case e of
                Enumeration es ->
                    (not (attval `Prelude.elem` es)) `gives`
                          ("Value \""++attval++"\" of attribute \""
                           ++attr++"\" in element <"++elem
                           ++"> is not in the required enumeration range: "
                           ++unwords es)
                _ -> []
            _ -> []

    checkRequired elem attrs req =
        (not (req `Prelude.elem` map fst attrs)) `gives`
            ("Element <"++elem++"> requires the attribute \""++req
             ++"\" but it is missing.")

    checkContentSpec elem ANY _ = []
    checkContentSpec elem EMPTY [] = []
    checkContentSpec elem EMPTY (_:_) =
        ["Element <"++elem++"> is not empty but should be."]
    checkContentSpec elem (Mixed PCDATA) cs = concatMap (checkMixed elem []) cs
    checkContentSpec elem (Mixed (PCDATAplus names)) cs =
        concatMap (checkMixed elem names) cs
    checkContentSpec elem (ContentSpec cp) cs = excludeText elem cs ++
        (let (errs,rest) = checkCP elem cp (flatten cs) in
         case rest of [] -> errs
                      _  -> errs++["Element <"++elem++"> contains extra "
                                  ++"elements beyond its content spec."])

    checkMixed elem permitted (CElem (Elem name _ _))
        | not (name `Prelude.elem` permitted) =
            ["Element <"++elem++"> contains an element <"++name
             ++"> but should not."]
    checkMixed elem permitted _ = []

    flatten (CElem (Elem name _ _): cs) = name: flatten cs
    flatten (_: cs)                     = flatten cs
    flatten []                          = []

    excludeText elem (CElem _: cs) = excludeText elem cs
    excludeText elem (CMisc _: cs) = excludeText elem cs
    excludeText elem (_:  cs) =
        ["Element <"++elem++"> contains text/references but should not."]
    excludeText elem [] = []

    -- This is a little parser really.  Returns any errors, plus the remainder
    -- of the input string.
    checkCP :: Name -> CP -> [Name] -> ([String],[Name])
    checkCP elem cp@(TagName n None) [] = (cpError elem cp, [])
    checkCP elem cp@(TagName n None) (n':ns)
        | n==n'     = ([], ns)
        | otherwise = (cpError elem cp, n':ns)
    checkCP elem cp@(TagName n Query) [] = ([],[])
    checkCP elem cp@(TagName n Query) (n':ns)
        | n==n'     = ([], ns)
        | otherwise = ([], n':ns)
    checkCP elem cp@(TagName n Star) [] = ([],[])
    checkCP elem cp@(TagName n Star) (n':ns)
        | n==n'     = checkCP elem (TagName n Star) ns
        | otherwise = ([], n':ns)
    checkCP elem cp@(TagName n Plus) [] = (cpError elem cp, [])
    checkCP elem cp@(TagName n Plus) (n':ns)
        | n==n'     = checkCP elem (TagName n Star) ns
        | otherwise = (cpError elem cp, n':ns)
    checkCP elem cp@(Choice cps None) [] = (cpError elem cp, [])
    checkCP elem cp@(Choice cps None) ns =
        let next = choice elem ns cps in
        if null next then (cpError elem cp, ns)
        else ([], head next)	-- choose the first alternative with no errors
    checkCP elem cp@(Choice cps Query) [] = ([],[])
    checkCP elem cp@(Choice cps Query) ns =
        let next = choice elem ns cps in
        if null next then ([],ns)
        else ([], head next)
    checkCP elem cp@(Choice cps Star) [] = ([],[])
    checkCP elem cp@(Choice cps Star) ns =
        let next = choice elem ns cps in
        if null next then ([],ns)
        else checkCP elem (Choice cps Star) (head next)
    checkCP elem cp@(Choice cps Plus) [] = (cpError elem cp, [])
    checkCP elem cp@(Choice cps Plus) ns =
        let next = choice elem ns cps in
        if null next then (cpError elem cp, ns)
        else checkCP elem (Choice cps Star) (head next)
    checkCP elem cp@(Seq cps None) [] = (cpError elem cp, [])
    checkCP elem cp@(Seq cps None) ns =
        let (errs,next) = sequence elem ns cps in
        if null errs then ([],next)
        else (cpError elem cp++errs, ns)
    checkCP elem cp@(Seq cps Query) [] = ([],[])
    checkCP elem cp@(Seq cps Query) ns =
        let (errs,next) = sequence elem ns cps in
        if null errs then ([],next)
        else ([], ns)
    checkCP elem cp@(Seq cps Star) [] = ([],[])
    checkCP elem cp@(Seq cps Star) ns =
        let (errs,next) = sequence elem ns cps in
        if null errs then checkCP elem (Seq cps Star) next
        else ([], ns)
    checkCP elem cp@(Seq cps Plus) [] = (cpError elem cp, [])
    checkCP elem cp@(Seq cps Plus) ns =
        let (errs,next) = sequence elem ns cps in
        if null errs then checkCP elem (Seq cps Star) next
        else (cpError elem cp++errs, ns)

    choice elem ns cps =  -- return only those parses that don't give any errors
        [ rem | ([],rem) <- map (\cp-> checkCP elem cp ns) cps ]
    sequence elem ns cps =  -- accumulate errors down the sequence
        foldl (\(es,ns) cp-> let (es',ns') = checkCP elem cp ns
                             in (es++es', ns'))
              ([],ns) cps

    checkIDs elem =
        let celem = CElem elem
            showAttr a = iffind a literal none
            idElems = concatMap (\(name,at)-> multi (showAttr at `o` tag name)
                                                    celem)
                                (ids dtd)
            badIds  = duplicates (map (\(CString _ s)->s) idElems)
        in not (null badIds) `gives`
               ("These attribute values of type ID are not unique: "
                ++concat (intersperse "," badIds)++".")


cpError :: Name -> CP -> [String]
cpError elem cp =
    ["Element <"++elem++"> should contain "++display cp++" but does not."]


display :: CP -> String
display (TagName name mod) = name ++ modifier mod
display (Choice cps mod)   = "(" ++ concat (intersperse "|" (map display cps))
                             ++ ")" ++ modifier mod
display (Seq cps mod)      = "(" ++ concat (intersperse "," (map display cps))
                             ++ ")" ++ modifier mod

modifier :: Modifier -> String
modifier None  = ""
modifier Query = "?"
modifier Star  = "*"
modifier Plus  = "+"

duplicates :: Eq a => [a] -> [a]
duplicates xs = xs \\ (nub xs)
