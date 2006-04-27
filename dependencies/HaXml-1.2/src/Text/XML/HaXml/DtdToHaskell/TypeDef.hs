-- | Defines an internal representation of Haskell data\/newtype definitions
--   that correspond to the XML DTD types, and provides pretty-printers to
--   convert these types into the 'Doc' type of "Text.PrettyPrint.HughesPJ".

module Text.XML.HaXml.DtdToHaskell.TypeDef
  ( -- * Internal representation of types
    TypeDef(..)
  , Constructors
  , AttrFields
  , StructType(..)
  -- * Pretty-print a TypeDef
  , ppTypeDef
  , ppHName
  , ppXName
  , ppAName
  -- * Name mangling
  , Name(..)
  , name, name_, name_a, name_ac, name_f, mangle, manglef
  ) where

import Data.Char (isLower, isUpper, toLower, toUpper, isDigit)
import Data.List (intersperse)
import Text.PrettyPrint.HughesPJ


---- Internal representation for typedefs ----

-- | Need to keep both the XML and Haskell versions of a name.
data Name = Name { xName :: String	-- ^ original XML name
		 , hName :: String	-- ^ mangled Haskell name
                 }
          deriving Eq

data TypeDef =
      DataDef Bool Name AttrFields Constructors	-- ^ Bool for main\/aux.
    | EnumDef Name [Name]
    deriving Eq
type Constructors = [(Name,[StructType])]
type AttrFields   = [(Name, StructType)]
data StructType =
      Maybe StructType
    | Defaultable StructType String	-- ^ String holds default value.
    | List StructType
    | List1 StructType			-- ^ Non-empty lists.
    | Tuple [StructType]
    | OneOf [StructType]
    | Any				-- ^ XML's contentspec allows ANY
    | String
    | Defined Name
    deriving Eq

-- used for converting StructType (roughly) back to an XML content model
instance Show StructType where
    showsPrec p (Maybe s)         = showsPrec (p+1) s . showChar '?'
    showsPrec p (Defaultable s _) = shows s
    showsPrec p (List s)          = showsPrec (p+1) s . showChar '*'
    showsPrec p (List1 s)         = showsPrec (p+1) s . showChar '+'
    showsPrec p (Tuple ss)        = showChar '('
                                    . foldr1 (.) (intersperse (showChar ',')
                                                              (map shows ss))
                                    . showChar ')'
    showsPrec p (OneOf ss)        = showChar '('
                                    . foldr1 (.) (intersperse (showChar '|')
                                                              (map shows ss))
                                    . showChar ')'
    showsPrec p (Any)             = showString "ANY"
    showsPrec p (String)          = showString "#PCDATA"
    showsPrec p (Defined (Name n _)) = showString n


---- Pretty-printing typedefs ----
ppTypeDef :: TypeDef -> Doc

--	no attrs, no constructors
ppTypeDef (DataDef _ n [] []) =
    let name = ppHName n in
    text "data" <+> name <+> text "=" <+> name <+> text "\t\t" <> derives

--	no attrs, single constructor
ppTypeDef (DataDef _ n [] [c@(_,[_])]) =
    text "newtype" <+> ppHName n <+> text "=" <+> ppC c <+> text "\t\t" <> derives

--	no attrs, multiple constrs
ppTypeDef (DataDef _ n [] cs) =
    text "data" <+> ppHName n <+>
           ( text "=" <+> ppC (head cs) $$
             vcat (map (\c-> text "|" <+> ppC c) (tail cs)) $$
             derives )

--	nonzero attrs, no constructors
ppTypeDef (DataDef _ n fs []) =
    let name = ppHName n in
    text "data" <+> name <+> text "=" <+> name $$
    nest 4 ( text "{" <+> ppF (head fs) $$
             vcat (map (\f-> text "," <+> ppF f) (tail fs)) $$
             text "}" <+> derives )

--	nonzero attrs, one or more constrs
ppTypeDef (DataDef _ n fs cs) =
    let attr = ppAName n in
    text "data" <+> ppHName n <+>
           ( text "=" <+> ppAC attr (head cs) $$
             vcat (map (\c-> text "|" <+> ppAC attr c) (tail cs)) $$
             derives )  $$
    text "data" <+> attr <+> text "=" <+> attr $$
    nest 4 ( text "{" <+> ppF (head fs) $$
             vcat (map (\f-> text "," <+> ppF f) (tail fs)) $$
             text "}" <+> derives )

--	enumerations (of attribute values)
ppTypeDef (EnumDef n es) =
    text "data" <+> ppHName n <+>
    ( text "=" <+>
      fsep (intersperse (text " | ") (map ppHName es))
    $$ derives )


ppST :: StructType -> Doc
ppST (Defaultable st _)  = parens (text "Defaultable" <+> ppST st)
ppST (Maybe st)  = parens (text "Maybe" <+> ppST st)
ppST (List st)   = text "[" <> ppST st <> text "]"
ppST (List1 st)  = parens (text "List1" <+> ppST st)
ppST (Tuple sts) = parens (commaList (map ppST sts))
ppST (OneOf sts) = parens (text "OneOf" <> text (show (length sts)) <+>
                           hsep (map ppST sts))
ppST  String     = text "String"
ppST  Any        = text "ANYContent"
ppST (Defined n) = ppHName n

-- constructor and components
ppC :: (Name,[StructType]) -> Doc
ppC (n,sts) = ppHName n <+> fsep (map ppST sts)

-- attribute (fieldname and type)
ppF :: (Name,StructType) -> Doc
ppF (n,st) = ppHName n <+> text "::" <+> ppST st

-- constructor and components with initial attr-type
ppAC :: Doc -> (Name,[StructType]) -> Doc
ppAC atype (n,sts) = ppHName n <+> fsep (atype: map ppST sts)

-- | Pretty print Haskell name.
ppHName :: Name -> Doc
ppHName (Name _ s) = text s
-- | Pretty print XML name.
ppXName :: Name -> Doc
ppXName (Name s _) = text s
-- | Pretty print Haskell attributes name.
ppAName :: Name -> Doc
ppAName (Name _ s) = text s <> text "_Attrs"
                      
derives = text "deriving" <+> parens (commaList (map text ["Eq","Show"]))


---- Some operations on Names ----

-- | Make a name valid in both XML and Haskell.
name :: String -> Name
name n     = Name n (mangle n)

-- | Append an underscore to the Haskell version of the name.
name_ :: String -> Name
name_ n    = Name n (mangle n ++ "_")

-- | Prefix an attribute enumeration type name with its containing element
--   name.
name_a :: String -> String -> Name
name_a e n = Name n (mangle e ++ "_" ++ map decolonify n)

-- | Prefix an attribute enumeration constructor with its element-tag name,
--   and its enumeration type name.
name_ac :: String -> String -> String -> Name
name_ac e t n = Name n (mangle e ++ "_" ++ map decolonify t
                                 ++ "_" ++ map decolonify n)

-- | Prefix a field name with its enclosing element name.
name_f :: String -> String -> Name
name_f e n = Name n (manglef e ++ mangle n)

---- obsolete
-- elementname_at :: String -> Name
-- elementname_at n  = Name n (mangle n ++ "_Attrs")

-- | Convert an XML name to a Haskell conid.
mangle :: String -> String
mangle (n:ns)
    | isLower n   = toUpper n: map decolonify ns
    | isDigit n   = 'I': n: map decolonify ns
    | otherwise   = n: map decolonify ns

-- | Convert an XML name to a Haskell varid.
manglef :: String -> String
manglef (n:ns)
    | isUpper n   = toLower n: map decolonify ns
    | isDigit n   = '_': n: map decolonify ns
    | otherwise   = n: map decolonify ns

-- | Convert colon to prime, hyphen to underscore.
decolonify :: Char -> Char
decolonify ':' = '\''   -- TODO: turn namespaces into qualified identifiers
decolonify '-' = '_'
decolonify  c  = c

commaList = hcat . intersperse comma
