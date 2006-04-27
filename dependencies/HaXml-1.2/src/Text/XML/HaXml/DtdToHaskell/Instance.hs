module Text.XML.HaXml.DtdToHaskell.Instance
  ( mkInstance
  ) where

import Data.List (intersperse)

import Text.XML.HaXml.DtdToHaskell.TypeDef
import Text.PrettyPrint.HughesPJ


-- | Convert typedef to appropriate instance declaration, either @XmlContent@,
--   @XmlAttributes@, or @XmlAttrType@.
mkInstance :: TypeDef -> Doc

-- no constructors
mkInstance (DataDef aux n fs []) =
    let (frpat, frattr, topat, toattr) = attrpats fs
        frretval = if null fs then ppHName n else frattr
        topatval = if null fs then ppHName n else topat
    in
    text "instance XmlContent" <+> ppHName n <+> text "where" $$
    nest 4 ( text "fromElem (CElem (Elem \"" <> ppXName n <> text "\""
                  <+> frpat <+> text "[]):rest) =" $$
             nest 4 (text "(Just" <+> frretval <> text ", rest)") $$
             text "fromElem (CMisc _:rest) = fromElem rest" $$
             text "fromElem rest = (Nothing, rest)"
           $$
             text "toElem" <+> topatval <+> text "=" $$
             nest 4 (text "[CElem (Elem \"" <> ppXName n <> text "\""
                          <+> toattr <+> text "[])]")
           )
    $$
    mkInstanceAttrs Same n fs

-- single constructor, "real" (non-auxiliary) type
mkInstance (DataDef False n fs [(n0,sts)]) =
    let vs = nameSupply sts
        (frpat, frattr, topat, toattr) = attrpats fs
    in
    text "instance XmlContent" <+> ppHName n <+> text "where" $$
    nest 4 ( text "fromElem (CElem (Elem \"" <> ppXName n <> text "\""
                  <+> frpat <+> text "c0):rest) =" $$
             nest 4 (mkFrElem n sts vs (
                         text "(Just" <+> parens (mkCpat n0 frattr vs)
                               <> text ", rest)")
                    ) $$
             text "fromElem (CMisc _:rest) = fromElem rest" $$
             text "fromElem rest = (Nothing, rest)"
           $$
             text "toElem" <+> parens (mkCpat n0 topat vs) <+> text "=" $$
             nest 4 (text "[CElem (Elem \"" <> ppXName n <> text "\""
                          <+> toattr <+> parens (mkToElem sts vs) <> text ")]")
           )
    $$
    mkInstanceAttrs Extended n fs

-- single constructor, auxiliary type
mkInstance (DataDef True n fs [(n0,sts)]) =
    let vs = nameSupply sts
        (frpat, frattr, topat, toattr) = attrpats fs
    in
    text "instance XmlContent" <+> ppHName n <+> text "where" $$
    nest 4 ( text "fromElem c0 =" $$
             mkFrAux True frattr [(n0,sts)]
           $$
             text "toElem" <+> parens (mkCpat n0 topat vs) <+> text "=" $$
      --     nest 4 (text "[CElem (Elem \"" <> ppXName n <> text "\""
      --                  <+> toattr <+> parens (mkToElem sts vs) <> text ")]")
             nest 4 (parens (mkToElem sts vs))
           )
    $$
    mkInstanceAttrs Extended n fs

-- multiple constructors
mkInstance (DataDef aux n fs cs) =
    let vs = nameSupply cs
        (frpat, frattr, topat, toattr) = attrpats fs
        mixattrs = if null fs then False else True
    in
    text "instance XmlContent" <+> ppHName n <+> text "where" $$
    nest 4 ( ( if aux then text "fromElem c0 ="
               else text "fromElem (CElem (Elem \"" <> ppXName n <> text "\""
                         <+> frpat <+> text "c0):rest) =" ) $$
             mkFrAux aux frattr cs $$
             text "fromElem (CMisc _:rest) = fromElem rest" $$
             text "fromElem rest = (Nothing, rest)"
           $$
             if aux then vcat (map (mkToAux mixattrs) cs)
             else vcat (map (mkToMult n topat toattr) cs)
           )
    $$
    mkInstanceAttrs Extended n fs

-- enumeration of attribute values
mkInstance (EnumDef n es) =
    text "instance XmlAttrType" <+> ppHName n <+> text "where" $$
    nest 4 ( text "fromAttrToTyp n (n',v)" $$
             nest 4 (text "| n==n'     = translate (attr2str v)" $$
                     text "| otherwise = Nothing") $$
             nest 2 (text "where" <+> mkTranslate es)
           $$
             vcat (map mkToAttr es)
           )


data SameName = Same | Extended

mkInstanceAttrs        :: SameName -> Name -> AttrFields -> Doc
mkInstanceAttrs s n []  = empty
mkInstanceAttrs s n fs  =
    let ppName = case s of { Same-> ppHName;  Extended-> ppAName; }
    in
    text "instance XmlAttributes" <+> ppName n <+> text "where" $$
    nest 4 ( text "fromAttrs as =" $$
             nest 4 ( ppName n $$
                      nest 2 (vcat ((text "{" <+> mkFrFld n (head fs)):
                                     map (\x-> comma <+> mkFrFld n x) (tail fs)) $$
                              text "}"))
           $$
             text "toAttrs v = catMaybes " $$
             nest 4 (vcat ((text "[" <+> mkToFld (head fs)):
                           map (\x-> comma <+> mkToFld x) (tail fs)) $$
                     text "]")
           )


--                  respectively (frpat,frattr,topat,toattr)
attrpats :: AttrFields -> (Doc,Doc,Doc,Doc)
attrpats fs =
  if null fs then (text "[]", empty, empty, text "[]")
  else (text "as", parens (text "fromAttrs as"), text "as", parens (text "toAttrs as"))




mkFrElem :: Name -> [StructType] -> [Doc] -> Doc -> Doc
mkFrElem n sts vs inner =
    foldr (frElem n) inner (zip3 sts vs cvs)
  where
    cvs = let ns = nameSupply2 vs
          in zip ns (text "c0": init ns)
    frElem n (st,v,(cvi,cvo)) inner =
        parens (text "\\" <> parens (v<>comma<>cvi) <> text "->" $$
                nest 2 inner) $$
        parens (
          case st of
            (Maybe String)  -> text "fromText" <+> cvo
            (Maybe s)       -> text "fromElem" <+> cvo
            (List String)   -> text "many fromText" <+> cvo
            (List s)        -> text "many fromElem" <+> cvo
            (List1 s)       -> text "definite fromElem"
                               <+> text "\"" <> text (show s)<> text "+\""
                               <+> text "\"" <> ppXName n <> text "\""
                               <+> cvo
            (Tuple ss)  -> text "definite fromElem"
                           <+> text "\"(" <> hcat (intersperse (text ",")
                                                           (map (text.show) ss))
                                          <> text ")\""
                           <+> text "\"" <> ppXName n <> text "\""
                           <+> cvo
            (OneOf ss)  -> text "definite fromElem"
                           <+> text "\"OneOf\""
                           <+> text "\"" <> ppXName n <> text "\""
                           <+> cvo
            (String)    -> text "definite fromText" <+> text "\"text\" \"" <>
                                                 ppXName n <> text "\"" <+> cvo
            (Any)       -> text "definite fromElem" <+> text "\"ANY\" \"" <>
                                                 ppXName n <> text "\"" <+> cvo
            (Defined m) -> text "definite fromElem" <+>
				 text "\"<" <> ppXName m <> text ">\" \"" <>
                                                 ppXName n <> text "\"" <+> cvo
            (Defaultable _ _)  -> text "nyi_fromElem_Defaultable" <+> cvo
          )

mkToElem :: [StructType] -> [Doc] -> Doc
mkToElem []  [] = text "[]"
mkToElem sts vs =
    fsep (intersperse (text "++") (zipWith toElem sts vs))
  where
    toElem st v =
      case st of
        (Maybe String)  -> text "maybe [] toText" <+> v
        (Maybe s)       -> text "maybe [] toElem" <+> v
        (List String)   -> text "concatMap toText" <+> v
        (List s)        -> text "concatMap toElem" <+> v
        (List1 s)       -> text "toElem" <+> v
        (Tuple ss)      -> text "toElem" <+> v
        (OneOf ss)      -> text "toElem" <+> v
        (String)        -> text "toText" <+> v
        (Any)           -> text "toElem" <+> v
        (Defined m)     -> text "toElem" <+> v
        (Defaultable _ _)  -> text "nyi_toElem_Defaultable" <+> v

mkRpat :: [Doc] -> Doc
mkRpat [v] = v
mkRpat vs  = (parens . hcat . intersperse comma) vs

mkCpat :: Name -> Doc -> [Doc] -> Doc
mkCpat n i vs = ppHName n <+> i <+> fsep vs

nameSupply,nameSupply2 :: [b] -> [Doc]
nameSupply  ss = take (length ss) (map char ['a'..])
nameSupply2 ss = take (length ss) [ text ('c':v:[]) | v <- ['a'..]]

mkTranslate :: [Name] -> Doc
mkTranslate es =
    vcat (map trans es) $$
    text "translate _ = Nothing"
  where
    trans n = text "translate \"" <> ppXName n <> text "\" =" <+>
              text "Just" <+> ppHName n

mkToAttr n = text "toAttrFrTyp n" <+> ppHName n <+> text "=" <+>
             text "Just (n, str2attr" <+> doubleQuotes (ppXName n) <> text ")"

mkFrFld :: Name -> (Name,StructType) -> Doc
mkFrFld tag (n,st) =
    ppHName n <+> text "=" <+>
    ( case st of
        (Defaultable String s) -> text "defaultA fromAttrToStr" <+>
						 doubleQuotes (text s)
        (Defaultable _ s)      -> text "defaultA fromAttrToTyp" <+> text s
        (Maybe String)         -> text "possibleA fromAttrToStr"
        (Maybe _)              -> text "possibleA fromAttrToTyp"
        String                 -> text "definiteA fromAttrToStr" <+>
						 doubleQuotes (ppXName tag)
        _                      -> text "definiteA fromAttrToTyp" <+>
						 doubleQuotes (ppXName tag)
    ) <+> doubleQuotes (ppXName n) <+> text "as"

mkToFld :: (Name,StructType) -> Doc
mkToFld (n,st) =
    ( case st of
        (Defaultable String _) -> text "defaultToAttr toAttrFrStr"
        (Defaultable _ _)      -> text "defaultToAttr toAttrFrTyp"
        (Maybe String)         -> text "maybeToAttr toAttrFrStr"
        (Maybe _)              -> text "maybeToAttr toAttrFrTyp"
        String                 -> text "toAttrFrStr"
        _                      -> text "toAttrFrTyp"
    ) <+> doubleQuotes (ppXName n) <+> parens (ppHName n <+> text "v")

mkFrAux :: Bool -> Doc -> [(Name,[StructType])] -> Doc
mkFrAux keeprest attrs cs = foldr frAux inner cs
  where
    inner = text "(Nothing, c0)"
    rest  = if keeprest then text "rest" else text "_"
    frAux (n,sts) inner =
        let vs  = nameSupply sts in
        nest 4 (text "case" <+> blah sts vs <+> text "of" $$
                succpat sts vs <+> text "-> (Just" <+>
                                   parens (mkCpat n attrs vs) <> text ", rest)"
                $$
                failpat sts <+> text "->" $$ nest 4 inner
               )
    blah [st] [v] =
        blahblahblah st (text "c0")
    blah sts vs =
        let ns = nameSupply2 vs
            cvs = zip ns (text "c0": init ns)
            blahblah (st,v,(cvi,cvo)) inner =
                parens (text "\\" <> parens (v<>comma<>cvi) <> text "->" $$
                        nest 2 inner) $$
                blahblahblah st cvo
        in
        foldr blahblah (mkRpat (vs++[last ns])) (zip3 sts vs cvs)
    blahblahblah st cvo = parens (
        case st of
          (Maybe String) -> text "fromText" <+> cvo
          (Maybe s)      -> text "fromElem" <+> cvo
          (List String)  -> text "many fromText" <+> cvo
          (List s)       -> text "many fromElem" <+> cvo
          (List1 s)      -> text "fromElem" <+> cvo
          (Tuple ss)     -> text "fromElem" <+> cvo	-- ??
          (OneOf ss)     -> text "fromElem" <+> cvo
          (String)       -> text "fromText" <+> cvo
          (Any)          -> text "fromElem" <+> cvo
          (Defined m)    -> text "fromElem" <+> cvo
        )
    failpat sts =
        let fp st =
                case st of
                  (Maybe s)   -> text "Nothing"
                  (List s)    -> text "[]"
                  (List1 s)   -> text "_"
                  (Tuple ss)  -> text "_"
                  (OneOf ss)  -> text "_"
                  (String)    -> text "_"
                  (Any)       -> text "_"
                  (Defined m) -> text "_"
        in parens (hcat (intersperse comma (map fp sts++[text "_"])))
    succpat sts vs =
        let sp st v =
                case st of
                  (Maybe s)   -> v
                  (List s)    -> v
                  (List1 s)   -> text "Just" <+> v
                  (Tuple ss)  -> text "Just" <+> v
                  (OneOf ss)  -> text "Just" <+> v
                  (String)    -> text "Just" <+> v
                  (Any)       -> text "Just" <+> v
                  (Defined m) -> text "Just" <+> v
        in parens (hcat (intersperse comma (zipWith sp sts vs++[rest])))

mkToAux :: Bool -> (Name,[StructType]) -> Doc
mkToAux mixattrs (n,sts) =
    let vs = nameSupply sts
        attrs = if mixattrs then text "as" else empty
    in
    text "toElem" <+> parens (mkCpat n attrs vs) <+> text "=" <+>
    mkToElem sts vs

mkToMult :: Name -> Doc -> Doc -> (Name,[StructType]) -> Doc
mkToMult tag attrpat attrexp (n,sts) =
    let vs = nameSupply sts
    in
    text "toElem" <+> parens (mkCpat n attrpat vs) <+> text "=" <+>
    text "[CElem (Elem \"" <> ppXName tag <> text "\""<+> attrexp <+>
    parens (mkToElem sts vs) <+> text ")]"

