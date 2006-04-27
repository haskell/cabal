--------------------------------------------
-- | This module defines the notion of filters and filter combinators
--   for processing XML documents.
--
--   These XML transformation combinators are described in the paper
--   ``Haskell and XML: Generic Combinators or Type-Based Translation?''
--   Malcolm Wallace and Colin Runciman, Proceedings ICFP'99.
--------------------------------------------
module Text.XML.HaXml.Combinators
  (-- * The content filter type.
    CFilter

   -- * Simple filters.
   -- ** Selection filters.
   -- $selection
  , keep, none, children, position

   -- ** Predicate filters.
   -- $pred
  , elm, txt, tag, attr, attrval, tagWith

   -- ** Search filters.
  , find, iffind, ifTxt

   -- * Filter combinators
   -- ** Basic combinators.
  , o, union, cat, andThen
  , (|>|), with, without
  , (/>), (</), et
  , path
   -- ** Recursive search.
   -- $recursive
  , deep, deepest, multi
   -- ** Interior editing.
  , when, guards, chip, foldXml
   -- ** Constructive filters.
  , mkElem, mkElemAttr, literal, cdata, replaceTag, replaceAttrs

   -- * C-like conditionals.
   -- $cond
  , ThenElse(..), (?>)

   -- * Filters with labelled results.
  , LabelFilter
   -- ** Using and combining labelled filters.
  , oo, x
   -- ** Some label-generating filters.
  , numbered, interspersed, tagged, attributed, textlabelled, extracted

  ) where


import Text.XML.HaXml.Types
import Maybe (fromMaybe)

infixl 6 `with`, `without`
infixr 5 `o`, `oo`, `union`, `andThen`		-- , `orelse`
infixl 5 />, </, |>|
infixr 4 `when`, `guards`
infixr 3 ?>, :>



-- THE CONTENT FILTER TYPE

-- | All document transformations are /content filters/.
--   A filter takes a single XML 'Content' value and returns a sequence
--   of 'Content' values, possibly empty.
type CFilter    = Content -> [Content]



-- BASIC SELECTION FILTERS
-- $selection
-- In the algebra of combinators, @none@ is the zero, and @keep@ the identity.
-- (They have a more general type than just CFilter.)
keep, none :: a->[a]
keep = \x->[x]
none = \x->[]

-- | Throw away current node, keep just the (unprocessed) children.
children :: CFilter
children (CElem (Elem _ _ cs)) = cs
children _ = []

-- | Select the @n@'th positional result of a filter.
position :: Int -> CFilter -> CFilter
position n f = (\cs-> [cs!!n]) . f



-- BASIC PREDICATE FILTERS
-- $pred
-- These filters either keep or throw away some content based on
-- a simple test.  For instance, @elm@ keeps only a tagged element,
-- @txt@ keeps only non-element text, @tag@ keeps only an element
-- with the named tag, @attr@ keeps only an element with the named
-- attribute, @attrval@ keeps only an element with the given
-- attribute value, @tagWith@ keeps only an element whose tag name
-- satisfies the given predicate.

elm, txt   :: CFilter
tag        :: String -> CFilter
attr       :: Name -> CFilter
attrval    :: Attribute -> CFilter
tagWith    :: (String->Bool) -> CFilter

elm x@(CElem _)   = [x]
elm _             = []

txt x@(CString _ _) = [x]
txt x@(CRef _)      = [x]
txt _               = []

tag t x@(CElem (Elem n _ _)) | t==n  = [x]
tag t _  = []

tagWith p x@(CElem (Elem n _ _)) | p n  = [x]
tagWith p _  = []

attr n x@(CElem (Elem _ as _)) | n `elem` (map fst as)  = [x]
attr n _  = []

attrval av x@(CElem (Elem _ as _)) | av `elem` as  = [x]
attrval av _  = []



-- SEARCH FILTERS

-- | For a mandatory attribute field, @find key cont@ looks up the value of
--   the attribute name @key@, and applies the continuation @cont@ to
--   the value.
find :: String -> (String->CFilter) -> CFilter
find key cont c@(CElem (Elem _ as _)) = cont (value (lookfor key as)) c
  where lookfor x = fromMaybe (error ("missing attribute: "++show x)) . lookup x
        value (AttValue [Left x]) = x
-- 'lookfor' has the more general type :: (Eq a,Show a) => a -> [(a,b)] -> b

-- | When an attribute field may be absent, use @iffind key yes no@ to lookup
--   its value.  If the attribute is absent, it acts as the @no@ filter,
--   otherwise it applies the @yes@ filter.
iffind :: String -> (String->CFilter) -> CFilter -> CFilter
iffind key yes no c@(CElem (Elem _ as _)) =
  case (lookup key as) of
    Nothing  -> no c
    (Just (AttValue [Left s])) -> yes s c
iffind key yes no other = no other

-- | @ifTxt yes no@ processes any textual content with the @yes@ filter,
--   but otherwise is the same as the @no@ filter.
ifTxt :: (String->CFilter) -> CFilter -> CFilter
ifTxt yes no c@(CString _ s) = yes s c
ifTxt yes no c               = no c



-- C-LIKE CONDITIONALS
--
-- $cond
-- These definitions provide C-like conditionals, lifted to the filter level.
--
-- The @(cond ? yes : no)@ style in C becomes @(cond ?> yes :> no)@ in Haskell.

-- | Conjoin the two branches of a conditional.
data ThenElse a = a :> a

-- | Select between the two branches of a joined conditional.
(?>) :: (a->[b]) -> ThenElse (a->[b]) -> (a->[b])
p ?> (f :> g) = \c-> if (not.null.p) c then f c else g c



-- FILTER COMBINATORS


-- | Sequential (/Irish/,/backwards/) composition
o :: CFilter -> CFilter -> CFilter
f `o` g = concatMap f . g

-- | Binary parallel composition.  Each filter uses a copy of the input,
-- rather than one filter using the result of the other.
--   (Has a more general type than just CFilter.)
union :: (a->[b]) -> (a->[b]) -> (a->[b])
union = lift (++)		-- in Haskell 98:   union = lift List.union
  where
    lift :: (a->b->d) -> (c->a) -> (c->b) -> c -> d
    lift f g h = \x-> f (g x) (h x)

-- | Glue a list of filters together.  (A list version of union;
--   also has a more general type than just CFilter.)
cat :: [a->[b]] -> (a->[b])
--   Specification: cat fs = \e-> concat [ f e | f <- fs ]
--   more efficient implementation below:
cat [] = const []
cat fs = foldr1 union fs

-- | A special form of filter composition where the second filter
--   works over the same data as the first, but also uses the
--   first's result.
andThen :: (a->c) -> (c->a->b) -> (a->b)
andThen f g = \x-> g (f x) x			-- lift g f id

-- | Process children using specified filters.  /not exported/
childrenBy :: CFilter -> CFilter
childrenBy f = f `o` children

-- | Directional choice:
--   in @f |>| g@ give g-productions only if no f-productions
(|>|) :: (a->[b]) -> (a->[b]) -> (a->[b])
f |>| g = \x-> let fx = f x in if null fx then g x else fx
--      f |>| g  =  f ?> f :> g

-- | Pruning: in @f `with` g@,
--   keep only those f-productions which have at least one g-production
with :: CFilter -> CFilter -> CFilter
f `with` g = filter (not.null.g) . f

-- | Pruning: in @f `without` g@,
--   keep only those f-productions which have no g-productions
without :: CFilter -> CFilter -> CFilter
f `without` g = filter (null.g) . f

-- | Pronounced /slash/, @f \/> g@ means g inside f
(/>) :: CFilter -> CFilter -> CFilter
f /> g = g `o` children `o` f

-- | Pronounced /outside/, @f \<\/ g@ means f containing g
(</) :: CFilter -> CFilter -> CFilter
f </ g = f `with` (g `o` children)

-- | Join an element-matching filter with a text-only filter
et :: (String->CFilter) -> CFilter -> CFilter
et f g = (f `oo` tagged elm)
            |>|
         (g `o` txt)

-- | Express a list of filters like an XPath query, e.g.
--   @path [children, tag \"name1\", attr \"attr1\", children, tag \"name2\"]@
--   is like the XPath query @\/name1[\@attr1]\/name2@.
path :: [CFilter] -> CFilter
path fs = foldr (flip (o)) keep fs


-- RECURSIVE SEARCH
-- $recursive
-- Recursive search has three variants: @deep@ does a breadth-first
-- search of the tree, @deepest@ does a depth-first search, @multi@ returns
-- content at all tree-levels, even those strictly contained within results
-- that have already been returned.
deep, deepest, multi :: CFilter -> CFilter
deep f     = f |>| (deep f `o` children)
deepest f  = (deepest f `o` children) |>| f
multi f    = f `union` (multi f `o` children)

-- | Interior editing:
--   @f `when` g@ applies @f@ only when the predicate @g@ succeeds,
--   otherwise the content is unchanged.
when   :: CFilter -> CFilter -> CFilter
-- | Interior editing:
--   @g `guards` f@ applies @f@ only when the predicate @g@ succeeds,
--   otherwise the content is discarded.
guards :: CFilter -> CFilter -> CFilter
f `when` g       = g ?> f :> keep
g `guards` f     = g ?> f :> none	-- = f `o` (keep `with` g)

-- | Process CHildren In Place.  The filter is applied to any children
--   of an element content, and the element rebuilt around the results.
chip :: CFilter -> CFilter
chip f (CElem (Elem n as cs)) = [ CElem (Elem n as (concatMap f cs)) ]
chip f c = [c]

-- | Recursive application of filters: a fold-like operator.  Defined
--   as @f `o` chip (foldXml f)@.
foldXml :: CFilter -> CFilter
foldXml f = f `o` chip (foldXml f)




-- CONSTRUCTIVE CONTENT FILTERS

-- | Build an element with the given tag name - its content is the results
--   of the given list of filters.
mkElem :: String -> [CFilter] -> CFilter
mkElem h cfs = \t-> [ CElem (Elem h [] (cat cfs t)) ]

-- | Build an element with the given name, attributes, and content.
mkElemAttr :: String -> [(String,CFilter)] -> [CFilter] -> CFilter
mkElemAttr h as cfs = \t-> [ CElem (Elem h (map (attr t) as) (cat cfs t)) ]
  where attr t (n,vf) =
            let v = concat [ s | (CString _ s) <- (deep txt `o` vf) t ]
            in  (n, AttValue [Left v])

-- | Build some textual content.
literal :: String -> CFilter
literal s = const [CString False s]

-- | Build some CDATA content.
cdata :: String -> CFilter
cdata s = const [CString True s]

-- | Rename an element tag.
replaceTag :: String -> CFilter
replaceTag n (CElem (Elem _ _ cs)) = [CElem (Elem n [] cs)]
replaceTag n _ = []

-- | Replace the attributes of an element.
replaceAttrs :: [(String,String)] -> CFilter
replaceAttrs as (CElem (Elem n _ cs)) = [CElem (Elem n as' cs)]
    where as' = map (\(n,v)-> (n, AttValue [Left v])) as
replaceAttrs as _ = []



-- LABELLING

-- | A LabelFilter is like a CFilter except that it pairs up a polymorphic
--   value (label) with each of its results.
type LabelFilter a = Content -> [(a,Content)]

-- | Compose a label-processing filter with a label-generating filter.
oo :: (a->CFilter) -> LabelFilter a -> CFilter
f `oo` g = concatMap (uncurry f) . g

-- | Combine labels.  Think of this as a pair-wise zip on labels.
x :: (CFilter->LabelFilter a) -> (CFilter->LabelFilter b) ->
       (CFilter->LabelFilter (a,b))
f `x` g = \cf c-> let gs = map fst (g cf c)
                      fs = map fst (f cf c)
                  in zip (zip fs gs) (cf c)


-- Some basic label-generating filters.

-- | Number the results from 1 upwards.
numbered :: CFilter -> LabelFilter String
numbered f = zip (map show [(1::Int)..]) . f

-- | In @interspersed a f b@, label each result of @f@ with the string @a@,
--   except for the last one which is labelled with the string @b@.
interspersed :: String -> CFilter -> String -> LabelFilter String
interspersed a f b =
  (\xs-> zip (replicate (len xs) a ++ [b]) xs) . f
  where
  len [] = 0
  len xs = length xs - 1

-- | Label each element in the result with its tag name.  Non-element
--   results get an empty string label.
tagged :: CFilter -> LabelFilter String
tagged f = extracted name f
  where name (CElem (Elem n _ _)) = n
        name _                    = ""

-- | Label each element in the result with the value of the named attribute.
--   Elements without the attribute, and non-element results, get an
--   empty string label.
attributed :: String -> CFilter -> LabelFilter String
attributed key f = extracted att f
  where att (CElem (Elem _ as _)) =
            case (lookup key as) of
              Nothing  -> ""
              (Just (AttValue [Left s])) -> s
        att _ = ""

-- | Label each textual part of the result with its text.  Element
--   results get an empty string label.
textlabelled :: CFilter -> LabelFilter (Maybe String)
textlabelled f = extracted text f
  where text (CString _ s) = Just s
        text _             = Nothing

-- | Label each content with some information extracted from itself.
extracted :: (Content->a) -> CFilter -> LabelFilter a
extracted proj f = concatMap (\c->[(proj c, c)]) . f
                                                                                


{-
-- MISC

-- | I haven't yet remembered \/ worked out what this does.
combine :: (Read a,Show a) => ([a]->a) -> LabelFilter String -> CFilter
combine f lf = \c-> [ CString False (show (f [ read l | (l,_) <- lf c ])) ]
-}


{- OLD STUFF - OBSOLETE
-- Keep an element by its numbered position (starting at 1).
position :: Int -> [Content] -> [Content]
position n | n>0  = (:[]) . (!!(n-1))
           | otherwise = const []

-- Chop and remove the root portions of trees to depth n.
layer :: Int -> [Content] -> [Content]
layer n = apply n (concatMap lay)
  where lay (CElem (Elem _ _ cs)) = cs
        lay _ = []
        apply 0 f xs = xs
        apply n f xs = apply (n-1) f (f xs)

combine :: (Read a, Show a) => ([a]->a) -> [Content] -> [Content]
combine f = \cs-> [ CString False (show (f [ read s | CString _ s <- cs ])) ]
-}
