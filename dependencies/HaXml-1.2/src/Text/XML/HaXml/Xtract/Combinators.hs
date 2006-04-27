-- | This is a new set of XML combinators for Xtract, not standard,
--   but based on the standard set in "Text.Xml.Haxml.Combinators".
--   The main difference is that the Content Filter type becomes a
--   Double Filter.  A Double Filter always takes the whole document
--   as an extra argument, so you can start to traverse it again from
--   any inner location within the document tree.
--
--   The new combinators definitions are derived from the old ones.
--   New names are derived from the old by surrounding with the letter @o@,
--   or by doubling the operator symbols.

module Text.XML.HaXml.Xtract.Combinators where

import Text.XML.HaXml.Types
import Text.XML.HaXml.Combinators


-- | double content filter - takes document root + local subtree.
type DFilter = Content -> Content -> [Content]

-- | lift an ordinary content filter to a double filter.
local,global :: CFilter -> DFilter
local  f = \xml sub-> f sub
global f = \xml sub-> f xml

-- | lift a CFilter combinator to a DFilter combinator
oloco, oglobo :: (CFilter->CFilter) -> (DFilter->DFilter)
oloco ff  = \df-> \xml sub-> (ff (df xml)) sub
oglobo ff = \df-> \xml sub-> (ff (df xml)) xml

-- | lifted composition over double filters.
ooo :: DFilter -> DFilter -> DFilter
g `ooo` f = \xml-> concatMap (g xml) . (f xml)

-- | lifted choice.
(||>||) :: (a->b->[c]) -> (a->b->[c]) -> (a->b->[c])
f ||>|| g = \xml sub-> let first = f xml sub in
                       if null first then g xml sub else first

-- | lifted predicates.
owitho, owithouto :: DFilter -> DFilter -> DFilter
f `owitho` g    = \xml-> filter (not.null.g xml) . f xml
f `owithouto` g = \xml-> filter     (null.g xml) . f xml

-- | lifted unit and zero.
okeepo, ononeo :: DFilter
okeepo = \xml sub-> [sub]	-- local keep
ononeo = \xml sub-> []		-- local none

ochildreno, oelmo, otxto :: DFilter
ochildreno = local children
oelmo      = local elm
otxto      = local txt

applypred :: CFilter -> DFilter -> CFilter
applypred f p = \xml-> (const f `owitho` p) xml xml

oiffindo :: String -> (String -> DFilter) -> DFilter -> DFilter
oiffindo key yes no xml c@(CElem (Elem _ as _)) =
  case (lookup key as) of
    Nothing -> no xml c
    (Just (AttValue [Left s])) -> yes s xml c
oiffindo key yes no xml other = no xml other

oifTxto :: (String->DFilter) -> DFilter -> DFilter
oifTxto yes no xml c@(CString _ s) = yes s xml c
oifTxto yes no xml c               = no xml c

ocato :: [a->b->[c]] -> (a->b->[c])
ocato fs = \xml sub-> concat [ f xml sub | f <- fs ]

(//>>) :: DFilter -> DFilter -> DFilter
f //>> g = g `ooo` ochildreno `ooo` f

(<<//) :: DFilter -> DFilter -> DFilter
f <<// g = f `owitho` (g `ooo` ochildreno)

odeepo :: DFilter -> DFilter
odeepo f   = f ||>|| (odeepo f `ooo` ochildreno)

