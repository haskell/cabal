{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Distribution.Outputable (
    -- * Type class
    Outputable(..),

    -- * Pretty printing combinators
    SDoc,
    showSDoc,
    showPpr,
    docToSDoc,
    sdocWithVerbosity,

    -- No empty to avoid conflict with Applicative
    nest,
    char,
    text,
    int, integer, float, double, rational,
    parens, brackets, braces, quotes,
    doubleQuotes, angleBrackets, paBrackets,
    semi, comma, colon, space, equals, dot, vbar,
    lparen, rparen, lbrack, rbrack, lbrace, rbrace, underscore,
    blankLine,
    (<>), (<+>), hcat, hsep,
    ($$), ($+$), vcat,
    sep, cat,
    fsep, fcat,
    hang, punctuate, ppWhen, ppUnless,
) where

import Distribution.Verbosity
import Distribution.Pretty
import Distribution.Compat.Semigroup

import Data.Int
import Data.Word
import Data.Set (Set)
import Data.Map (Map)
import Data.IntMap (IntMap)
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Data.Set as Set

import Text.PrettyPrint (Doc)
import qualified Text.PrettyPrint as Pretty

newtype SDoc = SDoc { runSDoc :: SDocContext -> Doc }

instance Semigroup SDoc where
    (<>) = (<<>>)

instance Monoid SDoc where
    mempty = empty
    mappend = (<>)

showSDoc :: Verbosity -> SDoc -> String
showSDoc verbosity sdoc = Pretty.render $ runSDoc sdoc (SDC verbosity)

showPpr :: Outputable a => Verbosity -> a -> String
showPpr verbosity = showSDoc verbosity . ppr

data SDocContext = SDC
  { sdocVerbosity :: Verbosity
  }

sdocWithVerbosity :: (Verbosity -> SDoc) -> SDoc
sdocWithVerbosity f = SDoc $ \ctx -> runSDoc (f (sdocVerbosity ctx)) ctx

docToSDoc :: Doc -> SDoc
docToSDoc d = SDoc (\_ -> d)

empty    :: SDoc
char     :: Char       -> SDoc
text     :: String     -> SDoc
int      :: Int        -> SDoc
integer  :: Integer    -> SDoc
float    :: Float      -> SDoc
double   :: Double     -> SDoc
rational :: Rational   -> SDoc

empty       = docToSDoc $ Pretty.empty
char c      = docToSDoc $ Pretty.char c

text s      = docToSDoc $ Pretty.text s
{-# INLINE text #-}   -- Inline so that the RULE Pretty.text will fire

int n       = docToSDoc $ Pretty.int n
integer n   = docToSDoc $ Pretty.integer n
float n     = docToSDoc $ Pretty.float n
double n    = docToSDoc $ Pretty.double n
rational n  = docToSDoc $ Pretty.rational n

parens, braces, brackets, quotes,
        paBrackets, doubleQuotes, angleBrackets :: SDoc -> SDoc

parens d        = SDoc $ Pretty.parens . runSDoc d
braces d        = SDoc $ Pretty.braces . runSDoc d
brackets d      = SDoc $ Pretty.brackets . runSDoc d
doubleQuotes d  = SDoc $ Pretty.doubleQuotes . runSDoc d
angleBrackets d = char '<' <> d <> char '>'
paBrackets d    = text "[:" <> d <> text ":]"

quotes d = SDoc $ Pretty.quotes . runSDoc d

semi, comma, colon, equals, space, underscore, dot, vbar :: SDoc
lparen, rparen, lbrack, rbrack, lbrace, rbrace, blankLine :: SDoc

blankLine  = docToSDoc $ Pretty.text ""
semi       = docToSDoc $ Pretty.semi
comma      = docToSDoc $ Pretty.comma
colon      = docToSDoc $ Pretty.colon
equals     = docToSDoc $ Pretty.equals
space      = docToSDoc $ Pretty.space
underscore = char '_'
dot        = char '.'
vbar       = char '|'
lparen     = docToSDoc $ Pretty.lparen
rparen     = docToSDoc $ Pretty.rparen
lbrack     = docToSDoc $ Pretty.lbrack
rbrack     = docToSDoc $ Pretty.rbrack
lbrace     = docToSDoc $ Pretty.lbrace
rbrace     = docToSDoc $ Pretty.rbrace

nest :: Int -> SDoc -> SDoc
-- ^ Indent 'SDoc' some specified amount
(<<>>) :: SDoc -> SDoc -> SDoc
-- ^ Join two 'SDoc' together horizontally without a gap
(<+>) :: SDoc -> SDoc -> SDoc
-- ^ Join two 'SDoc' together horizontally with a gap between them
($$) :: SDoc -> SDoc -> SDoc
-- ^ Join two 'SDoc' together vertically; if there is
-- no vertical overlap it "dovetails" the two onto one line
($+$) :: SDoc -> SDoc -> SDoc
-- ^ Join two 'SDoc' together vertically

nest n d    = SDoc $ Pretty.nest n . runSDoc d
(<<>>) d1 d2  = SDoc $ \sty -> (Pretty.<>)  (runSDoc d1 sty) (runSDoc d2 sty)
(<+>) d1 d2 = SDoc $ \sty -> (Pretty.<+>) (runSDoc d1 sty) (runSDoc d2 sty)
($$) d1 d2  = SDoc $ \sty -> (Pretty.$$)  (runSDoc d1 sty) (runSDoc d2 sty)
($+$) d1 d2 = SDoc $ \sty -> (Pretty.$+$) (runSDoc d1 sty) (runSDoc d2 sty)

hcat :: [SDoc] -> SDoc
-- ^ Concatenate 'SDoc' horizontally
hsep :: [SDoc] -> SDoc
-- ^ Concatenate 'SDoc' horizontally with a space between each one
vcat :: [SDoc] -> SDoc
-- ^ Concatenate 'SDoc' vertically with dovetailing
sep :: [SDoc] -> SDoc
-- ^ Separate: is either like 'hsep' or like 'vcat', depending on what fits
cat :: [SDoc] -> SDoc
-- ^ Catenate: is either like 'hcat' or like 'vcat', depending on what fits
fsep :: [SDoc] -> SDoc
-- ^ A paragraph-fill combinator. It's much like sep, only it
-- keeps fitting things on one line until it can't fit any more.
fcat :: [SDoc] -> SDoc
-- ^ This behaves like 'fsep', but it uses '<>' for horizontal conposition rather than '<+>'


hcat ds = SDoc $ \sty -> Pretty.hcat [runSDoc d sty | d <- ds]
hsep ds = SDoc $ \sty -> Pretty.hsep [runSDoc d sty | d <- ds]
vcat ds = SDoc $ \sty -> Pretty.vcat [runSDoc d sty | d <- ds]
sep ds  = SDoc $ \sty -> Pretty.sep  [runSDoc d sty | d <- ds]
cat ds  = SDoc $ \sty -> Pretty.cat  [runSDoc d sty | d <- ds]
fsep ds = SDoc $ \sty -> Pretty.fsep [runSDoc d sty | d <- ds]
fcat ds = SDoc $ \sty -> Pretty.fcat [runSDoc d sty | d <- ds]

hang :: SDoc  -- ^ The header
      -> Int  -- ^ Amount to indent the hung body
      -> SDoc -- ^ The hung body, indented and placed below the header
      -> SDoc
hang d1 n d2   = SDoc $ \sty -> Pretty.hang (runSDoc d1 sty) n (runSDoc d2 sty)

punctuate :: SDoc   -- ^ The punctuation
          -> [SDoc] -- ^ The list that will have punctuation added between every adjacent pair of elements
          -> [SDoc] -- ^ Punctuated list
punctuate _ []     = []
punctuate p (d0:ds) = go d0 ds
                   where
                     go d [] = [d]
                     go d (e:es) = (d <> p) : go e es

ppWhen, ppUnless :: Bool -> SDoc -> SDoc
ppWhen True  doc = doc
ppWhen False _   = empty

ppUnless True  _   = empty
ppUnless False doc = doc

-- | Debug-printable entity.
class Outputable a where
    ppr :: a -> SDoc

-- The usual objection against overlapping instances is that
-- introducing extra instances can cause behavior change.  But
-- this is fairly benign for Outputable, which intended primarily
-- for debugging.  Also, this is an undecidable instance, but
-- you should NEVER define a Pretty depending on an Outputable.
instance {-# OVERLAPPABLE #-} Pretty a => Outputable a where
    ppr x = SDoc $ \_ -> pretty x

-- You really ought not to use this instance, but it is convenient
instance Outputable SDoc where
    ppr = id

instance Outputable Doc where
    ppr = docToSDoc

instance Outputable Char where
    ppr c = text [c]

instance {-# OVERLAPPING #-} Outputable [Char] where
    ppr s = text s

instance Outputable Bool where
    ppr True  = text "True"
    ppr False = text "False"

instance Outputable Ordering where
    ppr LT = text "LT"
    ppr EQ = text "EQ"
    ppr GT = text "GT"

instance Outputable Int32 where
   ppr n = integer $ fromIntegral n

instance Outputable Int64 where
   ppr n = integer $ fromIntegral n

instance Outputable Int where
    ppr n = int n

instance Outputable Integer where
    ppr n = integer n

instance Outputable Word16 where
    ppr n = integer $ fromIntegral n

instance Outputable Word32 where
    ppr n = integer $ fromIntegral n

instance Outputable Word where
    ppr n = integer $ fromIntegral n

instance Outputable () where
    ppr _ = text "()"

instance (Outputable a) => Outputable [a] where
    ppr xs = brackets (fsep (punctuate comma (map ppr xs)))

instance (Outputable a) => Outputable (Set a) where
    ppr s = braces (fsep (punctuate comma (map ppr (Set.toList s))))

instance (Outputable a, Outputable b) => Outputable (a, b) where
    ppr (x,y) = parens (sep [ppr x <> comma, ppr y])

instance Outputable a => Outputable (Maybe a) where
    ppr Nothing  = text "Nothing"
    ppr (Just x) = text "Just" <+> ppr x

instance (Outputable a, Outputable b) => Outputable (Either a b) where
    ppr (Left x)  = text "Left"  <+> ppr x
    ppr (Right y) = text "Right" <+> ppr y

-- ToDo: may not be used
instance (Outputable a, Outputable b, Outputable c) => Outputable (a, b, c) where
    ppr (x,y,z) =
      parens (sep [ppr x <> comma,
                   ppr y <> comma,
                   ppr z ])

instance (Outputable a, Outputable b, Outputable c, Outputable d) =>
         Outputable (a, b, c, d) where
    ppr (a,b,c,d) =
      parens (sep [ppr a <> comma,
                   ppr b <> comma,
                   ppr c <> comma,
                   ppr d])

instance (Outputable a, Outputable b, Outputable c, Outputable d, Outputable e) =>
         Outputable (a, b, c, d, e) where
    ppr (a,b,c,d,e) =
      parens (sep [ppr a <> comma,
                   ppr b <> comma,
                   ppr c <> comma,
                   ppr d <> comma,
                   ppr e])

instance (Outputable a, Outputable b, Outputable c, Outputable d, Outputable e, Outputable f) =>
         Outputable (a, b, c, d, e, f) where
    ppr (a,b,c,d,e,f) =
      parens (sep [ppr a <> comma,
                   ppr b <> comma,
                   ppr c <> comma,
                   ppr d <> comma,
                   ppr e <> comma,
                   ppr f])

instance (Outputable a, Outputable b, Outputable c, Outputable d, Outputable e, Outputable f, Outputable g) =>
         Outputable (a, b, c, d, e, f, g) where
    ppr (a,b,c,d,e,f,g) =
      parens (sep [ppr a <> comma,
                   ppr b <> comma,
                   ppr c <> comma,
                   ppr d <> comma,
                   ppr e <> comma,
                   ppr f <> comma,
                   ppr g])

instance (Outputable key, Outputable elt) => Outputable (Map key elt) where
    ppr m = ppr (Map.toList m)
instance (Outputable elt) => Outputable (IntMap elt) where
    ppr m = ppr (IntMap.toList m)
