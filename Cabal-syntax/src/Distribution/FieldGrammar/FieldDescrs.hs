{-# LANGUAGE UndecidableInstances #-}

module Distribution.FieldGrammar.FieldDescrs
  ( FieldDescrs
  , fieldDescrPretty
  , fieldDescrParse
  , fieldDescrsToList
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Compat.Lens (ALens', aview, cloneLens)
import Distribution.FieldGrammar
import Distribution.Pretty (Pretty (..), showFreeText)
import Distribution.Utils.String (trim)

import Data.Coerce
import qualified Data.Map as Map
import qualified Distribution.Compat.CharParsing as C
import qualified Distribution.Fields as P
import qualified Distribution.Parsec as P
import qualified Text.PrettyPrint as Disp

-- strict pair
data SP s = SP
  { pPretty :: !(s -> Disp.Doc)
  , pParse :: !(forall m. P.CabalParsing m => s -> m s)
  }

-- | A collection of field parsers and pretty-printers.
newtype FieldDescrs s a = F {runF :: Map P.FieldName (SP s)}
  deriving (Functor)

instance Applicative (FieldDescrs s) where
  pure _ = F mempty
  f <*> x = F (runF f <> runF x)

singletonF :: P.FieldName -> (s -> Disp.Doc) -> (forall m. P.CabalParsing m => s -> m s) -> FieldDescrs s a
singletonF fn f g = F $ Map.singleton fn (SP f g)

-- | Lookup a field value pretty-printer.
fieldDescrPretty :: FieldDescrs s a -> P.FieldName -> Maybe (s -> Disp.Doc)
fieldDescrPretty (F m) fn = pPretty <$> Map.lookup fn m

-- | Lookup a field value parser.
fieldDescrParse :: P.CabalParsing m => FieldDescrs s a -> P.FieldName -> Maybe (s -> m s)
fieldDescrParse (F m) fn = (\f -> pParse f) <$> Map.lookup fn m

fieldDescrsToList
  :: P.CabalParsing m
  => FieldDescrs s a
  -> [(P.FieldName, s -> Disp.Doc, s -> m s)]
fieldDescrsToList = map mk . Map.toList . runF
  where
    mk (name, SP ppr parse) = (name, ppr, parse)

-- | /Note:/ default values are printed.
instance FieldGrammar ParsecPretty FieldDescrs where
  blurFieldGrammar l (F m) = F (fmap blur m)
    where
      blur (SP f g) = SP (f . aview l) (cloneLens l g)

  booleanFieldDef fn l _def = singletonF fn f g
    where
      f s = Disp.text (show (aview l s))
      g s = cloneLens l (const P.parsec) s

  -- Note: eta expansion is needed for RankNTypes type-checking to work.

  uniqueFieldAla
    :: forall s proxy a b
     . (Coercible a b, Pretty b, P.Parsec b)
    => P.FieldName
    -> proxy a b
    -> ALens' s a
    -> FieldDescrs s a
  uniqueFieldAla fn _pack l = singletonF fn f g
    where
      f s = pretty (coerce @a @b (aview l s))
      g s = cloneLens l (const (coerce @b @a <$> P.parsec)) s

  optionalFieldAla
    :: forall s proxy a b
     . (Coercible a b, Pretty b, P.Parsec b)
    => P.FieldName
    -> proxy a b
    -> ALens' s (Maybe a)
    -> FieldDescrs s (Maybe a)
  optionalFieldAla fn _pack l = singletonF fn f g
    where
      f s = maybe mempty (pretty . coerce @a @b) (aview l s)
      g s = cloneLens l (const (Just . coerce @b @a <$> P.parsec)) s

  optionalFieldDefAla
    :: forall s proxy a b
     . (Coercible a b, Pretty b, P.Parsec b)
    => P.FieldName
    -> proxy a b
    -> ALens' s a
    -> a
    -> FieldDescrs s a
  optionalFieldDefAla fn _pack l _def = singletonF fn f g
    where
      f s = pretty (coerce @a @b (aview l s))
      g s = cloneLens l (const (coerce @b @a <$> P.parsec)) s

  freeTextField fn l = singletonF fn f g
    where
      f s = maybe mempty showFreeText (aview l s)
      g s = cloneLens l (const (Just <$> parsecFreeText)) s

  freeTextFieldDef fn l = singletonF fn f g
    where
      f s = showFreeText (aview l s)
      g s = cloneLens l (const parsecFreeText) s

  freeTextFieldDefST = defaultFreeTextFieldDefST

  monoidalFieldAla
    :: forall s proxy a b
     . (Monoid a, Coercible a b, Pretty b, P.Parsec b)
    => P.FieldName
    -> proxy a b
    -> ALens' s a
    -> FieldDescrs s a
  monoidalFieldAla fn _pack l = singletonF fn f g
    where
      f s = pretty (coerce @a @b (aview l s))
      g s = cloneLens l (\x -> (x <>) . coerce @b @a <$> P.parsec) s

  prefixedFields _fnPfx _l = F mempty
  knownField _ = pure ()
  deprecatedSince _ _ x = x
  removedIn _ _ x = x
  availableSince _ _ = id
  hiddenField _ = F mempty

parsecFreeText :: P.CabalParsing m => m String
parsecFreeText = dropDotLines <$ C.spaces <*> many C.anyChar
  where
    -- Example package with dot lines
    -- http://hackage.haskell.org/package/copilot-cbmc-0.1/copilot-cbmc.cabal
    dropDotLines "." = "."
    dropDotLines x = intercalate "\n" . map dotToEmpty . lines $ x

    dotToEmpty x | trim' x == "." = ""
    dotToEmpty x = trim x

    trim' :: String -> String
    trim' = dropWhileEnd (`elem` (" \t" :: String))

class (P.Parsec a, Pretty a) => ParsecPretty a
instance (P.Parsec a, Pretty a) => ParsecPretty a
