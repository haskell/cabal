{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes    #-}
module Distribution.FieldGrammar.FieldDescrs (
    FieldDescrs,
    fieldDescrPretty,
    fieldDescrParse,
    ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Compat.Lens    (aview, cloneLens)
import Distribution.Compat.Newtype
import Distribution.FieldGrammar
import Distribution.Pretty         (pretty)
import Distribution.Utils.Generic  (fromUTF8BS)

import qualified Data.Map                   as Map
import qualified Distribution.Parsec.Class  as P
import qualified Distribution.Parsec.Field  as P
import qualified Text.PrettyPrint           as Disp

-- strict pair
data SP s = SP
    { pPretty :: !(s -> Disp.Doc)
    , pParse  :: !(forall m. P.CabalParsing m => s -> m s)
    }

-- | A collection field parsers and pretty-printers.
newtype FieldDescrs s a = F { runF :: Map String (SP s) }
  deriving (Functor)

instance Applicative (FieldDescrs s) where
    pure _  = F mempty
    f <*> x = F (mappend (runF f) (runF x))

singletonF :: P.FieldName -> (s -> Disp.Doc) -> (forall m. P.CabalParsing m => s -> m s) -> FieldDescrs s a
singletonF fn f g = F $ Map.singleton (fromUTF8BS fn) (SP f g)

-- | Lookup a field value pretty-printer.
fieldDescrPretty :: FieldDescrs s a -> String -> Maybe (s -> Disp.Doc)
fieldDescrPretty (F m) fn = pPretty <$> Map.lookup fn m

-- | Lookup a field value parser.
fieldDescrParse :: P.CabalParsing m => FieldDescrs s a -> String -> Maybe (s -> m s)
fieldDescrParse (F m) fn = pParse <$> Map.lookup fn m

-- | /Note:/ default values are printed.
instance FieldGrammar FieldDescrs where
    blurFieldGrammar l (F m) = F (fmap blur m) where
        blur (SP f g) = SP (f . aview l) (cloneLens l g)

    booleanFieldDef fn l _def = singletonF fn f g where
        f s = Disp.text (show (aview l s))
        g s = cloneLens l (const P.parsec) s
      -- Note: eta expansion is needed for RankNTypes type-checking to work.

    uniqueFieldAla fn _pack l = singletonF fn f g where
        f s = pretty (pack' _pack (aview l s))
        g s = cloneLens l (const (unpack' _pack <$> P.parsec)) s

    optionalFieldAla fn _pack l = singletonF fn f g where
        f s = maybe mempty (pretty . pack' _pack) (aview l s)
        g s = cloneLens l (const (Just . unpack' _pack <$> P.parsec)) s

    optionalFieldDefAla fn _pack l _def = singletonF fn f g where
        f s = pretty (pack' _pack (aview l s))
        g s = cloneLens l (const (unpack' _pack <$> P.parsec)) s

    monoidalFieldAla fn _pack l = singletonF fn f g where
        f s = pretty (pack' _pack (aview l s))
        g s = cloneLens l (\x -> mappend x . unpack' _pack <$> P.parsec) s

    prefixedFields _fnPfx _l = F mempty
    knownField _           = pure ()
    deprecatedSince _  _ x = x
    availableSince _ _     = id
    hiddenField _          = F mempty
