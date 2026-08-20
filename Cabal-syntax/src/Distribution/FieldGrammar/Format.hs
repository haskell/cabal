{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Distribution.FieldGrammar.Format where

import Distribution.FieldGrammar.Class
import Distribution.FieldGrammar.Parsec
import Distribution.Fields.Field
import Distribution.Parsec
import Distribution.Pretty

import Data.Kind
import Data.Coerce

import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.ByteString as BS
import Distribution.Parsec.FieldLineStream
import qualified Data.ByteString.Char8 as BS8
import Distribution.Compat.Lens

data FormatterFieldGrammar s a = FormatterFG
  { fieldGrammarFormatter
      :: Map FieldName ([FieldLine (WithComments Position)] -> [FieldLine (WithComments Position)])
  }
  deriving (Functor)

-- TODO(lena8959): use in the cabal spec version
formatFieldGrammar :: FormatterFieldGrammar s a -> [Field (WithComments Position)] -> [Field (WithComments Position)]
formatFieldGrammar (FormatterFG formatter) = go
  where go [] = []
        go (section@(Section {}) : fs) = section : go fs
        go (Field colonPos fname fls : fs) =
          let newField = case formatter M.!? getName fname of
                Just f -> Field colonPos fname (f fls)
                Nothing -> Field colonPos fname fls
          in  newField : go fs

-- TODO(leana8959): how to do this properly
class (Pretty a, Parsec a) => Formattable a
instance (Pretty a, Parsec a) => Formattable a

instance Applicative (FormatterFieldGrammar s) where
  pure _ = FormatterFG M.empty
  FormatterFG x <*> FormatterFG y = FormatterFG (x <> y)

-- | Naive implementation, doesn't put comments back.
formatFieldLines
  :: forall (a :: Type)
   . (Parsec a, Pretty a)
  => [FieldLine (WithComments Position)] -> [FieldLine (WithComments Position)]
formatFieldLines = formatFieldLinesAla @a @a

-- | Naive implementation, doesn't put comments back.
formatFieldLinesAla
  :: forall (b :: Type) (a :: Type)
   . (Coercible a b, Parsec b, Pretty b)
  => [FieldLine (WithComments Position)] -> [FieldLine (WithComments Position)]
formatFieldLinesAla fls0 =
  let comments = foldMap extractComments fls0
      fls = fmap removeComments fls0
  in  case joinFieldLines <$> NE.nonEmpty fls of
      Nothing -> fls0
      Just (FieldLine ann0 bs0) ->
        let bs = formatBSAla @b @a bs0
         in (map . fmap) (WithComments mempty) (splitFieldLines (FieldLine ann0 bs))

formatBSAla
  :: forall (b :: Type) (a :: Type)
   . (Coercible a b, Parsec b, Pretty b)
  => BS.ByteString -> BS.ByteString
formatBSAla bs0 =
  let parsed =
        fmap (coerce @b @a)
          . runParsecParser (parsec @b) "<modifyValueAtomAla>"
          . fieldLineStreamFromBS
          $ bs0
  in case parsed of
    Left _ -> error "formatBS: failed to parse"
    Right parseOk -> BS8.pack $ show $ pretty @b $ coerce @a @b parseOk


instance FieldGrammar Formattable FormatterFieldGrammar where
  uniqueFieldAla
    :: forall b a s proxy. (Formattable b, Coercible a b) => FieldName -> proxy a b -> ALens' s a -> FormatterFieldGrammar s a
  uniqueFieldAla fn _ _extract = FormatterFG $ M.singleton fn $ formatFieldLinesAla @b @a

  booleanFieldDef fn _ _def = FormatterFG $ M.singleton fn $ formatFieldLines @Bool 

  optionalFieldAla
    :: forall b a s proxy. (Formattable b, Coercible a b) => FieldName -> proxy a b -> ALens' s (Maybe a) -> FormatterFieldGrammar s (Maybe a)
  optionalFieldAla fn _ _extract = FormatterFG $ M.singleton fn $ formatFieldLinesAla @b @a

  optionalFieldDefAla
    :: forall b a s proxy. (Formattable b, Coercible a b) => FieldName -> proxy a b -> ALens' s a -> a -> FormatterFieldGrammar s a
  optionalFieldDefAla fn _ _extract _def = FormatterFG $ M.singleton fn $ formatFieldLinesAla @b @a

  monoidalFieldAla
    :: forall b a proxy s. (Formattable b, Coercible a b) => FieldName -> proxy a b -> ALens' s a -> FormatterFieldGrammar s a
  monoidalFieldAla fn _ _extract = FormatterFG $ M.singleton fn $ formatFieldLinesAla @b @a

  -- Nothing to format?
  freeTextField _fn _ = FormatterFG M.empty
  freeTextFieldDef _fn _ = FormatterFG M.empty
  freeTextFieldDefST _fn _ = FormatterFG M.empty
  prefixedFields _fn _ = FormatterFG M.empty

  knownField _ = FormatterFG M.empty
  hiddenField _ = FormatterFG M.empty

  deprecatedSince _ _ = id
  removedIn _ _ = id
  availableSince _ _ = id
  availableSinceWarn _ = id

  blurFieldGrammar _ (FormatterFG formatter) = FormatterFG formatter
