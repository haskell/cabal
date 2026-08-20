{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UndecidableInstances #-}

{- | This is a field formatter directed by 
-}
module Distribution.FieldGrammar.Format where

import Distribution.FieldGrammar.Class
import Distribution.FieldGrammar.Parsec
import Distribution.Fields.Transform
import Distribution.Fields.Field
import Distribution.Parsec
import Distribution.Pretty

import Data.Coerce
import Data.Kind

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Distribution.Compat.Lens
import Distribution.Parsec.FieldLineStream
import Distribution.CabalSpecVersion

data FormatterFieldGrammar s a = FormatterFG
  { fieldGrammarFormatter
      :: CabalSpecVersion
      -> Map FieldName ([FieldLine (WithComments Position)] -> [FieldLine (WithComments Position)])
  }
  deriving (Functor)

-- TODO(lena8959): use in the cabal spec version
formatFieldGrammar :: CabalSpecVersion -> FormatterFieldGrammar s a -> [Field (WithComments Position)] -> [Field (WithComments Position)]
formatFieldGrammar spec (FormatterFG versionedFormatter) = go
  where
    go [] = []
    go (section@(Section{}) : fs) = section : go fs
    go (Field colonPos fname fls : fs) =
      let newField = case versionedFormatter spec M.!? getName fname of
            Just f -> Field colonPos fname (f fls)
            Nothing -> Field colonPos fname fls
       in newField : go fs

-- TODO(leana8959): how to do this properly
class (Pretty a, Parsec a) => Formattable a
instance (Pretty a, Parsec a) => Formattable a

instance Applicative (FormatterFieldGrammar s) where
  pure _ = FormatterFG $ \_ -> M.empty
  FormatterFG x <*> FormatterFG y = FormatterFG (x <> y)



-- | Naive implementation, doesn't put comments back.
formatFieldLines
  :: forall (a :: Type)
   . (Parsec a, Pretty a)
  => (CabalSpecVersion -> [FieldLine (WithComments Position)] -> [FieldLine (WithComments Position)])
formatFieldLines = formatFieldLinesAla @a @a

-- | Naive implementation, doesn't put comments back.
formatFieldLinesAla
  :: forall (b :: Type) (a :: Type)
   . (Coercible a b, Parsec b, Pretty b)
  => (CabalSpecVersion -> [FieldLine (WithComments Position)] -> [FieldLine (WithComments Position)])
formatFieldLinesAla spec fls = case modifyValueAtomAla @b @a Just spec fls of
    EditOk ok -> ok
    EditUnchanged u -> u
    EditErr _ ->  error $ "formatting failed"

instance FieldGrammar Formattable FormatterFieldGrammar where
  uniqueFieldAla
    :: forall b a s proxy. (Formattable b, Coercible a b) => FieldName -> proxy a b -> ALens' s a -> FormatterFieldGrammar s a
  uniqueFieldAla fn _ _extract = FormatterFG $ \spec -> M.singleton fn $ formatFieldLinesAla @b @a spec

  booleanFieldDef fn _ _def = FormatterFG $ \spec -> M.singleton fn $ formatFieldLines @Bool spec

  optionalFieldAla
    :: forall b a s proxy. (Formattable b, Coercible a b) => FieldName -> proxy a b -> ALens' s (Maybe a) -> FormatterFieldGrammar s (Maybe a)
  optionalFieldAla fn _ _extract = FormatterFG $ \spec -> M.singleton fn $ formatFieldLinesAla @b @a spec

  optionalFieldDefAla
    :: forall b a s proxy. (Formattable b, Coercible a b) => FieldName -> proxy a b -> ALens' s a -> a -> FormatterFieldGrammar s a
  optionalFieldDefAla fn _ _extract _def = FormatterFG $ \spec -> M.singleton fn $ formatFieldLinesAla @b @a spec

  monoidalFieldAla
    :: forall b a proxy s. (Formattable b, Coercible a b) => FieldName -> proxy a b -> ALens' s a -> FormatterFieldGrammar s a
  monoidalFieldAla fn _ _extract = FormatterFG $ \spec -> M.singleton fn $ formatFieldLinesAla @b @a spec

  -- Nothing to format?
  freeTextField _fn _ = FormatterFG $ \_ -> M.empty
  freeTextFieldDef _fn _ = FormatterFG $ \_ ->  M.empty
  freeTextFieldDefST _fn _ = FormatterFG $ \_ ->  M.empty
  prefixedFields _fn _ = FormatterFG $ \_ ->  M.empty

  knownField _ = FormatterFG $ \_ ->  M.empty
  hiddenField _ = FormatterFG $ \_ ->  M.empty

  deprecatedSince _ _ = id
  removedIn _ _ = id
  availableSince _ _ = id
  availableSinceWarn _ = id

  blurFieldGrammar _ (FormatterFG formatter) = FormatterFG formatter
