{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
module Distribution.PrettyCtx where

import Distribution.CabalSpecVersion
import Distribution.Pretty
import Distribution.Fields.Field
import Distribution.Parsec.Position

import qualified Text.PrettyPrint as PP

-- | An extended 'Pretty' class that consumes some extra context, allowing it to access associated comments.
class PrettyCtx a where
  default prettyCtx :: Pretty a => ([Comment Position], a) -> PP.Doc
  prettyCtx :: ([Comment Position], a) -> PP.Doc
  prettyCtx (_, x) = pretty x

  default prettyCtxVersioned :: Pretty a => CabalSpecVersion -> ([Comment Position], a) -> PP.Doc
  prettyCtxVersioned :: CabalSpecVersion -> ([Comment Position], a) -> PP.Doc
  prettyCtxVersioned csv (_, x) = prettyVersioned csv x

