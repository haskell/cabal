{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
module Distribution.PrettyCtx where

import Distribution.Compat.Prelude
import Prelude ()
import Distribution.CabalSpecVersion
import Distribution.Pretty
import Distribution.Fields.Field
import Distribution.Parsec.Position

import Debug.Pretty.Simple

import qualified Text.PrettyPrint as PP

-- | An extended 'Pretty' class that consumes some extra context, allowing it to access associated comments.
class PrettyCtx a where
  default prettyCtx :: Pretty a => ([Comment Position], a) -> PP.Doc
  prettyCtx :: ([Comment Position], a) -> PP.Doc
  prettyCtx (cmt, x) =
      pTraceShow (show (cmt, pretty x)) $
        pretty x

  default prettyCtxVersioned :: Pretty a => CabalSpecVersion -> ([Comment Position], a) -> PP.Doc
  prettyCtxVersioned :: CabalSpecVersion -> ([Comment Position], a) -> PP.Doc
  prettyCtxVersioned csv (cmt, x) =
      pTraceShow (show (cmt, prettyVersioned csv x)) $
        prettyVersioned csv x


instance Pretty a => PrettyCtx (Identity a)
