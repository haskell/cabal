{-# LANGUAGE FlexibleInstances #-}

module Distribution.PrettyCtx
  ( PrettyCtx (..)
  , defaultPrettyCtx
  , defaultPrettyCtxVersioned
  )
  where

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
  prettyCtx :: ([Comment Position], a) -> PP.Doc
  prettyCtxVersioned :: CabalSpecVersion -> ([Comment Position], a) -> PP.Doc

defaultPrettyCtx :: Pretty a => ([Comment Position], a) -> PP.Doc
defaultPrettyCtx (_, x) = pretty x

defaultPrettyCtxVersioned :: Pretty a => CabalSpecVersion -> ([Comment Position], a) -> PP.Doc
defaultPrettyCtxVersioned csv (_, x) = prettyVersioned csv x

instance Pretty a => PrettyCtx (Identity a) where
  prettyCtx = defaultPrettyCtx
  prettyCtxVersioned = defaultPrettyCtxVersioned
