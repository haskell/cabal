{-# LANGUAGE DeriveGeneric #-}

module Distribution.Types.ExposedModule where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Backpack
import Distribution.ModuleName
import Distribution.Parsec
import Distribution.Pretty

import Distribution.Types.Annotation

import qualified Distribution.Compat.CharParsing as P
import qualified Text.PrettyPrint as Disp

data ExposedModule = ExposedModule
  { exposedName :: ModuleName
  , exposedReexport :: Maybe OpenModule
  }
  deriving (Eq, Generic, Read, Show, Ord)

instance Markable ExposedModule

instance Pretty ExposedModule where
  pretty (ExposedModule m reexport) =
    Disp.hsep
      [ pretty m
      , case reexport of
          Just m' -> Disp.hsep [Disp.text "from", pretty m']
          Nothing -> Disp.empty
      ]

instance ExactPretty ExposedModule where
  

instance Parsec ExposedModule where
  parsec = do
    m <- parsecMaybeQuoted parsec
    P.spaces

    reexport <- P.optional $ do
      _ <- P.string "from"
      P.skipSpaces1
      parsec

    return (ExposedModule m reexport)

instance Binary ExposedModule
instance Structured ExposedModule
instance NFData ExposedModule where rnf = genericRnf
