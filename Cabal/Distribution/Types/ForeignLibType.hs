{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Distribution.Types.ForeignLibType(
    ForeignLibType(..),
    knownForeignLibTypes,
    foreignLibTypeIsShared,
) where

import Prelude ()
import Distribution.Compat.Prelude
import Distribution.PackageDescription.Utils

import Distribution.Pretty
import Distribution.Parsec
import Distribution.FieldGrammar.Described

import qualified Distribution.Compat.CharParsing as P
import qualified Text.PrettyPrint as Disp

-- | What kind of foreign library is to be built?
data ForeignLibType =
      -- | A native shared library (@.so@ on Linux, @.dylib@ on OSX, or
      -- @.dll@ on Windows).
      ForeignLibNativeShared
      -- | A native static library (not currently supported.)
    | ForeignLibNativeStatic
      -- TODO: Maybe this should record a string?
    | ForeignLibTypeUnknown
    deriving (Generic, Show, Read, Eq, Typeable, Data)

instance Pretty ForeignLibType where
  pretty ForeignLibNativeShared = Disp.text "native-shared"
  pretty ForeignLibNativeStatic = Disp.text "native-static"
  pretty ForeignLibTypeUnknown  = Disp.text "unknown"

instance Parsec ForeignLibType where
  parsec = do
    name <- P.munch1 (\c -> isAlphaNum c || c == '-')
    return $ case name of
      "native-shared" -> ForeignLibNativeShared
      "native-static" -> ForeignLibNativeStatic
      _               -> ForeignLibTypeUnknown

instance Described ForeignLibType where
  describe _ = REUnion ["native-shared","native-static"]

instance Binary ForeignLibType
instance Structured ForeignLibType
instance NFData ForeignLibType where rnf = genericRnf

instance Semigroup ForeignLibType where
  ForeignLibTypeUnknown <> b = b
  a <> ForeignLibTypeUnknown = a
  _ <> _ = error "Ambiguous foreign library type"

instance Monoid ForeignLibType where
  mempty = ForeignLibTypeUnknown
  mappend = (<>)

knownForeignLibTypes :: [ForeignLibType]
knownForeignLibTypes = [
      ForeignLibNativeShared
    , ForeignLibNativeStatic
    ]

foreignLibTypeIsShared :: ForeignLibType -> Bool
foreignLibTypeIsShared t =
    case t of
      ForeignLibNativeShared -> True
      ForeignLibNativeStatic -> False
      ForeignLibTypeUnknown  -> cabalBug "Unknown foreign library type"
