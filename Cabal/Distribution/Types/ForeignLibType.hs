{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Types.ForeignLibType(
    ForeignLibType(..),
    knownForeignLibTypes,
    foreignLibTypeIsShared,
) where

import Prelude ()
import Distribution.Compat.Prelude

import Text.PrettyPrint hiding ((<>))
import Distribution.Text
import qualified Distribution.Compat.ReadP as Parse
import Distribution.PackageDescription.Utils

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

instance Text ForeignLibType where
  disp ForeignLibNativeShared = text "native-shared"
  disp ForeignLibNativeStatic = text "native-static"
  disp ForeignLibTypeUnknown  = text "unknown"

  parse = Parse.choice [
      do _ <- Parse.string "native-shared" ; return ForeignLibNativeShared
    , do _ <- Parse.string "native-static" ; return ForeignLibNativeStatic
    ]

instance Binary ForeignLibType

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
