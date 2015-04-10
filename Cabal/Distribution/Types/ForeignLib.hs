{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Types.ForeignLib(
    ForeignLib(..),
    emptyForeignLib,
    foreignLibModules,
    foreignLibIsShared,
) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.ModuleName

import Distribution.Types.BuildInfo
import Distribution.Types.ForeignLibType
import Distribution.Types.ForeignLibOption

-- | A foreign library stanza is like a library stanza, except that
-- the built code is intended for consumption by a non-Haskell client.
data ForeignLib = ForeignLib {
      -- | Name of the foreign library
      foreignLibName       :: String
      -- | What kind of foreign library is this (static or dynamic).
    , foreignLibType       :: ForeignLibType
      -- | What options apply to this foreign library (e.g., are we
      -- merging in all foreign dependencies.)
    , foreignLibOptions    :: [ForeignLibOption]
      -- | Build information for this foreign library.
    , foreignLibBuildInfo  :: BuildInfo

      -- | (Windows-specific) module definition files
      --
      -- This is a list rather than a maybe field so that we can flatten
      -- the condition trees (for instance, when creating an sdist)
    , foreignLibModDefFile :: [FilePath]
    }
    deriving (Generic, Show, Read, Eq, Typeable, Data)

instance Binary ForeignLib

instance Semigroup ForeignLib where
  a <> b = ForeignLib {
      foreignLibName       = combine' foreignLibName
    , foreignLibType       = combine  foreignLibType
    , foreignLibOptions    = combine  foreignLibOptions
    , foreignLibBuildInfo  = combine  foreignLibBuildInfo
    , foreignLibModDefFile = combine  foreignLibModDefFile
    }
    where combine field = field a `mappend` field b
          combine' field = case (field a, field b) of
            ("","") -> ""
            ("", x) -> x
            (x, "") -> x
            (x, y) -> error $ "Ambiguous values for foreign library field: '"
                        ++ x ++ "' and '" ++ y ++ "'"

instance Monoid ForeignLib where
  mempty = ForeignLib {
      foreignLibName       = mempty
    , foreignLibType       = ForeignLibTypeUnknown
    , foreignLibOptions    = []
    , foreignLibBuildInfo  = mempty
    , foreignLibModDefFile = []
    }
  mappend = (<>)

-- | An empty foreign library.
emptyForeignLib :: ForeignLib
emptyForeignLib = mempty

-- | Modules defined by a foreign library.
foreignLibModules :: ForeignLib -> [ModuleName]
foreignLibModules = otherModules . foreignLibBuildInfo

-- | Is the foreign library shared?
foreignLibIsShared :: ForeignLib -> Bool
foreignLibIsShared = foreignLibTypeIsShared . foreignLibType
