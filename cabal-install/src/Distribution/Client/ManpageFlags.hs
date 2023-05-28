{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Distribution.Client.ManpageFlags
  ( ManpageFlags (..)
  , defaultManpageFlags
  , manpageOptions
  ) where

import Distribution.Client.Compat.Prelude

import Distribution.Simple.Command (OptionField (..), ShowOrParseArgs (..), option)
import Distribution.Simple.Setup (Flag (..), optionVerbosity, toFlag, trueArg)
import Distribution.Verbosity (normal)

data ManpageFlags = ManpageFlags
  { manpageVerbosity :: Flag Verbosity
  , manpageRaw :: Flag Bool
  }
  deriving (Eq, Show, Generic)

instance Monoid ManpageFlags where
  mempty = gmempty
  mappend = (<>)

instance Semigroup ManpageFlags where
  (<>) = gmappend

defaultManpageFlags :: ManpageFlags
defaultManpageFlags =
  ManpageFlags
    { manpageVerbosity = toFlag normal
    , manpageRaw = toFlag False
    }

manpageOptions :: ShowOrParseArgs -> [OptionField ManpageFlags]
manpageOptions _ =
  [ optionVerbosity manpageVerbosity (\v flags -> flags{manpageVerbosity = v})
  , option
      ""
      ["raw"]
      "Output raw troff content"
      manpageRaw
      (\v flags -> flags{manpageRaw = v})
      trueArg
  ]
