{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE LambdaCase         #-}

module Distribution.Client.ManpageFlags
( ManpageFlags (..)
, defaultManpageFlags
, manpageOptions,
) where

import Distribution.Client.Compat.Prelude

import Distribution.Simple.Command (OptionField (..), ShowOrParseArgs (..), option)
import Distribution.Simple.Setup   (Flag (..), toFlag, trueArg, optionVerbosity)
import Distribution.Verbosity      (normal)

data ManpageFlags = ManpageFlags
  { manpageVerbosity :: Flag Verbosity
  , manpageRaw       :: Flag Bool
  }
  deriving
  stock (Eq, Show, Generic)

  deriving
    (Semigroup, Monoid)
  via GenericMonoid ManpageFlags

defaultManpageFlags :: ManpageFlags
defaultManpageFlags = ManpageFlags
    { manpageVerbosity = toFlag normal
    , manpageRaw       = toFlag False
    }

manpageOptions :: ShowOrParseArgs -> [OptionField ManpageFlags]
manpageOptions _ =
    [ optionVerbosity manpageVerbosity (\v flags -> flags { manpageVerbosity = v })
    , option "" ["raw"]
      "Output raw troff content"
      manpageRaw (\v flags -> flags { manpageRaw = v })
      trueArg
    ]
