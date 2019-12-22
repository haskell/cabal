{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}
module Distribution.Client.CmdRun.ClientRunFlags
( ClientRunFlags(..)
, defaultClientRunFlags
, clientRunOptions
) where

import Distribution.Client.Compat.Prelude

import Distribution.Simple.Command (OptionField (..), ShowOrParseArgs (..), option)
import Distribution.Simple.Setup   (Flag (..), toFlag, trueArg)

data ClientRunFlags = ClientRunFlags
  { crunIgnoreProject   :: Flag Bool
  } deriving (Eq, Show, Generic)

instance Monoid ClientRunFlags where
  mempty = gmempty
  mappend = (<>)

instance Semigroup ClientRunFlags where
  (<>) = gmappend

instance Binary ClientRunFlags
instance Structured ClientRunFlags

defaultClientRunFlags :: ClientRunFlags
defaultClientRunFlags = ClientRunFlags
  { crunIgnoreProject   = toFlag False
  }

clientRunOptions :: ShowOrParseArgs -> [OptionField ClientRunFlags]
clientRunOptions _ =
  [ option "z" ["ignore-project"]
    "Ignore local project configuration"
    crunIgnoreProject (\v flags -> flags { crunIgnoreProject = v })
    trueArg
  ]
