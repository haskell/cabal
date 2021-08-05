{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Distribution.Client.ProjectFlags (
    ProjectFlags(..),
    defaultProjectFlags,
    projectFlagsOptions,
) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.ReadE          (succeedReadE)
import Distribution.Simple.Command (MkOptDescr, OptionField, ShowOrParseArgs (..), boolOpt', option, reqArg)
import Distribution.Simple.Setup   (Flag (..), flagToList, flagToMaybe, toFlag, trueArg)

data ProjectFlags = ProjectFlags
    { flagProjectFileName :: Flag FilePath
      -- ^ The cabal project file name; defaults to @cabal.project@.
      -- The name itself denotes the cabal project file name, but it also
      -- is the base of auxiliary project files, such as
      -- @cabal.project.local@ and @cabal.project.freeze@ which are also
      -- read and written out in some cases.  If the path is not found
      -- in the current working directory, we will successively probe
      -- relative to parent directories until this name is found.

    , flagIgnoreProject   :: Flag Bool
      -- ^ Whether to ignore the local project (i.e. don't search for cabal.project)
      -- The exact interpretation might be slightly different per command.
    }
  deriving (Show, Generic)

defaultProjectFlags :: ProjectFlags
defaultProjectFlags = ProjectFlags
    { flagProjectFileName = mempty
    , flagIgnoreProject   = toFlag False
      -- Should we use 'Last' here?
    }

projectFlagsOptions :: ShowOrParseArgs -> [OptionField ProjectFlags]
projectFlagsOptions showOrParseArgs =
    [ option [] ["project-file"]
        "Set the name of the cabal.project file to search for in parent directories"
        flagProjectFileName (\pf flags -> flags { flagProjectFileName = pf })
        (reqArg "FILE" (succeedReadE Flag) flagToList)
    , option ['z'] ["ignore-project"]
        "Ignore local project configuration"
        flagIgnoreProject (\v flags -> flags { flagIgnoreProject = v })
        (yesNoOpt showOrParseArgs)
    ]

instance Monoid ProjectFlags where
    mempty = gmempty
    mappend = (<>)

instance Semigroup ProjectFlags where
    (<>) = gmappend

yesNoOpt :: ShowOrParseArgs -> MkOptDescr (b -> Flag Bool) (Flag Bool -> b -> b) b
yesNoOpt ShowArgs sf lf = trueArg sf lf
yesNoOpt _        sf lf = boolOpt' flagToMaybe Flag (sf, lf) ([], map ("no-" ++) lf) sf lf
