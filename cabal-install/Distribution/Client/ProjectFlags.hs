{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
module Distribution.Client.ProjectFlags (
    ProjectFlags(..),
    defaultProjectFlags,
    projectFlagsOptions,
) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.ReadE          (succeedReadE)
import Distribution.Simple.Command (OptionField, option, reqArg)
import Distribution.Simple.Setup   (Flag (..), toFlag, trueArg, flagToList)

data ProjectFlags = ProjectFlags
    { flagProjectFileName :: Flag FilePath
    , flagIgnoreProject   :: Flag Bool
    }
  deriving (Show)

defaultProjectFlags :: ProjectFlags
defaultProjectFlags = ProjectFlags
    { flagProjectFileName = mempty
    , flagIgnoreProject   = toFlag False
    }

projectFlagsOptions :: [OptionField ProjectFlags]
projectFlagsOptions =
    [ option [] ["project-file"]
        "Set the name of the cabal.project file to search for in parent directories"
        flagProjectFileName (\pf flags -> flags { flagProjectFileName = pf })
        (reqArg "FILE" (succeedReadE Flag) flagToList)
    , option ['z'] ["ignore-project"]
        "Ignore local project configuration"
        flagIgnoreProject (\v flags -> flags { flagIgnoreProject = v })
        trueArg
    ]
