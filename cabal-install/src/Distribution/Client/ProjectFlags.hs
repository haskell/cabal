{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Distribution.Client.ProjectFlags
  ( ProjectFlags (..)
  , defaultProjectFlags
  , projectFlagsOptions
  , removeIgnoreProjectOption
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.ReadE (succeedReadE)
import Distribution.Simple.Command
  ( MkOptDescr
  , OptionField (optionName)
  , ShowOrParseArgs (..)
  , boolOpt'
  , option
  , reqArg
  )
import Distribution.Simple.Setup (Flag (..), flagToList, flagToMaybe, toFlag, trueArg)

data ProjectFlags = ProjectFlags
  { flagProjectDir :: Flag FilePath
  -- ^ The project directory.
  , flagProjectFile :: Flag FilePath
  -- ^ The cabal project file path; defaults to @cabal.project@.
  -- This path, when relative, is relative to the project directory.
  -- The filename portion of the path denotes the cabal project file name, but it also
  -- is the base of auxiliary project files, such as
  -- @cabal.project.local@ and @cabal.project.freeze@ which are also
  -- read and written out in some cases.
  -- If a project directory was not specified, and the path is not found
  -- in the current working directory, we will successively probe
  -- relative to parent directories until this name is found.
  , flagIgnoreProject :: Flag Bool
  -- ^ Whether to ignore the local project (i.e. don't search for cabal.project)
  -- The exact interpretation might be slightly different per command.
  }
  deriving (Show, Generic)

defaultProjectFlags :: ProjectFlags
defaultProjectFlags =
  ProjectFlags
    { flagProjectDir = mempty
    , flagProjectFile = mempty
    , flagIgnoreProject = toFlag False
    -- Should we use 'Last' here?
    }

projectFlagsOptions :: ShowOrParseArgs -> [OptionField ProjectFlags]
projectFlagsOptions showOrParseArgs =
  [ option
      []
      ["project-dir"]
      "Set the path of the project directory"
      flagProjectDir
      (\path flags -> flags{flagProjectDir = path})
      (reqArg "DIR" (succeedReadE Flag) flagToList)
  , option
      []
      ["project-file"]
      "Set the path of the cabal.project file (relative to the project directory when relative)"
      flagProjectFile
      (\pf flags -> flags{flagProjectFile = pf})
      (reqArg "FILE" (succeedReadE Flag) flagToList)
  , option
      ['z']
      ["ignore-project"]
      "Ignore local project configuration (unless --project-dir or --project-file is also set)"
      flagIgnoreProject
      ( \v flags ->
          flags
            { flagIgnoreProject = case v of
                Flag True -> toFlag (flagProjectDir flags == NoFlag && flagProjectFile flags == NoFlag)
                _ -> v
            }
      )
      (yesNoOpt showOrParseArgs)
  ]

-- | As almost all commands use 'ProjectFlags' but not all can honour
-- "ignore-project" flag, provide this utility to remove the flag
-- parsing from the help message.
removeIgnoreProjectOption :: [OptionField a] -> [OptionField a]
removeIgnoreProjectOption = filter (\o -> optionName o /= "ignore-project")

instance Monoid ProjectFlags where
  mempty = gmempty
  mappend = (<>)

instance Semigroup ProjectFlags where
  (<>) = gmappend

yesNoOpt :: ShowOrParseArgs -> MkOptDescr (b -> Flag Bool) (Flag Bool -> b -> b) b
yesNoOpt ShowArgs sf lf = trueArg sf lf
yesNoOpt _ sf lf = boolOpt' flagToMaybe Flag (sf, lf) ([], map ("no-" ++) lf) sf lf
