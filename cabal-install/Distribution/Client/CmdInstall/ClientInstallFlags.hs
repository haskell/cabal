{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module Distribution.Client.CmdInstall.ClientInstallFlags
( InstallMethod(..)
, ClientInstallFlags(..)
, defaultClientInstallFlags
, clientInstallOptions
) where

import Distribution.Client.Compat.Prelude

import Distribution.ReadE
         ( ReadE(..), succeedReadE )
import Distribution.Simple.Command
         ( ShowOrParseArgs(..), OptionField(..), option, reqArg )
import Distribution.Simple.Setup
         ( Flag(..), boolOpt, boolOpt', flagToList, toFlag )

import Distribution.Client.InstallSymlink
         ( OverwritePolicy(..) )


data InstallMethod = InstallMethodCopy
                   | InstallMethodSymlink
  deriving (Eq, Show, Generic, Bounded, Enum)

instance Binary InstallMethod

data ClientInstallFlags = ClientInstallFlags
  { cinstInstallExes :: Flag Bool
  , cinstInstallLibs :: Flag Bool
  , cinstInstallFLibs :: Flag Bool
  , cinstEnvironmentPath :: Flag FilePath
  , cinstOverwritePolicy :: Flag OverwritePolicy
  , cinstInstallMethod :: Flag InstallMethod
  , cinstInstallDir :: Flag FilePath
  , cinstFLibInstallDir :: Flag FilePath
  } deriving (Eq, Show, Generic)

instance Monoid ClientInstallFlags where
  mempty = gmempty
  mappend = (<>)

instance Semigroup ClientInstallFlags where
  (<>) = gmappend

instance Binary ClientInstallFlags

defaultClientInstallFlags :: ClientInstallFlags
defaultClientInstallFlags = ClientInstallFlags
  { cinstInstallExes = toFlag True
  , cinstInstallLibs = toFlag False
  , cinstInstallFLibs = toFlag False
  , cinstEnvironmentPath = mempty
  , cinstOverwritePolicy = mempty
  , cinstInstallMethod = mempty
  , cinstInstallDir = mempty
  , cinstFLibInstallDir = mempty
  }

clientInstallOptions :: ShowOrParseArgs -> [OptionField ClientInstallFlags]
clientInstallOptions _ =
  [ option [] ["exes"]
    "installing executables from the target package."
    cinstInstallExes (\v flags -> flags { cinstInstallExes = v })
    (boolOpt [] [])
  , option [] ["libs"]
    "installing libraries from the target package."
    cinstInstallLibs (\v flags -> flags { cinstInstallLibs = v })
    (boolOpt' ([], ["enable-libs", "lib"]) ([], ["disable-libs"]))
  , option [] ["foreign-libs"]
    "installing foreign libraries from the target package."
    cinstInstallFLibs (\v flags -> flags { cinstInstallFLibs = v })
    (boolOpt [] [])
  , option [] ["package-env", "env"]
    "Set the environment file that may be modified."
    cinstEnvironmentPath (\pf flags -> flags { cinstEnvironmentPath = pf })
    (reqArg "ENV" (succeedReadE Flag) flagToList)
  , option [] ["overwrite-policy"]
    "How to handle already existing symlinks."
    cinstOverwritePolicy (\v flags -> flags { cinstOverwritePolicy = v })
    $ reqArg
        "always|never"
        readOverwritePolicyFlag
        showOverwritePolicyFlag
  , option [] ["install-method"]
    "How to install the executables."
    cinstInstallMethod (\v flags -> flags { cinstInstallMethod = v })
    $ reqArg
        "copy|symlink"
        readInstallMethodFlag
        showInstallMethodFlag
  , option [] ["installdir"]
    "Where to install (by symlinking or copying) executables."
    cinstInstallDir (\v flags -> flags { cinstInstallDir = v })
    $ reqArg "DIR" (succeedReadE Flag) flagToList
  , option [] ["lib-installdir"]
    "Where to install (by symlinking or copying) foreign libraries."
    cinstFLibInstallDir (\v flags -> flags { cinstFLibInstallDir = v })
    $ reqArg "DIR" (succeedReadE Flag) flagToList
  ]

readOverwritePolicyFlag :: ReadE (Flag OverwritePolicy)
readOverwritePolicyFlag = ReadE $ \case
  "always" -> Right $ Flag AlwaysOverwrite
  "never"  -> Right $ Flag NeverOverwrite
  policy   -> Left  $ "'" <> policy <> "' isn't a valid overwrite policy"

showOverwritePolicyFlag :: Flag OverwritePolicy -> [String]
showOverwritePolicyFlag (Flag AlwaysOverwrite) = ["always"]
showOverwritePolicyFlag (Flag NeverOverwrite)  = ["never"]
showOverwritePolicyFlag NoFlag                 = []

readInstallMethodFlag :: ReadE (Flag InstallMethod)
readInstallMethodFlag = ReadE $ \case
  "copy"    -> Right $ Flag InstallMethodCopy
  "symlink" -> Right $ Flag InstallMethodSymlink
  method    -> Left  $ "'" <> method <> "' isn't a valid install-method"

showInstallMethodFlag :: Flag InstallMethod -> [String]
showInstallMethodFlag (Flag InstallMethodCopy)    = ["copy"]
showInstallMethodFlag (Flag InstallMethodSymlink) = ["symlink"]
showInstallMethodFlag NoFlag                      = []
