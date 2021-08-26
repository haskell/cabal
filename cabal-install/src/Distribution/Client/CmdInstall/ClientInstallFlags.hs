{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module Distribution.Client.CmdInstall.ClientInstallFlags
( InstallMethod(..)
, ClientInstallFlags(..)
, defaultClientInstallFlags
, clientInstallOptions
) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.ReadE
         ( succeedReadE, parsecToReadE )
import Distribution.Simple.Command
         ( ShowOrParseArgs(..), OptionField(..), option, reqArg )
import Distribution.Simple.Setup
         ( Flag(..), trueArg, flagToList, toFlag )

import Distribution.Client.Types.InstallMethod
         ( InstallMethod (..) )
import Distribution.Client.Types.OverwritePolicy
         ( OverwritePolicy(..) )

import qualified Distribution.Compat.CharParsing as P

data ClientInstallFlags = ClientInstallFlags
  { cinstInstallLibs     :: Flag Bool
  , cinstEnvironmentPath :: Flag FilePath
  , cinstOverwritePolicy :: Flag OverwritePolicy
  , cinstInstallMethod   :: Flag InstallMethod
  , cinstInstalldir      :: Flag FilePath
  } deriving (Eq, Show, Generic)

instance Monoid ClientInstallFlags where
  mempty = gmempty
  mappend = (<>)

instance Semigroup ClientInstallFlags where
  (<>) = gmappend

instance Binary ClientInstallFlags
instance Structured ClientInstallFlags

defaultClientInstallFlags :: ClientInstallFlags
defaultClientInstallFlags = ClientInstallFlags
  { cinstInstallLibs     = toFlag False
  , cinstEnvironmentPath = mempty
  , cinstOverwritePolicy = mempty
  , cinstInstallMethod   = mempty
  , cinstInstalldir      = mempty
  }

clientInstallOptions :: ShowOrParseArgs -> [OptionField ClientInstallFlags]
clientInstallOptions _ =
  [ option [] ["lib"]
    ( "Install libraries rather than executables from the target package " <>
      "(provisional, see https://github.com/haskell/cabal/issues/6481 for more information)." )
    cinstInstallLibs (\v flags -> flags { cinstInstallLibs = v })
    trueArg
  , option [] ["package-env", "env"]
    "Set the environment file that may be modified."
    cinstEnvironmentPath (\pf flags -> flags { cinstEnvironmentPath = pf })
    (reqArg "ENV" (succeedReadE Flag) flagToList)
  , option [] ["overwrite-policy"]
    "How to handle already existing symlinks."
    cinstOverwritePolicy (\v flags -> flags { cinstOverwritePolicy = v })
    $ reqArg "always|never"
        (parsecToReadE (\err -> "Error parsing overwrite-policy: " ++ err) (toFlag `fmap` parsec)) 
        (map prettyShow . flagToList)
  , option [] ["install-method"]
    "How to install the executables."
    cinstInstallMethod (\v flags -> flags { cinstInstallMethod = v })
    $ reqArg
        "default|copy|symlink"
        (parsecToReadE (\err -> "Error parsing install-method: " ++ err) (toFlag `fmap` parsecInstallMethod))
        (map prettyShow . flagToList)
  , option [] ["installdir"]
    "Where to install (by symlinking or copying) the executables in."
    cinstInstalldir (\v flags -> flags { cinstInstalldir = v })
    $ reqArg "DIR" (succeedReadE Flag) flagToList
  ]

parsecInstallMethod :: CabalParsing m => m InstallMethod
parsecInstallMethod = do
    name <- P.munch1 isAlpha
    case name of
        "copy"    -> pure InstallMethodCopy
        "symlink" -> pure InstallMethodSymlink
        _         -> P.unexpected $ "InstallMethod: " ++ name
