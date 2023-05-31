{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.Simple.Setup.Install
-- Copyright   :  Isaac Jones 2003-2004
--                Duncan Coutts 2007
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Definition of the install command-line options.
-- See: @Distribution.Simple.Setup@
module Distribution.Simple.Setup.Install
  ( InstallFlags (..)
  , emptyInstallFlags
  , defaultInstallFlags
  , installCommand
  ) where

import Distribution.Compat.Prelude hiding (get)
import Prelude ()

import Distribution.ReadE
import Distribution.Simple.Command hiding (boolOpt, boolOpt')
import Distribution.Simple.Compiler
import Distribution.Simple.Flag
import Distribution.Simple.InstallDirs
import Distribution.Simple.Utils
import Distribution.Verbosity

import Distribution.Simple.Setup.Common

-- ------------------------------------------------------------

-- * Install flags

-- ------------------------------------------------------------

-- | Flags to @install@: (package db, verbosity)
data InstallFlags = InstallFlags
  { installPackageDB :: Flag PackageDB
  , installDest :: Flag CopyDest
  , installDistPref :: Flag FilePath
  , installUseWrapper :: Flag Bool
  , installInPlace :: Flag Bool
  , installVerbosity :: Flag Verbosity
  , -- this is only here, because we can not
    -- change the hooks API.
    installCabalFilePath :: Flag FilePath
  }
  deriving (Show, Generic)

defaultInstallFlags :: InstallFlags
defaultInstallFlags =
  InstallFlags
    { installPackageDB = NoFlag
    , installDest = Flag NoCopyDest
    , installDistPref = NoFlag
    , installUseWrapper = Flag False
    , installInPlace = Flag False
    , installVerbosity = Flag normal
    , installCabalFilePath = mempty
    }

installCommand :: CommandUI InstallFlags
installCommand =
  CommandUI
    { commandName = "install"
    , commandSynopsis =
        "Copy the files into the install locations. Run register."
    , commandDescription = Just $ \_ ->
        wrapText $
          "Unlike the copy command, install calls the register command."
            ++ "If you want to install into a location that is not what was"
            ++ "specified in the configure step, use the copy command.\n"
    , commandNotes = Nothing
    , commandUsage = \pname ->
        "Usage: " ++ pname ++ " install [FLAGS]\n"
    , commandDefaultFlags = defaultInstallFlags
    , commandOptions = \showOrParseArgs -> case showOrParseArgs of
        ShowArgs ->
          filter
            ( (`notElem` ["target-package-db"])
                . optionName
            )
            $ installOptions ShowArgs
        ParseArgs -> installOptions ParseArgs
    }

installOptions :: ShowOrParseArgs -> [OptionField InstallFlags]
installOptions showOrParseArgs =
  [ optionVerbosity installVerbosity (\v flags -> flags{installVerbosity = v})
  , optionDistPref
      installDistPref
      (\d flags -> flags{installDistPref = d})
      showOrParseArgs
  , option
      ""
      ["inplace"]
      "install the package in the install subdirectory of the dist prefix, so it can be used without being installed"
      installInPlace
      (\v flags -> flags{installInPlace = v})
      trueArg
  , option
      ""
      ["shell-wrappers"]
      "using shell script wrappers around executables"
      installUseWrapper
      (\v flags -> flags{installUseWrapper = v})
      (boolOpt [] [])
  , option
      ""
      ["package-db"]
      ""
      installPackageDB
      (\v flags -> flags{installPackageDB = v})
      ( choiceOpt
          [
            ( Flag UserPackageDB
            , ([], ["user"])
            , "upon configuration register this package in the user's local package database"
            )
          ,
            ( Flag GlobalPackageDB
            , ([], ["global"])
            , "(default) upon configuration register this package in the system-wide package database"
            )
          ]
      )
  , option
      ""
      ["target-package-db"]
      "package database to install into. Required when using ${pkgroot} prefix."
      installDest
      (\v flags -> flags{installDest = v})
      ( reqArg
          "DATABASE"
          (succeedReadE (Flag . CopyToDb))
          (\f -> case f of Flag (CopyToDb p) -> [p]; _ -> [])
      )
  ]

emptyInstallFlags :: InstallFlags
emptyInstallFlags = mempty

instance Monoid InstallFlags where
  mempty = gmempty
  mappend = (<>)

instance Semigroup InstallFlags where
  (<>) = gmappend
