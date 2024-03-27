{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

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
  ( InstallFlags
      ( InstallCommonFlags
      , installVerbosity
      , installDistPref
      , installCabalFilePath
      , installWorkingDir
      , installTargets
      , ..
      )
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
import Distribution.Simple.Setup.Common
import Distribution.Simple.Utils
import Distribution.Utils.Path
import Distribution.Verbosity

-- ------------------------------------------------------------

-- * Install flags

-- ------------------------------------------------------------

-- | Flags to @install@: (package db, verbosity)
data InstallFlags = InstallFlags
  { installCommonFlags :: !CommonSetupFlags
  , installPackageDB :: Flag PackageDB
  , installDest :: Flag CopyDest
  , installUseWrapper :: Flag Bool
  , installInPlace :: Flag Bool
  }
  deriving (Show, Generic)

pattern InstallCommonFlags
  :: Flag Verbosity
  -> Flag (SymbolicPath Pkg (Dir Dist))
  -> Flag (SymbolicPath CWD (Dir Pkg))
  -> Flag (SymbolicPath Pkg File)
  -> [String]
  -> InstallFlags
pattern InstallCommonFlags
  { installVerbosity
  , installDistPref
  , installWorkingDir
  , installCabalFilePath
  , installTargets
  } <-
  ( installCommonFlags ->
      CommonSetupFlags
        { setupVerbosity = installVerbosity
        , setupDistPref = installDistPref
        , setupWorkingDir = installWorkingDir
        , setupCabalFilePath = installCabalFilePath
        , setupTargets = installTargets
        }
    )

defaultInstallFlags :: InstallFlags
defaultInstallFlags =
  InstallFlags
    { installCommonFlags = defaultCommonSetupFlags
    , installPackageDB = NoFlag
    , installDest = Flag NoCopyDest
    , installUseWrapper = Flag False
    , installInPlace = Flag False
    }

installCommand :: CommandUI InstallFlags
installCommand =
  CommandUI
    { commandName = "install"
    , commandSynopsis =
        "Copy the files into the install locations. Run register."
    , commandDescription = Just $ \_ ->
        wrapText $
          "Unlike the copy command, install calls the register command. "
            ++ "If you want to install into a location that is not what was "
            ++ "specified in the configure step, use the copy command.\n"
    , commandNotes = Nothing
    , commandUsage = \pname ->
        "Usage: " ++ pname ++ " install [FLAGS]\n"
    , commandDefaultFlags = defaultInstallFlags
    , commandOptions = \showOrParseArgs ->
        withCommonSetupOptions
          installCommonFlags
          (\c f -> f{installCommonFlags = c})
          showOrParseArgs
          $ case showOrParseArgs of
            ShowArgs ->
              filter
                ( (`notElem` ["target-package-db"])
                    . optionName
                )
                installOptions
            ParseArgs -> installOptions
    }

installOptions :: [OptionField InstallFlags]
installOptions =
  [ option
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
