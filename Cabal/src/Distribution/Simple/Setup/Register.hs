{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.Simple.Setup.Register
-- Copyright   :  Isaac Jones 2003-2004
--                Duncan Coutts 2007
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Definition of the register command-line options.
-- See: @Distribution.Simple.Setup@
module Distribution.Simple.Setup.Register
  ( RegisterFlags (..)
  , emptyRegisterFlags
  , defaultRegisterFlags
  , registerCommand
  , unregisterCommand
  ) where

import Distribution.Compat.Prelude hiding (get)
import Prelude ()

import Distribution.Simple.Command hiding (boolOpt, boolOpt')
import Distribution.Simple.Compiler
import Distribution.Simple.Flag
import Distribution.Verbosity

import Distribution.Simple.Setup.Common

-- ------------------------------------------------------------

-- * Register flags

-- ------------------------------------------------------------

-- | Flags to @register@ and @unregister@: (user package, gen-script,
-- in-place, verbosity)
data RegisterFlags = RegisterFlags
  { regPackageDB :: Flag PackageDB
  , regGenScript :: Flag Bool
  , regGenPkgConf :: Flag (Maybe FilePath)
  , regInPlace :: Flag Bool
  , regDistPref :: Flag FilePath
  , regPrintId :: Flag Bool
  , regVerbosity :: Flag Verbosity
  , -- Same as in 'buildArgs' and 'copyArgs'
    regArgs :: [String]
  , regCabalFilePath :: Flag FilePath
  }
  deriving (Show, Generic, Typeable)

defaultRegisterFlags :: RegisterFlags
defaultRegisterFlags =
  RegisterFlags
    { regPackageDB = NoFlag
    , regGenScript = Flag False
    , regGenPkgConf = NoFlag
    , regInPlace = Flag False
    , regDistPref = NoFlag
    , regPrintId = Flag False
    , regArgs = []
    , regCabalFilePath = mempty
    , regVerbosity = Flag normal
    }

registerCommand :: CommandUI RegisterFlags
registerCommand =
  CommandUI
    { commandName = "register"
    , commandSynopsis =
        "Register this package with the compiler."
    , commandDescription = Nothing
    , commandNotes = Nothing
    , commandUsage = \pname ->
        "Usage: " ++ pname ++ " register [FLAGS]\n"
    , commandDefaultFlags = defaultRegisterFlags
    , commandOptions = \showOrParseArgs ->
        [ optionVerbosity regVerbosity (\v flags -> flags{regVerbosity = v})
        , optionDistPref
            regDistPref
            (\d flags -> flags{regDistPref = d})
            showOrParseArgs
        , option
            ""
            ["packageDB"]
            ""
            regPackageDB
            (\v flags -> flags{regPackageDB = v})
            ( choiceOpt
                [
                  ( Flag UserPackageDB
                  , ([], ["user"])
                  , "upon registration, register this package in the user's local package database"
                  )
                ,
                  ( Flag GlobalPackageDB
                  , ([], ["global"])
                  , "(default)upon registration, register this package in the system-wide package database"
                  )
                ]
            )
        , option
            ""
            ["inplace"]
            "register the package in the build location, so it can be used without being installed"
            regInPlace
            (\v flags -> flags{regInPlace = v})
            trueArg
        , option
            ""
            ["gen-script"]
            "instead of registering, generate a script to register later"
            regGenScript
            (\v flags -> flags{regGenScript = v})
            trueArg
        , option
            ""
            ["gen-pkg-config"]
            "instead of registering, generate a package registration file/directory"
            regGenPkgConf
            (\v flags -> flags{regGenPkgConf = v})
            (optArg' "PKG" Flag flagToList)
        , option
            ""
            ["print-ipid"]
            "print the installed package ID calculated for this package"
            regPrintId
            (\v flags -> flags{regPrintId = v})
            trueArg
        ]
    }

unregisterCommand :: CommandUI RegisterFlags
unregisterCommand =
  CommandUI
    { commandName = "unregister"
    , commandSynopsis =
        "Unregister this package with the compiler."
    , commandDescription = Nothing
    , commandNotes = Nothing
    , commandUsage = \pname ->
        "Usage: " ++ pname ++ " unregister [FLAGS]\n"
    , commandDefaultFlags = defaultRegisterFlags
    , commandOptions = \showOrParseArgs ->
        [ optionVerbosity regVerbosity (\v flags -> flags{regVerbosity = v})
        , optionDistPref
            regDistPref
            (\d flags -> flags{regDistPref = d})
            showOrParseArgs
        , option
            ""
            ["user"]
            ""
            regPackageDB
            (\v flags -> flags{regPackageDB = v})
            ( choiceOpt
                [
                  ( Flag UserPackageDB
                  , ([], ["user"])
                  , "unregister this package in the user's local package database"
                  )
                ,
                  ( Flag GlobalPackageDB
                  , ([], ["global"])
                  , "(default) unregister this package in the  system-wide package database"
                  )
                ]
            )
        , option
            ""
            ["gen-script"]
            "Instead of performing the unregister command, generate a script to unregister later"
            regGenScript
            (\v flags -> flags{regGenScript = v})
            trueArg
        ]
    }

emptyRegisterFlags :: RegisterFlags
emptyRegisterFlags = mempty

instance Monoid RegisterFlags where
  mempty = gmempty
  mappend = (<>)

instance Semigroup RegisterFlags where
  (<>) = gmappend
