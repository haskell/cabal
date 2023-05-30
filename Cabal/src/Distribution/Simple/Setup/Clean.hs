{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.Simple.Setup.Clean
-- Copyright   :  Isaac Jones 2003-2004
--                Duncan Coutts 2007
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Definition of the clean command-line options.
-- See: @Distribution.Simple.Setup@
module Distribution.Simple.Setup.Clean
  ( CleanFlags (..)
  , emptyCleanFlags
  , defaultCleanFlags
  , cleanCommand
  ) where

import Distribution.Compat.Prelude hiding (get)
import Prelude ()

import Distribution.Simple.Command hiding (boolOpt, boolOpt')
import Distribution.Simple.Flag
import Distribution.Verbosity

import Distribution.Simple.Setup.Common

-- ------------------------------------------------------------

-- * Clean flags

-- ------------------------------------------------------------

data CleanFlags = CleanFlags
  { cleanSaveConf :: Flag Bool
  , cleanDistPref :: Flag FilePath
  , cleanVerbosity :: Flag Verbosity
  , cleanCabalFilePath :: Flag FilePath
  }
  deriving (Show, Generic, Typeable)

defaultCleanFlags :: CleanFlags
defaultCleanFlags =
  CleanFlags
    { cleanSaveConf = Flag False
    , cleanDistPref = NoFlag
    , cleanVerbosity = Flag normal
    , cleanCabalFilePath = mempty
    }

cleanCommand :: CommandUI CleanFlags
cleanCommand =
  CommandUI
    { commandName = "clean"
    , commandSynopsis = "Clean up after a build."
    , commandDescription = Just $ \_ ->
        "Removes .hi, .o, preprocessed sources, etc.\n"
    , commandNotes = Nothing
    , commandUsage = \pname ->
        "Usage: " ++ pname ++ " clean [FLAGS]\n"
    , commandDefaultFlags = defaultCleanFlags
    , commandOptions = \showOrParseArgs ->
        [ optionVerbosity cleanVerbosity (\v flags -> flags{cleanVerbosity = v})
        , optionDistPref
            cleanDistPref
            (\d flags -> flags{cleanDistPref = d})
            showOrParseArgs
        , option
            "s"
            ["save-configure"]
            "Do not remove the configuration file (dist/setup-config) during cleaning.  Saves need to reconfigure."
            cleanSaveConf
            (\v flags -> flags{cleanSaveConf = v})
            trueArg
        ]
    }

emptyCleanFlags :: CleanFlags
emptyCleanFlags = mempty

instance Monoid CleanFlags where
  mempty = gmempty
  mappend = (<>)

instance Semigroup CleanFlags where
  (<>) = gmappend
