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
  ( CleanFlags
      ( CleanCommonFlags
      , cleanVerbosity
      , cleanDistPref
      , cleanCabalFilePath
      , cleanWorkingDir
      , cleanTargets
      , ..
      )
  , emptyCleanFlags
  , defaultCleanFlags
  , cleanCommand
  ) where

import Distribution.Compat.Prelude hiding (get)
import Prelude ()

import Distribution.Simple.Command hiding (boolOpt, boolOpt')
import Distribution.Simple.Flag
import Distribution.Simple.Setup.Common
import Distribution.Utils.Path
import Distribution.Verbosity

-- ------------------------------------------------------------

-- * Clean flags

-- ------------------------------------------------------------

data CleanFlags = CleanFlags
  { cleanCommonFlags :: !CommonSetupFlags
  , cleanSaveConf :: Flag Bool
  }
  deriving (Show, Generic, Typeable)

pattern CleanCommonFlags
  :: Flag Verbosity
  -> Flag (SymbolicPath Pkg (Dir Dist))
  -> Flag (SymbolicPath CWD (Dir Pkg))
  -> Flag (SymbolicPath Pkg File)
  -> [String]
  -> CleanFlags
pattern CleanCommonFlags
  { cleanVerbosity
  , cleanDistPref
  , cleanWorkingDir
  , cleanCabalFilePath
  , cleanTargets
  } <-
  ( cleanCommonFlags ->
      CommonSetupFlags
        { setupVerbosity = cleanVerbosity
        , setupDistPref = cleanDistPref
        , setupWorkingDir = cleanWorkingDir
        , setupCabalFilePath = cleanCabalFilePath
        , setupTargets = cleanTargets
        }
    )

instance Binary CleanFlags
instance Structured CleanFlags

defaultCleanFlags :: CleanFlags
defaultCleanFlags =
  CleanFlags
    { cleanCommonFlags = defaultCommonSetupFlags
    , cleanSaveConf = Flag False
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
        withCommonSetupOptions
          cleanCommonFlags
          (\c f -> f{cleanCommonFlags = c})
          showOrParseArgs
          [ option
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
