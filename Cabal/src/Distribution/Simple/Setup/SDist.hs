{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.Simple.Setup.SDist
-- Copyright   :  Isaac Jones 2003-2004
--                Duncan Coutts 2007
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Definition of the sdist command-line options.
-- See: @Distribution.Simple.Setup@
module Distribution.Simple.Setup.SDist
  ( SDistFlags (..)
  , emptySDistFlags
  , defaultSDistFlags
  , sdistCommand
  ) where

import Distribution.Compat.Prelude hiding (get)
import Prelude ()

import Distribution.Simple.Command hiding (boolOpt, boolOpt')
import Distribution.Simple.Flag
import Distribution.Verbosity

import Distribution.Simple.Setup.Common

-- ------------------------------------------------------------

-- * SDist flags

-- ------------------------------------------------------------

-- | Flags to @sdist@: (snapshot, verbosity)
data SDistFlags = SDistFlags
  { sDistSnapshot :: Flag Bool
  , sDistDirectory :: Flag FilePath
  , sDistDistPref :: Flag FilePath
  , sDistListSources :: Flag FilePath
  , sDistVerbosity :: Flag Verbosity
  }
  deriving (Show, Generic, Typeable)

defaultSDistFlags :: SDistFlags
defaultSDistFlags =
  SDistFlags
    { sDistSnapshot = Flag False
    , sDistDirectory = mempty
    , sDistDistPref = NoFlag
    , sDistListSources = mempty
    , sDistVerbosity = Flag normal
    }

sdistCommand :: CommandUI SDistFlags
sdistCommand =
  CommandUI
    { commandName = "sdist"
    , commandSynopsis =
        "Generate a source distribution file (.tar.gz)."
    , commandDescription = Nothing
    , commandNotes = Nothing
    , commandUsage = \pname ->
        "Usage: " ++ pname ++ " sdist [FLAGS]\n"
    , commandDefaultFlags = defaultSDistFlags
    , commandOptions = \showOrParseArgs ->
        [ optionVerbosity sDistVerbosity (\v flags -> flags{sDistVerbosity = v})
        , optionDistPref
            sDistDistPref
            (\d flags -> flags{sDistDistPref = d})
            showOrParseArgs
        , option
            ""
            ["list-sources"]
            "Just write a list of the package's sources to a file"
            sDistListSources
            (\v flags -> flags{sDistListSources = v})
            (reqArgFlag "FILE")
        , option
            ""
            ["snapshot"]
            "Produce a snapshot source distribution"
            sDistSnapshot
            (\v flags -> flags{sDistSnapshot = v})
            trueArg
        , option
            ""
            ["output-directory"]
            ( "Generate a source distribution in the given directory, "
                ++ "without creating a tarball"
            )
            sDistDirectory
            (\v flags -> flags{sDistDirectory = v})
            (reqArgFlag "DIR")
        ]
    }

emptySDistFlags :: SDistFlags
emptySDistFlags = mempty

instance Monoid SDistFlags where
  mempty = gmempty
  mappend = (<>)

instance Semigroup SDistFlags where
  (<>) = gmappend
