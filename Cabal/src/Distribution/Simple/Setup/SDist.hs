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
  ( SDistFlags
      ( SDistCommonFlags
      , sDistVerbosity
      , sDistDistPref
      , sDistCabalFilePath
      , sDistWorkingDir
      , sDistTargets
      , ..
      )
  , emptySDistFlags
  , defaultSDistFlags
  , sdistCommand
  ) where

import Distribution.Compat.Prelude hiding (get)
import Prelude ()

import Distribution.Simple.Command hiding (boolOpt, boolOpt')
import Distribution.Simple.Flag
import Distribution.Simple.Setup.Common
import Distribution.Utils.Path
import Distribution.Verbosity

-- ------------------------------------------------------------

-- * SDist flags

-- ------------------------------------------------------------

-- | Flags to @sdist@: (snapshot, verbosity)
data SDistFlags = SDistFlags
  { sDistCommonFlags :: !CommonSetupFlags
  , sDistSnapshot :: Flag Bool
  , sDistDirectory :: Flag FilePath
  , sDistListSources :: Flag FilePath
  }
  deriving (Show, Generic, Typeable)

pattern SDistCommonFlags
  :: Flag Verbosity
  -> Flag (SymbolicPath Pkg (Dir Dist))
  -> Flag (SymbolicPath CWD (Dir Pkg))
  -> Flag (SymbolicPath Pkg File)
  -> [String]
  -> SDistFlags
pattern SDistCommonFlags
  { sDistVerbosity
  , sDistDistPref
  , sDistWorkingDir
  , sDistCabalFilePath
  , sDistTargets
  } <-
  ( sDistCommonFlags ->
      CommonSetupFlags
        { setupVerbosity = sDistVerbosity
        , setupDistPref = sDistDistPref
        , setupWorkingDir = sDistWorkingDir
        , setupCabalFilePath = sDistCabalFilePath
        , setupTargets = sDistTargets
        }
    )

defaultSDistFlags :: SDistFlags
defaultSDistFlags =
  SDistFlags
    { sDistCommonFlags = defaultCommonSetupFlags
    , sDistSnapshot = Flag False
    , sDistDirectory = mempty
    , sDistListSources = mempty
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
        withCommonSetupOptions
          sDistCommonFlags
          (\c f -> f{sDistCommonFlags = c})
          showOrParseArgs
          [ option
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
