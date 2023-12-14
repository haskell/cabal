{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.Simple.Setup.Copy
-- Copyright   :  Isaac Jones 2003-2004
--                Duncan Coutts 2007
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Definition of the copy command-line options.
-- See: @Distribution.Simple.Setup@
module Distribution.Simple.Setup.Copy
  ( CopyFlags (..)
  , emptyCopyFlags
  , defaultCopyFlags
  , copyCommand
  ) where

import Distribution.Compat.Prelude hiding (get)
import Prelude ()

import Distribution.ReadE
import Distribution.Simple.Command hiding (boolOpt, boolOpt')
import Distribution.Simple.Flag
import Distribution.Simple.InstallDirs
import Distribution.Simple.Utils
import Distribution.Verbosity

import Distribution.Simple.Setup.Common

-- ------------------------------------------------------------

-- * Copy flags

-- ------------------------------------------------------------

-- | Flags to @copy@: (destdir, copy-prefix (backwards compat), verbosity)
data CopyFlags = CopyFlags
  { copyDest :: Flag CopyDest
  , copyDistPref :: Flag FilePath
  , copyVerbosity :: Flag Verbosity
  , -- This is the same hack as in 'buildArgs'.  But I (ezyang) don't
    -- think it's a hack, it's the right way to make hooks more robust
    -- TODO: Stop using this eventually when 'UserHooks' gets changed
    copyArgs :: [String]
  , copyCabalFilePath :: Flag FilePath
  }
  deriving (Show, Generic)

instance Binary CopyFlags
instance Structured CopyFlags

defaultCopyFlags :: CopyFlags
defaultCopyFlags =
  CopyFlags
    { copyDest = Flag NoCopyDest
    , copyDistPref = NoFlag
    , copyVerbosity = Flag normal
    , copyArgs = []
    , copyCabalFilePath = mempty
    }

copyCommand :: CommandUI CopyFlags
copyCommand =
  CommandUI
    { commandName = "copy"
    , commandSynopsis = "Copy the files of all/specific components to install locations."
    , commandDescription = Just $ \_ ->
        wrapText $
          "Components encompass executables and libraries. "
            ++ "Does not call register, and allows a prefix at install time. "
            ++ "Without the --destdir flag, configure determines location.\n"
    , commandNotes = Just $ \pname ->
        "Examples:\n"
          ++ "  "
          ++ pname
          ++ " copy           "
          ++ "    All the components in the package\n"
          ++ "  "
          ++ pname
          ++ " copy foo       "
          ++ "    A component (i.e. lib, exe, test suite)"
    , commandUsage =
        usageAlternatives "copy" $
          [ "[FLAGS]"
          , "COMPONENTS [FLAGS]"
          ]
    , commandDefaultFlags = defaultCopyFlags
    , commandOptions = \showOrParseArgs -> case showOrParseArgs of
        ShowArgs ->
          filter
            ( (`notElem` ["target-package-db"])
                . optionName
            )
            $ copyOptions ShowArgs
        ParseArgs -> copyOptions ParseArgs
    }

copyOptions :: ShowOrParseArgs -> [OptionField CopyFlags]
copyOptions showOrParseArgs =
  [ optionVerbosity copyVerbosity (\v flags -> flags{copyVerbosity = v})
  , optionDistPref
      copyDistPref
      (\d flags -> flags{copyDistPref = d})
      showOrParseArgs
  , option
      ""
      ["destdir"]
      "directory to copy files to, prepended to installation directories"
      copyDest
      ( \v flags -> case copyDest flags of
          Flag (CopyToDb _) -> error "Use either 'destdir' or 'target-package-db'."
          _ -> flags{copyDest = v}
      )
      ( reqArg
          "DIR"
          (succeedReadE (Flag . CopyTo))
          (\f -> case f of Flag (CopyTo p) -> [p]; _ -> [])
      )
  , option
      ""
      ["target-package-db"]
      "package database to copy files into. Required when using ${pkgroot} prefix."
      copyDest
      ( \v flags -> case copyDest flags of
          NoFlag -> flags{copyDest = v}
          Flag NoCopyDest -> flags{copyDest = v}
          _ -> error "Use either 'destdir' or 'target-package-db'."
      )
      ( reqArg
          "DATABASE"
          (succeedReadE (Flag . CopyToDb))
          (\f -> case f of Flag (CopyToDb p) -> [p]; _ -> [])
      )
  ]

emptyCopyFlags :: CopyFlags
emptyCopyFlags = mempty

instance Monoid CopyFlags where
  mempty = gmempty
  mappend = (<>)

instance Semigroup CopyFlags where
  (<>) = gmappend
