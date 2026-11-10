{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

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
  , cleanTargets :: [String]
  -- ^ The targets of this invocation, given as positional arguments.
  , cleanSaveConf :: Flag Bool
  }
  deriving (Show, Generic)
  deriving (Semigroup, Monoid) via Generically CleanFlags

pattern CleanCommonFlags
  :: Flag VerbosityFlags
  -> Flag (SymbolicPath Pkg (Dir Dist))
  -> Flag (SymbolicPath CWD (Dir Pkg))
  -> Flag (SymbolicPath Pkg File)
  -> CleanFlags
pattern CleanCommonFlags
  { cleanVerbosity
  , cleanDistPref
  , cleanWorkingDir
  , cleanCabalFilePath
  } <-
  ( cleanCommonFlags ->
      CommonSetupFlags
        { setupVerbosity = cleanVerbosity
        , setupDistPref = cleanDistPref
        , setupWorkingDir = cleanWorkingDir
        , setupCabalFilePath = cleanCabalFilePath
        }
    )

instance Binary CleanFlags
instance Structured CleanFlags

defaultCleanFlags :: CleanFlags
defaultCleanFlags =
  CleanFlags
    { cleanCommonFlags = defaultCommonSetupFlags
    , cleanTargets = []
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
