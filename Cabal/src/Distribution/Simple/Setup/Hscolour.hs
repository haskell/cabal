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
-- Module      :  Distribution.Simple.Setup.Hscolour
-- Copyright   :  Isaac Jones 2003-2004
--                Duncan Coutts 2007
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Definition of the hscolour command-line options.
-- See: @Distribution.Simple.Setup@
module Distribution.Simple.Setup.Hscolour
  ( HscolourFlags
      ( HscolourCommonFlags
      , hscolourVerbosity
      , hscolourDistPref
      , hscolourCabalFilePath
      , hscolourWorkingDir
      , hscolourTargets
      , ..
      )
  , emptyHscolourFlags
  , defaultHscolourFlags
  , hscolourCommand
  ) where

import Distribution.Compat.Prelude hiding (get)
import Prelude ()

import Distribution.Simple.Command hiding (boolOpt, boolOpt')
import Distribution.Simple.Flag
import Distribution.Simple.Setup.Common
import Distribution.Utils.Path
import Distribution.Verbosity

-- ------------------------------------------------------------

-- * HsColour flags

-- ------------------------------------------------------------

data HscolourFlags = HscolourFlags
  { hscolourCommonFlags :: !CommonSetupFlags
  , hscolourCSS :: Flag FilePath
  , hscolourExecutables :: Flag Bool
  , hscolourTestSuites :: Flag Bool
  , hscolourBenchmarks :: Flag Bool
  , hscolourForeignLibs :: Flag Bool
  }
  deriving (Show, Generic, Typeable)

pattern HscolourCommonFlags
  :: Flag Verbosity
  -> Flag (SymbolicPath Pkg (Dir Dist))
  -> Flag (SymbolicPath CWD (Dir Pkg))
  -> Flag (SymbolicPath Pkg File)
  -> [String]
  -> HscolourFlags
pattern HscolourCommonFlags
  { hscolourVerbosity
  , hscolourDistPref
  , hscolourWorkingDir
  , hscolourCabalFilePath
  , hscolourTargets
  } <-
  ( hscolourCommonFlags ->
      CommonSetupFlags
        { setupVerbosity = hscolourVerbosity
        , setupDistPref = hscolourDistPref
        , setupWorkingDir = hscolourWorkingDir
        , setupCabalFilePath = hscolourCabalFilePath
        , setupTargets = hscolourTargets
        }
    )

instance Binary HscolourFlags
instance Structured HscolourFlags

emptyHscolourFlags :: HscolourFlags
emptyHscolourFlags = mempty

defaultHscolourFlags :: HscolourFlags
defaultHscolourFlags =
  HscolourFlags
    { hscolourCommonFlags = defaultCommonSetupFlags
    , hscolourCSS = NoFlag
    , hscolourExecutables = Flag False
    , hscolourTestSuites = Flag False
    , hscolourBenchmarks = Flag False
    , hscolourForeignLibs = Flag False
    }

instance Monoid HscolourFlags where
  mempty = gmempty
  mappend = (<>)

instance Semigroup HscolourFlags where
  (<>) = gmappend

hscolourCommand :: CommandUI HscolourFlags
hscolourCommand =
  CommandUI
    { commandName = "hscolour"
    , commandSynopsis =
        "Generate HsColour colourised code, in HTML format."
    , commandDescription = Just (\_ -> "Requires the hscolour program.\n")
    , commandNotes = Just $ \_ ->
        "Deprecated in favour of 'cabal haddock --hyperlink-source'."
    , commandUsage = \pname ->
        "Usage: " ++ pname ++ " hscolour [FLAGS]\n"
    , commandDefaultFlags = defaultHscolourFlags
    , commandOptions = \showOrParseArgs ->
        withCommonSetupOptions
          hscolourCommonFlags
          (\c f -> f{hscolourCommonFlags = c})
          showOrParseArgs
          [ option
              ""
              ["executables"]
              "Run hscolour for Executables targets"
              hscolourExecutables
              (\v flags -> flags{hscolourExecutables = v})
              trueArg
          , option
              ""
              ["tests"]
              "Run hscolour for Test Suite targets"
              hscolourTestSuites
              (\v flags -> flags{hscolourTestSuites = v})
              trueArg
          , option
              ""
              ["benchmarks"]
              "Run hscolour for Benchmark targets"
              hscolourBenchmarks
              (\v flags -> flags{hscolourBenchmarks = v})
              trueArg
          , option
              ""
              ["foreign-libraries"]
              "Run hscolour for Foreign Library targets"
              hscolourForeignLibs
              (\v flags -> flags{hscolourForeignLibs = v})
              trueArg
          , option
              ""
              ["all"]
              "Run hscolour for all targets"
              ( \f ->
                  allFlags
                    [ hscolourExecutables f
                    , hscolourTestSuites f
                    , hscolourBenchmarks f
                    , hscolourForeignLibs f
                    ]
              )
              ( \v flags ->
                  flags
                    { hscolourExecutables = v
                    , hscolourTestSuites = v
                    , hscolourBenchmarks = v
                    , hscolourForeignLibs = v
                    }
              )
              trueArg
          , option
              ""
              ["css"]
              "Use a cascading style sheet"
              hscolourCSS
              (\v flags -> flags{hscolourCSS = v})
              (reqArgFlag "PATH")
          ]
    }
