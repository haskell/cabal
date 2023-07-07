{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.Simple.Setup.Haddock
-- Copyright   :  Isaac Jones 2003-2004
--                Duncan Coutts 2007
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Definition of the haddock command-line options.
-- See: @Distribution.Simple.Setup@
module Distribution.Simple.Setup.Haddock
  ( HaddockTarget (..)
  , HaddockFlags (..)
  , emptyHaddockFlags
  , defaultHaddockFlags
  , haddockCommand
  , Visibility (..)
  , HaddockProjectFlags (..)
  , emptyHaddockProjectFlags
  , defaultHaddockProjectFlags
  , haddockProjectCommand
  , haddockOptions
  , haddockProjectOptions
  ) where

import Distribution.Compat.Prelude hiding (get)
import Prelude ()

import qualified Distribution.Compat.CharParsing as P
import Distribution.Parsec
import Distribution.Pretty
import Distribution.Simple.Command hiding (boolOpt, boolOpt')
import Distribution.Simple.Flag
import Distribution.Simple.InstallDirs
import Distribution.Simple.Program
import Distribution.Verbosity
import qualified Text.PrettyPrint as Disp

import Distribution.Simple.Setup.Common

-- ------------------------------------------------------------

-- * Haddock flags

-- ------------------------------------------------------------

-- | When we build haddock documentation, there are two cases:
--
-- 1. We build haddocks only for the current development version,
--    intended for local use and not for distribution. In this case,
--    we store the generated documentation in @<dist>/doc/html/<package name>@.
--
-- 2. We build haddocks for intended for uploading them to hackage.
--    In this case, we need to follow the layout that hackage expects
--    from documentation tarballs, and we might also want to use different
--    flags than for development builds, so in this case we store the generated
--    documentation in @<dist>/doc/html/<package id>-docs@.
data HaddockTarget = ForHackage | ForDevelopment deriving (Eq, Show, Generic, Typeable)

instance Binary HaddockTarget
instance Structured HaddockTarget

instance Pretty HaddockTarget where
  pretty ForHackage = Disp.text "for-hackage"
  pretty ForDevelopment = Disp.text "for-development"

instance Parsec HaddockTarget where
  parsec =
    P.choice
      [ P.try $ P.string "for-hackage" >> return ForHackage
      , P.string "for-development" >> return ForDevelopment
      ]

data HaddockFlags = HaddockFlags
  { haddockProgramPaths :: [(String, FilePath)]
  , haddockProgramArgs :: [(String, [String])]
  , haddockHoogle :: Flag Bool
  , haddockHtml :: Flag Bool
  , haddockHtmlLocation :: Flag String
  , haddockForHackage :: Flag HaddockTarget
  , haddockExecutables :: Flag Bool
  , haddockTestSuites :: Flag Bool
  , haddockBenchmarks :: Flag Bool
  , haddockForeignLibs :: Flag Bool
  , haddockInternal :: Flag Bool
  , haddockCss :: Flag FilePath
  , haddockLinkedSource :: Flag Bool
  , haddockQuickJump :: Flag Bool
  , haddockHscolourCss :: Flag FilePath
  , haddockContents :: Flag PathTemplate
  , haddockIndex :: Flag PathTemplate
  , haddockDistPref :: Flag FilePath
  , haddockKeepTempFiles :: Flag Bool
  , haddockVerbosity :: Flag Verbosity
  , haddockCabalFilePath :: Flag FilePath
  , haddockBaseUrl :: Flag String
  , haddockLib :: Flag String
  , haddockOutputDir :: Flag FilePath
  , haddockArgs :: [String]
  }
  deriving (Show, Generic, Typeable)

defaultHaddockFlags :: HaddockFlags
defaultHaddockFlags =
  HaddockFlags
    { haddockProgramPaths = mempty
    , haddockProgramArgs = []
    , haddockHoogle = Flag False
    , haddockHtml = Flag False
    , haddockHtmlLocation = NoFlag
    , haddockForHackage = NoFlag
    , haddockExecutables = Flag False
    , haddockTestSuites = Flag False
    , haddockBenchmarks = Flag False
    , haddockForeignLibs = Flag False
    , haddockInternal = Flag False
    , haddockCss = NoFlag
    , haddockLinkedSource = Flag False
    , haddockQuickJump = Flag False
    , haddockHscolourCss = NoFlag
    , haddockContents = NoFlag
    , haddockDistPref = NoFlag
    , haddockKeepTempFiles = Flag False
    , haddockVerbosity = Flag normal
    , haddockCabalFilePath = mempty
    , haddockIndex = NoFlag
    , haddockBaseUrl = NoFlag
    , haddockLib = NoFlag
    , haddockOutputDir = NoFlag
    , haddockArgs = mempty
    }

haddockCommand :: CommandUI HaddockFlags
haddockCommand =
  CommandUI
    { commandName = "haddock"
    , commandSynopsis = "Generate Haddock HTML documentation."
    , commandDescription = Just $ \_ ->
        "Requires the program haddock, version 2.x.\n"
    , commandNotes = Nothing
    , commandUsage =
        usageAlternatives "haddock" $
          [ "[FLAGS]"
          , "COMPONENTS [FLAGS]"
          ]
    , commandDefaultFlags = defaultHaddockFlags
    , commandOptions = \showOrParseArgs ->
        haddockOptions showOrParseArgs
          ++ programDbPaths
            progDb
            ParseArgs
            haddockProgramPaths
            (\v flags -> flags{haddockProgramPaths = v})
          ++ programDbOption
            progDb
            showOrParseArgs
            haddockProgramArgs
            (\v fs -> fs{haddockProgramArgs = v})
          ++ programDbOptions
            progDb
            ParseArgs
            haddockProgramArgs
            (\v flags -> flags{haddockProgramArgs = v})
    }
  where
    progDb =
      addKnownProgram haddockProgram $
        addKnownProgram ghcProgram $
          emptyProgramDb

haddockOptions :: ShowOrParseArgs -> [OptionField HaddockFlags]
haddockOptions showOrParseArgs =
  [ optionVerbosity
      haddockVerbosity
      (\v flags -> flags{haddockVerbosity = v})
  , optionDistPref
      haddockDistPref
      (\d flags -> flags{haddockDistPref = d})
      showOrParseArgs
  , option
      ""
      ["keep-temp-files"]
      "Keep temporary files"
      haddockKeepTempFiles
      (\b flags -> flags{haddockKeepTempFiles = b})
      trueArg
  , option
      ""
      ["hoogle"]
      "Generate a hoogle database"
      haddockHoogle
      (\v flags -> flags{haddockHoogle = v})
      trueArg
  , option
      ""
      ["html"]
      "Generate HTML documentation (the default)"
      haddockHtml
      (\v flags -> flags{haddockHtml = v})
      trueArg
  , option
      ""
      ["html-location"]
      "Location of HTML documentation for pre-requisite packages"
      haddockHtmlLocation
      (\v flags -> flags{haddockHtmlLocation = v})
      (reqArgFlag "URL")
  , option
      ""
      ["for-hackage"]
      "Collection of flags to generate documentation suitable for upload to hackage"
      haddockForHackage
      (\v flags -> flags{haddockForHackage = v})
      (noArg (Flag ForHackage))
  , option
      ""
      ["executables"]
      "Run haddock for Executables targets"
      haddockExecutables
      (\v flags -> flags{haddockExecutables = v})
      trueArg
  , option
      ""
      ["tests"]
      "Run haddock for Test Suite targets"
      haddockTestSuites
      (\v flags -> flags{haddockTestSuites = v})
      trueArg
  , option
      ""
      ["benchmarks"]
      "Run haddock for Benchmark targets"
      haddockBenchmarks
      (\v flags -> flags{haddockBenchmarks = v})
      trueArg
  , option
      ""
      ["foreign-libraries"]
      "Run haddock for Foreign Library targets"
      haddockForeignLibs
      (\v flags -> flags{haddockForeignLibs = v})
      trueArg
  , option
      ""
      ["all"]
      "Run haddock for all targets"
      ( \f ->
          allFlags
            [ haddockExecutables f
            , haddockTestSuites f
            , haddockBenchmarks f
            , haddockForeignLibs f
            ]
      )
      ( \v flags ->
          flags
            { haddockExecutables = v
            , haddockTestSuites = v
            , haddockBenchmarks = v
            , haddockForeignLibs = v
            }
      )
      trueArg
  , option
      ""
      ["internal"]
      "Run haddock for internal modules and include all symbols"
      haddockInternal
      (\v flags -> flags{haddockInternal = v})
      trueArg
  , option
      ""
      ["css"]
      "Use PATH as the haddock stylesheet"
      haddockCss
      (\v flags -> flags{haddockCss = v})
      (reqArgFlag "PATH")
  , option
      ""
      ["hyperlink-source", "hyperlink-sources", "hyperlinked-source"]
      "Hyperlink the documentation to the source code"
      haddockLinkedSource
      (\v flags -> flags{haddockLinkedSource = v})
      trueArg
  , option
      ""
      ["quickjump"]
      "Generate an index for interactive documentation navigation"
      haddockQuickJump
      (\v flags -> flags{haddockQuickJump = v})
      trueArg
  , option
      ""
      ["hscolour-css"]
      "Use PATH as the HsColour stylesheet"
      haddockHscolourCss
      (\v flags -> flags{haddockHscolourCss = v})
      (reqArgFlag "PATH")
  , option
      ""
      ["contents-location"]
      "Bake URL in as the location for the contents page"
      haddockContents
      (\v flags -> flags{haddockContents = v})
      ( reqArg'
          "URL"
          (toFlag . toPathTemplate)
          (flagToList . fmap fromPathTemplate)
      )
  , option
      ""
      ["index-location"]
      "Use a separately-generated HTML index"
      haddockIndex
      (\v flags -> flags{haddockIndex = v})
      ( reqArg'
          "URL"
          (toFlag . toPathTemplate)
          (flagToList . fmap fromPathTemplate)
      )
  , option
      ""
      ["base-url"]
      "Base URL for static files."
      haddockBaseUrl
      (\v flags -> flags{haddockBaseUrl = v})
      (reqArgFlag "URL")
  , option
      ""
      ["lib"]
      "location of Haddocks static / auxiliary files"
      haddockLib
      (\v flags -> flags{haddockLib = v})
      (reqArgFlag "DIR")
  , option
      ""
      ["output-dir"]
      "Generate haddock documentation into this directory. This flag is provided as a technology preview and is subject to change in the next releases."
      haddockOutputDir
      (\v flags -> flags{haddockOutputDir = v})
      (reqArgFlag "DIR")
  ]

emptyHaddockFlags :: HaddockFlags
emptyHaddockFlags = mempty

instance Monoid HaddockFlags where
  mempty = gmempty
  mappend = (<>)

instance Semigroup HaddockFlags where
  (<>) = gmappend

-- ------------------------------------------------------------

-- * HaddocksFlags flags

-- ------------------------------------------------------------

-- | Governs whether modules from a given interface should be visible or
-- hidden in the Haddock generated content page.  We don't expose this
-- functionality to the user, but simply use 'Visible' for only local packages.
-- Visibility of modules is available since @haddock-2.26.1@.
data Visibility = Visible | Hidden
  deriving (Eq, Show)

data HaddockProjectFlags = HaddockProjectFlags
  { haddockProjectHackage :: Flag Bool
  -- ^ a shortcut option which builds documentation linked to hackage.  It implies:
  -- * `--html-location='https://hackage.haskell.org/package/$prg-$version/docs'
  -- * `--quickjump`
  -- * `--gen-index`
  -- * `--gen-contents`
  -- * `--hyperlinked-source`
  , -- options passed to @haddock@ via 'createHaddockIndex'
    haddockProjectDir :: Flag String
  -- ^ output directory of combined haddocks, the default is './haddocks'
  , haddockProjectPrologue :: Flag String
  , haddockProjectInterfaces :: Flag [(FilePath, Maybe FilePath, Maybe FilePath, Visibility)]
  -- ^ 'haddocksInterfaces' is inferred by the 'haddocksAction'; currently not
  -- exposed to the user.
  , -- options passed to @haddock@ via 'HaddockFlags' when building
    -- documentation

    haddockProjectProgramPaths :: [(String, FilePath)]
  , haddockProjectProgramArgs :: [(String, [String])]
  , haddockProjectHoogle :: Flag Bool
  , -- haddockHtml is not supported
    haddockProjectHtmlLocation :: Flag String
  , -- haddockForHackage is not supported
    haddockProjectExecutables :: Flag Bool
  , haddockProjectTestSuites :: Flag Bool
  , haddockProjectBenchmarks :: Flag Bool
  , haddockProjectForeignLibs :: Flag Bool
  , haddockProjectInternal :: Flag Bool
  , haddockProjectCss :: Flag FilePath
  , haddockProjectHscolourCss :: Flag FilePath
  , -- haddockContent is not supported, a fixed value is provided
    -- haddockIndex is not supported, a fixed value is provided
    -- haddockDistPerf is not supported, note: it changes location of the haddocks
    haddockProjectKeepTempFiles :: Flag Bool
  , haddockProjectVerbosity :: Flag Verbosity
  , -- haddockBaseUrl is not supported, a fixed value is provided
    haddockProjectLib :: Flag String
  , haddockProjectOutputDir :: Flag FilePath
  }
  deriving (Show, Generic, Typeable)

defaultHaddockProjectFlags :: HaddockProjectFlags
defaultHaddockProjectFlags =
  HaddockProjectFlags
    { haddockProjectHackage = Flag False
    , haddockProjectDir = Flag "./haddocks"
    , haddockProjectPrologue = NoFlag
    , haddockProjectTestSuites = Flag False
    , haddockProjectProgramPaths = mempty
    , haddockProjectProgramArgs = mempty
    , haddockProjectHoogle = Flag False
    , haddockProjectHtmlLocation = NoFlag
    , haddockProjectExecutables = Flag False
    , haddockProjectBenchmarks = Flag False
    , haddockProjectForeignLibs = Flag False
    , haddockProjectInternal = Flag False
    , haddockProjectCss = NoFlag
    , haddockProjectHscolourCss = NoFlag
    , haddockProjectKeepTempFiles = Flag False
    , haddockProjectVerbosity = Flag normal
    , haddockProjectLib = NoFlag
    , haddockProjectOutputDir = NoFlag
    , haddockProjectInterfaces = NoFlag
    }

haddockProjectCommand :: CommandUI HaddockProjectFlags
haddockProjectCommand =
  CommandUI
    { commandName = "v2-haddock-project"
    , commandSynopsis = "Generate Haddocks HTML documentation for the cabal project."
    , commandDescription = Just $ \_ ->
        "Require the programm haddock, version 2.26.\n"
    , commandNotes = Nothing
    , commandUsage =
        usageAlternatives "haddocks" $
          [ "[FLAGS]"
          , "COMPONENTS [FLAGS]"
          ]
    , commandDefaultFlags = defaultHaddockProjectFlags
    , commandOptions = \showOrParseArgs ->
        haddockProjectOptions showOrParseArgs
          ++ programDbPaths
            progDb
            ParseArgs
            haddockProjectProgramPaths
            (\v flags -> flags{haddockProjectProgramPaths = v})
          ++ programDbOption
            progDb
            showOrParseArgs
            haddockProjectProgramArgs
            (\v fs -> fs{haddockProjectProgramArgs = v})
          ++ programDbOptions
            progDb
            ParseArgs
            haddockProjectProgramArgs
            (\v flags -> flags{haddockProjectProgramArgs = v})
    }
  where
    progDb =
      addKnownProgram haddockProgram $
        addKnownProgram ghcProgram $
          emptyProgramDb

haddockProjectOptions :: ShowOrParseArgs -> [OptionField HaddockProjectFlags]
haddockProjectOptions _showOrParseArgs =
  [ option
      ""
      ["hackage"]
      ( concat
          [ "A short-cut option to build documentation linked to hackage."
          ]
      )
      haddockProjectHackage
      (\v flags -> flags{haddockProjectHackage = v})
      trueArg
  , option
      ""
      ["output"]
      "Output directory"
      haddockProjectDir
      (\v flags -> flags{haddockProjectDir = v})
      (optArg' "DIRECTORY" maybeToFlag (fmap Just . flagToList))
  , option
      ""
      ["prologue"]
      "File path to a prologue file in haddock format"
      haddockProjectPrologue
      (\v flags -> flags{haddockProjectPrologue = v})
      (optArg' "PATH" maybeToFlag (fmap Just . flagToList))
  , option
      ""
      ["hoogle"]
      "Generate a hoogle database"
      haddockProjectHoogle
      (\v flags -> flags{haddockProjectHoogle = v})
      trueArg
  , option
      ""
      ["html-location"]
      "Location of HTML documentation for pre-requisite packages"
      haddockProjectHtmlLocation
      (\v flags -> flags{haddockProjectHtmlLocation = v})
      (reqArgFlag "URL")
  , option
      ""
      ["executables"]
      "Run haddock for Executables targets"
      haddockProjectExecutables
      (\v flags -> flags{haddockProjectExecutables = v})
      trueArg
  , option
      ""
      ["tests"]
      "Run haddock for Test Suite targets"
      haddockProjectTestSuites
      (\v flags -> flags{haddockProjectTestSuites = v})
      trueArg
  , option
      ""
      ["benchmarks"]
      "Run haddock for Benchmark targets"
      haddockProjectBenchmarks
      (\v flags -> flags{haddockProjectBenchmarks = v})
      trueArg
  , option
      ""
      ["foreign-libraries"]
      "Run haddock for Foreign Library targets"
      haddockProjectForeignLibs
      (\v flags -> flags{haddockProjectForeignLibs = v})
      trueArg
  , option
      ""
      ["internal"]
      "Run haddock for internal modules and include all symbols"
      haddockProjectInternal
      (\v flags -> flags{haddockProjectInternal = v})
      trueArg
  , option
      ""
      ["css"]
      "Use PATH as the haddock stylesheet"
      haddockProjectCss
      (\v flags -> flags{haddockProjectCss = v})
      (reqArgFlag "PATH")
  , option
      ""
      ["hscolour-css"]
      "Use PATH as the HsColour stylesheet"
      haddockProjectHscolourCss
      (\v flags -> flags{haddockProjectHscolourCss = v})
      (reqArgFlag "PATH")
  , option
      ""
      ["keep-temp-files"]
      "Keep temporary files"
      haddockProjectKeepTempFiles
      (\b flags -> flags{haddockProjectKeepTempFiles = b})
      trueArg
  , optionVerbosity
      haddockProjectVerbosity
      (\v flags -> flags{haddockProjectVerbosity = v})
  , option
      ""
      ["lib"]
      "location of Haddocks static / auxiliary files"
      haddockProjectLib
      (\v flags -> flags{haddockProjectLib = v})
      (reqArgFlag "DIR")
  , option
      ""
      ["output-dir"]
      "Generate haddock documentation into this directory. This flag is provided as a technology preview and is subject to change in the next releases."
      haddockProjectOutputDir
      (\v flags -> flags{haddockProjectOutputDir = v})
      (reqArgFlag "DIR")
  ]

emptyHaddockProjectFlags :: HaddockProjectFlags
emptyHaddockProjectFlags = mempty

instance Monoid HaddockProjectFlags where
  mempty = gmempty
  mappend = (<>)

instance Semigroup HaddockProjectFlags where
  (<>) = gmappend
