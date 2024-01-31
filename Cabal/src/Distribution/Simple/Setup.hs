{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.Simple.Setup
-- Copyright   :  Isaac Jones 2003-2004
--                Duncan Coutts 2007
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This module defines the command line interface for all the Cabal
-- commands. For each command (like @configure@, @build@ etc) it defines a type
-- that holds all the flags, the default set of flags and a 'CommandUI' that
-- maps command line flags to and from the corresponding flags type.
--
-- All the flags types are instances of 'Monoid', see
-- <http://www.haskell.org/pipermail/cabal-devel/2007-December/001509.html>
-- for an explanation.
--
-- The types defined here get used in the front end and especially in
-- @cabal-install@ which has to do quite a bit of manipulating sets of command
-- line flags.
--
-- This is actually relatively nice, it works quite well. The main change it
-- needs is to unify it with the code for managing sets of fields that can be
-- read and written from files. This would allow us to save configure flags in
-- config files.
module Distribution.Simple.Setup
  ( GlobalFlags (..)
  , emptyGlobalFlags
  , defaultGlobalFlags
  , globalCommand
  , ConfigFlags (..)
  , emptyConfigFlags
  , defaultConfigFlags
  , configureCommand
  , configPrograms
  , configAbsolutePaths
  , readPackageDb
  , readPackageDbList
  , showPackageDb
  , showPackageDbList
  , CopyFlags (..)
  , emptyCopyFlags
  , defaultCopyFlags
  , copyCommand
  , InstallFlags (..)
  , emptyInstallFlags
  , defaultInstallFlags
  , installCommand
  , HaddockTarget (..)
  , HaddockFlags (..)
  , emptyHaddockFlags
  , defaultHaddockFlags
  , haddockCommand
  , Visibility (..)
  , HaddockProjectFlags (..)
  , emptyHaddockProjectFlags
  , defaultHaddockProjectFlags
  , haddockProjectCommand
  , HscolourFlags (..)
  , emptyHscolourFlags
  , defaultHscolourFlags
  , hscolourCommand
  , BuildFlags (..)
  , emptyBuildFlags
  , defaultBuildFlags
  , buildCommand
  , DumpBuildInfo (..)
  , ReplFlags (..)
  , defaultReplFlags
  , replCommand
  , ReplOptions (..)
  , CleanFlags (..)
  , emptyCleanFlags
  , defaultCleanFlags
  , cleanCommand
  , RegisterFlags (..)
  , emptyRegisterFlags
  , defaultRegisterFlags
  , registerCommand
  , unregisterCommand
  , SDistFlags (..)
  , emptySDistFlags
  , defaultSDistFlags
  , sdistCommand
  , TestFlags (..)
  , emptyTestFlags
  , defaultTestFlags
  , testCommand
  , TestShowDetails (..)
  , BenchmarkFlags (..)
  , emptyBenchmarkFlags
  , defaultBenchmarkFlags
  , benchmarkCommand
  , CopyDest (..)
  , configureArgs
  , configureOptions
  , configureCCompiler
  , configureLinker
  , buildOptions
  , haddockOptions
  , haddockProjectOptions
  , installDirsOptions
  , testOptions'
  , benchmarkOptions'
  , programDbOptions
  , programDbPaths'
  , programFlagsDescription
  , replOptions
  , splitArgs
  , defaultDistPref
  , optionDistPref
  , Flag (..)
  , toFlag
  , fromFlag
  , fromFlagOrDefault
  , flagToMaybe
  , flagToList
  , maybeToFlag
  , BooleanFlag (..)
  , boolOpt
  , boolOpt'
  , trueArg
  , falseArg
  , optionVerbosity
  , BuildingWhat (..)
  , buildingWhatVerbosity
  , buildingWhatDistPref
  ) where

import GHC.Generics (Generic)
import Prelude (FilePath, Show, ($))

import Distribution.Simple.Flag
import Distribution.Simple.InstallDirs
import Distribution.Types.DumpBuildInfo

import Distribution.Simple.Setup.Benchmark
import Distribution.Simple.Setup.Build
import Distribution.Simple.Setup.Clean
import Distribution.Simple.Setup.Common
import Distribution.Simple.Setup.Config
import Distribution.Simple.Setup.Copy
import Distribution.Simple.Setup.Global
import Distribution.Simple.Setup.Haddock
import Distribution.Simple.Setup.Hscolour
import Distribution.Simple.Setup.Install
import Distribution.Simple.Setup.Register
import Distribution.Simple.Setup.Repl
import Distribution.Simple.Setup.SDist
import Distribution.Simple.Setup.Test

import Distribution.Verbosity (Verbosity)

-- | What kind of build are we doing?
--
-- Is this a normal build, or is it perhaps for running an interactive
-- session or Haddock?
data BuildingWhat
  = -- | A normal build.
    BuildNormal BuildFlags
  | -- | Build steps for an interactive session.
    BuildRepl ReplFlags
  | -- | Build steps for generating documentation.
    BuildHaddock HaddockFlags
  | -- | Build steps for Hscolour.
    BuildHscolour HscolourFlags
  deriving (Generic, Show)

buildingWhatVerbosity :: BuildingWhat -> Verbosity
buildingWhatVerbosity = \case
  BuildNormal flags -> fromFlag $ buildVerbosity flags
  BuildRepl flags -> fromFlag $ replVerbosity flags
  BuildHaddock flags -> fromFlag $ haddockVerbosity flags
  BuildHscolour flags -> fromFlag $ hscolourVerbosity flags

buildingWhatDistPref :: BuildingWhat -> FilePath
buildingWhatDistPref = \case
  BuildNormal flags -> fromFlag $ buildDistPref flags
  BuildRepl flags -> fromFlag $ replDistPref flags
  BuildHaddock flags -> fromFlag $ haddockDistPref flags
  BuildHscolour flags -> fromFlag $ hscolourDistPref flags

-- The test cases kinda have to be rewritten from the ground up... :/
-- hunitTests :: [Test]
-- hunitTests =
--    let m = [("ghc", GHC), ("nhc98", NHC), ("hugs", Hugs)]
--        (flags, commands', unkFlags, ers)
--               = getOpt Permute options ["configure", "foobar", "--prefix=/foo", "--ghc", "--nhc98", "--hugs", "--with-compiler=/comp", "--unknown1", "--unknown2", "--install-prefix=/foo", "--user", "--global"]
--       in  [TestLabel "very basic option parsing" $ TestList [
--                 "getOpt flags" ~: "failed" ~:
--                 [Prefix "/foo", GhcFlag, NhcFlag, HugsFlag,
--                  WithCompiler "/comp", InstPrefix "/foo", UserFlag, GlobalFlag]
--                 ~=? flags,
--                 "getOpt commands" ~: "failed" ~: ["configure", "foobar"] ~=? commands',
--                 "getOpt unknown opts" ~: "failed" ~:
--                      ["--unknown1", "--unknown2"] ~=? unkFlags,
--                 "getOpt errors" ~: "failed" ~: [] ~=? ers],
--
--               TestLabel "test location of various compilers" $ TestList
--               ["configure parsing for prefix and compiler flag" ~: "failed" ~:
--                    (Right (ConfigCmd (Just comp, Nothing, Just "/usr/local"), []))
--                   ~=? (parseArgs ["--prefix=/usr/local", "--"++name, "configure"])
--                   | (name, comp) <- m],
--
--               TestLabel "find the package tool" $ TestList
--               ["configure parsing for prefix comp flag, withcompiler" ~: "failed" ~:
--                    (Right (ConfigCmd (Just comp, Just "/foo/comp", Just "/usr/local"), []))
--                   ~=? (parseArgs ["--prefix=/usr/local", "--"++name,
--                                   "--with-compiler=/foo/comp", "configure"])
--                   | (name, comp) <- m],
--
--               TestLabel "simpler commands" $ TestList
--               [flag ~: "failed" ~: (Right (flagCmd, [])) ~=? (parseArgs [flag])
--                   | (flag, flagCmd) <- [("build", BuildCmd),
--                                         ("install", InstallCmd Nothing False),
--                                         ("sdist", SDistCmd),
--                                         ("register", RegisterCmd False)]
--                  ]
--               ]

{- Testing ideas:
   * IO to look for hugs and hugs-pkg (which hugs, etc)
   * quickCheck to test permutations of arguments
   * what other options can we over-ride with a command-line flag?
-}
