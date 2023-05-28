{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.Simple.Setup.Common
-- Copyright   :  Isaac Jones 2003-2004
--                Duncan Coutts 2007
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Common utilities for defining command-line options.
-- See: @Distribution.Simple.Setup@
module Distribution.Simple.Setup.Common
  ( CopyDest (..)
  , configureCCompiler
  , configureLinker
  , programDbOption
  , programDbOptions
  , programDbPaths
  , programDbPaths'
  , programFlagsDescription
  , splitArgs
  , testOrBenchmarkHelpText
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
  , reqArgFlag
  , optionVerbosity
  , optionNumJobs
  ) where

import Distribution.Compat.Prelude hiding (get)
import Prelude ()

import Distribution.ReadE
import Distribution.Simple.Command hiding (boolOpt, boolOpt')
import qualified Distribution.Simple.Command as Command
import Distribution.Simple.Flag
import Distribution.Simple.InstallDirs
import Distribution.Simple.Program
import Distribution.Simple.Utils
import Distribution.Verbosity

-- FIXME Not sure where this should live
defaultDistPref :: FilePath
defaultDistPref = "dist"

-- | Help text for @test@ and @bench@ commands.
testOrBenchmarkHelpText
  :: String
  -- ^ Either @"test"@ or @"benchmark"@.
  -> String
  -- ^ Help text.
testOrBenchmarkHelpText s =
  unlines $
    map
      unwords
      [
        [ "The package must have been build with configuration"
        , concat ["flag `--enable-", s, "s`."]
        ]
      , [] -- blank line
      ,
        [ concat ["Note that additional dependencies of the ", s, "s"]
        , "must have already been installed."
        ]
      , []
      ,
        [ "By defining UserHooks in a custom Setup.hs, the package can define"
        , concat ["actions to be executed before and after running ", s, "s."]
        ]
      ]

-- ------------------------------------------------------------

-- * Shared options utils

-- ------------------------------------------------------------

programFlagsDescription :: ProgramDb -> String
programFlagsDescription progDb =
  "The flags --with-PROG and --PROG-option(s) can be used with"
    ++ " the following programs:"
    ++ (concatMap (\line -> "\n  " ++ unwords line) . wrapLine 77 . sort)
      [programName prog | (prog, _) <- knownPrograms progDb]
    ++ "\n"

-- | For each known program @PROG@ in 'progDb', produce a @with-PROG@
-- 'OptionField'.
programDbPaths
  :: ProgramDb
  -> ShowOrParseArgs
  -> (flags -> [(String, FilePath)])
  -> ([(String, FilePath)] -> (flags -> flags))
  -> [OptionField flags]
programDbPaths progDb showOrParseArgs get set =
  programDbPaths' ("with-" ++) progDb showOrParseArgs get set

-- | Like 'programDbPaths', but allows to customise the option name.
programDbPaths'
  :: (String -> String)
  -> ProgramDb
  -> ShowOrParseArgs
  -> (flags -> [(String, FilePath)])
  -> ([(String, FilePath)] -> (flags -> flags))
  -> [OptionField flags]
programDbPaths' mkName progDb showOrParseArgs get set =
  case showOrParseArgs of
    -- we don't want a verbose help text list so we just show a generic one:
    ShowArgs -> [withProgramPath "PROG"]
    ParseArgs ->
      map
        (withProgramPath . programName . fst)
        (knownPrograms progDb)
  where
    withProgramPath prog =
      option
        ""
        [mkName prog]
        ("give the path to " ++ prog)
        get
        set
        ( reqArg'
            "PATH"
            (\path -> [(prog, path)])
            (\progPaths -> [path | (prog', path) <- progPaths, prog == prog'])
        )

-- | For each known program @PROG@ in 'progDb', produce a @PROG-option@
-- 'OptionField'.
programDbOption
  :: ProgramDb
  -> ShowOrParseArgs
  -> (flags -> [(String, [String])])
  -> ([(String, [String])] -> (flags -> flags))
  -> [OptionField flags]
programDbOption progDb showOrParseArgs get set =
  case showOrParseArgs of
    -- we don't want a verbose help text list so we just show a generic one:
    ShowArgs -> [programOption "PROG"]
    ParseArgs ->
      map
        (programOption . programName . fst)
        (knownPrograms progDb)
  where
    programOption prog =
      option
        ""
        [prog ++ "-option"]
        ( "give an extra option to "
            ++ prog
            ++ " (no need to quote options containing spaces)"
        )
        get
        set
        ( reqArg'
            "OPT"
            (\arg -> [(prog, [arg])])
            ( \progArgs ->
                concat
                  [ args
                  | (prog', args) <- progArgs
                  , prog == prog'
                  ]
            )
        )

-- | For each known program @PROG@ in 'progDb', produce a @PROG-options@
-- 'OptionField'.
programDbOptions
  :: ProgramDb
  -> ShowOrParseArgs
  -> (flags -> [(String, [String])])
  -> ([(String, [String])] -> (flags -> flags))
  -> [OptionField flags]
programDbOptions progDb showOrParseArgs get set =
  case showOrParseArgs of
    -- we don't want a verbose help text list so we just show a generic one:
    ShowArgs -> [programOptions "PROG"]
    ParseArgs ->
      map
        (programOptions . programName . fst)
        (knownPrograms progDb)
  where
    programOptions prog =
      option
        ""
        [prog ++ "-options"]
        ("give extra options to " ++ prog)
        get
        set
        (reqArg' "OPTS" (\args -> [(prog, splitArgs args)]) (const []))

-- ------------------------------------------------------------

-- * GetOpt Utils

-- ------------------------------------------------------------

boolOpt
  :: SFlags
  -> SFlags
  -> MkOptDescr (a -> Flag Bool) (Flag Bool -> a -> a) a
boolOpt = Command.boolOpt flagToMaybe Flag

boolOpt'
  :: OptFlags
  -> OptFlags
  -> MkOptDescr (a -> Flag Bool) (Flag Bool -> a -> a) a
boolOpt' = Command.boolOpt' flagToMaybe Flag

trueArg, falseArg :: MkOptDescr (a -> Flag Bool) (Flag Bool -> a -> a) a
trueArg sfT lfT = boolOpt' (sfT, lfT) ([], []) sfT lfT
falseArg sfF lfF = boolOpt' ([], []) (sfF, lfF) sfF lfF

reqArgFlag
  :: ArgPlaceHolder
  -> SFlags
  -> LFlags
  -> Description
  -> (b -> Flag String)
  -> (Flag String -> b -> b)
  -> OptDescr b
reqArgFlag ad = reqArg ad (succeedReadE Flag) flagToList

optionDistPref
  :: (flags -> Flag FilePath)
  -> (Flag FilePath -> flags -> flags)
  -> ShowOrParseArgs
  -> OptionField flags
optionDistPref get set = \showOrParseArgs ->
  option
    ""
    (distPrefFlagName showOrParseArgs)
    ( "The directory where Cabal puts generated build files "
        ++ "(default "
        ++ defaultDistPref
        ++ ")"
    )
    get
    set
    (reqArgFlag "DIR")
  where
    distPrefFlagName ShowArgs = ["builddir"]
    distPrefFlagName ParseArgs = ["builddir", "distdir", "distpref"]

optionVerbosity
  :: (flags -> Flag Verbosity)
  -> (Flag Verbosity -> flags -> flags)
  -> OptionField flags
optionVerbosity get set =
  option
    "v"
    ["verbose"]
    "Control verbosity (n is 0--3, default verbosity level is 1)"
    get
    set
    ( optArg
        "n"
        (fmap Flag flagToVerbosity)
        (Flag verbose) -- default Value if no n is given
        (fmap (Just . showForCabal) . flagToList)
    )

optionNumJobs
  :: (flags -> Flag (Maybe Int))
  -> (Flag (Maybe Int) -> flags -> flags)
  -> OptionField flags
optionNumJobs get set =
  option
    "j"
    ["jobs"]
    "Run NUM jobs simultaneously (or '$ncpus' if no NUM is given)."
    get
    set
    ( optArg
        "NUM"
        (fmap Flag numJobsParser)
        (Flag Nothing)
        (map (Just . maybe "$ncpus" show) . flagToList)
    )
  where
    numJobsParser :: ReadE (Maybe Int)
    numJobsParser = ReadE $ \s ->
      case s of
        "$ncpus" -> Right Nothing
        _ -> case reads s of
          [(n, "")]
            | n < 1 -> Left "The number of jobs should be 1 or more."
            | otherwise -> Right (Just n)
          _ -> Left "The jobs value should be a number or '$ncpus'"

-- ------------------------------------------------------------

-- * Other Utils

-- ------------------------------------------------------------

configureCCompiler
  :: Verbosity
  -> ProgramDb
  -> IO (FilePath, [String])
configureCCompiler verbosity progdb = configureProg verbosity progdb gccProgram

configureLinker :: Verbosity -> ProgramDb -> IO (FilePath, [String])
configureLinker verbosity progdb = configureProg verbosity progdb ldProgram

configureProg
  :: Verbosity
  -> ProgramDb
  -> Program
  -> IO (FilePath, [String])
configureProg verbosity programDb prog = do
  (p, _) <- requireProgram verbosity prog programDb
  let pInv = programInvocation p []
  return (progInvokePath pInv, progInvokeArgs pInv)

-- | Helper function to split a string into a list of arguments.
-- It's supposed to handle quoted things sensibly, eg:
--
-- > splitArgs "--foo=\"C:/Program Files/Bar/" --baz"
-- >   = ["--foo=C:/Program Files/Bar", "--baz"]
--
-- > splitArgs "\"-DMSGSTR=\\\"foo bar\\\"\" --baz"
-- >   = ["-DMSGSTR=\"foo bar\"","--baz"]
splitArgs :: String -> [String]
splitArgs = space []
  where
    space :: String -> String -> [String]
    space w [] = word w []
    space w (c : s)
      | isSpace c = word w (space [] s)
    space w ('"' : s) = string w s
    space w s = nonstring w s

    string :: String -> String -> [String]
    string w [] = word w []
    string w ('"' : s) = space w s
    string w ('\\' : '"' : s) = string ('"' : w) s
    string w (c : s) = string (c : w) s

    nonstring :: String -> String -> [String]
    nonstring w [] = word w []
    nonstring w ('"' : s) = string w s
    nonstring w (c : s) = space (c : w) s

    word [] s = s
    word w s = reverse w : s
