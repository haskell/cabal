{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.Client.Manpage
-- Copyright   :  (c) Maciek Makowski 2015
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Functions for building the manual page.
module Distribution.Client.Manpage
  ( -- * Manual page generation
    manpage
  , manpageCmd
  , ManpageFlags
  , defaultManpageFlags
  , manpageOptions
  ) where

import qualified Data.List.NonEmpty as List1
import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Client.Init.Utils (trim)
import Distribution.Client.ManpageFlags
import Distribution.Client.Setup (globalCommand)
import Distribution.Compat.Process (proc)
import Distribution.Simple.Command
import Distribution.Simple.Flag (fromFlag, fromFlagOrDefault)
import Distribution.Simple.Utils
  ( IOData (..)
  , IODataMode (..)
  , die'
  , fromCreatePipe
  , ignoreSigPipe
  , rawSystemProcAction
  , rawSystemStdInOut
  )
import System.Environment (lookupEnv)
import System.IO (hClose, hPutStr)
import qualified System.Process as Process

data FileInfo
  = -- | path, description
    FileInfo String String

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

-- | A list of files that should be documented in the manual page.
files :: [FileInfo]
files =
  [ (FileInfo "~/.config/cabal/config" "The defaults that can be overridden with command-line options.")
  ]

manpageCmd :: String -> [CommandSpec a] -> ManpageFlags -> IO ()
manpageCmd pname commands flags
  | fromFlagOrDefault False (manpageRaw flags) =
      putStrLn contents
  | otherwise =
      ignoreSigPipe $ do
        -- 2021-10-08, issue #7714
        -- @cabal man --raw | man -l -@ does not work on macOS/BSD,
        -- because BSD-man does not support option @-l@, rather would
        -- accept directly a file argument, e.g. @man /dev/stdin@.
        -- The following works both on macOS and Linux
        -- (but not on Windows out-of-the-box):
        --
        --   cabal man --raw | nroff -man /dev/stdin | less
        --
        -- So let us simulate this!

        -- Feed contents into @nroff -man /dev/stdin@
        (formatted, _errors, ec1) <-
          rawSystemStdInOut
            verbosity
            "nroff"
            ["-man", "/dev/stdin"]
            Nothing -- Inherit working directory
            Nothing -- Inherit environment
            (Just $ IODataText contents)
            IODataModeText

        unless (ec1 == ExitSuccess) $ exitWith ec1

        pagerAndArgs <- fromMaybe "less -R" <$> lookupEnv "PAGER"
        -- 'less' is borked with color sequences otherwise, hence -R
        (pager, pagerArgs) <- case words pagerAndArgs of
          [] -> die' verbosity "man: empty value of the PAGER environment variable"
          (p : pa) -> pure (p, pa)
        -- Pipe output of @nroff@ into @less@
        (ec2, _) <- rawSystemProcAction
          verbosity
          (proc pager pagerArgs){Process.std_in = Process.CreatePipe}
          $ \mIn _ _ -> do
            let wIn = fromCreatePipe mIn
            hPutStr wIn formatted
            hClose wIn
        exitWith ec2
  where
    contents :: String
    contents = manpage pname commands
    verbosity = fromFlag $ manpageVerbosity flags

-- | Produces a manual page with @troff@ markup.
manpage :: String -> [CommandSpec a] -> String
manpage pname commands =
  unlines $
    [ ".TH " ++ map toUpper pname ++ " 1"
    , ".SH NAME"
    , pname ++ " \\- a system for building and packaging Haskell libraries and programs"
    , ".SH SYNOPSIS"
    , ".B " ++ pname
    , ".I command"
    , ".RI < arguments |[ options ]>..."
    , ""
    , "Where the"
    , ".I commands"
    , "are"
    , ""
    ]
      ++ concatMap (commandSynopsisLines pname) commands
      ++ [ ".SH DESCRIPTION"
         , "Cabal is the standard package system for Haskell software. It helps people to configure, "
         , "build and install Haskell software and to distribute it easily to other users and developers."
         , ""
         , "The command line " ++ pname ++ " tool (also referred to as cabal-install) helps with "
         , "installing existing packages and developing new packages. "
         , "It can be used to work with local packages or to install packages from online package archives, "
         , "including automatically installing dependencies. By default it is configured to use Hackage, "
         , "which is Haskell's central package archive that contains thousands of libraries and applications "
         , "in the Cabal package format."
         , ".SH OPTIONS"
         , "Global options:"
         , ""
         ]
      ++ optionsLines (globalCommand [])
      ++ [ ".SH COMMANDS"
         ]
      ++ concatMap (commandDetailsLines pname) commands
      ++ [ ".SH FILES"
         ]
      ++ concatMap fileLines files
      ++ [ ".SH BUGS"
         , "To browse the list of known issues or report a new one please see "
         , "https://github.com/haskell/cabal/labels/cabal-install."
         ]

commandSynopsisLines :: String -> CommandSpec action -> [String]
commandSynopsisLines pname (CommandSpec ui _ NormalCommand) =
  [ ".B " ++ pname ++ " " ++ (commandName ui)
  , "- " ++ commandSynopsis ui
  , ".br"
  ]
commandSynopsisLines _ (CommandSpec _ _ HiddenCommand) = []

commandDetailsLines :: String -> CommandSpec action -> [String]
commandDetailsLines pname (CommandSpec ui _ NormalCommand) =
  [ ".B " ++ pname ++ " " ++ (commandName ui)
  , ""
  , commandUsage ui pname
  , ""
  ]
    ++ optional removeLineBreaks commandDescription
    ++ optional id commandNotes
    ++ [ "Flags:"
       , ".RS"
       ]
    ++ optionsLines ui
    ++ [ ".RE"
       , ""
       ]
  where
    optional f field =
      case field ui of
        Just text -> [f $ text pname, ""]
        Nothing -> []
    -- 2021-10-12, https://github.com/haskell/cabal/issues/7714#issuecomment-940842905
    -- Line breaks just before e.g. 'new-build' cause weird @nroff@ warnings.
    -- Thus:
    -- Remove line breaks but preserve paragraph breaks.
    -- We group lines by empty/non-empty and then 'unwords'
    -- blocks consisting of non-empty lines.
    removeLineBreaks =
      unlines
        . concatMap unwordsNonEmpty
        . List1.groupWith null
        . map trim
        . lines
    unwordsNonEmpty :: List1.NonEmpty String -> [String]
    unwordsNonEmpty ls1 = if null (List1.head ls1) then ls else [unwords ls]
      where
        ls = List1.toList ls1
commandDetailsLines _ (CommandSpec _ _ HiddenCommand) = []

optionsLines :: CommandUI flags -> [String]
optionsLines command = concatMap optionLines (concatMap optionDescr (commandOptions command ParseArgs))

data ArgumentRequired = Optional | Required
type OptionArg = (ArgumentRequired, ArgPlaceHolder)

optionLines :: OptDescr flags -> [String]
optionLines (ReqArg description (optionChars, optionStrings) placeHolder _ _) =
  argOptionLines description optionChars optionStrings (Required, placeHolder)
optionLines (OptArg description (optionChars, optionStrings) placeHolder _ _ _) =
  argOptionLines description optionChars optionStrings (Optional, placeHolder)
optionLines (BoolOpt description (trueChars, trueStrings) (falseChars, falseStrings) _ _) =
  optionLinesIfPresent trueChars trueStrings
    ++ optionLinesIfPresent falseChars falseStrings
    ++ optionDescriptionLines description
optionLines (ChoiceOpt options) =
  concatMap choiceLines options
  where
    choiceLines (description, (optionChars, optionStrings), _, _) =
      [optionsLine optionChars optionStrings]
        ++ optionDescriptionLines description

argOptionLines :: String -> [Char] -> [String] -> OptionArg -> [String]
argOptionLines description optionChars optionStrings arg =
  [ optionsLine optionChars optionStrings
  , optionArgLine arg
  ]
    ++ optionDescriptionLines description

optionLinesIfPresent :: [Char] -> [String] -> [String]
optionLinesIfPresent optionChars optionStrings =
  if null optionChars && null optionStrings
    then []
    else [optionsLine optionChars optionStrings, ".br"]

optionDescriptionLines :: String -> [String]
optionDescriptionLines description =
  [ ".RS"
  , description
  , ".RE"
  , ""
  ]

optionsLine :: [Char] -> [String] -> String
optionsLine optionChars optionStrings =
  intercalate ", " (shortOptions optionChars ++ longOptions optionStrings)

shortOptions :: [Char] -> [String]
shortOptions = map (\c -> "\\-" ++ [c])

longOptions :: [String] -> [String]
longOptions = map (\s -> "\\-\\-" ++ s)

optionArgLine :: OptionArg -> String
optionArgLine (Required, placeHolder) = ".I " ++ placeHolder
optionArgLine (Optional, placeHolder) = ".RI [ " ++ placeHolder ++ " ]"

fileLines :: FileInfo -> [String]
fileLines (FileInfo path description) =
  [ path
  , ".RS"
  , description
  , ".RE"
  , ""
  ]
