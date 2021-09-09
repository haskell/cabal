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

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Client.ManpageFlags
import Distribution.Client.Setup        (globalCommand)
import Distribution.Compat.Process      (createProcess)
import Distribution.Simple.Command
import Distribution.Simple.Flag         (fromFlagOrDefault)
import System.IO                        (hClose, hPutStr)

import qualified System.Process as Process

data FileInfo = FileInfo String String -- ^ path, description

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

-- | A list of files that should be documented in the manual page.
files :: [FileInfo]
files =
  [ (FileInfo "~/.cabal/config" "The defaults that can be overridden with command-line options.")
  , (FileInfo "~/.cabal/world"  "A list of all packages whose installation has been explicitly requested.")
  ]

manpageCmd :: String -> [CommandSpec a] -> ManpageFlags -> IO ()
manpageCmd pname commands flags
    | fromFlagOrDefault False (manpageRaw flags)
    = putStrLn contents
    | otherwise
    = do
        let cmd  = "man"
            args = ["-l", "-"]

        (mb_in, _, _, ph) <- createProcess (Process.proc cmd args)
            { Process.std_in  = Process.CreatePipe
            , Process.std_out = Process.Inherit
            , Process.std_err = Process.Inherit
            }

        -- put contents
        for_ mb_in $ \hin -> do
            hPutStr hin contents
            hClose hin

        -- wait for process to exit, propagate exit code
        ec <- Process.waitForProcess ph
        exitWith ec
  where
    contents :: String
    contents = manpage pname commands

-- | Produces a manual page with @troff@ markup.
manpage :: String -> [CommandSpec a] -> String
manpage pname commands = unlines $
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
  ] ++
  concatMap (commandSynopsisLines pname) commands ++
  [ ".SH DESCRIPTION"
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
  ] ++
  optionsLines (globalCommand []) ++
  [ ".SH COMMANDS"
  ] ++
  concatMap (commandDetailsLines pname) commands ++
  [ ".SH FILES"
  ] ++
  concatMap fileLines files ++
  [ ".SH BUGS"
  , "To browse the list of known issues or report a new one please see "
  , "https://github.com/haskell/cabal/labels/cabal-install."
  ]

commandSynopsisLines :: String -> CommandSpec action -> [String]
commandSynopsisLines pname (CommandSpec ui _ NormalCommand) =
  [ ".B " ++ pname ++ " " ++ (commandName ui)
  , ".R - " ++ commandSynopsis ui
  , ".br"
  ]
commandSynopsisLines _ (CommandSpec _ _ HiddenCommand) = []

commandDetailsLines :: String -> CommandSpec action -> [String]
commandDetailsLines pname (CommandSpec ui _ NormalCommand) =
  [ ".B " ++ pname ++ " " ++ (commandName ui)
  , ""
  , commandUsage ui pname
  , ""
  ] ++
  optional commandDescription ++
  optional commandNotes ++
  [ "Flags:"
  , ".RS"
  ] ++
  optionsLines ui ++
  [ ".RE"
  , ""
  ]
  where
    optional field =
      case field ui of
        Just text -> [text pname, ""]
        Nothing   -> []
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
  optionLinesIfPresent trueChars trueStrings ++
  optionLinesIfPresent falseChars falseStrings ++
  optionDescriptionLines description
optionLines (ChoiceOpt options) =
  concatMap choiceLines options
  where
    choiceLines (description, (optionChars, optionStrings), _, _) =
      [ optionsLine optionChars optionStrings ] ++
      optionDescriptionLines description

argOptionLines :: String -> [Char] -> [String] -> OptionArg -> [String]
argOptionLines description optionChars optionStrings arg =
  [ optionsLine optionChars optionStrings
  , optionArgLine arg
  ] ++
  optionDescriptionLines description

optionLinesIfPresent :: [Char] -> [String] -> [String]
optionLinesIfPresent optionChars optionStrings =
  if null optionChars && null optionStrings then []
  else                                           [ optionsLine optionChars optionStrings, ".br" ]

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
