{-# LANGUAGE LambdaCase #-}

-- | Parsing the command line of a command with optparse-applicative, by
-- translating the command's 'OptionField's into optparse-applicative parsers.
--
-- Only parsing is affected: help, the list of options and the command registry
-- keep using "Distribution.Simple.Command".
module Distribution.Client.Cmd.UI
  ( -- * Parsing a command with optparse-applicative
    NamedCommandParser (..)
  , commandParserByName
  , parseCommandWithOptparseMany
  , parseCommand
  , commandNames

    -- * Converting CommandUI options to optparse-applicative parsers
  , CmdItem (..)
  , ParsedCommand (..)
  , parsedCommandParser
  , cmdItemParser
  , cmdOptionParsers
  , optionFieldFlagParsers
  , optionFieldParser
  , optDescrParser
  , supplyOptArgDefaults
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Data.List (stripPrefix)
import Data.Monoid (Endo (..))

import Distribution.Client.NixStyleOptions (NixStyleFlags (..))
import Distribution.ReadE (runReadE)
import Distribution.Simple.Command
  ( CommandParse (..)
  , CommandUI (..)
  , OptDescr (..)
  , OptionField (..)
  , ShowOrParseArgs (..)
  , commandParseArgs
  )

import Options.Applicative
  ( ParserInfo
  , ParserResult (..)
  , asum
  , defaultPrefs
  , execParserPure
  , flag'
  , fullDesc
  , help
  , helper
  , info
  , long
  , metavar
  , progDesc
  , renderFailure
  , strArgument
  , (<**>)
  )
import qualified Options.Applicative as O

-- | A parser for a command under any of its names.
data NamedCommandParser action = NamedCommandParser
  { namedCommandNames :: [String]
  -- ^ The command name and its aliases.
  , namedCommandParser :: String -> [String] -> CommandParse action
  -- ^ Parse the arguments of the command, given the name it was invoked by.
  }

-- | Wrap a command's optparse parser together with the names it should match.
commandParserByName
  :: CommandUI (NixStyleFlags flags)
  -> (NixStyleFlags flags -> [String] -> action)
  -> NamedCommandParser action
commandParserByName command action =
  NamedCommandParser
    { namedCommandNames = commandNames command
    , namedCommandParser = parseCommand command action
    }

-- | Parse the global flags with "Distribution.Simple.Command", then the
-- command with the first parser whose names include the command name.
-- Returns 'Nothing' when no parser matches, so the caller can fall back to
-- the command registry.
parseCommandWithOptparseMany
  :: CommandUI globalFlags
  -> [NamedCommandParser action]
  -> [String]
  -> Maybe (CommandParse (globalFlags, CommandParse action))
parseCommandWithOptparseMany globalCommand commands argv =
  case commandParseArgs globalCommand True argv of
    CommandReadyToGo (mkGlobalFlags, cmdArgs0) -> do
      cmdName : cmdArgs <- pure cmdArgs0
      parser <- find ((cmdName `elem`) . namedCommandNames) commands
      let globalFlags = mkGlobalFlags (commandDefaultFlags globalCommand)
      pure $ CommandReadyToGo (globalFlags, namedCommandParser parser cmdName cmdArgs)
    _ -> Nothing

-- | Assuming a v2- prefix for the 'commandName' of the given command, the
-- bare name, the new- prefixed name and the v2- prefixed name, in that order.
commandNames :: CommandUI flags -> [String]
commandNames command = [stripVersionPrefix name, affixVersionPrefix "new-" name, name]
  where
    name = commandName command

-- | Puts a prefix before a bare command name.
affixVersionPrefix :: String -> String -> String
affixVersionPrefix = replaceText "v2-"

-- | Removes the v2- prefix from a command name, leaving the bare command name.
stripVersionPrefix :: String -> String
stripVersionPrefix = affixVersionPrefix ""

replaceText :: String -> String -> String -> String
replaceText needle replacement = go
  where
    go [] = []
    go input@(char : rest)
      | Just remainder <- stripPrefix needle input = replacement ++ go remainder
      | otherwise = char : go rest

-- | The command as it presents itself under the given name: its usage,
-- description and notes refer to that name instead of the canonical one.
renameCommand :: String -> CommandUI flags -> CommandUI flags
renameCommand name command =
  command
    { commandName = name
    , commandUsage = rename . commandUsage command
    , commandDescription = (rename .) <$> commandDescription command
    , commandNotes = (rename .) <$> commandNotes command
    }
  where
    rename = replaceText (commandName command) name

-- | Parse a command's arguments with optparse-applicative. Help and the list
-- of options come from "Distribution.Simple.Command", so they are the same
-- as for commands that are not parsed this way.
parseCommand
  :: CommandUI (NixStyleFlags a)
  -> (NixStyleFlags a -> [String] -> action)
  -> String
  -- ^ The name the command was invoked by.
  -> [String]
  -> CommandParse action
parseCommand cmdui action invokedName cmdArgs =
  case execParserPure defaultPrefs pInfo (supplyOptArgDefaults optionFields cmdArgs) of
    Success parsed
      | parsedListOptions parsed -> legacy ["--list-options"]
      | otherwise ->
          let flags = appEndo (parsedFlagEdits parsed) (commandDefaultFlags cmdui)
           in CommandReadyToGo (action flags (parsedTargets parsed))
    Failure failure ->
      let (msg, exitCode) = renderFailure failure ("cabal " ++ invokedName)
       in if exitCode == ExitSuccess
            then legacy ["--help"]
            else CommandErrors [msg]
    CompletionInvoked _ ->
      CommandErrors ["Shell completion is not supported by this parser path."]
  where
    pInfo = parserInfo invokedName flagParsers cmdui
    optionFields = commandOptions cmdui ParseArgs
    flagParsers = cmdOptionParsers optionFields

    -- Help and the options list, rendered by "Distribution.Simple.Command"
    -- for the command under the name it was invoked by.
    legacy args =
      case commandParseArgs (renameCommand invokedName cmdui) False args of
        CommandHelp helpText -> CommandHelp helpText
        CommandList options -> CommandList options
        _ -> CommandErrors ["Unexpected result from the command parser."]

-- | Insert an empty argument after each bare occurrence of an
-- optional-argument option, so that @--allow-newer@ and @-j@ keep their
-- GetOpt meaning: the option's default, with the next word left as a
-- positional argument. Attached forms such as @--allow-newer=base@ and
-- @-j4@ are left alone, as is everything after @--@.
supplyOptArgDefaults :: [OptionField flags] -> [String] -> [String]
supplyOptArgDefaults fields = go
  where
    go [] = []
    go ("--" : rest) = "--" : rest
    go (arg : rest)
      | arg `elem` bareForms = arg : "" : go rest
      | otherwise = arg : go rest

    bareForms =
      [ form
      | OptionField _ descrs <- fields
      , OptArg _ (shortFlags, longFlags) _ _ _ _ <- descrs
      , form <- map (\c -> ['-', c]) shortFlags ++ map ("--" ++) longFlags
      ]

parserInfo :: String -> [O.Parser (CmdItem a)] -> CommandUI flags -> ParserInfo (ParsedCommand a)
parserInfo invokedName flagParsers cmdui =
  info
    (parsedCommandParser flagParsers <**> helper)
    (fullDesc <> progDesc (commandSynopsis cmdui) <> O.header ("cabal " ++ invokedName))

-- | One item of a command line: a flag, a target, or the request to list
-- the options.
data CmdItem a
  = CmdItemFlag (Endo (NixStyleFlags a))
  | CmdItemTarget String
  | CmdItemListOptions

data ParsedCommand a = ParsedCommand
  { parsedFlagEdits :: Endo (NixStyleFlags a)
  , parsedTargets :: [String]
  , parsedListOptions :: Bool
  }

parsedCommandParser :: [O.Parser (CmdItem a)] -> O.Parser (ParsedCommand a)
parsedCommandParser flagParsers = toParsed <$> many (cmdItemParser flagParsers)
  where
    toParsed items =
      let edits = [e | CmdItemFlag e <- items]
          targets = [t | CmdItemTarget t <- items]
          listOptionsSeen = any isListOptions items
       in ParsedCommand
            { -- Apply edits in command-line order, first to last, so that a
              -- later option overrides an earlier one and list options
              -- accumulate in the order given. @Endo f <> Endo g@ applies
              -- @g@ first, so the list is reversed before being combined.
              -- This matches @accum@ in 'commandParseArgs'.
              parsedFlagEdits = mconcat (reverse edits)
            , parsedTargets = targets
            , parsedListOptions = listOptionsSeen
            }

    isListOptions CmdItemListOptions = True
    isListOptions _ = False

cmdItemParser :: [O.Parser (CmdItem a)] -> O.Parser (CmdItem a)
cmdItemParser flags =
  asum
    ( flags
        ++ [ CmdItemListOptions
              <$ flag' () (long "list-options" <> help "Print a list of command line flags")
           , CmdItemTarget <$> strArgument (metavar "TARGET")
           ]
    )

cmdOptionParsers :: [OptionField (NixStyleFlags a)] -> [O.Parser (CmdItem a)]
cmdOptionParsers fields = (fmap . fmap) CmdItemFlag (optionFieldFlagParsers fields)

optionFieldFlagParsers :: [OptionField flags] -> [O.Parser (Endo flags)]
optionFieldFlagParsers = concatMap optionFieldParser

optionFieldParser :: OptionField flags -> [O.Parser (Endo flags)]
optionFieldParser (OptionField _ descrs) = concatMap optDescrParser descrs

optDescrParser :: OptDescr flags -> [O.Parser (Endo flags)]
optDescrParser = \case
  ReqArg desc optFlags placeHolder reader _show ->
    [ Endo
        <$> O.option
          (O.eitherReader (runReadE reader))
          (optionMods optFlags <> O.metavar placeHolder <> O.help desc)
    ]
  OptArg desc optFlags placeHolder reader defaultFn _show ->
    -- optparse-applicative has no optional-argument options: an option
    -- either always takes an argument or never does, and a bare @-j@ would
    -- swallow the next word as its value. 'supplyOptArgDefaults' gives each
    -- bare occurrence an empty argument instead, which the reader maps to the
    -- option's default. An explicit @--jobs=@ therefore also means the default.
    [ Endo
        <$> O.option
          (O.eitherReader readOrDefault)
          (optionMods optFlags <> O.metavar placeHolder <> O.help desc)
    ]
    where
      readOrDefault "" = Right defaultFn
      readOrDefault s = runReadE reader s
  ChoiceOpt choices ->
    [ Endo setFn
      <$ O.flag' () (flagMods optFlags <> O.help desc)
    | (desc, optFlags, setFn, _get) <- choices
    ]
  BoolOpt desc trueFlags falseFlags setFn _get ->
    [ Endo (setFn True)
        <$ O.flag' () (flagMods trueFlags <> O.help desc)
    , Endo (setFn False)
        <$ O.flag' () (flagMods falseFlags <> O.help desc)
    ]

optionMods :: (String, [String]) -> O.Mod O.OptionFields a
optionMods (shortFlags, longFlags) =
  mconcat (map O.short shortFlags) <> mconcat (map O.long longFlags)

flagMods :: (String, [String]) -> O.Mod O.FlagFields a
flagMods (shortFlags, longFlags) =
  mconcat (map O.short shortFlags) <> mconcat (map O.long longFlags)
