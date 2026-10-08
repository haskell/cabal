{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Distribution.Client.Cmd.UI
  ( -- * Converting CommandUI options to optparse-applicative parsers
    optionFieldFlagParsers
  , optionFieldParser
  , optDescrParser
  , optionMods
  , flagMods

    -- * Converting CommandUI options to GetOpt descriptions
  , optionFieldToGetOpt
  , optDescrToGetOpt

    -- * Command data types
  , Examples
  , ReplaceCommandAlias
  , CmdItem (..)
  , ParsedCommand (..)
  , parsedCommandParser
  , cmdItemParser
  , cmdOptionParsers
  , cmdSpec
  , cmdListOptions
  , commandNames
  , NamedCommandParser (..)
  , commandParserByName
  , parseCommandWithOptparse
  , parseCommandWithOptparseMany
  , parseCommand
  , replaceCommandAlias
  , helpDescriptionOrSynopsis
  , parserInfo

    -- * Help text layout helpers
  , renderOptionRows
  , getOptToColumns
  , wrapDescription
  , capitalizeDescription
  , helpText
  , HelpColor (..)

    -- * Option grouping helpers
  , groupPredicates
  , groupSequentially
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Data.Char (isLower)
import Data.List (mapAccumL, stripPrefix)
import Data.Monoid (Endo (..))
import qualified Data.Text as T
import qualified System.Console.GetOpt as GetOpt

import Distribution.Client.NixStyleOptions
  ( NixStyleFlags (..)
  , keepBenchOptions
  , keepCompilerOptions
  , keepConfigureOptions
  , keepCoverageOptions
  , keepDeprecatedOptions
  , keepExeOptions
  , keepHaddockOptions
  , keepIncludeOptions
  , keepInstallOptions
  , keepIrrelevantOptions
  , keepLibOptions
  , keepLoggingOptions
  , keepOutputOptions
  , keepPhaseOptions
  , keepProfilingOptions
  , keepProgOptions
  , keepSolvingOptions
  , keepTestOptions
  , keepUnsupportedOptions
  )
import Distribution.ReadE (runReadE)
import Distribution.Simple.Command
  ( CommandParse (..)
  , CommandSpec (..)
  , CommandType (NormalCommand)
  , CommandUI (..)
  , OptDescr (..)
  , OptionField (..)
  , ShowOrParseArgs (..)
  , commandAddAction
  , commandParseArgs
  )
import Distribution.Simple.Utils (ordNub)

import Options.Applicative
  ( ParserInfo
  , ParserResult (..)
  , asum
  , defaultPrefs
  , execParserPure
  , flag'
  , footer
  , fullDesc
  , header
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

helpDescriptionOrSynopsis :: CommandUI flags -> String
helpDescriptionOrSynopsis x =
  case commandDescription x of
    Nothing -> commandSynopsis x
    Just mkDescription -> mkDescription "cabal"

data CmdItem a
  = CmdItemFlag (Endo (NixStyleFlags a))
  | CmdItemTarget String
  | CmdItemListOptions

data ParsedCommand a = ParsedCommand
  { parsedFlagEdits :: Endo (NixStyleFlags a)
  , parsedTargets :: [String]
  , parsedListOptions :: Bool
  }

-- | Examples text for a command, given the program name and command name.
type Examples =
  String
  -- ^ program name
  -> String
  -- ^ command name
  -> String
  -- ^ examples text

-- | Replacements for v2- prefixed commands, such as;
--
-- * v2-build -> new-build or
-- * v2-build -> build.
type ReplaceCommandAlias =
  String
  -- ^ the new prefix
  -> String
  -- ^ the command
  -> String
  -- ^ the command name with the prefix replaced

-- | Given a v2- prefixed command name, returns a function for replacing that
-- prefix with a new prefix.
replaceCommandAlias :: String -> ReplaceCommandAlias
replaceCommandAlias = replaceText

-- SEE: generic-sop-lens.hs
replaceText :: String -> String -> String -> String
replaceText needle replacement = go
  where
    go [] = []
    go input@(char : rest)
      | Just remainder <- stripPrefix needle input = replacement ++ go remainder
      | otherwise = char : go rest

-- | Puts a prefix before a bare command name.
affixVersionPrefix :: String -> String -> String
affixVersionPrefix = replaceText "v2-"

-- | Removes the v2- prefix from a command name, leaving the bare command name.
stripVersionPrefix :: String -> String
stripVersionPrefix = affixVersionPrefix ""

-- | Assuming a v2- prefix for a command name, for the 'commandName' of the
-- given command, makes a list that includes the bare name, the new- prefixed
-- name, and the v2- prefixed name.
commandNames :: CommandUI flags -> [String]
commandNames command =
  [ stripVersionPrefix name
  , affixVersionPrefix "new-" name
  , name
  ]
  where
    name = commandName command

cmdSpec
  :: CommandUI flags
  -> (flags -> [String] -> action)
  -> [CommandSpec action]
cmdSpec command action =
  [CommandSpec ui (`commandAddAction` action) NormalCommand]
  where
    ui =
      command
        { commandName = stripVersionPrefix (commandName command)
        , commandUsage = stripVersionPrefix . commandUsage command
        , commandDescription = (stripVersionPrefix .) <$> commandDescription command
        , commandNotes = (stripVersionPrefix .) <$> commandNotes command
        }

cmdListOptions :: CommandUI flags -> [String]
cmdListOptions command =
  case commandParseArgs command False ["--list-options"] of
    CommandList opts -> opts
    _ -> []

parseCommand
  :: HelpColor
  -> Examples
  -> CommandUI (NixStyleFlags a)
  -> (NixStyleFlags a -> [String] -> action)
  -> String
  -> [String]
  -> CommandParse action
parseCommand helpColor examples cmdui action invokedName cmdArgs =
  case execParserPure defaultPrefs pInfo (supplyOptArgDefaults optionFields cmdArgs) of
    Success parsed ->
      if parsedListOptions parsed
        then CommandList (cmdListOptions cmdui)
        else
          let flags = appEndo (parsedFlagEdits parsed) (commandDefaultFlags cmdui)
           in CommandReadyToGo (action flags (parsedTargets parsed))
    Failure failure ->
      let (msg, exitCode) = renderFailure failure ("cabal " ++ invokedName)
       in if exitCode == ExitSuccess
            then CommandHelp (helpText helpColor (replaceCommandAlias (commandName cmdui)) cmdui invokedName)
            else CommandErrors [msg]
    CompletionInvoked _ ->
      CommandErrors ["Shell completion is not supported by this parser path."]
  where
    pInfo = parserInfo invokedName examples flagParsers cmdui
    optionFields = commandOptions cmdui ParseArgs
    flagParsers = cmdOptionParsers optionFields

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

parseCommandWithOptparse
  :: CommandUI globalFlags
  -> (String -> Maybe (String -> [String] -> CommandParse action))
  -> [String]
  -> Maybe (CommandParse (globalFlags, CommandParse action))
parseCommandWithOptparse globalCommand parserForCommand argv =
  case commandParseArgs globalCommand True argv of
    CommandReadyToGo (mkGlobalFlags, cmdArgs0) -> do
      cmdName : cmdArgs <- pure cmdArgs0
      cmdParser <- parserForCommand cmdName
      let globalFlags = mkGlobalFlags (commandDefaultFlags globalCommand)
      pure $ CommandReadyToGo (globalFlags, cmdParser cmdName cmdArgs)
    _ -> Nothing

-- | A parser for one or more command names.
data NamedCommandParser action = NamedCommandParser
  { namedCommandNames :: [String]
  -- ^ The command name and its aliases.
  , namedCommandParser :: String -> [String] -> CommandParse action
  }

-- | Wrap a command's optparse parser together with the names it should match.
commandParserByName
  :: HelpColor
  -> Examples
  -> CommandUI (NixStyleFlags flags)
  -> (NixStyleFlags flags -> [String] -> action)
  -> NamedCommandParser action
commandParserByName helpColor examples command action =
  NamedCommandParser
    { namedCommandNames = commandNames command
    , namedCommandParser = \name args -> parseCommand helpColor examples command action name args
    }

-- | Parse a command using a list of name/parser associations, picking the first
-- match in the list.
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

parserInfo :: String -> Examples -> [O.Parser (CmdItem a)] -> CommandUI flags -> ParserInfo (ParsedCommand a)
parserInfo invokedName examples flagParsers cmdui =
  info
    (parsedCommandParser flagParsers <**> helper)
    ( fullDesc
        <> progDesc (helpDescriptionOrSynopsis cmdui)
        <> header ("cabal " ++ invokedName)
        <> footer (examples "cabal" invokedName)
    )

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
              <$ flag'
                ()
                (long "list-options" <> help "Print a list of command line flags")
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

optionFieldToGetOpt :: OptionField flags -> [GetOpt.OptDescr ()]
optionFieldToGetOpt (OptionField _ descrs) = concatMap optDescrToGetOpt descrs

optDescrToGetOpt :: OptDescr flags -> [GetOpt.OptDescr ()]
optDescrToGetOpt = \case
  ReqArg desc (shortFlags, longFlags) placeHolder _reader _showFlag ->
    [GetOpt.Option shortFlags longFlags (GetOpt.ReqArg (const ()) placeHolder) desc]
  OptArg desc (shortFlags, longFlags) placeHolder _reader _defaultFn _showFlag ->
    [GetOpt.Option shortFlags longFlags (GetOpt.OptArg (const ()) placeHolder) desc]
  ChoiceOpt choices ->
    [ GetOpt.Option shortFlags longFlags (GetOpt.NoArg ()) desc
    | (desc, (shortFlags, longFlags), _setFn, _getFn) <- choices
    ]
  BoolOpt desc trueFlags@(shortTrue, longTrue) falseFlags@(shortFalse, longFalse) _setFn _getFn
    | null shortFalse && null longFalse ->
        [GetOpt.Option shortTrue longTrue (GetOpt.NoArg ()) desc]
    | null shortTrue && null longTrue ->
        [GetOpt.Option shortFalse longFalse (GetOpt.NoArg ()) desc]
    | Just groupedLongFlag <- mkGroupedBoolLongFlag trueFlags falseFlags ->
        [GetOpt.Option [] [groupedLongFlag] (GetOpt.NoArg ()) ("Toggle " <> desc)]
    | otherwise ->
        [ GetOpt.Option shortTrue longTrue (GetOpt.NoArg ()) ("Enable " <> desc)
        , GetOpt.Option shortFalse longFalse (GetOpt.NoArg ()) ("Disable " <> desc)
        ]

mkGroupedBoolLongFlag :: (String, [String]) -> (String, [String]) -> Maybe String
mkGroupedBoolLongFlag ([], [longA]) ([], [longB]) =
  checkPair longA longB <|> checkPair longB longA
  where
    checkPair longEnable longDisable = do
      suffixEnable <- stripPrefix "enable-" longEnable
      suffixDisable <- stripPrefix "disable-" longDisable
      guard (suffixEnable == suffixDisable)
      pure ("[enable|disable]-" <> suffixEnable)
mkGroupedBoolLongFlag _ _ = Nothing

renderOptionRows :: (String -> String) -> Int -> Int -> Int -> [GetOpt.OptDescr ()] -> (String, [String])
renderOptionRows colorizeWarning maxFlagColumnWidth descColumn helpOutputWidth options =
  let rendered = [renderOption (index == 0) opt | (index, opt) <- zip [0 :: Int ..] options]
   in (concatMap fst rendered, concatMap snd rendered)
  where
    descriptionMarker = "• "
    markerPadding = replicate (length descriptionMarker) ' '
    descriptionIndent = replicate (2 + descColumn) ' '
    descriptionWidth = max 20 (helpOutputWidth - (2 + descColumn) - length descriptionMarker)

    renderOption isFirstInGroup opt =
      let (flagColumn, description) = getOptToColumns opt
          (capitalizedDescription, wasAutoCapitalized) = capitalizeDescription description
          wrappedDescription = wrapDescription descriptionWidth capitalizedDescription
          displayDescription =
            if wasAutoCapitalized
              then colorizeFirstAlpha wrappedDescription
              else wrappedDescription
          isStacked = length flagColumn > maxFlagColumnWidth
          spacer = if isStacked && not isFirstInGroup then "\n" else ""
          warning = ["Auto-capitalized help text for " <> flagColumn | wasAutoCapitalized]
          renderedRow =
            spacer
              <> if isStacked
                then renderStacked flagColumn displayDescription
                else renderInline flagColumn displayDescription
       in (renderedRow, warning)

    colorizeFirstAlpha :: [String] -> [String]
    colorizeFirstAlpha = go
      where
        go [] = []
        go (line : rest) =
          case colorizeFirstAlphaInLine line of
            Nothing -> line : go rest
            Just colored -> colored : rest

        colorizeFirstAlphaInLine :: String -> Maybe String
        colorizeFirstAlphaInLine = scan []
          where
            scan _ [] = Nothing
            scan acc (ch : cs)
              | isAlpha ch = Just (reverse acc <> colorizeWarning [ch] <> cs)
              | otherwise = scan (ch : acc) cs

    renderInline flagColumn descriptionLines =
      let padding = max 1 (descColumn - length flagColumn)
       in case descriptionLines of
            [] -> "  " <> flagColumn <> "\n"
            firstLineText : continuation ->
              let firstLine = "  " <> flagColumn <> replicate padding ' ' <> descriptionMarker <> firstLineText <> "\n"
                  continuationLines = [descriptionIndent <> markerPadding <> line <> "\n" | line <- continuation]
               in firstLine <> concat continuationLines

    renderStacked flagColumn descriptionLines =
      case descriptionLines of
        [] -> "  " <> flagColumn <> "\n"
        firstLineText : continuation ->
          "  "
            <> flagColumn
            <> "\n"
            <> descriptionIndent
            <> descriptionMarker
            <> firstLineText
            <> "\n"
            <> concat [descriptionIndent <> markerPadding <> line <> "\n" | line <- continuation]

wrapDescription :: Int -> String -> [String]
wrapDescription width description =
  case concatMap wrapParagraph (lines description) of
    [] -> [""]
    wrapped -> wrapped
  where
    wrapParagraph paragraph
      | null ws = [""]
      | otherwise = reverse (foldl' step [""] ws)
      where
        ws = words paragraph

        step (current : previous) word
          | null current = word : previous
          | length current + 1 + length word <= width = (current <> " " <> word) : previous
          | otherwise = word : current : previous
        step [] _ = []

capitalizeDescription :: String -> (String, Bool)
capitalizeDescription = go []
  where
    go acc [] = (reverse acc, False)
    go acc (ch : rest)
      | isAlpha ch =
          if isLower ch
            then (reverse acc <> (toUpper ch : rest), True)
            else (reverse acc <> (ch : rest), False)
      | otherwise = go (ch : acc) rest

getOptToColumns :: GetOpt.OptDescr () -> (String, String)
getOptToColumns (GetOpt.Option shortFlags longFlags argDescr description) =
  (intercalate ", " (renderShortFlags ++ renderLongFlags), description)
  where
    renderShortFlags = map renderShortFlag shortFlags

    renderShortFlag shortFlag =
      case argDescr of
        GetOpt.NoArg _ -> "-" <> [shortFlag]
        GetOpt.ReqArg _ metaVar -> "-" <> [shortFlag] <> " " <> metaVar
        GetOpt.OptArg _ metaVar -> "-" <> [shortFlag] <> "[" <> metaVar <> "]"

    renderLongFlags = map renderLongFlag longFlags

    renderLongFlag longFlag =
      case argDescr of
        GetOpt.NoArg _ -> "--" <> longFlag
        GetOpt.ReqArg _ metaVar -> "--" <> longFlag <> "=" <> metaVar
        GetOpt.OptArg _ metaVar -> "--" <> longFlag <> "[=" <> metaVar <> "]"

groupSequentially :: [a] -> [(groupName, a -> Bool)] -> ([(groupName, [a])], [a])
groupSequentially options groupingSpecs =
  let step remaining (groupName, keepPred) =
        let (groupMembers, leftovers) = partition keepPred remaining
         in (leftovers, (groupName, groupMembers))
      (leftoverOptions, groupedBuckets) = mapAccumL step options groupingSpecs
   in (groupedBuckets, leftoverOptions)

data OptionGroupKey
  = DeprecatedOptions
  | UnsupportedOptions
  | InstallLayoutOptions
  | IrrelevantOptions
  | HaddockOptions
  | TestOptions
  | BenchmarkOptions
  | ProfilingOptions
  | DependencySolvingOptions
  | ExecutableBuildOptions
  | LibraryBuildOptions
  | CoverageOptions
  | OutputAndArtifactOptions
  | ConfigurePhaseOptions
  | BuildPhaseControlOptions
  | CompilerAndParallelismOptions
  | LoggingAndReportingOptions
  | IncludeAndLinkerPathOptions
  | ProgramOverrideOptions
  deriving (Eq)

instance Show OptionGroupKey where
  show DeprecatedOptions = "Deprecated options"
  show UnsupportedOptions = "Unsupported options"
  show InstallLayoutOptions = "Install layout options"
  show IrrelevantOptions = "Irrelevant options"
  show HaddockOptions = "Haddock options"
  show TestOptions = "Test options"
  show BenchmarkOptions = "Benchmark options"
  show ProfilingOptions = "Profiling options"
  show DependencySolvingOptions = "Dependency solving options"
  show ExecutableBuildOptions = "Executable build options"
  show LibraryBuildOptions = "Library build options"
  show CoverageOptions = "Coverage options"
  show OutputAndArtifactOptions = "Output and artifact options"
  show ConfigurePhaseOptions = "Configure-phase options"
  show BuildPhaseControlOptions = "Build phase control options"
  show CompilerAndParallelismOptions = "Compiler and parallelism options"
  show LoggingAndReportingOptions = "Logging and reporting options"
  show IncludeAndLinkerPathOptions = "Include and linker path options"
  show ProgramOverrideOptions = "Program override options"

groupPredicates :: [(OptionGroupKey, OptionField a -> Bool)]
groupPredicates =
  [ (DeprecatedOptions, keepDeprecatedOptions)
  , (UnsupportedOptions, keepUnsupportedOptions)
  , (InstallLayoutOptions, keepInstallOptions)
  , (IrrelevantOptions, keepIrrelevantOptions)
  , (HaddockOptions, keepHaddockOptions)
  , (TestOptions, keepTestOptions)
  , (BenchmarkOptions, keepBenchOptions)
  , (ProfilingOptions, keepProfilingOptions)
  , (DependencySolvingOptions, keepSolvingOptions)
  , (ExecutableBuildOptions, keepExeOptions)
  , (LibraryBuildOptions, keepLibOptions)
  , (CoverageOptions, keepCoverageOptions)
  , (OutputAndArtifactOptions, keepOutputOptions)
  , (ConfigurePhaseOptions, keepConfigureOptions)
  , (BuildPhaseControlOptions, keepPhaseOptions)
  , (CompilerAndParallelismOptions, keepCompilerOptions)
  , (LoggingAndReportingOptions, keepLoggingOptions)
  , (IncludeAndLinkerPathOptions, keepIncludeOptions)
  , (ProgramOverrideOptions, keepProgOptions)
  ]

helpText :: HelpColor -> ReplaceCommandAlias -> CommandUI (NixStyleFlags a) -> String -> String -> String
helpText helpColor replaceBuildAlias buildCommand invokedName pname =
  commandSynopsis buildCommand
    <> "\n\n"
    <> colorizeUsageHeader helpColor (replaceBuildAlias invokedName (commandUsage buildCommand pname))
    <> maybe "" (('\n' :) . ($ pname)) (commandDescription buildCommand)
    <> "\n"
    <> colorizeHeader helpColor "Flags for build:"
    <> "\n"
    <> ungroupedRows
    <> groupedRows
    <> warningSection
    <> maybe "" (('\n' :) . colorizeExamplesHeader helpColor . replaceBuildAlias invokedName . ($ pname)) (commandNotes buildCommand)
  where
    commonHelpOptions :: [GetOpt.OptDescr ()]
    commonHelpOptions =
      [GetOpt.Option ['h'] ["help"] (GetOpt.NoArg ()) "Show this help text"]

    maxFlagColumnWidth :: Int
    maxFlagColumnWidth = 30

    helpOutputWidth :: Int
    helpOutputWidth = 100

    allOptions :: [GetOpt.OptDescr ()]
    allOptions =
      commonHelpOptions
        ++ concatMap optionFieldToGetOpt optsUngrouped
        ++ concatMap (concatMap optionFieldToGetOpt . snd) optsGrouped

    descColumn :: Int
    descColumn =
      min
        maxFlagColumnWidth
        ( maximum
            ( 0
                : map
                  (length . fst . getOptToColumns)
                  allOptions
            )
        )
        + 2

    (ungroupedRows, ungroupedWarnings) =
      renderOptionRows
        (colorizeWarningHeader helpColor)
        maxFlagColumnWidth
        descColumn
        helpOutputWidth
        (commonHelpOptions ++ concatMap optionFieldToGetOpt optsUngrouped)

    renderGroupToWidth = renderGroup helpColor maxFlagColumnWidth descColumn helpOutputWidth
    renderedGroups = map renderGroupToWidth optsGrouped

    groupedRows = concatMap fst renderedGroups

    groupedWarnings = concatMap snd renderedGroups

    warningSection =
      case ungroupedWarnings ++ groupedWarnings of
        [] -> ""
        warnings ->
          "\n"
            <> colorizeWarningHeader helpColor "Warnings:"
            <> "\n"
            <> concat ["  - " <> warning <> "\n" | warning <- warnings]

    (optsGrouped, optsUngrouped) =
      groupSequentially (commandOptions buildCommand ShowArgs) groupPredicates

renderGroup :: HelpColor -> Int -> Int -> Int -> (OptionGroupKey, [OptionField a]) -> (String, [String])
renderGroup helpColor maxFlagColumnWidth descColumn helpOutputWidth (title, options)
  | null options = ("", [])
  | title == InstallLayoutOptions = renderInstallLayoutGroupCompact helpColor helpOutputWidth options
  | otherwise =
      let (rows, warnings) =
            renderOptionRows
              (colorizeWarningHeader helpColor)
              maxFlagColumnWidth
              descColumn
              helpOutputWidth
              (concatMap optionFieldToGetOpt options)
       in ( "\n"
              <> colorizeHeader helpColor (show title <> ":")
              <> "\n"
              <> rows
          , warnings
          )

renderInstallLayoutGroupCompact :: HelpColor -> Int -> [OptionField a] -> (String, [String])
renderInstallLayoutGroupCompact helpColor helpOutputWidth options =
  ( "\n"
      <> colorizeHeader helpColor (show InstallLayoutOptions <> ":")
      <> "\n"
      <> concat ["  " <> line <> "\n" | line <- wrappedFlagLines]
  , []
  )
  where
    flagColumns = map (fst . getOptToColumns) (concatMap optionFieldToGetOpt options)
    compactFlags = ordNub flagColumns
    flagsLine = intercalate ", " compactFlags
    wrappedFlagLines = wrapDescription (max 40 (helpOutputWidth - 2)) flagsLine

-- | Whether command help is rendered with ANSI colour codes. Colour is for
-- terminals only; redirected output such as the generated docs is plain text.
data HelpColor = HelpColor | HelpPlain
  deriving (Eq, Show)

colorize :: HelpColor -> String -> String -> String
colorize HelpPlain _ text = text
colorize HelpColor code text = "\ESC[" <> code <> "m" <> text <> "\ESC[0m"

colorizeHeader :: HelpColor -> String -> String
colorizeHeader helpColor = colorize helpColor "32"

colorizeWarningHeader :: HelpColor -> String -> String
colorizeWarningHeader helpColor = colorize helpColor "31"

colorizeUsageHeader :: HelpColor -> String -> String
colorizeUsageHeader helpColor = T.unpack . T.replace (T.pack "Usage:") (T.pack $ colorizeHeader helpColor "Usage:") . T.pack

colorizeExamplesHeader :: HelpColor -> String -> String
colorizeExamplesHeader helpColor = T.unpack . T.replace (T.pack "Examples:") (T.pack $ colorizeHeader helpColor "Examples:") . T.pack
