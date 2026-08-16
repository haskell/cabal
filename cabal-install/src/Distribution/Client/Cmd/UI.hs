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
  , CmdItem (..)
  , ParsedCommand (..)
  , parsedCommandParser
  , cmdItemParser
  , cmdOptionParsers
  , cmdSpec
  , cmdListOptions
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
  , ShowOrParseArgs (ShowArgs)
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

type Examples = String -> String -> String

replaceCommandAlias :: String -> ReplaceCommandAlias
replaceCommandAlias commandName invokedName =
  T.unpack . T.replace (T.pack commandName) (T.pack invokedName) . T.pack

cmdSpec
  :: CommandUI flags
  -> (flags -> [String] -> action)
  -> [CommandSpec action]
cmdSpec command action =
  [CommandSpec ui (`commandAddAction` action) NormalCommand]
  where
    defaultMsg = T.unpack . T.replace "v2-" "" . T.pack
    ui =
      command
        { commandName = defaultMsg (commandName command)
        , commandUsage = defaultMsg . commandUsage command
        , commandDescription = (defaultMsg .) <$> commandDescription command
        , commandNotes = (defaultMsg .) <$> commandNotes command
        }

cmdListOptions :: CommandUI flags -> [String]
cmdListOptions command =
  case commandParseArgs command False ["--list-options"] of
    CommandList opts -> opts
    _ -> []

parseCommand
  :: String
  -> [String]
  -> Examples
  -> [O.Parser (CmdItem a)]
  -> CommandUI (NixStyleFlags a)
  -> [String]
  -> (NixStyleFlags a -> [String] -> action)
  -> ReplaceCommandAlias
  -> CommandParse action
parseCommand invokedName cmdArgs examples flagParsers cmdui listOptions action replaceAlias =
  case execParserPure defaultPrefs pInfo cmdArgs of
    Success parsed ->
      if parsedListOptions parsed
        then CommandList listOptions
        else
          let flags = appEndo (parsedFlagEdits parsed) (commandDefaultFlags cmdui)
           in CommandReadyToGo (action flags (parsedTargets parsed))
    Failure failure ->
      let (msg, exitCode) = renderFailure failure ("cabal " ++ invokedName)
       in if exitCode == ExitSuccess
            then CommandHelp (helpText replaceAlias cmdui invokedName)
            else CommandErrors [msg]
    CompletionInvoked _ ->
      CommandErrors ["Shell completion is not supported by this parser path."]
  where
    pInfo = parserInfo invokedName examples flagParsers cmdui

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
            { parsedFlagEdits = mconcat edits
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
  OptArg desc optFlags placeHolder reader (_defaultText, defaultFn) _show ->
    [ Endo
        <$> ( O.option
                (O.eitherReader (runReadE reader))
                (optionMods optFlags <> O.metavar placeHolder <> O.help desc)
                <|> O.flag' defaultFn (flagMods optFlags <> O.internal)
            )
    ]
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
  OptArg desc (shortFlags, longFlags) placeHolder _reader (_defaultValue, _defaultSetter) _showFlag ->
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

type ReplaceCommandAlias = String -> String -> String

helpText :: ReplaceCommandAlias -> CommandUI (NixStyleFlags a) -> String -> String -> String
helpText replaceBuildAlias buildCommand invokedName pname =
  commandSynopsis buildCommand
    <> "\n\n"
    <> colorizeUsageHeader (replaceBuildAlias invokedName (commandUsage buildCommand pname))
    <> maybe "" (('\n' :) . ($ pname)) (commandDescription buildCommand)
    <> "\n"
    <> colorizeHeader "Flags for build:"
    <> "\n"
    <> ungroupedRows
    <> groupedRows
    <> warningSection
    <> maybe "" (('\n' :) . colorizeExamplesHeader . replaceBuildAlias invokedName . ($ pname)) (commandNotes buildCommand)
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
        colorizeWarningHeader
        maxFlagColumnWidth
        descColumn
        helpOutputWidth
        (commonHelpOptions ++ concatMap optionFieldToGetOpt optsUngrouped)

    renderGroupToWidth = renderGroup maxFlagColumnWidth descColumn helpOutputWidth
    renderedGroups = map renderGroupToWidth optsGrouped

    groupedRows = concatMap fst renderedGroups

    groupedWarnings = concatMap snd renderedGroups

    warningSection =
      case ungroupedWarnings ++ groupedWarnings of
        [] -> ""
        warnings ->
          "\n"
            <> colorizeWarningHeader "Warnings:"
            <> "\n"
            <> concat ["  - " <> warning <> "\n" | warning <- warnings]

    (optsGrouped, optsUngrouped) =
      groupSequentially (commandOptions buildCommand ShowArgs) groupPredicates

renderGroup :: Int -> Int -> Int -> (OptionGroupKey, [OptionField a]) -> (String, [String])
renderGroup maxFlagColumnWidth descColumn helpOutputWidth (title, options)
  | null options = ("", [])
  | title == InstallLayoutOptions = renderInstallLayoutGroupCompact helpOutputWidth options
  | otherwise =
      let (rows, warnings) =
            renderOptionRows
              colorizeWarningHeader
              maxFlagColumnWidth
              descColumn
              helpOutputWidth
              (concatMap optionFieldToGetOpt options)
       in ( "\n"
              <> colorizeHeader (show title <> ":")
              <> "\n"
              <> rows
          , warnings
          )

renderInstallLayoutGroupCompact :: Int -> [OptionField a] -> (String, [String])
renderInstallLayoutGroupCompact helpOutputWidth options =
  ( "\n"
      <> colorizeHeader (show InstallLayoutOptions <> ":")
      <> "\n"
      <> concat ["  " <> line <> "\n" | line <- wrappedFlagLines]
  , []
  )
  where
    flagColumns = map (fst . getOptToColumns) (concatMap optionFieldToGetOpt options)
    compactFlags = ordNub flagColumns
    flagsLine = intercalate ", " compactFlags
    wrappedFlagLines = wrapDescription (max 40 (helpOutputWidth - 2)) flagsLine

colorizeHeader :: String -> String
colorizeHeader text = "\ESC[32m" <> text <> "\ESC[0m"

colorizeWarningHeader :: String -> String
colorizeWarningHeader text = "\ESC[31m" <> text <> "\ESC[0m"

colorizeUsageHeader :: String -> String
colorizeUsageHeader = T.unpack . T.replace (T.pack "Usage:") (T.pack $ colorizeHeader "Usage:") . T.pack

colorizeExamplesHeader :: String -> String
colorizeExamplesHeader = T.unpack . T.replace (T.pack "Examples:") (T.pack $ colorizeHeader "Examples:") . T.pack
