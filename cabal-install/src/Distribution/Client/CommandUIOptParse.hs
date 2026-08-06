{-# LANGUAGE LambdaCase #-}

module Distribution.Client.CommandUIOptParse
  ( -- * Converting CommandUI options to optparse-applicative parsers
    optionFieldFlagParsers
  , optionFieldParser
  , optDescrParser
  , optionMods
  , flagMods

    -- * Converting CommandUI options to GetOpt descriptions
  , optionFieldToGetOpt
  , optDescrToGetOpt

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

import qualified Data.Text as T
import Data.Char (isLower)
import Data.List (mapAccumL, stripPrefix)
import Data.Monoid (Endo (..))
import qualified System.Console.GetOpt as GetOpt

import Distribution.ReadE (runReadE)
import Distribution.Simple.Command
  ( OptDescr (..)
  , OptionField (..)
  , ShowOrParseArgs (ShowArgs)
  , CommandUI (..)
  )
import Distribution.Client.NixStyleOptions
  ( keepBenchOptions
  , keepCompilerOptions
  , keepConfigureOptions
  , keepCoverageOptions
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
  , NixStyleFlags(..)
  )

import qualified Options.Applicative as O

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

groupPredicates :: [(String, OptionField a -> Bool)]
groupPredicates =
  [ ("Unsupported options", keepUnsupportedOptions)
  , ("Install layout options", keepInstallOptions)
  , ("Irrelevant options", keepIrrelevantOptions)
  , ("Haddock options", keepHaddockOptions)
  , ("Test options", keepTestOptions)
  , ("Benchmark options", keepBenchOptions)
  , ("Profiling options", keepProfilingOptions)
  , ("Dependency solving options", keepSolvingOptions)
  , ("Executable build options", keepExeOptions)
  , ("Library build options", keepLibOptions)
  , ("Coverage options", keepCoverageOptions)
  , ("Output and artifact options", keepOutputOptions)
  , ("Configure-phase options", keepConfigureOptions)
  , ("Build phase control options", keepPhaseOptions)
  , ("Compiler and parallelism options", keepCompilerOptions)
  , ("Logging and reporting options", keepLoggingOptions)
  , ("Include and linker path options", keepIncludeOptions)
  , ("Program override options", keepProgOptions)
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

    renderedGroups = map renderGroup optsGrouped

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

    renderGroup :: (String, [OptionField a]) -> (String, [String])
    renderGroup (title, options)
      | null options = ("", [])
      | otherwise =
          let (rows, warnings) =
                renderOptionRows
                  colorizeWarningHeader
                  maxFlagColumnWidth
                  descColumn
                  helpOutputWidth
                  (concatMap optionFieldToGetOpt options)
           in ( "\n"
                  <> colorizeHeader (title <> ":")
                  <> "\n"
                  <> rows
              , warnings
              )

    (optsGrouped, optsUngrouped) =
      groupSequentially (commandOptions buildCommand ShowArgs) groupPredicates

colorizeHeader :: String -> String
colorizeHeader text = "\ESC[32m" <> text <> "\ESC[0m"

colorizeWarningHeader :: String -> String
colorizeWarningHeader text = "\ESC[31m" <> text <> "\ESC[0m"

colorizeUsageHeader :: String -> String
colorizeUsageHeader = T.unpack . T.replace (T.pack "Usage:") (T.pack $ colorizeHeader "Usage:") . T.pack

colorizeExamplesHeader :: String -> String
colorizeExamplesHeader = T.unpack . T.replace (T.pack "Examples:") (T.pack $ colorizeHeader "Examples:") . T.pack
