{-# LANGUAGE LambdaCase #-}

-- | cabal-install CLI command: build
module Distribution.Client.CmdBuild
  ( -- * The @build@ CLI and action
    buildCommand
  , buildAction
  , parseBuildCommand
  , isBuildCommandName
  , BuildFlags (..)
  , defaultBuildFlags

    -- * Internals exposed for testing
  , selectPackageTargets
  , selectComponentTarget
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Client.CmdErrorMessages
import Distribution.Client.ProjectFlags
  ( removeIgnoreProjectOption
  )
import Distribution.Client.ProjectOrchestration
import Distribution.Client.TargetProblem
  ( TargetProblem (..)
  , TargetProblem'
  )

import qualified Data.Map as Map
import Data.Monoid (Endo (..), appEndo)
import qualified Data.Text as T
import qualified System.Console.GetOpt as GetOpt
import Distribution.Client.Errors
import Distribution.Client.NixStyleOptions
  ( NixStyleFlags (..)
  , cfgVerbosity
  , defaultNixStyleFlags
  , nixStyleOptions
  , removeBenchOptions
  , removeCompilerOptions
  , removeConfigureOptions
  , removeCoverageOptions
  , removeExeOptions
  , removeHaddockOptions
  , removeIncludeOptions
  , removeInstallOptions
  , removeIrrelevantOptions
  , removeLibOptions
  , removeLoggingOptions
  , removeOutputOptions
  , removePhaseOptions
  , removeProgOptions
  , removeProfilingOptions
  , removeSolvingOptions
  , removeTestOptions
  , removeUnsupportedOptions
  )
import Distribution.Client.ScriptUtils
  ( AcceptNoTargets (..)
  , TargetContext (..)
  , updateContextAndWriteProjectFile
  , withContextAndSelectors
  )
import Distribution.Client.Setup
  ( GlobalFlags
  , yesNoOpt
  )
import Distribution.Simple.Command
  ( CommandParse (..)
  , CommandUI (..)
  , OptDescr (..)
  , OptionField (..)
  , ShowOrParseArgs (ParseArgs, ShowArgs)
  , commandParseArgs
  , option
  , usageAlternatives
  )
import Distribution.Simple.Flag (Flag, fromFlag, toFlag)
import Distribution.Simple.Utils
  ( dieWithException
  , wrapText
  )
import Distribution.Verbosity
  ( normal
  )

import Distribution.ReadE (runReadE)

import qualified Options.Applicative as O

buildCommand :: CommandUI (NixStyleFlags BuildFlags)
buildCommand =
  CommandUI
    { commandName = "v2-build"
    , commandSynopsis = "Compile targets within the project."
    , commandUsage = usageAlternatives "v2-build" ["[TARGETS] [FLAGS]"]
    , commandDescription = Just $ \_ ->
        wrapText $
          "Build one or more targets from within the project. The available "
            ++ "targets are the packages in the project as well as individual "
            ++ "components within those packages, including libraries, executables, "
            ++ "test-suites or benchmarks. Targets can be specified by name or "
            ++ "location. If no target is specified then the default is to build "
            ++ "the package in the current directory.\n\n"
            ++ "Dependencies are built or rebuilt as necessary. Additional "
            ++ "configuration flags can be specified on the command line and these "
            ++ "extend the project configuration from the 'cabal.project', "
            ++ "'cabal.project.local' and other files."
    , commandNotes = Just $ \pname ->
        "Examples:\n"
          ++ "  - "
          ++ pname
          ++ " v2-build\n"
          ++ "      Build the package in the current directory "
          ++ "or all packages in the project\n"
          ++ "  - "
          ++ pname
          ++ " v2-build pkgname\n"
          ++ "      Build the package named pkgname in the project\n"
          ++ "  - "
          ++ pname
          ++ " v2-build ./pkgfoo\n"
          ++ "      Build the package in the ./pkgfoo directory\n"
          ++ "  - "
          ++ pname
          ++ " v2-build cname\n"
          ++ "      Build the component named cname in the project\n"
          ++ "  - "
          ++ pname
          ++ " v2-build cname --enable-profiling\n"
          ++ "      Build the component in profiling mode "
          ++ "(including dependencies as needed)\n"
    , commandDefaultFlags = defaultNixStyleFlags defaultBuildFlags
    , commandOptions =
        removeIgnoreProjectOption
          . nixStyleOptions
            ( \showOrParseArgs ->
                [ option
                    []
                    ["only-configure"]
                    "Instead of performing a full build just run the configure step"
                    buildOnlyConfigure
                    (\v flags -> flags{buildOnlyConfigure = v})
                    (yesNoOpt showOrParseArgs)
                ]
            )
    }

data BuildFlags = BuildFlags
  { buildOnlyConfigure :: Flag Bool
  }

defaultBuildFlags :: BuildFlags
defaultBuildFlags =
  BuildFlags
    { buildOnlyConfigure = toFlag False
    }

-- | The @build@ command does a lot. It brings the install plan up to date,
-- selects that part of the plan needed by the given or implicit targets and
-- then executes the plan.
--
-- For more details on how this works, see the module
-- "Distribution.Client.ProjectOrchestration"
buildAction :: NixStyleFlags BuildFlags -> [String] -> GlobalFlags -> IO ()
buildAction flags@NixStyleFlags{extraFlags = buildFlags} targetStrings globalFlags =
  withContextAndSelectors verbosity RejectNoTargets Nothing flags targetStrings globalFlags BuildCommand $ \targetCtx ctx targetSelectors -> do
    -- TODO: This flags defaults business is ugly
    let onlyConfigure =
          fromFlag
            ( buildOnlyConfigure defaultBuildFlags
                <> buildOnlyConfigure buildFlags
            )
        targetAction
          | onlyConfigure = TargetActionConfigure
          | otherwise = TargetActionBuild

    baseCtx <- case targetCtx of
      ProjectContext -> return ctx
      GlobalContext -> return ctx
      ScriptContext path exemeta -> updateContextAndWriteProjectFile ctx path exemeta

    buildCtx <-
      runProjectPreBuildPhase verbosity baseCtx $ \elaboratedPlan -> do
        -- Interpret the targets on the command line as build targets
        -- (as opposed to say repl or haddock targets).
        targets <-
          either (reportBuildTargetProblems verbosity) return $
            resolveTargetsFromSolver
              selectPackageTargets
              selectComponentTarget
              elaboratedPlan
              Nothing
              targetSelectors

        let elaboratedPlan' =
              pruneInstallPlanToTargets
                targetAction
                targets
                elaboratedPlan
        elaboratedPlan'' <-
          if buildSettingOnlyDeps (buildSettings baseCtx)
            then
              either (reportCannotPruneDependencies verbosity) return $
                pruneInstallPlanToDependencies
                  (Map.keysSet targets)
                  elaboratedPlan'
            else return elaboratedPlan'

        return (elaboratedPlan'', targets)

    printPlan verbosity baseCtx buildCtx

    buildOutcomes <- runProjectBuildPhase verbosity baseCtx buildCtx
    runProjectPostBuildPhase verbosity baseCtx buildCtx buildOutcomes
  where
    verbosity = cfgVerbosity normal flags

-- | This defines what a 'TargetSelector' means for the @bench@ command.
-- It selects the 'AvailableTarget's that the 'TargetSelector' refers to,
-- or otherwise classifies the problem.
--
-- For the @build@ command select all components except non-buildable
-- and disabled tests\/benchmarks, fail if there are no such
-- components
selectPackageTargets
  :: TargetSelector
  -> [AvailableTarget k]
  -> Either TargetProblem' [k]
selectPackageTargets targetSelector targets
  -- If there are any buildable targets then we select those
  | not (null targetsBuildable) =
      Right targetsBuildable
  -- If there are targets but none are buildable then we report those
  | not (null targets) =
      Left (TargetProblemNoneEnabled targetSelector targets')
  -- If there are no targets at all then we report that
  | otherwise =
      Left (TargetProblemNoTargets targetSelector)
  where
    targets' = forgetTargetsDetail targets
    targetsBuildable =
      selectBuildableTargetsWith
        (buildable targetSelector)
        targets

    -- When there's a target filter like "pkg:tests" then we do select tests,
    -- but if it's just a target like "pkg" then we don't build tests unless
    -- they are requested by default (i.e. by using --enable-tests)
    buildable (TargetPackage _ _ Nothing) TargetNotRequestedByDefault = False
    buildable (TargetAllPackages Nothing) TargetNotRequestedByDefault = False
    buildable _ _ = True

-- | For a 'TargetComponent' 'TargetSelector', check if the component can be
-- selected.
--
-- For the @build@ command we just need the basic checks on being buildable etc.
selectComponentTarget
  :: SubComponentTarget
  -> AvailableTarget k
  -> Either TargetProblem' k
selectComponentTarget = selectComponentTargetBasic

reportBuildTargetProblems :: Verbosity -> [TargetProblem'] -> IO a
reportBuildTargetProblems verbosity problems =
  reportTargetProblems verbosity "build" problems

reportCannotPruneDependencies :: Verbosity -> CannotPruneDependencies -> IO a
reportCannotPruneDependencies verbosity =
  dieWithException verbosity . ReportCannotPruneDependencies . renderCannotPruneDependencies

buildCommandNames :: [String]
buildCommandNames = ["build", "new-build", commandName buildCommand]

isBuildCommandName :: String -> Bool
isBuildCommandName name = name `elem` buildCommandNames

buildListOptions :: [String]
buildListOptions =
  case commandParseArgs buildCommand False ["--list-options"] of
    CommandList opts -> opts
    _ -> []

type BuildOptionField = OptionField (NixStyleFlags BuildFlags)

buildHelpText :: String -> String -> String
buildHelpText invokedName pname =
  commandSynopsis buildCommand
    <> "\n\n"
    <> colorizeUsageHeader (replaceBuildAlias invokedName (commandUsage buildCommand pname))
    <> maybe "" (('\n' :) . ($ pname)) (commandDescription buildCommand)
    <> "\n"
    <> colorizeHeader "Flags for build:"
    <> GetOpt.usageInfo "" (commonHelpOptions ++ concatMap optionFieldToGetOpt buildUngroupedOptions)
    <> concatMap renderGroup buildOptionGroups
    <> maybe "" (('\n' :) . colorizeExamplesHeader . replaceBuildAlias invokedName . ($ pname)) (commandNotes buildCommand)
  where
    commonHelpOptions :: [GetOpt.OptDescr ()]
    commonHelpOptions =
      [GetOpt.Option ['h'] ["help"] (GetOpt.NoArg ()) "Show this help text"]

    renderGroup :: (String, [BuildOptionField]) -> String
    renderGroup (title, options)
      | null options = ""
      | otherwise = "\n" <> colorizeHeader (title <> ":") <> GetOpt.usageInfo "" (concatMap optionFieldToGetOpt options)

buildOptionGroups :: [(String, [BuildOptionField])]
buildOptionGroups =
  [ ("Unsupported options", unsupported)
  , ("Install layout options", install)
  , ("Irrelevant options", irrelevant)
  , ("Haddock options", haddock)
  , ("Test options", test)
  , ("Benchmark options", bench)
  , ("Profiling options", profiling)
  , ("Dependency solving options", solving)
  , ("Executable build options", exe)
  , ("Library build options", lib)
  , ("Coverage options", coverage)
  , ("Output and artifact options", output)
  , ("Configure-phase options", configure)
  , ("Build phase control options", phase)
  , ("Compiler and parallelism options", compiler)
  , ("Logging and reporting options", logging)
  , ("Include and linker path options", includePaths)
  , ("Program override options", prog)
  ]
  where
    opts0 = commandOptions buildCommand ShowArgs

    (unsupported, opts1) = splitBy removeUnsupportedOptions opts0
    (install, opts2) = splitBy removeInstallOptions opts1
    (irrelevant, opts3) = splitBy removeIrrelevantOptions opts2
    (haddock, opts4) = splitBy removeHaddockOptions opts3
    (test, opts5) = splitBy removeTestOptions opts4
    (bench, opts6) = splitBy removeBenchOptions opts5
    (profiling, opts7) = splitBy removeProfilingOptions opts6
    (solving, opts8) = splitBy removeSolvingOptions opts7
    (exe, opts9) = splitBy removeExeOptions opts8
    (lib, opts10) = splitBy removeLibOptions opts9
    (coverage, opts11) = splitBy removeCoverageOptions opts10
    (output, opts12) = splitBy removeOutputOptions opts11
    (configure, opts13) = splitBy removeConfigureOptions opts12
    (phase, opts14) = splitBy removePhaseOptions opts13
    (compiler, opts15) = splitBy removeCompilerOptions opts14
    (logging, opts16) = splitBy removeLoggingOptions opts15
    (includePaths, opts17) = splitBy removeIncludeOptions opts16
    (prog, _opts18) = splitBy removeProgOptions opts17

buildUngroupedOptions :: [BuildOptionField]
buildUngroupedOptions =
  opts18
  where
    opts0 = commandOptions buildCommand ShowArgs
    (_, opts1) = splitBy removeUnsupportedOptions opts0
    (_, opts2) = splitBy removeInstallOptions opts1
    (_, opts3) = splitBy removeIrrelevantOptions opts2
    (_, opts4) = splitBy removeHaddockOptions opts3
    (_, opts5) = splitBy removeTestOptions opts4
    (_, opts6) = splitBy removeBenchOptions opts5
    (_, opts7) = splitBy removeProfilingOptions opts6
    (_, opts8) = splitBy removeSolvingOptions opts7
    (_, opts9) = splitBy removeExeOptions opts8
    (_, opts10) = splitBy removeLibOptions opts9
    (_, opts11) = splitBy removeCoverageOptions opts10
    (_, opts12) = splitBy removeOutputOptions opts11
    (_, opts13) = splitBy removeConfigureOptions opts12
    (_, opts14) = splitBy removePhaseOptions opts13
    (_, opts15) = splitBy removeCompilerOptions opts14
    (_, opts16) = splitBy removeLoggingOptions opts15
    (_, opts17) = splitBy removeIncludeOptions opts16
    (_, opts18) = splitBy removeProgOptions opts17

splitBy
  :: (BuildOptionField -> Bool)
  -> [BuildOptionField]
  -> ([BuildOptionField], [BuildOptionField])
splitBy keepPred = partition (not . keepPred)

optionFieldToGetOpt :: BuildOptionField -> [GetOpt.OptDescr ()]
optionFieldToGetOpt (OptionField _ descrs) = concatMap optDescrToGetOpt descrs

optDescrToGetOpt :: OptDescr (NixStyleFlags BuildFlags) -> [GetOpt.OptDescr ()]
optDescrToGetOpt = \case
  ReqArg desc (shortFlags, longFlags) placeHolder _reader _showFlag ->
    [GetOpt.Option shortFlags longFlags (GetOpt.ReqArg (const ()) placeHolder) desc]
  OptArg desc (shortFlags, longFlags) placeHolder _reader (_defaultValue, _defaultSetter) _showFlag ->
    [GetOpt.Option shortFlags longFlags (GetOpt.OptArg (const ()) placeHolder) desc]
  ChoiceOpt choices ->
    [ GetOpt.Option shortFlags longFlags (GetOpt.NoArg ()) desc
    | (desc, (shortFlags, longFlags), _setFn, _getFn) <- choices
    ]
  BoolOpt desc (shortTrue, longTrue) (shortFalse, longFalse) _setFn _getFn
    | null shortFalse && null longFalse ->
        [GetOpt.Option shortTrue longTrue (GetOpt.NoArg ()) desc]
    | null shortTrue && null longTrue ->
        [GetOpt.Option shortFalse longFalse (GetOpt.NoArg ()) desc]
    | otherwise ->
        [ GetOpt.Option shortTrue longTrue (GetOpt.NoArg ()) ("Enable " <> desc)
        , GetOpt.Option shortFalse longFalse (GetOpt.NoArg ()) ("Disable " <> desc)
        ]

replaceBuildAlias :: String -> String -> String
replaceBuildAlias invokedName = T.unpack . T.replace (T.pack "v2-build") (T.pack invokedName) . T.pack

colorizeHeader :: String -> String
colorizeHeader text = "\ESC[32m" <> text <> "\ESC[0m"

colorizeUsageHeader :: String -> String
colorizeUsageHeader = T.unpack . T.replace (T.pack "Usage:") (T.pack $ colorizeHeader "Usage:") . T.pack

colorizeExamplesHeader :: String -> String
colorizeExamplesHeader = T.unpack . T.replace (T.pack "Examples:") (T.pack $ colorizeHeader "Examples:") . T.pack

parseBuildCommand :: String -> [String] -> CommandParse (GlobalFlags -> IO ())
parseBuildCommand invokedName cmdArgs =
  case O.execParserPure O.defaultPrefs (buildParserInfo invokedName) cmdArgs of
    O.Success parsed ->
      if parsedListOptions parsed
        then CommandList buildListOptions
        else
          let flags = appEndo (parsedFlagEdits parsed) (commandDefaultFlags buildCommand)
           in CommandReadyToGo (buildAction flags (parsedTargets parsed))
    O.Failure failure ->
      let (msg, exitCode) = O.renderFailure failure ("cabal " ++ invokedName)
       in if exitCode == ExitSuccess
            then CommandHelp (buildHelpText invokedName)
            else CommandErrors [msg]
    O.CompletionInvoked _ ->
      CommandErrors ["Shell completion is not supported by this parser path."]

buildParserInfo :: String -> O.ParserInfo ParsedBuildCommand
buildParserInfo invokedName =
  O.info
    (parsedBuildCommandParser O.<**> O.helper)
    ( O.fullDesc
        <> O.progDesc (buildHelpDescription)
        <> O.header ("cabal " ++ invokedName)
        <> O.footer (buildExamplesSection invokedName)
    )

buildHelpDescription :: String
buildHelpDescription =
  case commandDescription buildCommand of
    Nothing -> commandSynopsis buildCommand
    Just mkDescription -> mkDescription "cabal"

buildExamplesSection :: String -> String
buildExamplesSection invokedName =
  unlines
    [ "Examples:"
    , "  - " <> invokedName
    , "      Build the package in the current directory or all packages in the project"
    , "  - " <> invokedName <> " pkgname"
    , "      Build the package named pkgname in the project"
    , "  - " <> invokedName <> " ./pkgfoo"
    , "      Build the package in the ./pkgfoo directory"
    , "  - " <> invokedName <> " cname"
    , "      Build the component named cname in the project"
    , "  - " <> invokedName <> " cname --enable-profiling"
    , "      Build the component in profiling mode (including dependencies as needed)"
    ]

data ParsedBuildCommand = ParsedBuildCommand
  { parsedFlagEdits :: Endo (NixStyleFlags BuildFlags)
  , parsedTargets :: [String]
  , parsedListOptions :: Bool
  }

data BuildItem
  = BuildItemFlag (Endo (NixStyleFlags BuildFlags))
  | BuildItemTarget String
  | BuildItemListOptions

parsedBuildCommandParser :: O.Parser ParsedBuildCommand
parsedBuildCommandParser = toParsed <$> O.many buildItemParser
  where
    toParsed items =
      let edits = [e | BuildItemFlag e <- items]
          targets = [t | BuildItemTarget t <- items]
          listOptionsSeen = any isListOptions items
       in ParsedBuildCommand
            { parsedFlagEdits = mconcat edits
            , parsedTargets = targets
            , parsedListOptions = listOptionsSeen
            }

    isListOptions BuildItemListOptions = True
    isListOptions _ = False

buildItemParser :: O.Parser BuildItem
buildItemParser =
  O.asum
    ( buildOptionParsers
        ++ [ BuildItemListOptions
               <$ O.flag'
                 ()
                 (O.long "list-options" <> O.help "Print a list of command line flags")
           , BuildItemTarget <$> O.strArgument (O.metavar "TARGET")
           ]
    )

buildOptionParsers :: [O.Parser BuildItem]
buildOptionParsers =
  concatMap optionFieldParsers (commandOptions buildCommand ParseArgs)

optionFieldParsers :: OptionField (NixStyleFlags BuildFlags) -> [O.Parser BuildItem]
optionFieldParsers (OptionField _ descrs) = concatMap optDescrParsers descrs

optDescrParsers :: OptDescr (NixStyleFlags BuildFlags) -> [O.Parser BuildItem]
optDescrParsers = \case
  ReqArg desc optFlags placeHolder reader _show ->
    [ BuildItemFlag . Endo
        <$> O.option
          (O.eitherReader (runReadE reader))
          (optionMods optFlags <> O.metavar placeHolder <> O.help desc)
    ]
  OptArg desc optFlags placeHolder reader (_defaultText, defaultFn) _show ->
    [ BuildItemFlag . Endo
        <$> ( O.option
                (O.eitherReader (runReadE reader))
                (optionMods optFlags <> O.metavar placeHolder <> O.help desc)
                <|> O.flag' defaultFn (flagMods optFlags <> O.internal)
            )
    ]
  ChoiceOpt choices ->
    [ BuildItemFlag (Endo setFn)
        <$ O.flag' () (flagMods optFlags <> O.help desc)
    | (desc, optFlags, setFn, _get) <- choices
    ]
  BoolOpt desc trueFlags falseFlags setFn _get ->
    [ BuildItemFlag (Endo (setFn True))
        <$ O.flag' () (flagMods trueFlags <> O.help desc)
    , BuildItemFlag (Endo (setFn False))
        <$ O.flag' () (flagMods falseFlags <> O.help desc)
    ]

optionMods :: (String, [String]) -> O.Mod O.OptionFields a
optionMods (shortFlags, longFlags) =
  mconcat (map O.short shortFlags) <> mconcat (map O.long longFlags)

flagMods :: (String, [String]) -> O.Mod O.FlagFields a
flagMods (shortFlags, longFlags) =
  mconcat (map O.short shortFlags) <> mconcat (map O.long longFlags)
