module Main where

import Control.Applicative (Alternative (many, (<|>)), (<**>))
import Control.Exception (Exception (displayException), catch, throw, throwIO)
import Control.Monad (forM_, unless, when)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString
import Data.Data (Typeable)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as T (toStrict)
import qualified Data.Text.Lazy.Encoding as T (decodeUtf8)
import Data.Time.Clock (DiffTime, secondsToDiffTime)
import Data.Time.Clock.System (getSystemTime, systemToTAITime)
import Data.Time.Clock.TAI (AbsoluteTime, diffAbsoluteTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Version (Version, makeVersion, parseVersion, showVersion)
import GHC.Conc (getNumCapabilities)
import Options.Applicative
  ( FlagFields
  , Mod
  , Parser
  , ParserInfo
  , auto
  , execParser
  , flag
  , flag'
  , fullDesc
  , help
  , helper
  , hidden
  , info
  , long
  , maybeReader
  , option
  , progDesc
  , short
  , strOption
  , switch
  , value
  )
import qualified Options.Applicative as Opt
import System.Console.ANSI
  ( Color (Blue, Cyan, Green, Red)
  , ColorIntensity (Vivid)
  , ConsoleIntensity (BoldIntensity)
  , ConsoleLayer (Foreground)
  , SGR (Reset, SetColor, SetConsoleIntensity)
  , setSGRCode
  )
import qualified System.Console.Terminal.Size as Terminal
import System.Directory (getCurrentDirectory, withCurrentDirectory)
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitFailure, exitSuccess)
import System.FilePath ((</>))
import System.Info (arch, os)
import System.Process.Typed (ExitCodeException (..), proc, readProcess, readProcessStdout_, runProcess)
import Text.ParserCombinators.ReadP (readP_to_S)

tShow :: Show a => a -> Text
tShow = T.pack . show

tSetSGRCode :: [SGR] -> Text
tSetSGRCode = T.pack . setSGRCode

decodeStrip :: ByteString -> Text
decodeStrip = T.strip . T.toStrict . T.decodeUtf8

-- | Command-line options, resolved with context from the environment.
data ResolvedOpts = ResolvedOpts
  { verbose :: Bool
  , jobs :: Int
  , cwd :: FilePath
  , startTime :: AbsoluteTime
  , compiler :: Compiler
  , extraCompilers :: [FilePath]
  , cabal :: FilePath
  , hackageTests :: HackageTests
  , archPath :: FilePath
  , projectFile :: FilePath
  , tastyArgs :: [String]
  , targets :: [String]
  , steps :: [Step]
  }
  deriving (Show)

data Compiler = Compiler
  { compilerExecutable :: FilePath
  , compilerVersion :: Version
  }
  deriving (Show)

data VersionParseException = VersionParseException
  { versionInput :: String
  , versionExecutable :: FilePath
  }
  deriving (Typeable, Show)

instance Exception VersionParseException where
  displayException exception =
    "Failed to parse `"
      <> versionExecutable exception
      <> " --numeric-version` output: "
      <> show (versionInput exception)

makeCompiler :: FilePath -> IO Compiler
makeCompiler executable = do
  stdout <-
    readProcessStdout_ $
      proc executable ["--numeric-version"]
  let version = T.unpack $ T.strip $ T.toStrict $ T.decodeUtf8 stdout
      parsedVersions = readP_to_S parseVersion version
      -- Who needs error messages? Those aren't in the API.
      maybeParsedVersion =
        listToMaybe
          [ parsed
          | (parsed, []) <- parsedVersions
          ]
      parsedVersion = case maybeParsedVersion of
        Just parsedVersion' -> parsedVersion'
        Nothing ->
          throw
            VersionParseException
              { versionInput = version
              , versionExecutable = executable
              }

  pure
    Compiler
      { compilerExecutable = executable
      , compilerVersion = parsedVersion
      }

baseHc :: ResolvedOpts -> FilePath
baseHc opts = "ghc-" <> showVersion (compilerVersion $ compiler opts)

baseBuildDir :: ResolvedOpts -> FilePath
baseBuildDir opts = "dist-newstyle-validate-" <> baseHc opts

buildDir :: ResolvedOpts -> FilePath
buildDir opts =
  cwd opts
    </> baseBuildDir opts
    </> "build"
    </> archPath opts
    </> baseHc opts

jobsArgs :: ResolvedOpts -> [String]
jobsArgs opts = ["--num-threads", show $ jobs opts]

cabalArgs :: ResolvedOpts -> [String]
cabalArgs opts =
  [ "--jobs=" <> show (jobs opts)
  , "--with-compiler=" <> compilerExecutable (compiler opts)
  , "--builddir=" <> baseBuildDir opts
  , "--project-file=" <> projectFile opts
  ]

cabalTestsuiteBuildDir :: ResolvedOpts -> FilePath
cabalTestsuiteBuildDir opts =
  buildDir opts
    </> "cabal-testsuite-3"

cabalNewBuildArgs :: ResolvedOpts -> [String]
cabalNewBuildArgs opts = "build" : cabalArgs opts

cabalListBinArgs :: ResolvedOpts -> [String]
cabalListBinArgs opts = "list-bin" : cabalArgs opts

cabalListBin :: ResolvedOpts -> String -> IO FilePath
cabalListBin opts target = do
  let args = cabalListBinArgs opts ++ [target]
  stdout <-
    readProcessStdout_ $
      proc (cabal opts) args

  pure (T.unpack $ T.strip $ T.toStrict $ T.decodeUtf8 stdout)

rtsArgs :: ResolvedOpts -> [String]
rtsArgs opts =
  case archPath opts of
    "x86_64-windows" ->
      -- See: https://github.com/haskell/cabal/issues/9571
      if compilerVersion (compiler opts) > makeVersion [9, 0, 2]
        then ["+RTS", "--io-manager=native", "-RTS"]
        else []
    _ -> []

resolveOpts :: Opts -> IO ResolvedOpts
resolveOpts opts = do
  let optionals :: Bool -> [a] -> [a]
      optionals True items = items
      optionals False _ = []

      optional :: Bool -> a -> [a]
      optional keep item = optionals keep [item]

      steps' =
        if not (null (rawSteps opts))
          then rawSteps opts
          else
            concat
              [
                [ PrintConfig
                , PrintToolVersions
                , Build
                ]
              , optional (rawDoctest opts) Doctest
              , optional (rawRunLibTests opts) LibTests
              , optional (rawRunLibSuite opts) LibSuite
              , optional (rawRunLibSuite opts && not (null (rawExtraCompilers opts))) LibSuiteExtras
              , optional (rawRunCliTests opts && not (rawLibOnly opts)) CliTests
              , optional (rawRunCliSuite opts && not (rawLibOnly opts)) CliSuite
              , optionals (rawSolverBenchmarks opts) [SolverBenchmarksTests, SolverBenchmarksRun]
              , [TimeSummary]
              ]

      targets' =
        concat
          [
            [ "Cabal"
            , "Cabal-hooks"
            , "cabal-testsuite"
            , "Cabal-tests"
            , "Cabal-QuickCheck"
            , "Cabal-tree-diff"
            , "Cabal-described"
            ]
          , optionals
              (CliTests `elem` steps')
              [ "cabal-install"
              , "cabal-install-solver"
              , "cabal-benchmarks"
              ]
          , optional (rawSolverBenchmarks opts) "solver-benchmarks"
          ]

      archPath' =
        let osPath =
              case os of
                "darwin" -> "osx"
                "linux" -> "linux"
                "mingw32" -> "windows"
                _ -> os -- TODO: Warning?
         in arch <> "-" <> osPath

      projectFile' =
        if rawLibOnly opts
          then "cabal.validate-libonly.project"
          else "cabal.validate.project"

      tastyArgs' =
        "--hide-successes"
          : case rawTastyPattern opts of
            Just tastyPattern -> ["--pattern", tastyPattern]
            Nothing -> []

  when (rawListSteps opts) $ do
    -- TODO: This should probably list _all_ available steps, not just the selected ones!
    putStrLn "Targets:"
    forM_ targets' $ \target -> do
      putStrLn $ "  " <> target
    putStrLn "Steps:"
    forM_ steps' $ \step -> do
      putStrLn $ "  " <> displayStep step
    exitSuccess

  startTime' <- getAbsoluteTime
  jobs' <- maybe getNumCapabilities pure (rawJobs opts)
  cwd' <- getCurrentDirectory
  compiler' <- makeCompiler (rawCompiler opts)

  pure
    ResolvedOpts
      { verbose = rawVerbose opts
      , jobs = jobs'
      , cwd = cwd'
      , startTime = startTime'
      , compiler = compiler'
      , extraCompilers = rawExtraCompilers opts
      , cabal = rawCabal opts
      , archPath = archPath'
      , projectFile = projectFile'
      , hackageTests = rawHackageTests opts
      , tastyArgs = tastyArgs'
      , targets = targets'
      , steps = steps'
      }

-- | Command-line options.
data Opts = Opts
  { rawVerbose :: Bool
  , rawJobs :: Maybe Int
  , rawCompiler :: FilePath
  , rawCabal :: FilePath
  , rawExtraCompilers :: [FilePath]
  , rawTastyPattern :: Maybe String
  , rawDoctest :: Bool
  , rawSteps :: [Step]
  , rawListSteps :: Bool
  , rawLibOnly :: Bool
  , rawRunLibTests :: Bool
  , rawRunCliTests :: Bool
  , rawRunLibSuite :: Bool
  , rawRunCliSuite :: Bool
  , rawSolverBenchmarks :: Bool
  , rawHackageTests :: HackageTests
  }
  deriving (Show)

optsParser :: Parser Opts
optsParser =
  Opts
    <$> ( flag'
            True
            ( short 'v'
                <> long "verbose"
                <> help "Always display build and test output"
            )
            <|> flag
              False
              False
              ( short 'q'
                  <> long "quiet"
                  <> help "Silence build and test output"
              )
        )
    <*> option
      (Just <$> auto)
      ( short 'j'
          <> long "jobs"
          <> help "Passed to `cabal build --jobs`"
          <> value Nothing
      )
    <*> strOption
      ( short 'w'
          <> long "with-compiler"
          <> help "Build Cabal with the given compiler instead of `ghc`"
          <> value "ghc"
      )
    <*> strOption
      ( long "with-cabal"
          <> help "Test the given `cabal-install` (the `cabal` on your `$PATH` is used for builds)"
          <> value "cabal"
      )
    <*> many
      ( strOption
          ( long "extra-hc"
              <> help "Extra compilers to run the test suites against"
          )
      )
    <*> option
      (Just <$> Opt.str)
      ( short 'p'
          <> long "pattern"
          <> help "Pattern to filter tests by"
          <> value Nothing
      )
    <*> boolOption
      False
      "doctest"
      ( help "Run doctest on the `Cabal` library"
      )
    <*> many
      ( option
          (maybeReader parseStep)
          ( short 's'
              <> long "step"
              <> help "Run only a specific step (can be specified multiple times)"
          )
      )
    <*> switch
      ( long "list-steps"
          <> help "List the available steps and exit"
      )
    <*> ( flag'
            True
            ( long "lib-only"
                <> help "Test only `Cabal` (the library)"
            )
            <|> flag
              False
              False
              ( long "cli"
                  <> help "Test `cabal-install` (the executable) in addition to `Cabal` (the library)"
              )
        )
    <*> boolOption
      True
      "run-lib-tests"
      ( help "Run tests for the `Cabal` library"
      )
    <*> boolOption
      True
      "run-cli-tests"
      ( help "Run client tests for the `cabal-install` executable"
      )
    <*> boolOption
      False
      "run-lib-suite"
      ( help "Run `cabal-testsuite` with the `Cabal` library"
      )
    <*> boolOption
      False
      "run-cli-suite"
      ( help "Run `cabal-testsuite` with the `cabal-install` executable"
      )
    <*> boolOption
      False
      "solver-benchmarks"
      ( help "Build and trial run `solver-benchmarks`"
      )
    <*> ( flag'
            CompleteHackageTests
            ( long "complete-hackage-tests"
                <> help "Run `hackage-tests` on complete Hackage data"
            )
            <|> flag
              NoHackageTests
              PartialHackageTests
              ( long "partial-hackage-tests"
                  <> help "Run `hackage-tests` on parts of Hackage data"
              )
        )

-- | Parse a boolean switch with separate names for the true and false options.
boolOption' :: Bool -> String -> String -> Mod FlagFields Bool -> Parser Bool
boolOption' defaultValue trueName falseName modifiers =
  flag' True (modifiers <> long trueName)
    <|> flag defaultValue False (modifiers <> hidden <> long falseName)

-- | Parse a boolean switch with a `--no-*` flag for setting the option to false.
boolOption :: Bool -> String -> Mod FlagFields Bool -> Parser Bool
boolOption defaultValue trueName =
  boolOption' defaultValue trueName ("no-" <> trueName)

fullOptsParser :: ParserInfo Opts
fullOptsParser =
  info
    (optsParser <**> helper)
    ( fullDesc
        <> progDesc "Test suite runner for `Cabal` and `cabal-install` developers"
    )

data HackageTests
  = CompleteHackageTests
  | PartialHackageTests
  | NoHackageTests
  deriving (Show)

data Step
  = PrintConfig
  | PrintToolVersions
  | Build
  | Doctest
  | LibTests
  | LibSuite
  | LibSuiteExtras
  | CliTests
  | CliSuite
  | SolverBenchmarksTests
  | SolverBenchmarksRun
  | TimeSummary
  deriving (Eq, Enum, Bounded, Show)

displayStep :: Step -> String
displayStep step =
  case step of
    PrintConfig -> "print-config"
    PrintToolVersions -> "print-tool-versions"
    Build -> "build"
    Doctest -> "doctest"
    LibTests -> "lib-tests"
    LibSuite -> "lib-suite"
    LibSuiteExtras -> "lib-suite-extras"
    CliTests -> "cli-tests"
    CliSuite -> "cli-suite"
    SolverBenchmarksTests -> "solver-benchmarks-tests"
    SolverBenchmarksRun -> "solver-benchmarks-run"
    TimeSummary -> "time-summary"

nameToStep :: Map String Step
nameToStep =
  Map.fromList
    [ (displayStep step, step)
    | step <- [minBound .. maxBound]
    ]

parseStep :: String -> Maybe Step
parseStep step = Map.lookup step nameToStep

runStep :: ResolvedOpts -> Step -> IO ()
runStep opts step = do
  let title = displayStep step
  printHeader title
  let action = case step of
        PrintConfig -> printConfig opts
        PrintToolVersions -> printToolVersions opts
        Build -> build opts
        Doctest -> doctest opts
        LibTests -> libTests opts
        LibSuite -> libSuite opts
        LibSuiteExtras -> libSuiteExtras opts
        CliSuite -> cliSuite opts
        CliTests -> cliTests opts
        SolverBenchmarksTests -> solverBenchmarksTests opts
        SolverBenchmarksRun -> solverBenchmarksRun opts
        TimeSummary -> timeSummary opts
  withTiming opts title action
  T.putStrLn ""

getTerminalWidth :: IO Int
getTerminalWidth = maybe 80 Terminal.width <$> Terminal.size @Int

printHeader :: String -> IO ()
printHeader title = do
  columns <- getTerminalWidth
  let left = 3
      right = columns - length title - left - 2
      header =
        setSGRCode [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Cyan]
          <> replicate left '═'
          <> " "
          <> title
          <> " "
          <> replicate right '═'
          <> setSGRCode [Reset]
  putStrLn header

withTiming :: ResolvedOpts -> String -> IO a -> IO a
withTiming opts title action = do
  startTime' <- getAbsoluteTime

  result <-
    (Right <$> action)
      `catch` (\exception -> pure (Left (exception :: ExitCodeException)))

  endTime <- getAbsoluteTime

  let duration = diffAbsoluteTime endTime startTime'
      totalDuration = diffAbsoluteTime endTime (startTime opts)

  case result of
    Right inner -> do
      putStrLn $
        setSGRCode [SetColor Foreground Vivid Green]
          <> title
          <> " finished after "
          <> formatDiffTime duration
          <> "\nTotal time so far: "
          <> formatDiffTime totalDuration
          <> setSGRCode [Reset]

      pure inner
    Left _procFailed -> do
      putStrLn $
        setSGRCode [SetColor Foreground Vivid Red]
          <> title
          <> " failed after "
          <> formatDiffTime duration
          <> "\nTotal time so far: "
          <> formatDiffTime totalDuration
          <> setSGRCode [Reset]

      -- TODO: `--keep-going` mode.
      exitFailure

-- TODO: Shell escaping
displayCommand :: String -> [String] -> String
displayCommand command args = command <> " " <> unwords args

timedCabalBin :: ResolvedOpts -> String -> String -> [String] -> IO ()
timedCabalBin opts package component args = do
  command <- cabalListBin opts (package <> ":" <> component)
  timedWithCwd
    opts
    package
    command
    args

timedWithCwd :: ResolvedOpts -> FilePath -> String -> [String] -> IO ()
timedWithCwd opts cdPath command args =
  withCurrentDirectory cdPath (timed opts command args)

timed :: ResolvedOpts -> String -> [String] -> IO ()
timed opts command args = do
  let prettyCommand = displayCommand command args
      process = proc command args

  startTime' <- getAbsoluteTime

  -- TODO: Replace `$HOME` or `opts.cwd` for brevity?
  putStrLn $
    setSGRCode [SetColor Foreground Vivid Blue]
      <> "$ "
      <> prettyCommand
      <> setSGRCode [Reset]

  (exitCode, rawStdout, rawStderr) <-
    if verbose opts
      then do
        exitCode <- runProcess process
        pure (exitCode, ByteString.empty, ByteString.empty)
      else readProcess process

  endTime <- getAbsoluteTime

  let duration = diffAbsoluteTime endTime startTime'
      totalDuration = diffAbsoluteTime endTime (startTime opts)

      output = decodeStrip rawStdout <> "\n" <> decodeStrip rawStderr
      linesLimit = 50
      outputLines = T.lines output
      hiddenLines = length outputLines - linesLimit
      tailLines = drop hiddenLines outputLines

  case exitCode of
    ExitSuccess -> do
      unless (verbose opts) $ do
        if hiddenLines <= 0
          then T.putStrLn output
          else
            T.putStrLn $
              "("
                <> tShow hiddenLines
                <> " lines hidden, use `--verbose` to show)\n"
                <> "...\n"
                <> T.unlines tailLines

      putStrLn $
        setSGRCode [SetColor Foreground Vivid Green]
          <> "Finished after "
          <> formatDiffTime duration
          <> ": "
          <> prettyCommand
          <> "\nTotal time so far: "
          <> formatDiffTime totalDuration
          <> setSGRCode [Reset]
    ExitFailure exitCode' -> do
      unless (verbose opts) $ do
        T.putStrLn output

      putStrLn $
        setSGRCode [SetColor Foreground Vivid Red]
          <> "Failed with exit code "
          <> show exitCode'
          <> " after "
          <> formatDiffTime duration
          <> ": "
          <> prettyCommand
          <> "\nTotal time so far: "
          <> formatDiffTime totalDuration
          <> setSGRCode [Reset]

      throwIO
        ExitCodeException
          { eceExitCode = exitCode
          , eceProcessConfig = process
          , eceStdout = rawStdout
          , eceStderr = rawStderr
          }

getAbsoluteTime :: IO AbsoluteTime
getAbsoluteTime = systemToTAITime <$> getSystemTime

formatDiffTime :: DiffTime -> String
formatDiffTime delta =
  let minute = secondsToDiffTime 60
      hour = 60 * minute
   in if delta >= hour
        then formatTime defaultTimeLocale "%h:%02M:%02ES" delta
        else
          if delta >= minute
            then formatTime defaultTimeLocale "%m:%2ES" delta
            else formatTime defaultTimeLocale "%2Ess" delta

main :: IO ()
main = do
  opts <- execParser fullOptsParser
  resolvedOpts <- resolveOpts opts
  mainInner resolvedOpts

mainInner :: ResolvedOpts -> IO ()
mainInner opts =
  forM_ (steps opts) $ \step -> do
    runStep opts step

printConfig :: ResolvedOpts -> IO ()
printConfig opts = do
  putStrLn $
    "compiler:          "
      <> compilerExecutable (compiler opts)
      <> "\ncabal-install:     "
      <> cabal opts
      <> "\njobs:              "
      <> show (jobs opts)
      <> "\nsteps:             "
      <> unwords (map displayStep (steps opts))
      <> "\nHackage tests:     "
      <> show (hackageTests opts)
      <> "\nverbose:           "
      <> show (verbose opts)
      <> "\nextra compilers:   "
      <> unwords (extraCompilers opts)
      <> "\nextra RTS options: "
      <> unwords (rtsArgs opts)

printToolVersions :: ResolvedOpts -> IO ()
printToolVersions opts = do
  timed opts (compilerExecutable (compiler opts)) ["--version"]
  timed opts (cabal opts) ["--version"]

  forM_ (extraCompilers opts) $ \compiler' -> do
    timed opts compiler' ["--version"]

build :: ResolvedOpts -> IO ()
build opts = do
  printHeader "build (dry run)"
  timed
    opts
    (cabal opts)
    ( cabalNewBuildArgs opts
        ++ targets opts
        ++ ["--dry-run"]
    )

  printHeader "build (full build plan; cached and to-be-built dependencies)"
  timed
    opts
    "jq"
    [ "-r"
    , -- TODO: Maybe use `cabal-plan`? It's a heavy dependency though...
      ".\"install-plan\" | map(.\"pkg-name\" + \"-\" + .\"pkg-version\" + \" \" + .\"component-name\") | join(\"\n\")"
    , baseBuildDir opts </> "cache" </> "plan.json"
    ]

  printHeader "build (actual build)"
  timed
    opts
    (cabal opts)
    (cabalNewBuildArgs opts ++ targets opts)

doctest :: ResolvedOpts -> IO ()
doctest opts = do
  timed
    opts
    "cabal-env"
    [ "--name"
    , "doctest-cabal"
    , "--transitive"
    , "QuickCheck"
    ]

  timed
    opts
    "cabal-env"
    [ "--name"
    , "doctest-cabal"
    , "array"
    , "bytestring"
    , "containers"
    , "deepseq"
    , "directory"
    , "filepath"
    , "pretty"
    , "process"
    , "time"
    , "binary"
    , "unix"
    , "text"
    , "parsec"
    , "mtl"
    ]

  timed
    opts
    "doctest"
    [ "-package-env=doctest-Cabal"
    , "--fast"
    , "Cabal/Distribution"
    , "Cabal/Language"
    ]

libTests :: ResolvedOpts -> IO ()
libTests opts = do
  let runCabalTests' suite extraArgs =
        timedCabalBin
          opts
          "Cabal-tests"
          ("test:" <> suite)
          ( tastyArgs opts
              ++ jobsArgs opts
              ++ extraArgs
          )

      runCabalTests suite = runCabalTests' suite []

  runCabalTests' "unit-tests" ["--with-ghc=" <> compilerExecutable (compiler opts)]
  runCabalTests "check-tests"
  runCabalTests "parser-tests"
  runCabalTests "rpmvercmp"
  runCabalTests "no-thunks-test"

  runHackageTests opts

runHackageTests :: ResolvedOpts -> IO ()
runHackageTests opts
  | NoHackageTests <- hackageTests opts = pure ()
  | otherwise = do
      command <- cabalListBin opts "Cabal-tests:test:hackage-tests"

      let
        -- See #10284 for why this value is pinned.
        hackageTestsIndexState = "--index-state=2024-08-25"

        hackageTest args =
          timedWithCwd
            opts
            "Cabal-tests"
            command
            (args ++ [hackageTestsIndexState])

      hackageTest ["read-fields"]

      case hackageTests opts of
        CompleteHackageTests -> do
          hackageTest ["parsec"]
          hackageTest ["roundtrip"]
        PartialHackageTests -> do
          hackageTest ["parsec", "d"]
          hackageTest ["roundtrip", "k"]

libSuiteWith :: ResolvedOpts -> FilePath -> [String] -> IO ()
libSuiteWith opts ghc extraArgs =
  timedCabalBin
    opts
    "cabal-testsuite"
    "exe:cabal-tests"
    ( [ "--builddir=" <> cabalTestsuiteBuildDir opts
      , "--with-ghc=" <> ghc
      , -- This test suite doesn't support `--jobs` _or_ `--num-threads`!
        "-j" <> show (jobs opts)
      ]
        ++ tastyArgs opts
        ++ extraArgs
    )

libSuite :: ResolvedOpts -> IO ()
libSuite opts = libSuiteWith opts (compilerExecutable (compiler opts)) (rtsArgs opts)

libSuiteExtras :: ResolvedOpts -> IO ()
libSuiteExtras opts = forM_ (extraCompilers opts) $ \compiler' ->
  libSuiteWith opts compiler' []

cliTests :: ResolvedOpts -> IO ()
cliTests opts = do
  -- These are sorted in asc time used, quicker tests first.
  timedCabalBin
    opts
    "cabal-install"
    "test:long-tests"
    ( jobsArgs opts
        ++ tastyArgs opts
    )

  -- This doesn't work in parallel either.
  timedCabalBin
    opts
    "cabal-install"
    "test:unit-tests"
    ( ["--num-threads", "1"]
        ++ tastyArgs opts
    )

  -- Only single job, otherwise we fail with "Heap exhausted"
  timedCabalBin
    opts
    "cabal-install"
    "test:mem-use-tests"
    ( ["--num-threads", "1"]
        ++ tastyArgs opts
    )

  -- This test-suite doesn't like concurrency
  timedCabalBin
    opts
    "cabal-install"
    "test:integration-tests2"
    ( [ "--num-threads"
      , "1"
      , "--with-ghc=" <> compilerExecutable (compiler opts)
      ]
        ++ tastyArgs opts
    )

cliSuite :: ResolvedOpts -> IO ()
cliSuite opts = do
  cabal' <- cabalListBin opts "cabal-install:exe:cabal"

  timedCabalBin
    opts
    "cabal-testsuite"
    "exe:cabal-tests"
    ( [ "--builddir=" <> cabalTestsuiteBuildDir opts
      , "--with-cabal=" <> cabal'
      , "--with-ghc=" <> compilerExecutable (compiler opts)
      , "--intree-cabal-lib=" <> cwd opts
      , "--test-tmp=" <> cwd opts </> "testdb"
      , -- This test suite doesn't support `--jobs` _or_ `--num-threads`!
        "-j"
      , show (jobs opts)
      ]
        ++ tastyArgs opts
        ++ rtsArgs opts
    )

solverBenchmarksTests :: ResolvedOpts -> IO ()
solverBenchmarksTests opts = do
  command <- cabalListBin opts "solver-benchmarks:test:unit-tests"

  timedWithCwd
    opts
    "Cabal"
    command
    []

solverBenchmarksRun :: ResolvedOpts -> IO ()
solverBenchmarksRun opts = do
  command <- cabalListBin opts "solver-benchmarks:exe:hackage-benchmark"
  cabal' <- cabalListBin opts "cabal-install:exe:cabal"

  timedWithCwd
    opts
    "Cabal"
    command
    [ "--cabal1=" <> cabal opts
    , "--cabal2=" <> cabal'
    , "--trials=5"
    , "--packages=Chart-diagrams"
    , "--print-trials"
    ]

timeSummary :: ResolvedOpts -> IO ()
timeSummary opts = do
  endTime <- getAbsoluteTime
  let totalDuration = diffAbsoluteTime endTime (startTime opts)
  putStrLn $
    setSGRCode [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Cyan]
      <> "!!! Validation completed in "
      <> formatDiffTime totalDuration
      <> setSGRCode [Reset]
