-- | Parse CLI arguments and resolve defaults from the environment.
module Cli
  ( Opts (..)
  , parseOpts
  , HackageTests (..)
  , Compiler (..)
  , VersionParseException (..)
  )
where

import Control.Applicative (Alternative (many, (<|>)), (<**>))
import Control.Exception (Exception (displayException), throw)
import Control.Monad (forM_, when)
import Data.Data (Typeable)
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import qualified Data.Text.Lazy as T (toStrict)
import qualified Data.Text.Lazy.Encoding as T (decodeUtf8)
import Data.Version (Version, parseVersion)
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
import System.Directory (getCurrentDirectory)
import System.Exit (exitSuccess)
import System.Info (arch, os)
import System.Process.Typed (proc, readProcessStdout_)
import Text.ParserCombinators.ReadP (readP_to_S)

import ClockUtil (AbsoluteTime, getAbsoluteTime)
import Step (Step (..), displayStep, parseStep)

-- | Command-line options, resolved with context from the environment.
data Opts = Opts
  { verbose :: Bool
  -- ^ Whether to display build and test output.
  , jobs :: Int
  -- ^ How many jobs to use when running tests.
  --
  -- Defaults to the number of physical cores.
  , cwd :: FilePath
  -- ^ Current working directory when @cabal-validate@ was started.
  , startTime :: AbsoluteTime
  -- ^ System time when @cabal-validate@ was started.
  --
  -- Used to determine the total test duration so far.
  , compiler :: Compiler
  -- ^ Compiler to build Cabal with.
  --
  -- Defaults to @ghc@.
  , extraCompilers :: [FilePath]
  -- ^ Extra compilers to run @cabal-testsuite@ with.
  , cabal :: FilePath
  -- ^ @cabal-install@ to build Cabal with.
  --
  -- Defaults to @cabal@.
  , hackageTests :: HackageTests
  -- ^ Whether to run tests on Hackage data, and if so how much.
  --
  -- Defaults to `NoHackageTests`.
  , archPath :: FilePath
  -- ^ The path for this system's architecture within the build directory.
  --
  -- Like @x86_64-windows@ or @aarch64-osx@ or @arm-linux@.
  , projectFile :: FilePath
  -- ^ Path to the @cabal.project@ file to use for running tests.
  , tastyArgs :: [String]
  -- ^ Extra arguments to pass to @tasty@ test suites.
  --
  -- This defaults to @--hide-successes@ (which cannot yet be changed) and
  -- includes the @--pattern@ argument if one is given.
  , targets :: [String]
  -- ^ Targets to build.
  , steps :: [Step]
  -- ^ Steps to run.
  }
  deriving (Show)

-- | Whether to run tests on Hackage data, and if so how much.
data HackageTests
  = -- | Run tests on complete Hackage data.
    CompleteHackageTests
  | -- | Run tests on partial Hackage data.
    PartialHackageTests
  | -- | Do not run tests on Hackage data.
    NoHackageTests
  deriving (Show)

-- | A compiler executable and version number.
data Compiler = Compiler
  { compilerExecutable :: FilePath
  -- ^ The compiler's executable.
  , compilerVersion :: Version
  -- ^ The compiler's version number.
  }
  deriving (Show)

-- | An `Exception` thrown when parsing @--numeric-version@ output from a compiler.
data VersionParseException = VersionParseException
  { versionInput :: String
  -- ^ The string we attempted to parse.
  , versionExecutable :: FilePath
  -- ^ The compiler which produced the string.
  }
  deriving (Typeable, Show)

instance Exception VersionParseException where
  displayException exception =
    "Failed to parse `"
      <> versionExecutable exception
      <> " --numeric-version` output: "
      <> show (versionInput exception)

-- | Runs @ghc --numeric-version@ for the given executable to construct a
-- `Compiler`.
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

-- | Resolve options and default values from the environment.
--
-- This makes the `Opts` type much nicer to deal with than `RawOpts`.
resolveOpts :: RawOpts -> IO Opts
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
            , "cabal-testsuite"
            , "Cabal-tests"
            , "Cabal-QuickCheck"
            , "Cabal-tree-diff"
            , "Cabal-described"
            ]
          , optionals
              (not (rawLibOnly opts))
              [ "cabal-install"
              , "cabal-install-solver"
              , "cabal-benchmarks"
              ]
          , optionals
              (rawSolverBenchmarks opts)
              [ "solver-benchmarks"
              , "solver-benchmarks:tests"
              ]
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
          : maybe
            []
            (\tastyPattern -> ["--pattern", tastyPattern])
            (rawTastyPattern opts)
          ++ rawTastyArgs opts

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
    Opts
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

-- | Literate command-line options as supplied by the user, before resolving
-- defaults and other values from the environment.
data RawOpts = RawOpts
  { rawVerbose :: Bool
  , rawJobs :: Maybe Int
  , rawCompiler :: FilePath
  , rawCabal :: FilePath
  , rawExtraCompilers :: [FilePath]
  , rawTastyPattern :: Maybe String
  , rawTastyArgs :: [String]
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

-- | `Parser` for `RawOpts`.
--
-- See: `fullRawOptsParser`
rawOptsParser :: Parser RawOpts
rawOptsParser =
  RawOpts
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
    <*> many
      ( strOption
          ( long "tasty-arg"
              <> help "Extra arguments to pass to Tasty test suites"
          )
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

-- | Parse a boolean switch with a @--no-*@ flag for setting the option to false.
boolOption :: Bool -> String -> Mod FlagFields Bool -> Parser Bool
boolOption defaultValue trueName =
  boolOption' defaultValue trueName ("no-" <> trueName)

-- | Full `Parser` for `RawOpts`, which includes a @--help@ argument and
-- information about the program.
fullRawOptsParser :: ParserInfo RawOpts
fullRawOptsParser =
  info
    (rawOptsParser <**> helper)
    ( fullDesc
        <> progDesc "Test suite runner for `Cabal` and `cabal-install` developers"
    )

-- | Parse command-line arguments and resolve defaults from the environment,
-- producing `Opts`.
parseOpts :: IO Opts
parseOpts = execParser fullRawOptsParser >>= resolveOpts
