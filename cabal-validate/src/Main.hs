-- | Entry-point to the @cabal-validate@ script.
--
-- This module encodes all the commands that are run for each step in
-- `runStep`.
module Main
  ( main
  , runStep
  ) where

import Control.Monad (forM_)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as T (toStrict)
import qualified Data.Text.Lazy.Encoding as T (decodeUtf8)
import Data.Version (makeVersion, showVersion)
import System.FilePath ((</>))
import System.IO (BufferMode (LineBuffering), hSetBuffering, stderr, stdout)
import System.Process.Typed (proc, readProcessStdout_)

import Cli (Compiler (..), HackageTests (..), Opts (..), parseOpts, whenVerbose)
import OutputUtil (printHeader, withTiming)
import ProcessUtil (timed, timedWithCwd)
import Step (Step (..), displayStep)

-- | Entry-point for @cabal-validate@.
main :: IO ()
main = do
  -- You'd _think_ that line-buffering for stdout and stderr would be the
  -- default behavior, and the documentation makes gestures at it, but it
  -- appears to not be the case!
  --
  -- > For most implementations, physical files will normally be
  -- > block-buffered and terminals will normally be line-buffered.
  --
  -- However, on GitHub Actions and on my machine (macOS M1), adding these
  -- lines makes output appear in the correct order!
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  opts <- parseOpts
  printConfig opts
  printToolVersions opts
  forM_ (steps opts) $ \step -> do
    runStep opts step

-- | Run a given `Step` with the given `Opts`.
runStep :: Opts -> Step -> IO ()
runStep opts step = do
  let title = displayStep step
  printHeader title
  let action = case step of
        Build -> build opts
        Doctest -> doctest opts
        LibTests -> libTests opts
        LibSuite -> libSuite opts
        LibSuiteExtras -> libSuiteExtras opts
        CliSuite -> cliSuite opts
        CliTests -> cliTests opts
        SolverBenchmarksTests -> solverBenchmarksTests opts
        SolverBenchmarksRun -> solverBenchmarksRun opts
  withTiming (startTime opts) title action
  T.putStrLn ""

-- | Compiler with version number like @ghc-9.6.6@.
baseHc :: Opts -> FilePath
baseHc opts = "ghc-" <> showVersion (compilerVersion $ compiler opts)

-- | Base build directory for @cabal-validate@.
baseBuildDir :: Opts -> FilePath
baseBuildDir opts = "dist-newstyle-validate-" <> baseHc opts

-- | Absolute path to the build directory for this architecture.
--
-- This is a path nested fairly deeply under `baseBuildDir`.
buildDir :: Opts -> FilePath
buildDir opts =
  cwd opts
    </> baseBuildDir opts
    </> "build"
    </> archPath opts
    </> baseHc opts

-- | @--num-threads@ argument for test suites.
--
-- This isn't always used because some test suites are finicky and only accept
-- @-j@.
jobsArgs :: Opts -> [String]
jobsArgs opts = ["--num-threads", show $ jobs opts]

-- | Default arguments for invoking @cabal@.
cabalArgs :: Opts -> [String]
cabalArgs opts =
  [ "--jobs=" <> show (jobs opts)
  , "--with-compiler=" <> compilerExecutable (compiler opts)
  , "--builddir=" <> baseBuildDir opts
  , "--project-file=" <> projectFile opts
  ]

-- | The `buildDir` for @cabal-testsuite-3@.
cabalTestsuiteBuildDir :: Opts -> FilePath
cabalTestsuiteBuildDir opts =
  buildDir opts
    </> "cabal-testsuite-3"

-- | Arguments for @cabal build@.
cabalNewBuildArgs :: Opts -> [String]
cabalNewBuildArgs opts = "build" : cabalArgs opts

-- | Arguments for @cabal list-bin@.
--
-- This is used to find the binaries for various test suites.
cabalListBinArgs :: Opts -> [String]
cabalListBinArgs opts = "list-bin" : cabalArgs opts

-- | Get the binary for a given @cabal@ target by running @cabal list-bin@.
cabalListBin :: Opts -> String -> IO FilePath
cabalListBin opts target = do
  let args = cabalListBinArgs opts ++ [target]
  stdout' <-
    readProcessStdout_ $
      proc (cabal opts) args

  pure (T.unpack $ T.strip $ T.toStrict $ T.decodeUtf8 stdout')

-- | Get the RTS arguments for invoking test suites.
--
-- These seem to only be used for some of the test suites, I'm not sure why.
rtsArgs :: Opts -> [String]
rtsArgs opts =
  case archPath opts of
    "x86_64-windows" ->
      -- See: https://github.com/haskell/cabal/issues/9571
      if compilerVersion (compiler opts) > makeVersion [9, 0, 2]
        then ["+RTS", "--io-manager=native", "-RTS"]
        else []
    _ -> []

-- | Run a binary built by @cabal@ and output timing information.
--
-- This is used to run many of the test suites.
timedCabalBin :: Opts -> String -> String -> [String] -> IO ()
timedCabalBin opts package component args = do
  command <- cabalListBin opts (package <> ":" <> component)
  timedWithCwd
    opts
    package
    command
    args

-- | Print the configuration for CI logs.
printConfig :: Opts -> IO ()
printConfig opts =
  whenVerbose opts $ do
    printHeader "Configuration"
    putStr $
      unlines
        [ "compiler:          "
            <> compilerExecutable (compiler opts)
        , "cabal-install:     "
            <> cabal opts
        , "jobs:              "
            <> show (jobs opts)
        , "steps:             "
            <> unwords (map displayStep (steps opts))
        , "Hackage tests:     "
            <> show (hackageTests opts)
        , "verbosity:         "
            <> show (verbosity opts)
        , "extra compilers:   "
            <> unwords (extraCompilers opts)
        , "extra RTS options: "
            <> unwords (rtsArgs opts)
        ]

-- | Print the versions of tools being used.
printToolVersions :: Opts -> IO ()
printToolVersions opts =
  whenVerbose opts $ do
    printHeader "Tool versions"
    timed opts (cabal opts) ["--version"]
    timed opts (compilerExecutable (compiler opts)) ["--version"]

    forM_ (extraCompilers opts) $ \compiler' -> do
      timed opts compiler' ["--version"]

-- | Run the build step.
build :: Opts -> IO ()
build opts = do
  whenVerbose opts $ do
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

-- | Run doctests.
--
-- This doesn't work on my machine, maybe @cabal.nix@ needs some love to
-- support @cabal-env@?
doctest :: Opts -> IO ()
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

-- | Run tests for the @Cabal@ library, and also `runHackageTests` if those are
-- enabled.
libTests :: Opts -> IO ()
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

-- | Run Hackage tests, if enabled.
runHackageTests :: Opts -> IO ()
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

-- | Run @cabal-testsuite@ with the @Cabal@ library with a non-default GHC.
libSuiteWith :: Opts -> FilePath -> [String] -> IO ()
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

-- | Run @cabal-testsuite@ with the @Cabal@ library with the default GHC.
libSuite :: Opts -> IO ()
libSuite opts = libSuiteWith opts (compilerExecutable (compiler opts)) (rtsArgs opts)

-- | Run @cabal-testsuite@ with the @Cabal@ library with all extra GHCs.
libSuiteExtras :: Opts -> IO ()
libSuiteExtras opts = forM_ (extraCompilers opts) $ \compiler' ->
  libSuiteWith opts compiler' []

-- | Test the @cabal-install@ executable.
--
-- These tests mostly run sequentially, so they're pretty slow as a result.
cliTests :: Opts -> IO ()
cliTests opts = do
  -- These are sorted in asc time used, quicker tests first.

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
    "test:unit-tests"
    ( ["--num-threads", "1"]
        ++ tastyArgs opts
    )

  timedCabalBin
    opts
    "cabal-install"
    "test:parser-tests"
    ( jobsArgs opts
        ++ tastyArgs opts
    )

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
    "test:integration-tests2"
    ( [ "--num-threads"
      , "1"
      , "--with-ghc=" <> compilerExecutable (compiler opts)
      ]
        ++ tastyArgs opts
    )

-- | Run @cabal-testsuite@ with the @cabal-install@ executable.
cliSuite :: Opts -> IO ()
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

-- | Run the @solver-benchmarks@ unit tests.
solverBenchmarksTests :: Opts -> IO ()
solverBenchmarksTests opts = do
  command <- cabalListBin opts "solver-benchmarks:test:unit-tests"

  timedWithCwd
    opts
    "Cabal"
    command
    []

-- | Run the @solver-benchmarks@.
solverBenchmarksRun :: Opts -> IO ()
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
