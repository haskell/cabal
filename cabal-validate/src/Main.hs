module Main where

import Control.Monad (forM_)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as T (toStrict)
import qualified Data.Text.Lazy.Encoding as T (decodeUtf8)
import Data.Version (makeVersion, showVersion)
import System.Console.ANSI
  ( Color (Cyan)
  , ColorIntensity (Vivid)
  , ConsoleIntensity (BoldIntensity)
  , ConsoleLayer (Foreground)
  , SGR (Reset, SetColor, SetConsoleIntensity)
  , setSGRCode
  )
import System.FilePath ((</>))
import System.Process.Typed (proc, readProcessStdout_)

import Cli (Compiler (..), HackageTests (..), Opts (..), parseOpts)
import ClockUtil (diffAbsoluteTime, formatDiffTime, getAbsoluteTime)
import OutputUtil (printHeader, withTiming)
import ProcessUtil (timed, timedWithCwd)
import Step (Step (..), displayStep)

main :: IO ()
main = do
  opts <- parseOpts
  forM_ (steps opts) $ \step -> do
    runStep opts step

baseHc :: Opts -> FilePath
baseHc opts = "ghc-" <> showVersion (compilerVersion $ compiler opts)

baseBuildDir :: Opts -> FilePath
baseBuildDir opts = "dist-newstyle-validate-" <> baseHc opts

buildDir :: Opts -> FilePath
buildDir opts =
  cwd opts
    </> baseBuildDir opts
    </> "build"
    </> archPath opts
    </> baseHc opts

jobsArgs :: Opts -> [String]
jobsArgs opts = ["--num-threads", show $ jobs opts]

cabalArgs :: Opts -> [String]
cabalArgs opts =
  [ "--jobs=" <> show (jobs opts)
  , "--with-compiler=" <> compilerExecutable (compiler opts)
  , "--builddir=" <> baseBuildDir opts
  , "--project-file=" <> projectFile opts
  ]

cabalTestsuiteBuildDir :: Opts -> FilePath
cabalTestsuiteBuildDir opts =
  buildDir opts
    </> "cabal-testsuite-3"

cabalNewBuildArgs :: Opts -> [String]
cabalNewBuildArgs opts = "build" : cabalArgs opts

cabalListBinArgs :: Opts -> [String]
cabalListBinArgs opts = "list-bin" : cabalArgs opts

cabalListBin :: Opts -> String -> IO FilePath
cabalListBin opts target = do
  let args = cabalListBinArgs opts ++ [target]
  stdout <-
    readProcessStdout_ $
      proc (cabal opts) args

  pure (T.unpack $ T.strip $ T.toStrict $ T.decodeUtf8 stdout)

rtsArgs :: Opts -> [String]
rtsArgs opts =
  case archPath opts of
    "x86_64-windows" ->
      -- See: https://github.com/haskell/cabal/issues/9571
      if compilerVersion (compiler opts) > makeVersion [9, 0, 2]
        then ["+RTS", "--io-manager=native", "-RTS"]
        else []
    _ -> []

runStep :: Opts -> Step -> IO ()
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

timedCabalBin :: Opts -> String -> String -> [String] -> IO ()
timedCabalBin opts package component args = do
  command <- cabalListBin opts (package <> ":" <> component)
  timedWithCwd
    opts
    package
    command
    args

printConfig :: Opts -> IO ()
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

printToolVersions :: Opts -> IO ()
printToolVersions opts = do
  timed opts (compilerExecutable (compiler opts)) ["--version"]
  timed opts (cabal opts) ["--version"]

  forM_ (extraCompilers opts) $ \compiler' -> do
    timed opts compiler' ["--version"]

build :: Opts -> IO ()
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

libSuite :: Opts -> IO ()
libSuite opts = libSuiteWith opts (compilerExecutable (compiler opts)) (rtsArgs opts)

libSuiteExtras :: Opts -> IO ()
libSuiteExtras opts = forM_ (extraCompilers opts) $ \compiler' ->
  libSuiteWith opts compiler' []

cliTests :: Opts -> IO ()
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

solverBenchmarksTests :: Opts -> IO ()
solverBenchmarksTests opts = do
  command <- cabalListBin opts "solver-benchmarks:test:unit-tests"

  timedWithCwd
    opts
    "Cabal"
    command
    []

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

timeSummary :: Opts -> IO ()
timeSummary opts = do
  endTime <- getAbsoluteTime
  let totalDuration = diffAbsoluteTime endTime (startTime opts)
  putStrLn $
    setSGRCode [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Cyan]
      <> "!!! Validation completed in "
      <> formatDiffTime totalDuration
      <> setSGRCode [Reset]
