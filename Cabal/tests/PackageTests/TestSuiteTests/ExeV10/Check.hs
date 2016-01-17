module PackageTests.TestSuiteTests.ExeV10.Check (tests) where

import qualified Control.Exception as E (IOException, catch)
import Control.Monad (when)
import Data.Maybe (catMaybes)
import System.FilePath
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import Distribution.Compiler (CompilerFlavor(..), CompilerId(..))
import Distribution.PackageDescription (package)
import Distribution.Simple.Compiler (compilerId)
import Distribution.Simple.LocalBuildInfo (compiler, localPkgDescr, localCompatPackageKey)
import Distribution.Simple.Hpc
import Distribution.Simple.Program.Builtin (hpcProgram)
import Distribution.Simple.Program.Db
    ( emptyProgramDb, configureProgram, requireProgramVersion )
import Distribution.Text (display)
import qualified Distribution.Verbosity as Verbosity
import Distribution.Version (Version(..), orLaterVersion)

import PackageTests.PackageTester

tests :: SuiteConfig -> [TestTree]
tests config =
    -- TODO: hierarchy and subnaming is a little unfortunate
    [ tc "Test" "Default" $ do
        cabal_build ["--enable-tests"]
        -- This one runs both tests, including the very LONG Foo
        -- test which prints a lot of output
        cabal "test" ["--show-details=direct"]
    , testGroup "WithHpc" $ hpcTestMatrix config
    , testGroup "WithoutHpc"
      -- Ensures that even if -fhpc is manually provided no .tix file is output.
      [ tc "NoTix" "NoHpcNoTix" $ do
            dist_dir <- distDir
            cabal_build
              [ "--enable-tests"
              , "--ghc-option=-fhpc"
              , "--ghc-option=-hpcdir"
              , "--ghc-option=" ++ dist_dir ++ "/hpc/vanilla" ]
            cabal "test" ["test-Short", "--show-details=direct"]
            lbi <- liftIO $ getPersistBuildConfig dist_dir
            let way = guessWay lbi
            shouldNotExist $ tixFilePath dist_dir way "test-Short"
      -- Ensures that even if a .tix file happens to be left around
      -- markup isn't generated.
      , tc "NoMarkup" "NoHpcNoMarkup" $ do
            dist_dir <- distDir
            let tixFile = tixFilePath dist_dir Vanilla "test-Short"
            withEnv [("HPCTIXFILE", Just tixFile)] $ do
                cabal_build
                  [ "--enable-tests"
                  , "--ghc-option=-fhpc"
                  , "--ghc-option=-hpcdir"
                  , "--ghc-option=" ++ dist_dir ++ "/hpc/vanilla" ]
                cabal "test" ["test-Short", "--show-details=direct"]
            shouldNotExist $ htmlDir dist_dir Vanilla "test-Short" </> "hpc_index.html"
      ]
    ]
  where
    tc :: String -> String -> TestM a -> TestTree
    tc name subname m
        = testCase name
            (runTestM config "TestSuiteTests/ExeV10" (Just subname) m)

hpcTestMatrix :: SuiteConfig -> [TestTree]
hpcTestMatrix config = do
    libProf <- [True, False]
    exeProf <- [True, False]
    exeDyn <- [True, False]
    shared <- [True, False]
    let name | null suffixes = "Vanilla"
             | otherwise = intercalate "-" suffixes
          where
            suffixes = catMaybes
                      [ if libProf then Just "LibProf" else Nothing
                      , if exeProf then Just "ExeProf" else Nothing
                      , if exeDyn then Just "ExeDyn" else Nothing
                      , if shared then Just "Shared" else Nothing
                      ]
        opts = catMaybes
            [ enable libProf "library-profiling"
            , enable exeProf "profiling"
            , enable exeDyn "executable-dynamic"
            , enable shared "shared"
            ]
          where
            enable cond flag
              | cond = Just $ "--enable-" ++ flag
              | otherwise = Nothing
    -- Ensure that both .tix file and markup are generated if coverage
    -- is enabled.
    return $ tc name ("WithHpc-" ++ name) $ do
        isCorrectVersion <- liftIO $ correctHpcVersion
        when isCorrectVersion $ do
            dist_dir <- distDir
            cabal_build ("--enable-tests" : "--enable-coverage" : opts)
            cabal "test" ["test-Short", "--show-details=direct"]
            lbi <- liftIO $ getPersistBuildConfig dist_dir
            let way = guessWay lbi
                CompilerId comp version = compilerId (compiler lbi)
                subdir
                  | comp == GHC && version >= Version [7, 10] [] =
                      display (localCompatPackageKey lbi)
                  | otherwise = display (package $ localPkgDescr lbi)
            mapM_ shouldExist
                [ mixDir dist_dir way "my-0.1" </> subdir </> "Foo.mix"
                , mixDir dist_dir way "test-Short" </> "Main.mix"
                , tixFilePath dist_dir way "test-Short"
                , htmlDir dist_dir way "test-Short" </> "hpc_index.html"
                ]
  where
    tc :: String -> String -> TestM a -> TestTree
    tc name subname m
        = testCase name
            (runTestM config "TestSuiteTests/ExeV10" (Just subname) m)

-- | Checks for a suitable HPC version for testing.
correctHpcVersion :: IO Bool
correctHpcVersion = do
    let programDb' = emptyProgramDb
    let verbosity = Verbosity.normal
    let verRange  = orLaterVersion (Version [0,7] [])
    programDb <- configureProgram verbosity hpcProgram programDb'
    (requireProgramVersion verbosity hpcProgram verRange programDb
     >> return True) `catchIO` (\_ -> return False)
  where
    -- Distribution.Compat.Exception is hidden.
    catchIO :: IO a -> (E.IOException -> IO a) -> IO a
    catchIO = E.catch
