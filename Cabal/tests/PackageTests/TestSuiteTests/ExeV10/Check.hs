module PackageTests.TestSuiteTests.ExeV10.Check (checks) where

import qualified Control.Exception as E (IOException, catch)
import Control.Monad (when)
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import System.Directory ( doesFileExist )
import System.FilePath
import Test.Tasty
import Test.Tasty.HUnit

import Distribution.Compiler (CompilerFlavor(..), CompilerId(..))
import Distribution.PackageDescription (package)
import Distribution.Simple.Compiler (compilerId)
import Distribution.Simple.Configure (getPersistBuildConfig)
import Distribution.Simple.LocalBuildInfo (compiler, localPkgDescr, localPackageKey)
import Distribution.Simple.Hpc
import Distribution.Simple.Program.Builtin (hpcProgram)
import Distribution.Simple.Program.Db
    ( emptyProgramDb, configureProgram, requireProgramVersion )
import Distribution.Text (display)
import qualified Distribution.Verbosity as Verbosity
import Distribution.Version (Version(..), orLaterVersion)

import PackageTests.PackageTester

checks :: SuiteConfig -> [TestTree]
checks config =
    [ testCase "Test" $ checkTest config
    , testGroup "WithHpc" $ hpcTestMatrix config
    , testGroup "WithoutHpc"
      [ testCase "NoTix" $ checkTestNoHpcNoTix config
      , testCase "NoMarkup" $ checkTestNoHpcNoMarkup config
      ]
    ]

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
    return $ testCase name $ checkTestWithHpc config ("WithHpc-" ++ name) opts

dir :: FilePath
dir = "PackageTests" </> "TestSuiteTests" </> "ExeV10"

checkTest :: SuiteConfig -> Assertion
checkTest config = buildAndTest config "Default" [] []

shouldExist :: FilePath -> Assertion
shouldExist path = doesFileExist path >>= assertBool (path ++ " should exist")

shouldNotExist :: FilePath -> Assertion
shouldNotExist path =
    doesFileExist path >>= assertBool (path ++ " should exist") . not

-- | Ensure that both .tix file and markup are generated if coverage is enabled.
checkTestWithHpc :: SuiteConfig -> String -> [String] -> Assertion
checkTestWithHpc config name extraOpts = do
    isCorrectVersion <- correctHpcVersion
    when isCorrectVersion $ do
        let distPref' = dir </> "dist-" ++ name
        buildAndTest config name [] ("--enable-coverage" : extraOpts)
        lbi <- getPersistBuildConfig distPref'
        let way = guessWay lbi
            CompilerId comp version = compilerId (compiler lbi)
            subdir
              | comp == GHC && version >= Version [7, 10] [] =
                  display (localPackageKey lbi)
              | otherwise = display (package $ localPkgDescr lbi)
        mapM_ shouldExist
            [ mixDir distPref' way "my-0.1" </> subdir </> "Foo.mix"
            , mixDir distPref' way "test-Foo" </> "Main.mix"
            , tixFilePath distPref' way "test-Foo"
            , htmlDir distPref' way "test-Foo" </> "hpc_index.html"
            ]

-- | Ensures that even if -fhpc is manually provided no .tix file is output.
checkTestNoHpcNoTix :: SuiteConfig -> Assertion
checkTestNoHpcNoTix config = do
    buildAndTest config "NoHpcNoTix" []
      [ "--ghc-option=-fhpc"
      , "--ghc-option=-hpcdir"
      , "--ghc-option=dist-NoHpcNoTix/hpc/vanilla" ]
    lbi <- getPersistBuildConfig (dir </> "dist-NoHpcNoTix")
    let way = guessWay lbi
    shouldNotExist $ tixFilePath (dir </> "dist-NoHpcNoTix") way "test-Foo"

-- | Ensures that even if a .tix file happens to be left around
-- markup isn't generated.
checkTestNoHpcNoMarkup :: SuiteConfig -> Assertion
checkTestNoHpcNoMarkup config = do
    let tixFile = tixFilePath "dist-NoHpcNoMarkup" Vanilla "test-Foo"
    buildAndTest config "NoHpcNoMarkup"
      [("HPCTIXFILE", Just tixFile)]
      [ "--ghc-option=-fhpc"
      , "--ghc-option=-hpcdir"
      , "--ghc-option=dist-NoHpcNoMarkup/hpc/vanilla" ]
    shouldNotExist $ htmlDir (dir </> "dist-NoHpcNoMarkup") Vanilla "test-Foo" </> "hpc_index.html"

-- | Build and test a package and ensure that both were successful.
--
-- The flag "--enable-tests" is provided in addition to the given flags.
buildAndTest :: SuiteConfig -> String -> [(String, Maybe String)] -> [String] -> IO ()
buildAndTest config name envOverrides flags = do
    let spec = PackageSpec
            { directory = dir
            , distPref = Just $ "dist-" ++ name
            , configOpts = "--enable-tests" : flags
            }
    buildResult <- cabal_build config spec
    assertBuildSucceeded buildResult
    testResult <- cabal_test config spec envOverrides []
    assertTestSucceeded testResult

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
