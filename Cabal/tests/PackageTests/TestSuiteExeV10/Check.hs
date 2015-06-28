module PackageTests.TestSuiteExeV10.Check (checks) where

import qualified Control.Exception as E (IOException, catch)
import Control.Monad (when)
import Data.Maybe (catMaybes)
import System.Directory ( doesFileExist )
import System.FilePath
import Test.Tasty
import Test.Tasty.HUnit

import Distribution.Compiler (CompilerFlavor(..), CompilerId(..))
import Distribution.PackageDescription (package)
import Distribution.Simple.Compiler (compilerId)
import Distribution.Simple.Configure (getPersistBuildConfig)
import Distribution.Simple.LocalBuildInfo (compiler, localPkgDescr, pkgKey)
import Distribution.Simple.Hpc
import Distribution.Simple.Program.Builtin (hpcProgram)
import Distribution.Simple.Program.Db
    ( emptyProgramDb, configureProgram, requireProgramVersion )
import Distribution.Text (display)
import qualified Distribution.Verbosity as Verbosity
import Distribution.Version (Version(..), orLaterVersion)

import PackageTests.PackageTester

checks :: IO TestsConfig -> [TestTree]
checks cfg =
    [ testCase "Test" $ checkTest cfg ]
    ++ hpcTestMatrix cfg ++
    [ testCase "TestNoHpc/NoTix" $ checkTestNoHpcNoTix cfg
    , testCase "TestNoHpc/NoMarkup" $ checkTestNoHpcNoMarkup cfg
    ]

hpcTestMatrix :: IO TestsConfig -> [TestTree]
hpcTestMatrix cfg = do
    libProf <- [True, False]
    exeProf <- [True, False]
    exeDyn <- [True, False]
    shared <- [True, False]
    let name = concat
            [ "WithHpc-"
            , if libProf then "LibProf" else ""
            , if exeProf then "ExeProf" else ""
            , if exeDyn then "ExeDyn" else ""
            , if shared then "Shared" else ""
            ]
        enable cond flag
          | cond = Just $ "--enable-" ++ flag
          | otherwise = Nothing
        opts = catMaybes
            [ enable libProf "library-profiling"
            , enable exeProf "profiling"
            , enable exeDyn "executable-dynamic"
            , enable shared "shared"
            ]
    return $ testCase name $ checkTestWithHpc cfg name opts

dir :: FilePath
dir = "PackageTests" </> "TestSuiteExeV10"

checkTest :: IO TestsConfig -> Assertion
checkTest cfg = buildAndTest cfg "Default" [] []

shouldExist :: FilePath -> Assertion
shouldExist path = doesFileExist path >>= assertBool (path ++ " should exist")

shouldNotExist :: FilePath -> Assertion
shouldNotExist path =
    doesFileExist path >>= assertBool (path ++ " should exist") . not

-- | Ensure that both .tix file and markup are generated if coverage is enabled.
checkTestWithHpc :: IO TestsConfig -> String -> [String] -> Assertion
checkTestWithHpc cfg name extraOpts = do
    isCorrectVersion <- correctHpcVersion
    when isCorrectVersion $ do
        let distPref' = dir </> "dist-" ++ name
        buildAndTest cfg name [] ("--enable-coverage" : extraOpts)
        lbi <- getPersistBuildConfig distPref'
        let way = guessWay lbi
            CompilerId comp version = compilerId (compiler lbi)
            subdir
              | comp == GHC && version >= Version [7, 10] [] =
                  display (pkgKey lbi)
              | otherwise = display (package $ localPkgDescr lbi)
        mapM_ shouldExist
            [ mixDir distPref' way "my-0.1" </> subdir </> "Foo.mix"
            , mixDir distPref' way "test-Foo" </> "Main.mix"
            , tixFilePath distPref' way "test-Foo"
            , htmlDir distPref' way "test-Foo" </> "hpc_index.html"
            ]

-- | Ensures that even if -fhpc is manually provided no .tix file is output.
checkTestNoHpcNoTix :: IO TestsConfig -> Assertion
checkTestNoHpcNoTix cfg = do
    buildAndTest cfg "NoHpcNoTix" []
      [ "--ghc-option=-fhpc"
      , "--ghc-option=-hpcdir"
      , "--ghc-option=dist-NoHpcNoTix/hpc/vanilla" ]
    lbi <- getPersistBuildConfig (dir </> "dist-NoHpcNoTix")
    let way = guessWay lbi
    shouldNotExist $ tixFilePath (dir </> "dist-NoHpcNoTix") way "test-Foo"

-- | Ensures that even if a .tix file happens to be left around
-- markup isn't generated.
checkTestNoHpcNoMarkup :: IO TestsConfig -> Assertion
checkTestNoHpcNoMarkup cfg = do
    let tixFile = tixFilePath "dist-NoHpcNoMarkup" Vanilla "test-Foo"
    buildAndTest cfg "NoHpcNoMarkup"
      [("HPCTIXFILE", Just tixFile)]
      [ "--ghc-option=-fhpc"
      , "--ghc-option=-hpcdir"
      , "--ghc-option=dist-NoHpcNoMarkup/hpc/vanilla" ]
    shouldNotExist $ htmlDir (dir </> "dist-NoHpcNoMarkup") Vanilla "test-Foo" </> "hpc_index.html"

-- | Build and test a package and ensure that both were successful.
--
-- The flag "--enable-tests" is provided in addition to the given flags.
buildAndTest :: IO TestsConfig -> String -> [(String, Maybe String)] -> [String] -> IO ()
buildAndTest cfg name envOverrides flags = do
    let spec = PackageSpec
            { directory = dir
            , distPref = Just $ "dist-" ++ name
            , configOpts = "--enable-tests" : flags
            }
    buildResult <- cabal_build cfg spec
    assertBuildSucceeded buildResult
    testResult <- cabal_test cfg spec envOverrides []
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
