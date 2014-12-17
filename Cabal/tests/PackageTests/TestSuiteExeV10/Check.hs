module PackageTests.TestSuiteExeV10.Check (checks) where

import Data.Maybe (catMaybes)
import System.Directory ( doesFileExist )
import System.FilePath
import qualified Test.Framework as TF
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.HUnit hiding ( path )

import Distribution.Simple.Hpc

import PackageTests.PackageTester

checks :: FilePath -> [TF.Test]
checks ghcPath =
    [ hunit "Test" $ checkTest ghcPath ]
    ++ hpcTestMatrix ghcPath ++
    [ hunit "TestWithoutHpc/NoTix" $ checkTestWithoutHpcNoTix ghcPath
    , hunit "TestWithoutHpc/NoMarkup" $ checkTestWithoutHpcNoMarkup ghcPath
    ]

hpcTestMatrix :: FilePath -> [TF.Test]
hpcTestMatrix ghcPath = do
    libProf <- [True, False]
    exeProf <- [True, False]
    exeDyn <- [True, False]
    shared <- [True, False]
    let name = concat
            [ "WithHpc/"
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
    return $ hunit name $ checkTestWithHpc ghcPath opts

dir :: FilePath
dir = "PackageTests" </> "TestSuiteExeV10"

checkTest :: FilePath -> Test
checkTest ghcPath = TestCase $ buildAndTest ghcPath [] []

shouldExist :: FilePath -> Assertion
shouldExist path = doesFileExist path >>= assertBool (path ++ " should exist")

shouldNotExist :: FilePath -> Assertion
shouldNotExist path =
    doesFileExist path >>= assertBool (path ++ " should exist") . not

-- | Ensure that both .tix file and markup are generated if coverage is enabled.
checkTestWithHpc :: FilePath -> [String] -> Test
checkTestWithHpc ghcPath extraOpts = TestCase $ do
    buildAndTest ghcPath [] ("--enable-coverage" : extraOpts)
    shouldExist $ mixDir (dir </> "dist") "my-0.1" </> "my-0.1" </> "Foo.mix"
    shouldExist $ mixDir (dir </> "dist") "test-Foo" </> "Main.mix"
    shouldExist $ tixFilePath (dir </> "dist") "test-Foo"
    shouldExist $ htmlDir (dir </> "dist") "test-Foo" </> "hpc_index.html"

-- | Ensures that even if -fhpc is manually provided no .tix file is output.
checkTestWithoutHpcNoTix :: FilePath -> Test
checkTestWithoutHpcNoTix ghcPath = TestCase $ do
    buildAndTest ghcPath [] [ "--ghc-option=-fhpc"
                            , "--ghc-option=-hpcdir"
                            , "--ghc-option=dist/hpc" ]
    shouldNotExist $ tixFilePath (dir </> "dist") "test-Foo"

-- | Ensures that even if a .tix file happens to be left around
-- markup isn't generated.
checkTestWithoutHpcNoMarkup :: FilePath -> Test
checkTestWithoutHpcNoMarkup ghcPath = TestCase $ do
    let tixFile = tixFilePath "dist" "test-Foo"
    buildAndTest ghcPath [("HPCTIXFILE", Just tixFile)]
                         [ "--ghc-option=-fhpc"
                         , "--ghc-option=-hpcdir"
                         , "--ghc-option=dist/hpc" ]
    shouldNotExist $ htmlDir (dir </> "dist") "test-Foo" </> "hpc_index.html"

-- | Build and test a package and ensure that both were successful.
--
-- The flag "--enable-tests" is provided in addition to the given flags.
buildAndTest :: FilePath -> [(String, Maybe String)] -> [String] -> IO ()
buildAndTest ghcPath envOverrides flags = do
    let spec = PackageSpec dir $ "--enable-tests" : flags
    buildResult <- cabal_build spec ghcPath
    assertBuildSucceeded buildResult
    testResult <- cabal_test spec envOverrides [] ghcPath
    assertTestSucceeded testResult

hunit :: TF.TestName -> Test -> TF.Test
hunit name = testGroup name . hUnitTestToTests
