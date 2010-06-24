-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Test
-- Copyright   :  Thomas Tuegel 2010
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This is the entry point into testing a built package. Performs the
-- \"@.\/setup test@\" action. It runs test suites designated in the package
-- description and reports on the results.

{- All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Isaac Jones nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. -}

module Distribution.Simple.Test
    ( test
    , runTests
    , writeSimpleTestStub
    , setStubExitCode
    , stubFilePath
    , stubName
    ) where

import Distribution.Package ( PackageIdentifier(..), PackageName(..) )
import Distribution.PackageDescription
        ( PackageDescription(..), TestSuite(..), TestType(..) )
import Distribution.Simple.BuildPaths ( exeExtension )
import Distribution.Simple.Compiler ( Compiler(..) )
import Distribution.Simple.InstallDirs
    ( fromPathTemplate, initialPathTemplateEnv, PathTemplateVariable(..)
    , substPathTemplate , toPathTemplate, refersTo, PathTemplate )
import Distribution.Simple.LocalBuildInfo ( LocalBuildInfo(..) )
import Distribution.Simple.Setup ( TestFlags(..), TestFilter(..), fromFlag )
import Distribution.Simple.Utils ( die, notice )
import Distribution.TestSuite
    ( Test, Result(..), ImpureTestable(..), TestOptions(..) )
import Distribution.Text
import Distribution.Verbosity ( Verbosity, silent )
import Distribution.Version ( Version(..), withinVersion, withinRange )

import Control.Exception ( bracket )
import Control.Monad ( unless, when, foldM, liftM )
import Data.List ( union )
import System.Directory
    ( createDirectoryIfMissing, doesFileExist, getCurrentDirectory
    , removeFile )
import System.Exit ( ExitCode(..), exitFailure, exitSuccess, exitWith )
import System.FilePath ( (</>), (<.>) )
import System.IO
    ( Handle, hClose, hGetContents, IOMode(..), openTempFile
    , withFile, hPutStrLn )
import System.Process ( runInteractiveProcess, runProcess, waitForProcess )

-- | Log the output of a test action to file(s).  The action should take, as its
-- arguments, two 'Handle's corresponding to @stdin@ and @stdout@, respectively.
-- The log files are located in the directory @dist/test@ and are named based on
-- a 'PathTemplate'.  To construct the actual 'FilePath', a function returning
-- a 'PathTemplateEnv' given the values of several 'PathTemplateVariable's is
-- required.  This function will overwrite test logs from previous invocations
-- of \"@.\/setup test@\" if they have the same name.  Test logs from the
-- current invocation with the same name will be appended; to select the correct
-- 'IOMode', a list of files modified by the current invocation is required.
doTestOutput :: PathTemplate -- ^ Path template for log file
             -> FilePath -- ^ File path for @dist@
             -> (String -> String -> [(PathTemplateVariable, PathTemplate)])
             -- ^ The 'PathTemplateEnv', given strings for the @$stdio@ and
             -- @$result@ variables
             -> [FilePath] -- ^ List of files to append instead of overwrite
             -> (Handle -> Handle -> IO Result) -- ^ The action to log
             -> IO ([FilePath], Result)
             -- ^ The result of the logged action and paths to the files
             -- where the action's output is logged.
doTestOutput template distPref env files action = do
    let path m r = distPref </> "test"
            </> fromPathTemplate (substPathTemplate (env m r) template)
    bracket
        (do
            (outTemp, hOutTemp) <- openTempFile
                (distPref </> "test") ("cabal-test-stdout" <.> "log")
            if template `refersTo` TestSuiteStdIoVar
                then do
                    (errTemp, hErrTemp) <- openTempFile
                        (distPref </> "test")
                        ("cabal-test-stderr" <.> "log")
                    return (outTemp, hOutTemp, errTemp, hErrTemp)
                else do
                    return (outTemp, hOutTemp, outTemp, hOutTemp)
        )
        (\(outTemp, hOutTemp, errTemp, hErrTemp) -> do
            hClose hOutTemp
            hClose hErrTemp
            removeFile outTemp
            errTempExists <- doesFileExist errTemp
            when errTempExists $ removeFile errTemp
        )
        (\(outTemp, hOutTemp, errTemp, hErrTemp) -> do
            r <- action hOutTemp hErrTemp
            let rStr = case r of
                    Pass -> "pass"
                    Fail _ -> "fail"
                    Error _ -> "error"
                outFile = path "stdout" rStr
                errFile = path "stderr" rStr
                appendOrWrite file =
                    if file `elem` files then AppendMode else WriteMode
                rewriteTemp temp file = withFile temp ReadMode $ \hTemp ->
                    withFile file (appendOrWrite file) $ \hFile ->
                        hGetContents hTemp >>= hPutStrLn hFile
            hClose hOutTemp
            hClose hErrTemp
            rewriteTemp outTemp outFile
            unless (outFile == errFile)
                $ rewriteTemp errTemp errFile
            return (union [outFile] [errFile], r)
        )

-- |Perform the \"@.\/setup test@\" action.
test :: PackageDescription  -- ^information from the .cabal file
     -> LocalBuildInfo      -- ^information from the configure step
     -> TestFlags           -- ^flags sent to test
     -> IO ()
test pkg_descr lbi flags = do
    shellEnv <- dataDirEnv pkg_descr
    let verbosity = fromFlag $ testVerbosity flags
        template = fromFlag $ testLogFile flags
        distPref = fromFlag $ testDistPref flags
        total = length $ testSuites pkg_descr
        withinVersion1 = flip withinRange $ withinVersion $ Version [1,0] []
        onFailureOrAll r a = when
            ((fromFlag (testFilter flags) > Summary && r /= Pass)
            || (fromFlag (testFilter flags) > Failures && r == Pass)) a

        doTest suite files = do
            notice verbosity $ "Test suite " ++ testName suite ++ ": RUNNING..."
            let pkgId = package pkg_descr
                compId = compilerId $ compiler lbi
                env m r = initialPathTemplateEnv pkgId compId
                    ++ [ (TestSuiteNameVar, toPathTemplate $ testName suite)
                       , (TestSuiteStdIoVar, toPathTemplate m)
                       , (TestSuiteResultVar, toPathTemplate r)
                       ]
            case testType suite of
                ExeTest v _ | withinVersion1 v -> do
                    (fs, r) <- doTestOutput template distPref env files $
                        \hOut hErr -> do
                            let cmd = buildDir lbi </> testName suite
                                    </> testName suite <.> exeExtension
                            proc <- runProcess cmd [] Nothing shellEnv
                                Nothing (Just hOut) (Just hErr)
                            exit <- waitForProcess proc
                            let r = case exit of
                                    ExitSuccess -> Pass
                                    ExitFailure c -> Fail $ show c
                            return r

                    notice verbosity $ "Test suite "
                        ++ testName suite ++ ": " ++
                        ( case r of
                            Pass -> "PASS"
                            _ -> "FAIL"
                        )
                    onFailureOrAll r $ mapM_ (showTestLog verbosity) fs
                    return (fs, r)

                LibTest v _ | withinVersion1 v -> do
                    let cmd = buildDir lbi </> stubName suite
                            </> stubName suite <.> exeExtension
                    (hIn, hOut, _, proc) <-
                        runInteractiveProcess cmd [] Nothing shellEnv
                    hPutStrLn hIn $ show $ TestStubOptions
                        { stubEnv = initialPathTemplateEnv pkgId compId
                            ++ [( TestSuiteNameVar
                                , toPathTemplate $ testName suite
                                )]
                        , stubTemplate = template
                        , stubDistPref = distPref
                        , stubAppendFiles = files
                        }
                    hClose hIn
                    exit <- waitForProcess proc
                    (fs, rs) <- liftM read $ hGetContents hOut
                    hClose hOut

                    let r = case exit of
                            ExitSuccess -> Pass
                            ExitFailure x | x == 1 -> Fail ""
                                          | otherwise -> Error ""
                    onFailureOrAll r $ do
                        mapM_ (showTestCase verbosity) rs
                        mapM_ (showTestLog verbosity) fs
                    notice verbosity $ "Test suite "
                        ++ testName suite ++ ": " ++
                        ( case r of
                            Pass -> "PASS"
                            Fail _ -> "FAIL"
                            Error _ -> "ERROR"
                        )
                    return (fs, r)
                _ -> do
                    _ <- die $ "No support for running test suite "
                        ++ "type: " ++ show (disp $ testType suite)
                    return ([], Error "")

    createDirectoryIfMissing True $ distPref </> "test"
    notice verbosity $ "Running " ++ show total ++ " test suites..."
    (_, results) <- foldM
        (\(appendFiles, rs) t -> do
            (filesWritten, r) <- doTest t appendFiles
            return (union appendFiles filesWritten,  union rs [r])
        ) ([], []) $ testSuites pkg_descr
    let successful = length $ filter (== Pass) results
    notice verbosity $ show successful ++ " of " ++ show total
        ++ " test suites successful."
    when (successful < total) exitFailure

    where showTestCase :: Verbosity -> (String, Result) -> IO ()
          showTestCase verbosity (n, r) = notice verbosity
            $ "    Test case " ++ n ++ ": " ++ show r

          showTestLog :: Verbosity -> FilePath -> IO ()
          showTestLog verbosity outFile = when (verbosity > silent) $ do
            withFile outFile ReadMode $ \hOut -> hGetContents hOut >>= putStrLn

-- | 'TestStubOptions' encapsulates the options required by 'runTests' which are
-- sent to the test stub.  The type exists for its 'Read' and 'Show'
-- instances.
data TestStubOptions = TestStubOptions
    { stubEnv :: [(PathTemplateVariable, PathTemplate)]
    -- ^ path template environment with all per-test-suite options supplied
    , stubTemplate :: PathTemplate
    -- ^ path template for naming log files
    , stubDistPref :: FilePath
    -- ^ location of @dist@; log files go in @dist/test@
    , stubAppendFiles :: [FilePath]
    -- ^ list of log files which should be appended instead of overwritten
    }
    deriving (Read, Show)

-- | The test runner for library 'TestSuite' stub executables.  Runs a list of
-- 'Test's.  The 'TestStubOptions' are read from @stdin@ by the stub executable.
runTests :: [Test] -> TestStubOptions -> IO ([FilePath], [(String, Result)])
runTests tests opts = do
    (filesWritten, results) <- foldM
        (\(appendFiles, rs) t -> do
            let env m r = stubEnv opts
                    ++ [ (TestSuiteStdIoVar, toPathTemplate m)
                       , (TestSuiteResultVar, toPathTemplate r)
                       ]
            (filesWritten, r) <- doTestOutput
                (stubTemplate opts) (stubDistPref opts) env appendFiles
                $ \hOut _ -> do
                    r <- defaultOptions t >>= runM t
                    hPutStrLn hOut $ show (name t, r)
                    return r
            return (union appendFiles filesWritten, union rs [r])
        ) (stubAppendFiles opts, []) tests
    return (filesWritten, zip (map name tests) results)

-- | The filename of the source file for the stub executable associated with a
-- library 'TestSuite'.
stubFilePath :: TestSuite -> FilePath
stubFilePath t = stubName t <.> "hs"

-- | The name of the stub executable associated with a library 'TestSuite'.
stubName :: TestSuite -> FilePath
stubName t = testName t ++ "Stub"

-- | Write the source file for a library 'TestSuite' stub executable.
writeSimpleTestStub :: TestSuite    -- ^ library 'TestSuite' for which a stub
                                    -- is being created
                    -> FilePath     -- ^ path to directory where stub source
                                    -- should be located
                    -> IO ()
writeSimpleTestStub t dir = do
    createDirectoryIfMissing True dir
    let filename = dir </> stubFilePath t
        LibTest _ m = testType t
    withFile filename WriteMode $ \h -> do
        hPutStrLn h $ unlines
            [ "module Main ( main ) where"
            , "import Distribution.Simple.Test ( runTests, setStubExitCode )"
            , "import " ++ show (disp m) ++ " ( tests )"
            , "main :: IO ()"
            , "main = do"
            , "    testsOutput <- getLine >>= runTests tests . read"
            , "    putStrLn $ show testsOutput"
            , "    setStubExitCode $ map snd $ snd testsOutput"
            ]
-- | Set the stub exit code based on the individual tests' results.
setStubExitCode :: [Result] -> IO ()
setStubExitCode results = do
    when (any isError results) $ exitWith $ ExitFailure 2
    when (any isFail results) $ exitWith $ ExitFailure 1
    exitSuccess
    where
        isFail (Fail _) = True
        isFail _ = False
        isError (Error _) = True
        isError _ = False

-- | Returns the shell environment for correctly running a test suite with
-- in-place data files.
dataDirEnv :: PackageDescription -> IO (Maybe [(String, String)])
dataDirEnv pkg_descr = do
    pwd <- getCurrentDirectory
    return $ Just [(n ++ "_datadir", pwd </> dataDir pkg_descr)]
    where PackageName n' = pkgName $ package pkg_descr
          n = map (\c -> if c == '-' then '_' else c) n'