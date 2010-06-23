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
    , stubFilePath
    , stubName
    ) where

import Distribution.PackageDescription
        ( PackageDescription(..), TestSuite(..), TestType(..) )
import Distribution.Simple.BuildPaths ( exeExtension )
import Distribution.Simple.Compiler ( Compiler(..) )
import Distribution.Simple.InstallDirs
    ( fromPathTemplate, initialPathTemplateEnv, PathTemplateVariable(..)
    , substPathTemplate , toPathTemplate, refersTo )
import Distribution.Simple.LocalBuildInfo ( LocalBuildInfo(..) )
import Distribution.Simple.Setup ( TestFlags(..), TestFilter(..), fromFlag )
import Distribution.Simple.Utils ( die, notice )
import Distribution.TestSuite
    ( Test(..) , Result(..), PureTestable(..), ImpureTestable(..)
    , TestOptions(..) )
import Distribution.Text
import Distribution.Verbosity ( Verbosity, silent )
import Distribution.Version ( Version(..), withinVersion, withinRange )

import Control.Monad ( unless, when )
import System.Directory ( getTemporaryDirectory, removeFile )
import System.Exit ( ExitCode(..), exitFailure )
import System.FilePath ( (</>), (<.>) )
import System.IO ( withFile, IOMode(..), hFlush, stdout, hGetContents )
import System.Process ( runProcess, waitForProcess )
import System.Time ( getClockTime, toUTCTime, CalendarTime(..) )

-- |Perform the \"@.\/setup test@\" action.
test :: PackageDescription  -- ^information from the .cabal file
     -> LocalBuildInfo      -- ^information from the configure step
     -> TestFlags           -- ^flags sent to test
     -> IO ()
test pkg_descr lbi flags = do
    let verbosity = fromFlag $ testVerbosity flags
        template = fromFlag $ testLogFile flags
        distPref = fromFlag $ testDistPref flags
        filterFlag = fromFlag $ testFilter flags
        total = length $ testSuites pkg_descr
        doTest suite files = do
            notice verbosity $ "Test suite " ++ testName suite ++ ": RUNNING..."
            let pkgId = package pkg_descr
                compId = compilerId $ compiler lbi
                env m r = initialPathTemplateEnv pkgId compId
                    ++ [ (TestSuiteNameVar, toPathTemplate $ testName suite)
                       , (TestSuiteStdIoVar, toPathTemplate m)
                       , (TestSuiteResultVar, toPathTemplate r)
                       ]
                path m r = distPref </> "test"
                    </> fromPathTemplate (substPathTemplate (env m r) template)
                cmd = buildDir lbi </> testName suite </>
                    testName suite <.> exeExtension
            bracket
                (do
                    (stdoutTemp, hStdout) <- openTempFile
                        (distPref </> "test")
                        (testName suite ++ "-stdout" <.> "log")
                    if template `refersTo` TestSuiteStdIoVar
                        then do
                            (stderrTemp, hStderr) <- openTempFile
                                (distPref </> "test")
                                (testName suite ++ "-stderr" <.> "log")
                            return (stdoutTemp, hStdout, stderrTemp, hStderr)
                        else do
                            return (stdoutTemp, hStdout, stdoutTemp, hStdout)
                )
                (\(stdoutTemp, hStdout, stderrTemp, hStderr) -> do
                    hClose hStdout
                    hClose hStderr
                    removeFile stdoutTemp
                    stderrExists <- doesFileExist stderrTemp
                    when stderrExists $ removeFile stderrTemp
                )
                (\(stdoutTemp, hStdout, stderrTemp, hStderr) -> do
                    result <- case testType suite of
                        ExeTest v _ | withinRange v
                            (withinVersion $ Version [1,0] []) -> do
                            proc <- runProcess cmd [] Nothing Nothing Nothing
                                (Just hStdout) (Just hStderr)
                            exit <- waitForProcess proc
                            return $ case exit of
                                ExitSuccess -> True
                                ExitFailure _ -> False
                        _ -> do _ <- die $ "No support for running test suite "
                                    ++ "type: " ++ show (disp $ testType suite)
                                return False
                    hClose hStdout
                    hClose hStderr
                    let rStr = if result then "pass" else "fail"
                        stdoutFile = path "stdout" rStr
                        stderrFile = path "stderr" rStr
                        stdoutMode = if stdoutFile `elem` files
                            then AppendMode else WriteMode
                        stderrMode = if stderrFile `elem` files
                            then AppendMode else WriteMode
                    withFile stdoutTemp ReadMode $ \hTemp ->
                        withFile stdoutFile stdoutMode $ \hOut ->
                        hGetContents hTemp >>= hPutStr hOut
                    unless (stdoutFile == stderrFile) $
                        withFile stderrTemp ReadMode $ \hTemp ->
                            withFile stderrFile stderrMode $ \hErr ->
                            hGetContents hTemp >>= hPutStr hErr
                    notice verbosity $ "Test suite " ++ testName suite
                        ++ ": " ++ map toUpper rStr
                    when (filterFlag > Summary && not result) $
                        showTestLog verbosity stdoutFile
                    when (filterFlag > Failures && result) $
                        showTestLog verbosity stdoutFile
                    return (files ++ [stdoutFile, stderrFile], result)
                )
    createDirectoryIfMissing True $ distPref </> "test"
    notice verbosity $ "Running " ++ show total ++ " test suites..."
    (_, results) <- foldM (\(files, rs) t -> do
        (files', r) <- doTest t files
        return (files', rs ++ [r])
        ) ([], []) $ testSuites pkg_descr
    let successful = length $ filter id results
    notice verbosity $ show successful ++ " of " ++ show total
        ++ " test suites successful."
    when (successful < total) exitFailure

showTestLog :: Verbosity -> FilePath -> IO ()
showTestLog verbosity outFile = when (verbosity > silent) $ do
    withFile outFile ReadMode $ \hOut -> hGetContents hOut >>= putStrLn

runTmpOutput :: FilePath -> FilePath -> IO (FilePath, ExitCode)
runTmpOutput cmd base = do
    tmp <- getTemporaryDirectory
    time <- getClockTime
    let timeString = formatTime $ toUTCTime time
        file = tmp </> base ++ "-" ++ timeString ++ ".log"
    withFile file WriteMode $ \hOut -> do
        proc <- runProcess cmd [] Nothing Nothing Nothing
                (Just hOut) (Just hOut)
        exit <- waitForProcess proc
        return (file, exit)

writeSimpleTestStub :: TestSuite -> FilePath -> IO ()
writeSimpleTestStub t dir = do
    createDirectoryIfMissing True dir
    let filename = dir </> stubFilePath t
    withFile filename WriteMode $ \h -> do
        hPutStr h $ unlines
            [ "module Main ( main ) where"
            , "import Distribution.Simple.Test ( runTests )"
            , "import " ++ testName t ++ " ( tests )"
            , "main :: IO ()"
            , "main = runTests tests", ""
            ]