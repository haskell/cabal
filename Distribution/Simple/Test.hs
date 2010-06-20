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

module Distribution.Simple.Test ( test ) where

import Distribution.Package ( PackageName(..), PackageIdentifier(..) )
import Distribution.PackageDescription
        ( PackageDescription(..), TestSuite(..), hasTests, TestType(..) )
import Distribution.Simple.LocalBuildInfo ( LocalBuildInfo(..) )
import Distribution.Simple.BuildPaths ( exeExtension )
import Distribution.Simple.Setup ( TestFlags(..), fromFlag, TestLogging(..) )
import Distribution.Simple.Utils ( die, notice )
import Distribution.Text
import Distribution.Verbosity ( normal )
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
        successFlag = fromFlag $ testSuccessOut flags
        failureFlag = fromFlag $ testFailOut flags
        PackageName pkg_name = pkgName $ package pkg_descr
        doTest t = case testType t of
            ExeTest v _ -> if withinRange v (withinVersion $ Version [1,0] [])
                then doExeTest t
                else do
                    _ <- die $ "No support for running test suite type: "
                            ++ show (disp $ testType t)
                    return False
            _ -> do
                    _ <- die $ "No support for running test suite type: "
                            ++ show (disp $ testType t)
                    return False
        doExeTest t = do
            when (verbosity >= normal) $ do
                putStr $ testName t ++ ": "
                hFlush stdout
            (outFile, exit) <- runTmpOutput exe $ "cabal-test-" ++ pkg_name
                                                    ++ "-" ++ testName t
            case exit of
                ExitSuccess -> do
                    go successFlag "Passed!" outFile
                    return True
                ExitFailure code -> do
                    go failureFlag ("Failed with exit code: "
                        ++ show code ++ "!") outFile
                    return False
            where exe = buildDir lbi </> testName t </>
                        testName t <.> exeExtension
                  go flag message outFile = do
                    when (verbosity >= normal) $ do
                        putStr $ message ++ " " ++
                            if flag == File || flag == Both
                                then "Output logged to: " ++ outFile ++ "\n"
                                else "\n"
                        when (flag == Terminal || flag == Both) $ do
                            withFile outFile ReadMode $ \h -> do
                                output <- hGetContents h
                                putStrLn output
                    unless (flag == File || flag == Both) $ removeFile outFile
                    hFlush stdout
    unless (hasTests pkg_descr) $ notice verbosity
            "Package has no tests or was configured with tests disabled."
    results <- mapM doTest $ testSuites pkg_descr
    let successful = length $ filter id results
        total = length $ testSuites pkg_descr
    notice verbosity $ show successful ++ " of " ++ show total ++
                       " test suites successful."
    when (successful < total) exitFailure

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

formatTime :: CalendarTime -> String
formatTime time =
    show (ctYear time)
    ++ pad (fromEnum . ctMonth)
    ++ pad ctDay
    ++ "-"
    ++ pad ctHour
    ++ pad ctMin
    ++ pad ctSec
    where pad f = (if f time < 10 then "0" else "") ++ show (f time)
