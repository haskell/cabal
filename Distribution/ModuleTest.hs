-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.ModuleTest
-- Copyright   :  Isaac Jones 2003-2004
-- 
-- Maintainer  :  Isaac Jones <ijones@syntaxpolice.org>
-- Stability   :  alpha
-- Portability :  GHC
--
-- Explanation: <FIX>
-- WHERE DOES THIS MODULE FIT IN AT A HIGH-LEVEL <FIX>

{- Copyright (c) 2003-2004, Isaac Jones
All rights reserved.

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

module Main where

-- Import everything, since we want to test the compilation of them:

import qualified Distribution.Version as D.V (hunitTests) 
-- import qualified Distribution.InstalledPackageInfo(hunitTests)
import qualified Distribution.Misc as D.M (hunitTests)
import qualified Distribution.Package as D.P (hunitTests)
import qualified Distribution.Setup as D.Setup (hunitTests)

import qualified Distribution.Simple as D.S (simpleHunitTests)
import qualified Distribution.Simple.Install as D.S.I (hunitTests)
import qualified Distribution.Simple.Build as D.S.B (hunitTests)
import qualified Distribution.Simple.SrcDist as D.S.S (hunitTests)
import qualified Distribution.Simple.Utils as D.S.U (hunitTests)
import qualified Distribution.Simple.Configure as D.S.C (hunitTests)
import qualified Distribution.Simple.Register as D.S.R (hunitTests)

-- base
import Control.Monad(when)
import Directory(setCurrentDirectory, doesFileExist,
                 doesDirectoryExist)
import System.Cmd(system)
import System.Exit(ExitCode(..))

import HUnit(runTestTT, Test(..), Counts, assertBool)

label :: String -> String
label t = "-= " ++ t ++ " =-"

runTestTT' :: Test -> IO Counts
runTestTT' t@(TestList _) = runTestTT t
runTestTT'  (TestLabel l t)
    = putStrLn (label l) >> runTestTT t
runTestTT' t = runTestTT t

tests :: [Test]
tests = [TestCase $
         do setCurrentDirectory "test"
            dirE1 <- doesDirectoryExist ",tmp"
            when dirE1 (system "rm -r ,tmp">>return())
--            system "ls"
            system "./setup configure --prefix=,tmp"
            let targetDir = ",tmp/lib/test-1.0/"
            system "./setup build"
            instRetCode <- system "./setup install --user"
            dirE <- doesDirectoryExist targetDir
            assertBool "target dir exists" dirE
            let files = ["A.hs", "B/A.hs", "libHStest-1.0.a"]
            allFilesE <- sequence [doesFileExist (targetDir ++ t)
                                     | t <- files]

            sequence [assertBool ("target file missing: " ++ targetDir ++ f) e
                       | (e, f) <- zip allFilesE files]
            assertBool "install returned error code" (instRetCode == ExitSuccess)
        ]


main :: IO ()
main = do putStrLn "compile successful"
          putStrLn "-= Setup Tests =-"
          runTestTT' $ TestList $
                        (TestLabel "Utils Tests" $ TestList D.S.U.hunitTests):
                        (TestLabel "Setup Tests" $ TestList D.Setup.hunitTests):
                        (TestLabel "config Tests" $ TestList D.S.C.hunitTests):
                          (D.S.R.hunitTests ++ D.V.hunitTests ++
                           D.S.S.hunitTests ++ D.S.B.hunitTests ++
                           D.S.I.hunitTests ++ D.S.simpleHunitTests ++
                           D.P.hunitTests ++ D.M.hunitTests)
          runTestTT' $ TestList tests
          return ()

-- Local Variables:
-- compile-command: "ghc -i../:/usr/local/src/HUnit-1.0 -Wall --make ModuleTest.hs -o moduleTest"
-- End:

