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

import Distribution.Version()
import Distribution.InstalledPackageInfo()
import Distribution.Misc()
import Distribution.Package()
import qualified Distribution.Setup(hunitTests)

import Distribution.Simple.Default()
import Distribution.Simple.Install()
import Distribution.Simple.Build()

import HUnit(runTestTT, Test(..))

label t = "-= " ++ t ++ " =-"

runTestTT' t@(TestList _) = runTestTT t
runTestTT'  (TestLabel l t)
    = putStrLn (label l) >> runTestTT t
runTestTT' t = runTestTT t

main :: IO ()
main = do putStrLn "compile successful"
          putStrLn "-= Setup Tests =-"
          setupTests <- Distribution.Setup.hunitTests
          mapM runTestTT' setupTests
          return ()

-- Local Variables:
-- compile-command: "ghc -i../:/usr/local/src/HUnit-1.0 -Wall --make ModuleTest.hs -o moduleTest"
-- End:
