-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.ModuleTest
-- Copyright   :  Isaac Jones 2003-2004
-- 
-- Maintainer  :  Isaac Jones <ijones@syntaxpolice.org>
-- Stability   :  alpha
-- Portability :  GHC
--
-- Explanation: Test this module and sub modules.

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

module Distribution.ModuleTest where
#ifdef DEBUG
-- Import everything, since we want to test the compilation of them:

import qualified Distribution.Version as D.V (hunitTests)
-- import qualified Distribution.InstalledPackageInfo(hunitTests)
import qualified Distribution.License as D.L
import qualified Distribution.Extension as D.E (hunitTests)
import qualified Distribution.Make ()
import qualified Distribution.Package as D.P ()
import qualified Distribution.PackageDescription as D.PD (hunitTests)
import qualified Distribution.Setup as D.Setup (hunitTests)

import qualified Distribution.Simple as D.S (simpleHunitTests)
import qualified Distribution.Simple.Install as D.S.I (hunitTests)
import qualified Distribution.Simple.Build as D.S.B (hunitTests)
import qualified Distribution.Simple.SrcDist as D.S.S (hunitTests)
import qualified Distribution.Simple.Utils as D.S.U (hunitTests)
import Distribution.Compat.FilePath(joinFileName)
import qualified Distribution.Simple.Configure as D.S.C (hunitTests, localBuildInfoFile)
import qualified Distribution.Simple.Register as D.S.R (hunitTests, installedPkgConfigFile)

import qualified Distribution.Simple.GHCPackageConfig
    as GHC (localPackageConfig, maybeCreateLocalPackageConfig)

-- base
import Directory(setCurrentDirectory, doesFileExist,
                 doesDirectoryExist, getCurrentDirectory,
                 getPermissions, Permissions(..))
import System.Cmd(system)
import System.Exit(ExitCode(..))

import HUnit(runTestTT, Test(..), Counts(..), assertBool,
             assertEqual, Assertion, showCounts)

label :: String -> String
label t = "-= " ++ t ++ " =-"

runTestTT' :: Test -> IO Counts
runTestTT' t@(TestList _) = runTestTT t
runTestTT'  (TestLabel l t)
    = putStrLn (label l) >> runTestTT t
runTestTT' t = runTestTT t


checkTargetDir :: FilePath
               -> [String] -- ^suffixes
               -> IO ()
checkTargetDir targetDir suffixes
    = do doesDirectoryExist targetDir >>=
           assertBool "target dir exists"
         let files = [x++y |
                      x <- ["A", "B/A"],
                      y <- suffixes]
         allFilesE <- sequence [doesFileExist (targetDir ++ t)
                                | t <- files]

         sequence [assertBool ("target file missing: " ++ targetDir ++ f) e
                   | (e, f) <- zip allFilesE files]
         return ()

-- |Run this command, and assert it returns a successful error code.
assertCmd :: String -- ^Command
          -> String -- ^Comment
          -> Assertion
assertCmd command comment
    = system command >>= assertEqual (command ++ ":" ++ comment) ExitSuccess

-- |Run this command, and assert it returns an unsuccessful error code.
assertCmdFail :: String -- ^Command
              -> String -- ^Comment
              -> Assertion
assertCmdFail command comment
    = do code <- system command
         assertBool (command ++ ":" ++ comment) (code /= ExitSuccess)

tests :: FilePath -> [Test]
tests currDir
    = let testdir = currDir `joinFileName` "test" in
      [TestLabel "testing the wash2hs package" $ TestCase $ 
         do setCurrentDirectory $ (testdir `joinFileName` "wash2hs")
            system "make clean"
            system "make"
            assertCmdFail "./setup configure --someUnknownFlag" "wash2hs configure with unknown flag"
            assertCmd "./setup configure --prefix=\",tmp\"" "wash2hs configure"
            assertCmd "./setup haddock" "setup haddock returned error code."
            assertCmd "./setup build" "wash2hs build"
            doesFileExist "dist/build/hs/wash2hs"
              >>= assertBool "wash2hs build didn't create executable!"
            assertCmd "./setup install --user" "wash2hs install"
            doesFileExist ",tmp/bin/wash2hs"
              >>= assertBool "wash2hs didn't put executable into place."
            perms <- getPermissions ",tmp/bin/wash2hs"
            assertBool "wash2hs isn't +x" (executable perms)
         ,TestLabel "testing the HUnit package" $ TestCase $ 
         do setCurrentDirectory $ (testdir `joinFileName` "HUnit-1.0")
            pkgConf <- GHC.localPackageConfig
            GHC.maybeCreateLocalPackageConfig
            system $ "ghc-pkg --config-file=" ++ pkgConf ++ " -r HUnit"
            system "make clean"
            system "make"
            system $ "touch " ++ D.S.C.localBuildInfoFile
            system $ "touch " ++ D.S.R.installedPkgConfigFile
            doesFileExist D.S.C.localBuildInfoFile >>= 
              assertBool ("touch " ++ D.S.C.localBuildInfoFile ++ " failed")
            assertCmd "./setup configure --prefix=\",tmp\"" "hunit configure"
            assertCmd "./setup haddock" "setup haddock returned error code."

            -- Test clean:
            assertCmd "./setup build" "hunit build"
            doesDirectoryExist "dist/build" >>= 
              assertBool "HUnit build did not create build directory"
            assertCmd "./setup clean" "hunit clean"
            doesDirectoryExist "dist/build" >>= 
              assertEqual "HUnit clean did not get rid of build directory" False
            doesFileExist D.S.C.localBuildInfoFile >>= 
              assertEqual ("clean " ++ D.S.C.localBuildInfoFile ++ " failed") False
            doesFileExist D.S.R.installedPkgConfigFile >>= 
              assertEqual ("clean " ++ D.S.R.installedPkgConfigFile ++ " failed") False

            assertCmd "./setup configure --prefix=\",tmp\"" "hunit configure"
            assertCmd "./setup haddock" "setup haddock returned error code."
            assertCmd "./setup build" "hunit build"
            assertCmd "./setup install --user" "hunit install"
            assertCmd ("ghc -package-conf " ++ pkgConf ++ " -package HUnit HUnitTester.hs -o ./hunitTest") "compile w/ hunit"
            assertCmd "./hunitTest" "hunit test"
            assertCmd ("ghc-pkg --config-file=" ++ pkgConf ++ " -r HUnit") "package remove"

         ,TestLabel "package A: configure GHC, sdist" $ TestCase $
         do pkgConf  <- GHC.localPackageConfig
            GHC.maybeCreateLocalPackageConfig
            system $ "ghc-pkg -r test --config-file=" ++ pkgConf
            setCurrentDirectory $ (testdir `joinFileName` "A")
            system "make clean"
            system "make"
            assertCmd "./setup configure --ghc --prefix=,tmp"
              "configure returned error code"
            assertCmd "./setup haddock" "setup haddock returned error code."

            assertCmd "./setup build"
              "build returned error code"
            doesFileExist "dist/build/testA" >>= 
              assertBool "build did not create the executable: testA"
            doesFileExist "dist/build/testB" >>= 
              assertBool "build did not create the executable: testB"
            assertCmd "./setup sdist"
             "setup sdist returned error code"
            doesFileExist "dist/test-1.0.tgz" >>= 
              assertBool "sdist did not put the expected file in place"
            doesFileExist "dist/src" >>=
              assertEqual "dist/src exists" False
            doesDirectoryExist "dist/build" >>=
              assertBool "dist/build doesn't exists"
         ,TestLabel "package A: GHC and copy-prefix" $ TestCase $ -- (uses above config)
         do let targetDir = ",tmp2"
            instRetCode <- system $ "./setup copy --copy-prefix=" ++ targetDir
            checkTargetDir ",tmp2/lib/test-1.0/" [".hi"]
            doesFileExist (",tmp2/lib/test-1.0/" `joinFileName` "libHStest-1.0.a")
              >>= assertBool "library doesn't exist"
            assertEqual "install returned error code" ExitSuccess instRetCode
         ,TestLabel "package A: GHC and copy to configure loc." $ TestCase $ -- (uses above config)
         do instRetCode <- system $ "./setup copy"
            checkTargetDir ",tmp/lib/test-1.0/" [".hi"]
            doesFileExist (",tmp/lib/test-1.0/" `joinFileName` "libHStest-1.0.a")
              >>= assertBool "library doesn't exist"
            assertEqual "install returned error code" ExitSuccess instRetCode
         ,TestLabel "package A: GHC and install w/ no prefix" $ TestCase $
         do let targetDir = ",tmp/lib/test-1.0/"
            instRetCode <- system $ "./setup install --user"
            checkTargetDir targetDir [".hi"]
            doesFileExist (targetDir `joinFileName` "libHStest-1.0.a")
              >>= assertBool "library doesn't exist"
            assertEqual "install returned error code" ExitSuccess instRetCode
         ,TestLabel "package HSQL (make-based): GHC building" $ TestCase $
         do setCurrentDirectory $ (testdir `joinFileName` "HSQL")
            system "make distclean"
            system "rm -rf /tmp/lib/HSQL"
            system "ghc -cpp --make -i../.. Setup.lhs -o setup 2>out.build"
            assertCmd "./setup configure --ghc --prefix=/tmp"
              "configure returned error code"
            doesFileExist "config.mk" >>=
              assertBool "config.mk not generated after configure"
            assertCmd "./setup build" "build hsql returned error code"
            assertCmd "./setup copy" "copy hsql returned error code"
            doesFileExist "/tmp/lib/HSQL/GHC/libHSsql.a" >>=
              assertBool "libHSsql.a doesn't exist. copy failed."
         ,TestLabel "package withHooks: GHC building" $ TestCase $
         do setCurrentDirectory $ (testdir `joinFileName` "withHooks")
            system "make clean"
            system "make"
            assertCmd "./setup configure --ghc --prefix=,tmp --woohoo"
              "configure returned error code"
            assertCmd "./setup haddock" "setup haddock returned error code."
            assertCmd "./setup build"
              "build returned error code"
            doesFileExist "dist/build/withHooks" >>= 
              assertBool "build did not create the executable: withHooks"
            doesFileExist "dist/build/C.o" >>=
              assertBool "C.testSuffix did not get compiled to C.o."
            doesFileExist "dist/build/D.o" >>=
              assertBool "D.gc did not get compiled to D.o this is an overriding test"
         ,TestLabel "package withHooks: GHC and copy" $ TestCase $
         do let targetDir = ",tmp"
            instRetCode <- system $ "./setup copy --copy-prefix=" ++ targetDir
            doesFileExist (",tmp/lib/withHooks-1.0/" `joinFileName` "libHSwithHooks-1.0.a")
              >>= assertBool "library doesn't exist"
            doesFileExist ",tmp/bin/withHooks"
              >>= assertBool "executable doesn't exist"
            assertEqual "install returned error code" ExitSuccess instRetCode
         ,TestLabel "package withHooks: GHC and clean" $ TestCase $
         do system "./setup clean"
            doesFileExist "C.hs" >>=
               assertEqual "C.hs (a generated file) not cleaned." False
         ,TestLabel "package twoMains: GHC building" $ TestCase $
         do setCurrentDirectory $ (testdir `joinFileName` "twoMains")
            system "make clean"
            system "make"
            assertCmd "./setup configure --ghc --prefix=,tmp"
              "configure returned error code"
            assertCmd "./setup haddock" "setup haddock returned error code."
            assertCmd "./setup build"
              "build returned error code"
            doesFileExist "dist/build/testA" >>= 
              assertBool "build did not create the executable: testA"
            doesFileExist "dist/build/testB" >>= 
              assertBool "build did not create the executable: testB"
            assertCmd "./dist/build/testA isA" "A is not A"
            assertCmd "./dist/build/testB isB" "B is not B"
         ,TestLabel "package depOnLib: GHC building (executable depending on its lib)" $ TestCase $
         do setCurrentDirectory $ (testdir `joinFileName` "depOnLib")
            system "make clean"
            system "make"
            assertCmd "./setup configure --ghc --prefix=,tmp"
              "configure returned error code"
            assertCmd "./setup haddock" "setup haddock returned error code."
            assertCmd "./setup build"
              "build returned error code"
            assertCmd "./setup copy"
              "copy returned error code"
            doesFileExist "dist/build/mains/mainForA" >>= 
              assertBool "build did not create the executable: mainForA"
            doesFileExist ("dist/build/" `joinFileName` "libHStest-1.0.a")
              >>= assertBool "library doesn't exist"
            doesFileExist (",tmp/bin/mainForA")
              >>= assertBool "installed bin doesn't exist"
            doesFileExist (",tmp/lib/test-1.0/libHStest-1.0.a")
              >>= assertBool "installed lib doesn't exist"

--          ,TestLabel "package A:no install-prefix and hugs" $ TestCase $
--          do assertCmd "./setup configure --hugs --prefix=,tmp"
--              "HUGS configure returned error code"
--             assertCmd "./setup build"
--              "HUGS build returned error code"
--             instRetCode <- system "./setup install --user"
--             let targetDir = ",tmp/lib/test-1.0/"
--             checkTargetDir targetDir [".hs"]
--             assertEqual "install HUGS returned error code" ExitSuccess instRetCode
         ]

main :: IO ()
main = do putStrLn "compile successful"
          putStrLn "-= Setup Tests =-"
          count1 <- runTestTT' $ TestList $
                        (TestLabel "Utils Tests" $ TestList D.S.U.hunitTests):
                        (TestLabel "Setup Tests" $ TestList D.Setup.hunitTests):
                        (TestLabel "config Tests" $ TestList D.S.C.hunitTests):
                          (D.S.R.hunitTests ++ D.V.hunitTests ++
                           D.S.S.hunitTests ++ D.S.B.hunitTests ++
                           D.S.I.hunitTests ++ D.S.simpleHunitTests ++
                           D.PD.hunitTests ++ D.E.hunitTests)
          dir <- getCurrentDirectory
          count2 <- runTestTT' $ TestList (tests dir)
          putStrLn "-------------"
          putStrLn "Test Summary:"
          putStrLn $ showCounts $ combineCounts count1 count2
          return ()

combineCounts :: Counts -> Counts -> Counts
combineCounts (Counts a b c d) (Counts a' b' c' d')
    = Counts (a + a') (b + b') (c + c') (d + d')
#endif
-- Local Variables:
-- compile-command: "ghc -i../:/usr/local/src/HUnit-1.0 -Wall --make ModuleTest.hs -o moduleTest"
-- End:

