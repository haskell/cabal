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

module Main where
#ifdef DEBUG
-- Import everything, since we want to test the compilation of them:

import qualified Distribution.Version as D.V (hunitTests)
-- import qualified Distribution.InstalledPackageInfo(hunitTests)
import qualified Distribution.License as D.L
import qualified Distribution.Compiler as D.C (hunitTests)
import qualified Distribution.Make ()
import qualified Distribution.Package as D.P ()
import qualified Distribution.PackageDescription as D.PD (hunitTests)
import qualified Distribution.Setup as D.Setup (hunitTests)
import Distribution.Compiler (CompilerFlavor(..), Compiler(..))
import Distribution.Version (Version(..))

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

import Distribution.Simple.Configure (configCompiler)

-- base
import Data.List (intersperse)
import Control.Monad(when, unless)
import Directory(setCurrentDirectory, doesFileExist,
                 doesDirectoryExist, getCurrentDirectory,
                 getPermissions, Permissions(..))
import Distribution.Compat.Directory (removeDirectoryRecursive)
import System.Cmd(system)
import System.Exit(ExitCode(..))
import System.Environment (getArgs)

import HUnit(runTestTT, Test(..), Counts(..), assertBool,
             assertEqual, Assertion, showCounts)


-- ------------------------------------------------------------
-- * Helpers
-- ------------------------------------------------------------

combineCounts :: Counts -> Counts -> Counts
combineCounts (Counts a b c d) (Counts a' b' c' d')
    = Counts (a + a') (b + b') (c + c') (d + d')

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
         let mods = ["A", "B/A"]
         allFilesE <- mapM anyExists [[(targetDir ++ t ++ y)
                                           | y <- suffixes]
                                            | t <- mods]

         sequence [assertBool ("target file missing: " ++ targetDir ++ f) e
                   | (e, f) <- zip allFilesE mods]
         return ()

  where anyExists :: [FilePath] -> IO Bool
        anyExists l = do l' <- mapM doesFileExist l
                         return $ any (== True) l'

-- |Run this command, and assert it returns a successful error code.
assertCmd :: String -- ^Command
          -> String -- ^Comment
          -> Assertion
assertCmd command comment
    = system command >>= assertEqual (command ++ ":" ++ comment) ExitSuccess

-- |like assertCmd, but separates command and args
assertCmd' :: String -- ^Command
           -> String -- ^args
           -> String -- ^Comment
           -> Assertion
assertCmd' command args comment
    = system (command ++ " "++ args ++ ">>out.build")
        >>= assertEqual (command ++ ":" ++ comment) ExitSuccess

-- |Run this command, and assert it returns an unsuccessful error code.
assertCmdFail :: String -- ^Command
              -> String -- ^Comment
              -> Assertion
assertCmdFail command comment
    = do code <- system command
         assertBool (command ++ ":" ++ comment) (code /= ExitSuccess)


-- ------------------------------------------------------------
-- * Integration Tests
-- ------------------------------------------------------------

tests :: FilePath       -- ^Currdir
      -> CompilerFlavor -- ^build setup with compiler
      -> CompilerFlavor -- ^configure with which compiler
      -> Version        -- ^version of the compiler to use
      -> [Test]
tests currDir comp compConf compVersion = [
-- executableWithC
         TestLabel ("package exeWithC: " ++ compIdent) $ TestCase $
         do let targetDir =",tmp"
            setCurrentDirectory $ (testdir `joinFileName` "exeWithC")
            testPrelude
            assertConfigure targetDir
            assertClean
            assertConfigure targetDir
            assertBuild
            assertCopy
            assertCmd ",tmp/bin/tt" "exeWithC failed"
-- A
         ,TestLabel ("package A: " ++ compIdent) $ TestCase $
         do let targetDir=",tmp"
            setCurrentDirectory $ (testdir `joinFileName` "A")
            testPrelude
            assertConfigure targetDir
            assertHaddock
            assertBuild
            when (comp == GHC) -- are these tests silly?
              (do doesDirectoryExist "dist/build" >>=
                    assertBool "dist/build doesn't exist"
                  doesFileExist "dist/build/testA/testA" >>= 
                    assertBool "build did not create the executable: testA"
                  doesFileExist "dist/build/testB/testB" >>= 
                    assertBool "build did not create the executable: testB"
                  doesFileExist "dist/build/testA/testA-tmp/c_src/hello.o" >>=
                    assertBool "build did not build c source for testA"
                  doesFileExist "dist/build/hello.o" >>=
                    assertBool "build did not build c source for A library"
              )
            assertCopy
            libForA targetDir
            doesFileExist ",tmp/bin/testA" >>=
              assertBool "testA not produced"
            doesFileExist ",tmp/bin/testB" >>=
              assertBool "testB not produced"
            assertCmd' compCmd "sdist" "setup sdist returned error code"
            doesFileExist "dist/test-1.0.tar.gz" >>= 
              assertBool "sdist did not put the expected file in place"
            doesFileExist "dist/src" >>=
              assertEqual "dist/src exists" False
            assertCmd' compCmd "register --user" "pkg A, register failed"
            assertCmd' compCmd "unregister --user" "pkg A, unregister failed"
            -- tricky, script-based register
            registerAndExecute comp "pkg A: register with script failed"
            unregisterAndExecute comp "pkg A: unregister with script failed"
            -- non-trick non-script based register
            assertCmd' compCmd "register --user" "regular register returned error"
            assertCmd' compCmd "unregister --user" "regular unregister returned error"

         ,TestLabel ("package A copy-prefix: " ++ compIdent) $ TestCase $ -- (uses above config)
         do let targetDir = ",tmp2"
            assertCmd' compCmd ("copy --copy-prefix=" ++ targetDir) "copy --copy-prefix failed"
            doesFileExist ",tmp2/bin/testA" >>=
              assertBool "testA not produced"
            doesFileExist ",tmp2/bin/testB" >>=
              assertBool "testB not produced"
            libForA ",tmp2"
         ,TestLabel ("package A and install w/ no prefix: " ++ compIdent) $ TestCase $
         do let targetDir = ",tmp/lib/test-1.0/ghc-6.4" -- FIX: Compiler-version
            removeDirectoryRecursive ",tmp"
            when (comp == GHC) -- FIX: hugs can't do --user yet
              (do system $ "ghc-pkg unregister --user test-1.0"
                  assertCmd' compCmd "install --user" "install --user failed"
                  libForA ",tmp"
                  assertCmd' compCmd "unregister --user" "unregister failed")
-- HUnit
         ,TestLabel ("testing the HUnit package" ++ compIdent) $ TestCase $ 
         do setCurrentDirectory $ (testdir `joinFileName` "HUnit-1.0")
            GHC.maybeCreateLocalPackageConfig
            system "make clean"
            system "make"
            assertCmd' compCmd "configure" "configure failed"
            system "setup unregister --user"


            system $ "touch " ++ D.S.C.localBuildInfoFile
            system $ "touch " ++ D.S.R.installedPkgConfigFile
            doesFileExist D.S.C.localBuildInfoFile >>= 
              assertBool ("touch " ++ D.S.C.localBuildInfoFile ++ " failed")

            -- Test clean:
            assertBuild
            doesDirectoryExist "dist/build" >>= 
              assertBool "HUnit build did not create build directory"
            assertCmd' compCmd "clean" "hunit clean"
            doesDirectoryExist "dist/build" >>= 
              assertEqual "HUnit clean did not get rid of build directory" False

            doesFileExist D.S.C.localBuildInfoFile >>= 
              assertEqual ("clean " ++ D.S.C.localBuildInfoFile ++ " failed") False
            doesFileExist D.S.R.installedPkgConfigFile >>= 
              assertEqual ("clean " ++ D.S.R.installedPkgConfigFile ++ " failed") False

            assertConfigure ",tmp"
            assertHaddock
            doesDirectoryExist "dist/doc" >>= assertEqual "create of dist/doc" True
            assertBuild
            when (comp == GHC) -- tests building w/ an installed -package
                 (do pkgConf <- GHC.localPackageConfig
                     assertCmd' compCmd "install --user" "hunit install"
                     assertCmd ("ghc -package-conf " ++ pkgConf
                                ++ " -package HUnitTest HUnitTester.hs -o ./hunitTest")
                                "compile w/ hunit"
                     assertCmd "./hunitTest" "hunit test"
                     assertCmd' compCmd "unregister --user" "unregister failed")
            assertClean
            doesDirectoryExist "dist/doc" >>= assertEqual "clean dist/doc" False
            assertCmd "make clean" "make clean failed"

-- twoMains
         ,TestLabel ("package twoMains: building " ++ compIdent) $ TestCase $
         do setCurrentDirectory $ (testdir `joinFileName` "twoMains")
            testPrelude
            assertConfigure ",tmp"
            assertCmd' compCmd "haddock" "setup haddock returned error code."
            assertBuild
            assertCopy
            doesFileExist ",tmp/bin/testA" >>= 
              assertBool "install did not create the executable: testA"
            doesFileExist ",tmp/bin/testB" >>= 
              assertBool "install did not create the executable: testB"
            assertCmd "./,tmp/bin/testA isA" "A is not A"
            assertCmd "./,tmp/bin/testB isB" "B is not B"
            -- no register, since there's no library
-- buildinfo
         ,TestLabel ("buildinfo with multiple executables " ++ compIdent) $ TestCase $
         do setCurrentDirectory $ (testdir `joinFileName` "buildInfo")
            testPrelude
            assertConfigure ",tmp"
            assertCmd' compCmd "haddock" "setup haddock returned error code."
            assertBuild
            assertCopy
            doesFileExist ",tmp/bin/exe1" >>= 
              assertBool "install did not create the executable: exe1"
            doesFileExist ",tmp/bin/exe2" >>= 
              assertBool "install did not create the executable: exe2"
            -- no register, since there's no library
-- mutually recursive modules
         ,TestLabel ("package recursive: building " ++ compIdent) $ TestCase $
           when (comp == GHC) (do
            setCurrentDirectory $ (testdir `joinFileName` "recursive")
            testPrelude
            assertConfigure ",tmp"
            assertBuild
            assertCopy
            doesFileExist "dist/build/A.hi-boot" >>=
              assertBool "build did not move A.hi-boot file into place lib"
            doesFileExist (",tmp/lib/recursive-1.0/ghc-" ++ compVerStr
	               ++ "/libHSrecursive-1.0.a") >>=
              assertBool "recursive build didn't create library"
            doesFileExist "dist/build/testExe/testExe-tmp/A.hi" >>=
              assertBool "build did not move A.hi-boot file into place exe"
            doesFileExist "dist/build/testExe/testExe" >>=
              assertBool "recursive build didn't create binary")
-- linking in ffi stubs
         ,TestLabel ("package ffi: " ++ compIdent) $ TestCase $
         do setCurrentDirectory (testdir `joinFileName` "ffi-package")
            testPrelude
            assertConfigure "/tmp"
            assertBuild
            -- install it so we can test building with it.
            assertCmd' compCmd "install --user" "ffi-package install"
            assertClean
            doesFileExist "src/TestFFI_stub.c" >>=
                assertEqual "FFI-generated stub not cleaned." False
            -- now build something that depends on it
            setCurrentDirectory (".." `joinFileName` "ffi-bin")
            testPrelude
            assertConfigure ",tmp"
            assertBuild
            assertCopy
-- depOnLib
       ,TestLabel ("package depOnLib: (executable depending on its lib)"++ compIdent) $ TestCase $
         do setCurrentDirectory $ (testdir `joinFileName` "depOnLib")
            testPrelude
            assertConfigure ",tmp"
            assertHaddock
            assertBuild
            assertCopy
            registerAndExecute comp "pkg depOnLib: register with script failed"
            unregisterAndExecute comp "pkg DepOnLib: unregister with script failed"
            when (comp == GHC) (do 
                                doesFileExist "dist/build/mainForA/mainForA" >>= 
                                  assertBool "build did not create the executable: mainForA"
                                doesFileExist ("dist/build/" `joinFileName` "libHStest-1.0.a")
                                  >>= assertBool "library doesn't exist"
                                doesFileExist (",tmp/bin/mainForA")
                                  >>= assertBool "installed bin doesn't exist"
                                doesFileExist (",tmp/lib/test-1.0/ghc-" ++ compVerStr ++ "/libHStest-1.0.a")
                                  >>= assertBool "installed lib doesn't exist")
-- wash2hs
       ,TestLabel ("testing the wash2hs package" ++ compIdent) $ TestCase $ 
         do setCurrentDirectory $ (testdir `joinFileName` "wash2hs")
            testPrelude
            assertCmdFail (compCmd ++ " configure --someUnknownFlag")
                          "wash2hs configure with unknown flag"
            assertConfigure ",tmp"
            assertHaddock
            assertBuild
            assertCopy
            -- no library to register
            doesFileExist ",tmp/bin/wash2hs"
              >>= assertBool "wash2hs didn't put executable into place."
            perms <- getPermissions ",tmp/bin/wash2hs"
            assertBool "wash2hs isn't +x" (executable perms)
            assertClean
            -- no unregister, because it has no libs!
-- withHooks
         ,TestLabel ("package withHooks: "++compIdent) $ TestCase $
         do setCurrentDirectory $ (testdir `joinFileName` "withHooks")
            testPrelude
            assertCmd' compCmd ("configure --prefix=,tmp --woohoo " ++ compFlag)
              "configure returned error code"
            assertCmdFail (compCmd ++ " test --asdf") "test was supposed to fail"
            assertCmd' compCmd ("test --pass") "test should not have failed"

            assertHaddock
            assertBuild
            assertCmd' compCmd "copy --copy-prefix=,tmp" "copy w/ prefix"
            doesFileExist ",tmp/withHooks" >>=  -- this file is added w/ the hook.
              assertBool "hooked copy, redirecting prefix didn't work."
            assertCmd' compCmd "register --user" "regular register returned error"
            assertCmd' compCmd "unregister --user" "regular unregister returned error"
            when (comp == GHC) -- FIX: come up with good test for Hugs
                 (do doesFileExist "dist/build/C.o" >>=
                       assertBool "C.testSuffix did not get compiled to C.o."
                     doesFileExist "dist/build/D.o" >>=
                       assertBool "D.gc did not get compiled to D.o this is an overriding test"
                     doesFileExist (",tmp/lib/withHooks-1.0/ghc-" ++ compVerStr
		                 ++ "/" `joinFileName` "libHSwithHooks-1.0.a")
                       >>= assertBool "library doesn't exist")

            doesFileExist ",tmp/bin/withHooks" >>= 
              assertBool "copy did not create the executable: withHooks"
            assertClean
            doesFileExist "C.hs" >>=
               assertEqual "C.hs (a generated file) not cleaned." False
-- HSQL
{-         ,TestLabel ("package HSQL (make-based): " ++ show compIdent) $
         TestCase $ unless (compFlag == "--hugs") $ -- FIX: won't compile w/ hugs
         do setCurrentDirectory $ (testdir `joinFileName` "HSQL")
            system "make distclean"
            system "rm -rf /tmp/lib/HSQL"
            when (comp == GHC)
                 (system "ghc -cpp --make -i../.. Setup.lhs -o setup 2>out.build" >> return())
            assertConfigure "/tmp"
            doesFileExist "config.mk" >>=
              assertBool "config.mk not generated after configure"
            assertBuild
            assertCopy
            when (comp == GHC) -- FIX: do something for hugs
                 (doesFileExist "/tmp/lib/HSQL/GHC/libHSsql.a" >>=
                   assertBool "libHSsql.a doesn't exist. copy failed.")-}
      ]
    where testdir = currDir `joinFileName` "tests"
          compStr = show comp
	  compVerStr = concat . intersperse "." . map show . versionBranch $ compVersion 
          compCmd = command comp
          compFlag = case compConf of
                      GHC -> "--ghc"
                      Hugs -> "--hugs"
          compIdent = compStr ++ "/" ++ compFlag
          testPrelude = system "make clean >> out.build" >> system "make >> out.build"
          assertConfigure pref
              = assertCmd' compCmd ("configure --user --prefix=" ++ pref ++ " " ++ compFlag)
                           "configure returned error code"
          assertBuild = assertCmd' compCmd "build" "build returned error code"
          assertCopy  = assertCmd' compCmd "copy"  "copy returned error code"
          assertClean  = assertCmd' compCmd "clean"  "clean returned error code"
          assertHaddock = assertCmd' compCmd "haddock" "setup haddock returned error code."
          command GHC = "./setup"
          command Hugs = "runhugs -98 Setup.lhs"
          libForA pref  -- checks to see if the lib exists, for tests/A
              = let ghcTargetDir = pref ++ "/lib/test-1.0/ghc-" ++ compVerStr ++ "/" in
                 case compConf of
                  Hugs -> checkTargetDir (pref ++ "/lib/hugs/packages/test/") [".hs", ".lhs"]
                  GHC  -> do checkTargetDir ghcTargetDir [".hi"]
                             doesFileExist (ghcTargetDir `joinFileName` "libHStest-1.0.a")
                               >>= assertBool "library doesn't exist"
          dumpScriptFlag = "--gen-script"
          registerAndExecute comp comment = do
            assertCmd' compCmd ("register --user "++dumpScriptFlag) comment
            if comp == GHC
               then assertCmd' "./register.sh" "" "reg script failed" 
               else do ex <- doesFileExist "register.sh"
                       assertBool "hugs should not produce register.sh" (not ex) 
          unregisterAndExecute comp comment = do
            assertCmd' compCmd ("unregister --user "++dumpScriptFlag) comment
            if comp == GHC
               then assertCmd' "./unregister.sh" "" "reg script failed"
               else do ex <- doesFileExist "unregister.sh" 
                       assertBool "hugs should not produce unregister.sh" (not ex)

main :: IO ()
main = do putStrLn "compile successful"
          putStrLn "-= Setup Tests =-"
          setupCount <- runTestTT' $ TestList $
                        (TestLabel "Utils Tests" $ TestList D.S.U.hunitTests):
                        (TestLabel "Setup Tests" $ TestList D.Setup.hunitTests):
                        (TestLabel "config Tests" $ TestList D.S.C.hunitTests):
                          (D.S.R.hunitTests ++ D.V.hunitTests ++
                           D.S.S.hunitTests ++ D.S.B.hunitTests ++
                           D.S.I.hunitTests ++ D.S.simpleHunitTests ++
                           D.PD.hunitTests ++ D.C.hunitTests)
          dir <- getCurrentDirectory
--          count' <- runTestTT' $ TestList (tests dir Hugs GHC)
          args <- getArgs
	  let testList :: CompilerFlavor -> Version -> [Test]
	      testList compiler version
	        | null args = tests dir compiler compiler version
                | otherwise =
		    case reads (head args) of
                      [(n,_)] -> [ tests dir compiler compiler version !! n ]
                      _ -> error "usage: moduleTest [test_num]"
	      compilers = [GHC] --, Hugs]
          globalTests <-
	    flip mapM compilers $ \compilerFlavour -> do
              compiler <- configCompiler (Just compilerFlavour) Nothing Nothing 0
              let version = compilerVersion compiler
              runTestTT' $ TestList (testList compilerFlavour version)
          putStrLn "-------------"
          putStrLn "Test Summary:"
          putStrLn $ showCounts $
                      foldl1 combineCounts (setupCount:globalTests)
          return ()

#endif
-- Local Variables:
-- compile-command: "ghc -i../:/usr/local/src/HUnit-1.0 -Wall --make ModuleTest.hs -o moduleTest"
-- End:

