-----------------------------------------------------------------------------

-- |
-- Module      :  UnitTest
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
-- Import everything, since we want to test the compilation of them:

import qualified UnitTest.Distribution.Version as D.V (hunitTests)
import qualified UnitTest.Distribution.PackageDescription as D.PD (hunitTests)

import Distribution.Simple.Compiler (CompilerFlavor(..), compilerVersion)
import Distribution.Simple.Program (defaultProgramConfiguration)
import Distribution.Version (Version(..))

import System.FilePath( (</>) )

import Distribution.Simple.Configure (configCompiler)
import Distribution.Verbosity ( silent )
-- base
import Data.List (intersperse)
import Control.Monad (when)
import System.Directory (setCurrentDirectory, doesFileExist,
                         doesDirectoryExist, getCurrentDirectory,
                         getPermissions, Permissions(..),
                         removeDirectoryRecursive)
import System.Cmd (system)
import System.Exit(ExitCode(..))
import System.Environment (getArgs)

import Test.HUnit(runTestTT, Test(..), Counts(..), assertBool,
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
         do let targetDir = ",tmp"
            setCurrentDirectory (testdir </> "exeWithC")
            testPrelude
            assertConfigure targetDir
            assertClean
            assertConfigure targetDir
            assertBuild
            assertCopy
            assertCmd (targetDir </> "bin/tt" ++ " > "
                    ++ targetDir </> "out")
                      "exeWithC failed"
-- A
        ,TestLabel ("package A: " ++ compIdent) $ TestCase $
         do let targetDir = ",tmp"
            setCurrentDirectory (testdir </> "A")
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
            assertCmd' compCmd "sdist -v0" "setup sdist returned error code"
            doesFileExist "dist/test-1.0.tar.gz" >>=
              assertBool "sdist did not put the expected file in place"
            doesFileExist "dist/src" >>=
              assertEqual "dist/src exists" False
            assertCmd' compCmd "register -v0 --user" "pkg A, register failed"
            assertCmd' compCmd "unregister -v0 --user" "pkg A, unregister failed"
            -- tricky, script-based register
            registerAndExecute "pkg A: register with script failed"
            unregisterAndExecute "pkg A: unregister with script failed"
            -- non-trick non-script based register
            assertCmd' compCmd "register -v0 --user" "regular register returned error"
            assertCmd' compCmd "unregister -v0 --user" "regular unregister returned error"

        ,TestLabel ("package A copy-prefix: " ++ compIdent) $ TestCase $ -- (uses above config)
         do let targetDir = ",tmp2"
            assertCmd' compCmd ("copy --copy-prefix=" ++ targetDir) "copy --copy-prefix failed"
            doesFileExist ",tmp2/bin/testA" >>=
              assertBool "testA not produced"
            doesFileExist ",tmp2/bin/testB" >>=
              assertBool "testB not produced"
            libForA ",tmp2"
        ,TestLabel ("package A and install w/ no prefix: " ++ compIdent) $ TestCase $
         do let targetDir = ",tmp"
            removeDirectoryRecursive targetDir
            when (comp == GHC) -- FIX: hugs can't do --user yet
              (do assertCmd "make -s unregister-test" "unregister test"
                  assertCmd' compCmd "install -v0 --user" "install --user failed"
                  libForA targetDir
                  assertCmd' compCmd "unregister -v0 --user" "unregister failed")
-- HUnit
        ,TestLabel ("testing the HUnit package" ++ compIdent) $ TestCase $
         do setCurrentDirectory $ (testdir </> "HUnit-1.0")
            system "make -s clean"
            system "make -s"
            assertCmd' compCmd "configure -v0" "configure failed"
            assertCmd' compCmd "unregister -v0 --user" "unregister failed"

            system $ "touch dist/setup-config"
            system $ "touch dist/installed-pkg-config"
            doesFileExist "dist/setup-config" >>=
              assertBool ("touch dist/setup-config failed")

            -- Test clean:
            assertBuild
            doesDirectoryExist "dist/build" >>=
              assertBool "HUnit build did not create build directory"
            assertCmd' compCmd "clean -v0" "hunit clean"
            doesDirectoryExist "dist/build" >>=
              assertEqual "HUnit clean did not get rid of build directory" False

            doesFileExist "dist/setup-config" >>=
              assertEqual ("clean dist/setup-config failed") False
            doesFileExist "dist/installed-pkg-config" >>=
              assertEqual ("clean dist/installed-pkg-config failed") False

            assertConfigure ",tmp"
            assertHaddock
            doesDirectoryExist "dist/doc" >>= assertEqual "create of dist/doc" True
            assertBuild
            when (comp == GHC) -- tests building w/ an installed -package
                 (do assertCmd' compCmd "install -v0 --user" "hunit install"
                     assertCmd ("ghc -package HUnitTest HUnitTester.hs -o ./hunitTest")
                                "compile w/ hunit"
                     assertCmd "./hunitTest" "hunit test"
                     assertCmd' compCmd "unregister --user" "unregister failed")
            assertClean
            doesDirectoryExist "dist/doc" >>= assertEqual "clean dist/doc" False
            assertCmd "make -s clean" "make clean failed"

-- twoMains
        ,TestLabel ("package twoMains: building " ++ compIdent) $ TestCase $
         do setCurrentDirectory (testdir </> "twoMains")
            testPrelude
            assertConfigure ",tmp"
            assertCmd' compCmd "haddock" "setup haddock returned error code."
            assertBuild
            assertCopy
            doesFileExist ",tmp/bin/testA" >>=
              assertBool "install did not create the executable: testA"
            doesFileExist ",tmp/bin/testB" >>=
              assertBool "install did not create the executable: testB"
            assertCmd "./,tmp/bin/testA isA >  out" "A is not A"
            assertCmd "./,tmp/bin/testB isB >> out" "B is not B"
            -- no register, since there's no library
-- buildinfo
        ,TestLabel ("buildinfo with multiple executables " ++ compIdent) $ TestCase $
         do setCurrentDirectory (testdir </> "buildInfo")
            testPrelude
            assertConfigure ",tmp"
            assertHaddock
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
            setCurrentDirectory (testdir </> "recursive")
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
         do setCurrentDirectory (testdir </> "ffi-package")
            testPrelude
            assertConfigure "/tmp"
            assertBuild
            -- install it so we can test building with it.
            assertCmd' compCmd "install -v0 --user" "ffi-package install"
            assertClean
            doesFileExist "src/TestFFI_stub.c" >>=
                assertEqual "FFI-generated stub not cleaned." False
            -- now build something that depends on it
            setCurrentDirectory (".." </> "ffi-bin")
            testPrelude
            assertConfigure ",tmp"
            assertBuild
            assertCopy
-- depOnLib
        ,TestLabel ("package depOnLib: (executable depending on its lib)" ++ compIdent) $ TestCase $
         do setCurrentDirectory (testdir </> "depOnLib")
            testPrelude
            assertConfigure ",tmp"
            assertHaddock
            assertBuild
            assertCopy
            registerAndExecute "pkg depOnLib: register with script failed"
            unregisterAndExecute "pkg DepOnLib: unregister with script failed"
            when (comp == GHC) (do
                                doesFileExist "dist/build/mainForA/mainForA" >>=
                                  assertBool "build did not create the executable: mainForA"
                                doesFileExist ("dist/build/" </> "libHStest-1.0.a")
                                  >>= assertBool "library doesn't exist"
                                doesFileExist (",tmp/bin/mainForA")
                                  >>= assertBool "installed bin doesn't exist"
                                doesFileExist (",tmp/lib/test-1.0/ghc-" ++ compVerStr ++ "/libHStest-1.0.a")
                                  >>= assertBool "installed lib doesn't exist")
-- wash2hs
        ,TestLabel ("testing the wash2hs package" ++ compIdent) $ TestCase $
         do setCurrentDirectory (testdir </> "wash2hs")
            testPrelude
            assertCmdFail (compCmd ++ " configure -v0 --someUnknownFlag 2> err")
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
        ,TestLabel ("package withHooks: " ++ compIdent) $ TestCase $
         do setCurrentDirectory (testdir </> "withHooks")
            testPrelude
            assertCmd' compCmd ("configure -v0 --prefix=,tmp --woohoo " ++ compFlag)
              "configure returned error code"
            assertCmdFail (compCmd ++ " test -v0 --asdf > out") "test was supposed to fail"
            assertCmd' compCmd ("test -v0 --pass >> out") "test should not have failed"

            assertHaddock
            assertBuild
            assertCmd' compCmd "copy -v0 --copy-prefix=,tmp" "copy w/ prefix"
            doesFileExist ",tmp/withHooks" >>=  -- this file is added w/ the hook.
              assertBool "hooked copy, redirecting prefix didn't work."
            assertCmd' compCmd "register -v0 --user" "regular register returned error"
            assertCmd' compCmd "unregister -v0 --user" "regular unregister returned error"
            when (comp == GHC) -- FIX: come up with good test for Hugs
                 (do doesFileExist "dist/build/C.o" >>=
                       assertBool "C.testSuffix did not get compiled to C.o."
                     doesFileExist "dist/build/D.o" >>=
                       assertBool "D.gc did not get compiled to D.o this is an overriding test"
                     doesFileExist (",tmp/lib/withHooks-1.0/ghc-" ++ compVerStr
                                 ++ "/" </> "libHSwithHooks-1.0.a")
                       >>= assertBool "library doesn't exist")

            doesFileExist ",tmp/bin/withHooks" >>=
              assertBool "copy did not create the executable: withHooks"
            assertClean
            doesFileExist "C.hs" >>=
               assertEqual "C.hs (a generated file) not cleaned." False
-- HSQL
{-         ,TestLabel ("package HSQL (make-based): " ++ show compIdent) $
         TestCase $ unless (compFlag == "--hugs") $ -- FIX: won't compile w/ hugs
         do setCurrentDirectory $ (testdir </> "HSQL")
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
    where testdir = currDir </> "systemTests"
          compStr = show comp
          compVerStr = concat . intersperse "." . map show . versionBranch $ compVersion
          compCmd = command comp
          compFlag = case compConf of
                      GHC -> "--ghc"
                      Hugs -> "--hugs"
                      _ -> error ("Unhandled compiler: " ++ show compConf)
          compIdent = compStr ++ "/" ++ compFlag
          testPrelude = system "make clean >> out.build" >> system "make >> out.build"
          assertConfigure pref
              = assertCmd' compCmd ("configure -v0 --user --prefix=" ++ pref ++ " " ++ compFlag)
                           "configure returned error code"
          -- XXX redirecting stderr is a hack. ar says
          -- /usr/bin/ar: creating dist/build/libHStest-1.0.a
          -- in the A test
          assertBuild = assertCmd' compCmd "build -v0 2> err" "build returned error code"
          assertCopy  = assertCmd' compCmd "copy -v0"  "copy returned error code"
          assertClean  = assertCmd' compCmd "clean -v0"  "clean returned error code"
          -- XXX Redirecting stderr is a hack - haddock needs to allow
          -- us to tell it to be quiet
          assertHaddock = assertCmd' compCmd "haddock -v0 2> err" "setup haddock returned error code."
          command GHC = "./setup"
          command Hugs = "runhugs -98 Setup.lhs"
          command c = error ("Unhandled compiler: " ++ show c)
          libForA pref  -- checks to see if the lib exists, for tests/A
              = let ghcTargetDir = pref ++ "/lib/test-1.0/ghc-" ++ compVerStr ++ "/" in
                 case compConf of
                  Hugs -> checkTargetDir (pref ++ "/lib/hugs/packages/test/") [".hs", ".lhs"]
                  GHC  -> do checkTargetDir ghcTargetDir [".hi"]
                             doesFileExist (ghcTargetDir </> "libHStest-1.0.a")
                               >>= assertBool "library doesn't exist"
                  _ -> error ("Unhandled compiler: " ++ show compConf)
          dumpScriptFlag = "--gen-script"
          registerAndExecute comment = do
            assertCmd' compCmd ("register -v0 --user "++dumpScriptFlag) comment
            if comp == GHC
               then assertCmd' "./register.sh" "" "reg script failed"
               else do ex <- doesFileExist "register.sh"
                       assertBool "hugs should not produce register.sh" (not ex)
          unregisterAndExecute comment = do
            assertCmd' compCmd ("unregister -v0 --user "++dumpScriptFlag) comment
            if comp == GHC
               then assertCmd' "./unregister.sh" "" "reg script failed"
               else do ex <- doesFileExist "unregister.sh"
                       assertBool "hugs should not produce unregister.sh" (not ex)

main :: IO ()
main = do putStrLn "compile successful"
          putStrLn "-= Setup Tests =-"
          setupCount <- runTestTT' $ TestList (D.V.hunitTests ++ D.PD.hunitTests)
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
              (compiler, _) <- configCompiler (Just compilerFlavour)
	                         Nothing Nothing
	                         defaultProgramConfiguration silent
              let version = compilerVersion compiler
              runTestTT' $ TestList (testList compilerFlavour version)
          putStrLn "-------------"
          putStrLn "Test Summary:"
          putStrLn $ showCounts $
                      foldl1 combineCounts (setupCount:globalTests)
          return ()

-- Local Variables:
-- compile-command: "ghc -i../:/usr/local/src/HUnit-1.0 -Wall --make ModuleTest.hs -o moduleTest"
-- End:
