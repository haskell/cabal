-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.ParseUtils
-- Copyright   :  (c) The University of Glasgow 2004
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  alpha
-- Portability :  portable
--
-- Utilities for parsing PackageDescription and InstalledPackageInfo.


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

    * Neither the name of the University nor the names of other
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

module UnitTest.Distribution.ParseUtils where

import Distribution.ParseUtils
import Distribution.Compiler (CompilerFlavor, parseCompilerFlavorCompat)
import Distribution.License (License)
import Distribution.Version
import Distribution.Package	( parsePackageName )
import Distribution.Compat.ReadP as ReadP hiding (get)
import Distribution.Simple.Utils (intercalate)
import Language.Haskell.Extension (Extension)

import Text.PrettyPrint hiding (braces)
import Data.Char (isSpace, isUpper, toLower, isAlphaNum, isSymbol, isDigit)
import Data.Maybe	(fromMaybe)
import Data.Tree as Tree (Tree(..), flatten)

import Test.HUnit (Test(..), assertBool, Assertion, runTestTT, Counts, assertEqual)
import IO
import System.Environment ( getArgs )
import Control.Monad ( zipWithM_ )

------------------------------------------------------------------------------
-- TESTING

test_readFields = case 
                    readFields testFile 
                  of
                    ParseOk _ x -> x == expectedResult
                    _ -> False
  where 
    testFile = unlines $
          [ "Cabal-version: 3"
          , ""
          , "Description: This is a test file   "
          , "  with a description longer than two lines.  "
          , "if os(windows) {"
          , "  License:  You may not use this software"
          , "    ."
          , "    If you do use this software you will be seeked and destroyed."
          , "}"
          , "if os(linux) {"
          , "  Main-is:  foo1  "
          , "}"
          , ""
          , "if os(vista) {"
          , "  executable RootKit {"
          , "    Main-is: DRMManager.hs"
          , "  }"
          , "} else {"
          , "  executable VistaRemoteAccess {"
          , "    Main-is: VCtrl"
          , "}}"
          , ""
          , "executable Foo-bar {"
          , "  Main-is: Foo.hs"
          , "}"
          ]
    expectedResult = 
          [ F 1 "cabal-version" "3"
          , F 3 "description" 
                  "This is a test file\nwith a description longer than two lines."
          , IfBlock 5 "os(windows) " 
              [ F 6 "license" 
                      "You may not use this software\n\nIf you do use this software you will be seeked and destroyed."
              ]
              []
          , IfBlock 10 "os(linux) " 
              [ F 11 "main-is" "foo1" ] 
              [ ]
          , IfBlock 14 "os(vista) " 
              [ Section 15 "executable" "RootKit " 
                [ F 16 "main-is" "DRMManager.hs"]
              ] 
              [ Section 19 "executable" "VistaRemoteAccess "
                 [F 20 "main-is" "VCtrl"]
              ]
          , Section 23 "executable" "Foo-bar " 
              [F 24 "main-is" "Foo.hs"]
          ]

test_readFieldsCompat' = case test_readFieldsCompat of
                           ParseOk _ fs -> mapM_ (putStrLn . show) fs
                           x -> putStrLn $ "Failed: " ++ show x
test_readFieldsCompat = readFields testPkgDesc
  where 
    testPkgDesc = unlines [
        "-- Required",
        "Name: Cabal",
        "Version: 0.1.1.1.1-rain",
        "License: LGPL",
        "License-File: foo",
        "Copyright: Free Text String",
        "Cabal-version: >1.1.1",
        "-- Optional - may be in source?",
        "Author: Happy Haskell Hacker",
        "Homepage: http://www.haskell.org/foo",
        "Package-url: http://www.haskell.org/foo",
        "Synopsis: a nice package!",
        "Description: a really nice package!",
        "Category: tools",
        "buildable: True",
        "CC-OPTIONS: -g -o",
        "LD-OPTIONS: -BStatic -dn",
        "Frameworks: foo",
        "Tested-with: GHC",
        "Stability: Free Text String",
        "Build-Depends: haskell-src, HUnit>=1.0.0-rain",
        "Other-Modules: Distribution.Package, Distribution.Version,",
        "                Distribution.Simple.GHCPackageConfig",
        "Other-files: file1, file2",
        "Extra-Tmp-Files:    file1, file2",
        "C-Sources: not/even/rain.c, such/small/hands",
        "HS-Source-Dirs: src, src2",
        "Exposed-Modules: Distribution.Void, Foo.Bar",
        "Extensions: OverlappingInstances, TypeSynonymInstances",
        "Extra-Libraries: libfoo, bar, bang",
        "Extra-Lib-Dirs: \"/usr/local/libs\"",
        "Include-Dirs: your/slightest, look/will",
        "Includes: /easily/unclose, /me, \"funky, path\\\\name\"",
        "Install-Includes: /easily/unclose, /me, \"funky, path\\\\name\"",
        "GHC-Options: -fTH -fglasgow-exts",
        "Hugs-Options: +TH",
        "Nhc-Options: ",
        "Jhc-Options: ",
        "",
        "-- Next is an executable",
        "Executable: somescript",
        "Main-is: SomeFile.hs",
        "Other-Modules: Foo1, Util, Main",
        "HS-Source-Dir: scripts",
        "Extensions: OverlappingInstances",
        "GHC-Options: ",
        "Hugs-Options: ",
        "Nhc-Options: ",
        "Jhc-Options: "
        ]
{-
test' = do h <- openFile "../Cabal.cabal" ReadMode
           s <- hGetContents h
           let r = readFields s
           case r of
             ParseOk _ fs -> mapM_ (putStrLn . show) fs
             x -> putStrLn $ "Failed: " ++ show x
           putStrLn "==================="
           mapM_ (putStrLn . show) $
                 merge . zip [1..] . lines $ s
           hClose h
-}

-- ghc -DDEBUG --make Distribution/ParseUtils.hs -o test

main :: IO ()
main = do
  inputFiles <- getArgs
  ok <- mapM checkResult inputFiles

  zipWithM_ summary inputFiles ok
  putStrLn $ show (length (filter not ok)) ++ " out of " ++ show (length ok) ++ " failed"

  where summary f True  = return ()
        summary f False = putStrLn $ f  ++ " failed :-("

checkResult :: FilePath -> IO Bool
checkResult inputFile = do
  file <- readTextFile inputFile
  case readFields file of
    ParseOk _ result -> do
       hPutStrLn stderr $ inputFile ++ " parses ok :-)"
       return True
    ParseFailed err -> do
       hPutStrLn stderr $ inputFile ++ " parse failed:"
       hPutStrLn stderr $ show err
       return False
