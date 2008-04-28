-----------------------------------------------------------------------------
-- |
-- Module      :  PackageDescriptionTests
-- Copyright   :  Isaac Jones 2003-2005
-- 
-- Maintainer  :  Isaac Jones <ijones@syntaxpolice.org>
-- Stability   :  alpha
-- Portability :  portable
--
-- Package description and parsing.

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

module UnitTest.Distribution.PackageDescription (
        -- * Debugging
        hunitTests,

  ) where


import Distribution.ParseUtils
import Distribution.Package   (PackageIdentifier(..), Dependency(..))
import Distribution.Version   (Version(..), VersionRange(..))
import Distribution.Compiler  (CompilerFlavor(..), CompilerId(..))
import Distribution.System    (OS(..), buildOS, Arch(..), buildArch)

import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration
import Distribution.PackageDescription.Parse
import Distribution.PackageDescription.Check
import qualified Distribution.Simple.PackageIndex as PackageIndex

import Data.Maybe (catMaybes)
import Data.List  (sortBy)
import Control.Monad (liftM)
import Test.HUnit (Test(..), assertBool, Assertion, assertEqual)
import Distribution.License
import Language.Haskell.Extension


-- ------------------------------------------------------------
-- * Testing
-- ------------------------------------------------------------

compatTestPkgDesc :: String
compatTestPkgDesc = unlines [
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

compatTestPkgDescAnswer :: PackageDescription
compatTestPkgDescAnswer = 
    emptyPackageDescription 
    { package = PackageIdentifier 
                { pkgName = "Cabal",
                  pkgVersion = Version {versionBranch = [0,1,1,1,1],
                                        versionTags = ["rain"]}},
      license = LGPL,
      licenseFile = "foo",
      copyright = "Free Text String",
      author  = "Happy Haskell Hacker",
      homepage = "http://www.haskell.org/foo",
      pkgUrl   = "http://www.haskell.org/foo",
      synopsis = "a nice package!",
      description = "a really nice package!",
      category = "tools",
      descCabalVersion = LaterVersion (Version [1,1,1] []),
      buildType = Just Custom,
      buildDepends = [Dependency "haskell-src" AnyVersion,
                      Dependency "HUnit"
                        (UnionVersionRanges 
                         (ThisVersion (Version [1,0,0] ["rain"]))
                         (LaterVersion (Version [1,0,0] ["rain"])))],
      testedWith = [(GHC, AnyVersion)],
      maintainer = "",
      stability = "Free Text String",
      extraTmpFiles = ["file1", "file2"],
      extraSrcFiles = ["file1", "file2"],
      dataFiles = [],

      library = Just $ Library {
          exposedModules = ["Distribution.Void", "Foo.Bar"],
          libBuildInfo = emptyBuildInfo {
              buildable = True,
              ccOptions = ["-g", "-o"],
              ldOptions = ["-BStatic", "-dn"],
              frameworks = ["foo"],
              cSources = ["not/even/rain.c", "such/small/hands"],
              hsSourceDirs = ["src", "src2"],
              otherModules = ["Distribution.Package",
                              "Distribution.Version",
                              "Distribution.Simple.GHCPackageConfig"],
              extensions = [OverlappingInstances, TypeSynonymInstances],
              extraLibs = ["libfoo", "bar", "bang"],
              extraLibDirs = ["/usr/local/libs"],
              includeDirs = ["your/slightest", "look/will"],
              includes = ["/easily/unclose", "/me", "funky, path\\name"],
              installIncludes = ["/easily/unclose", "/me", "funky, path\\name"],
              ghcProfOptions = [],
              options = [(GHC,["-fTH","-fglasgow-exts"])
                        ,(Hugs,["+TH"]),(NHC,[]),(JHC,[])]
         }},

      executables = [Executable "somescript" 
                     "SomeFile.hs" (emptyBuildInfo {
                         otherModules=["Foo1","Util","Main"],
                         hsSourceDirs = ["scripts"],
                         extensions = [OverlappingInstances],
                         options = [(GHC,[]),(Hugs,[]),(NHC,[]),(JHC,[])]
                      })]
  }

-- Parse an old style package description.  Assumes no flags etc. being used.
compatParseDescription :: String -> ParseResult PackageDescription
compatParseDescription descr = do
    gpd <- parsePackageDescription descr
    case finalizePackageDescription [] (Nothing :: Maybe (PackageIndex.PackageIndex PackageIdentifier))
           buildOS buildArch (CompilerId GHC (Version [] [])) [] gpd of
      Left _ -> syntaxError (-1) "finalize failed"
      Right (pd,_) -> return pd

hunitTests :: [Test]
hunitTests = 
    [ TestLabel "license parsers" $ TestCase $
      sequence_ [ assertParseOk ("license " ++ show lVal) lVal
                    (runP 1 "license" parseLicenseQ (show lVal))
                | lVal <- [GPL,LGPL,BSD3,BSD4] ]

    , TestLabel "Required fields" $ TestCase $
      do assertParseOk "some fields"
           emptyPackageDescription {
             package = (PackageIdentifier "foo"
                        (Version [0,0] ["asdf"])) }
           (compatParseDescription "Name: foo\nVersion: 0.0-asdf")

         assertParseOk "more fields foo"
           emptyPackageDescription {
             package = (PackageIdentifier "foo"
                        (Version [0,0] ["asdf"])),
             license = GPL }
           (compatParseDescription "Name: foo\nVersion:0.0-asdf\nLicense: GPL")

         assertParseOk "required fields for foo"
           emptyPackageDescription { 
             package = (PackageIdentifier "foo"
                        (Version [0,0] ["asdf"])),
             license = GPL, copyright="2004 isaac jones" }
           (compatParseDescription $ "Name: foo\nVersion:0.0-asdf\n" 
               ++ "Copyright: 2004 isaac jones\nLicense: GPL")
                                          
    , TestCase $ assertParseOk "no library" Nothing
        (library `liftM` (compatParseDescription $ 
           "Name: foo\nVersion: 1\nLicense: GPL\n" ++
           "Maintainer: someone\n\nExecutable: script\n" ++ 
           "Main-is: SomeFile.hs\n"))

    , TestCase $ assertParseOk "translate deprecated fields"
        emptyPackageDescription {
             extraSrcFiles = ["foo.c", "bar.ml"],
             library = Just $ emptyLibrary {
               libBuildInfo = emptyBuildInfo { hsSourceDirs = ["foo","bar"] }}}
        (compatParseDescription $ 
           "hs-source-dir: foo bar\nother-files: foo.c bar.ml")

    , TestLabel "Package description" $ TestCase $ 
        assertParseOk "entire package description" 
                      compatTestPkgDescAnswer
                      (compatParseDescription compatTestPkgDesc)
    , TestLabel "Package description pretty" $ TestCase $ 
      case compatParseDescription compatTestPkgDesc of
        ParseFailed _ -> assertBool "can't parse description" False
        ParseOk _ d -> 
            case compatParseDescription $ showPackageDescription d of
              ParseFailed _ ->
                assertBool "can't parse description after pretty print!" False
              ParseOk _ d' -> 
                assertBool ("parse . show . parse not identity."
                            ++"   Incorrect fields:\n"
                            ++ (unlines $ comparePackageDescriptions d d'))
                (d == d')
    , TestLabel "Sanity checker" $ TestCase $ do
        let checks = checkConfiguredPackage emptyPackageDescription
            ers   = [ s | PackageBuildImpossible s <- checks ]
            warns = [ s | PackageBuildWarning    s <- checks ]
        assertEqual "Wrong number of errors"   2 (length ers)
        assertEqual "Wrong number of warnings" 3 (length warns)
    ]

-- |Compare two package descriptions and see which fields aren't the same.
comparePackageDescriptions :: PackageDescription
                           -> PackageDescription
                           -> [String]      -- ^Errors
comparePackageDescriptions p1 p2
    = catMaybes $ myCmp package          "package" 
                : myCmp license          "license"
                : myCmp licenseFile      "licenseFile"
                : myCmp copyright        "copyright"
                : myCmp maintainer       "maintainer"
                : myCmp author           "author"
                : myCmp stability        "stability"
                : myCmp testedWith       "testedWith"
                : myCmp homepage         "homepage"
                : myCmp pkgUrl           "pkgUrl"
                : myCmp synopsis         "synopsis"
                : myCmp description      "description"
                : myCmp category         "category"
                : myCmp buildDepends     "buildDepends"
                : myCmp library          "library"
                : myCmp executables      "executables"
                : myCmp descCabalVersion "cabal-version" 
                : myCmp buildType        "build-type" : []
      where canon_p1 = canonOptions p1
            canon_p2 = canonOptions p2
        
            myCmp :: (Eq a, Show a) => (PackageDescription -> a)
                  -> String       -- Error message
                  -> Maybe String -- 
            myCmp f er = let e1 = f canon_p1
                             e2 = f canon_p2
                          in if e1 /= e2
                               then Just $ er ++ " Expected: " ++ show e1
                                              ++ " Got: " ++ show e2
                               else Nothing

canonOptions :: PackageDescription -> PackageDescription
canonOptions pd =
   pd{ library = fmap canonLib (library pd),
       executables = map canonExe (executables pd) }
  where
        canonLib l = l { libBuildInfo = canonBI (libBuildInfo l) }
        canonExe e = e { buildInfo = canonBI (buildInfo e) }

        canonBI bi = bi { options = canonOptions (options bi) }

        canonOptions opts = sortBy (comparing fst) opts

        comparing f a b = f a `compare` f b

-- |Assert that the 2nd value parses correctly and matches the first value
assertParseOk :: (Eq val) => String -> val -> ParseResult val -> Assertion
assertParseOk mes expected actual
    =  assertBool mes
           (case actual of
             ParseOk _ v -> v == expected
             _         -> False)

------------------------------------------------------------------------------

test_stanzas' = parsePackageDescription testFile
--                    ParseOk _ x -> putStrLn $ show x
--                    _ -> return ()

testFile = unlines $
          [ "Name: dwim"
          , "Cabal-version: >= 1.7"
          , ""
          , "Description: This is a test file   "
          , "  with a description longer than two lines.  "
          , ""
          , "flag Debug {"
          , "  Description: Enable debug information"
          , "  Default: False" 
          , "}"
          , "flag build_wibble {"
          , "}"
          , ""
          , "library {"
          , "  build-depends: blub"
          , "  exposed-modules: DWIM.Main, DWIM"
          , "  if os(win32) && flag(debug) {"
          , "    build-depends: hunit"
          , "    ghc-options: -DDEBUG"
          , "    exposed-modules: DWIM.Internal"
          , "    if !flag(debug) {"
          , "      build-depends: impossible"
          , "    }"
          , "  }"
          , "}" 
          , ""
          , "executable foo-bar {"
          , "  Main-is: Foo.hs"
          , "  Build-depends: blab"
          , "}"
          , "executable wobble {"
          , "  Main-is: Wobble.hs"
          , "  if flag(debug) {"
          , "    Build-depends: hunit"
          , "  }"
          , "}"
          , "executable wibble {"
          , "  Main-is: Wibble.hs"
          , "  hs-source-dirs: wib-stuff"
          , "  if flag(build_wibble) {"
          , "    Build-depends: wiblib >= 0.42"
          , "  } else {"
          , "    buildable: False"
          , "  }"
          , "}"
          ]

{-
test_compatParsing = 
    let ParseOk ws (p, pold) = do 
          fs <- readFields testPkgDesc 
          ppd <- parsePackageDescription' fs
          let Right (pd,_) = finalizePackageDescription [] (Just pkgs) os arch ppd
          pdold <- parsePackageDescription testPkgDesc
          return (pd, pdold)
    in do putStrLn $ unlines $ map show ws
          putStrLn "==========="
          putStrLn $ showPackageDescription p
          putStrLn "==========="
          putStrLn $ showPackageDescription testPkgDescAnswer
          putStrLn "==========="
          putStrLn $ showPackageDescription pold
          putStrLn $ show (p == pold)
  where
    pkgs = [ PackageIdentifier "haskell-src" (Version [1,0] []) 
           , PackageIdentifier "HUnit" (Version [1,1] ["rain"]) 
           ]
    os = (MkOSName "win32")
    arch = (MkArchName "amd64")
-}
test_finalizePD =
    case parsePackageDescription testFile of
      ParseFailed err -> print err
      ParseOk _ ppd -> do
       case finalizePackageDescription [(FlagName "debug",True)] (Just pkgs) os arch impl [] ppd of
         Right (pd,fs) -> do putStrLn $ showPackageDescription pd
                             print fs
         Left missing -> putStrLn $ "missing: " ++ show missing
       putStrLn $ showPackageDescription $ 
                flattenPackageDescription ppd
  where
    pkgs = PackageIndex.fromList[ PackageIdentifier "blub" (Version [1,0] []) 
           --, PackageIdentifier "hunit" (Version [1,1] []) 
           , PackageIdentifier "blab" (Version [0,1] []) 
           ]
    os = Windows
    arch = X86_64
    impl = CompilerId GHC (Version [6,6] [])
