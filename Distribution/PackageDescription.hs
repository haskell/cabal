{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -cpp #-}
{-# OPTIONS_NHC98 -cpp #-}
{-# OPTIONS_JHC -fcpp #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.PackageDescription
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

module Distribution.PackageDescription (
        -- * Package descriptions
        PackageDescription(..),
        GenericPackageDescription(..),
        finalizePackageDescription,
        flattenPackageDescription,
        emptyPackageDescription,
        readPackageDescription,
        writePackageDescription,
        parsePackageDescription,
        showPackageDescription,
        BuildType(..),

        -- ** Libraries
        Library(..),
        withLib,
        hasLibs,
        libModules,

        -- ** Executables
        Executable(..),
        withExe,
        hasExes,
        exeModules,

        -- ** Parsing
        FieldDescr(..),
        LineNo,

        -- ** Sanity checking
        sanityCheckPackage,

        -- * Build information
        BuildInfo(..),
        emptyBuildInfo,
        allBuildInfo,
        unionBuildInfo,

        -- ** Supplementary build information
        HookedBuildInfo,
        emptyHookedBuildInfo,
        readHookedBuildInfo,
        parseHookedBuildInfo,
        writeHookedBuildInfo,
        showHookedBuildInfo,        
        updatePackageDescription,

        -- * Utilities
        satisfyDependency,
        ParseResult(..),
        hcOptions,
        autogenModuleName,
        haddockName,
        setupMessage,
        cabalVersion,

#ifdef DEBUG
	-- * Debugging
        hunitTests,
        test
#endif
  ) where

import Data.Maybe  (isJust, maybeToList)
import Data.List   (nub, maximumBy)
import Data.Monoid (Monoid(..))
import Text.PrettyPrint.HughesPJ
import System.FilePath((<.>))

import Distribution.ParseUtils
import Distribution.Package   (PackageIdentifier(..), showPackageId)
import Distribution.Version   (Version(..), Dependency(..), withinRange)
import Distribution.Verbosity (Verbosity)
import Distribution.Compiler  (CompilerFlavor(..))
import Distribution.Simple.Utils (currentDir, notice)

import Distribution.PackageDescription.Types
import Distribution.PackageDescription.Parse
import Distribution.PackageDescription.QA (cabalVersion, sanityCheckPackage)
import Distribution.Configuration

#ifdef DEBUG
import Data.Maybe (catMaybes)
import Data.List  (sortBy)
import Control.Monad (liftM)
import Test.HUnit (Test(..), assertBool, Assertion, runTestTT, Counts, assertEqual)
import Distribution.License
import Distribution.Version hiding (hunitTests)
import Language.Haskell.Extension
#endif

-- -----------------------------------------------------------------------------
-- The PackageDescription type

-- XXX: I think we really want a PPrint or Pretty or ShowPretty class.
instance Show GenericPackageDescription where
    show (GenericPackageDescription pkg flgs mlib exes) =
        showPackageDescription pkg ++ "\n" ++
        (render $ vcat $ map ppFlag flgs) ++ "\n" ++
        render (maybe empty (\l -> showStanza "Library" (ppCondTree l showDeps)) mlib)
        ++ "\n" ++
        (render $ vcat $ 
            map (\(n,ct) -> showStanza ("Executable " ++ n) (ppCondTree ct showDeps)) exes)
      where
        ppFlag (MkFlag name desc dflt) =
            showStanza ("Flag " ++ name)
              ((if (null desc) then empty else 
                   text ("Description: " ++ desc)) $+$
              text ("Default: " ++ show dflt))
        showDeps = fsep . punctuate comma . map showDependency
        showStanza h b = text h <+> lbrace $+$ nest 2 b $+$ rbrace

data PDTagged = Lib Library | Exe String Executable | PDNull

instance Monoid PDTagged where
    mempty = PDNull
    PDNull `mappend` x = x
    x `mappend` PDNull = x
    Lib l `mappend` Lib l' = Lib (l `mappend` l')
    Exe n e `mappend` Exe n' e' | n == n' = Exe n (e `mappend` e')
    _ `mappend` _ = bug "Cannot combine incompatible tags"

finalizePackageDescription 
  :: [(String,Bool)]  -- ^ Explicitly specified flag assignments
  -> Maybe [PackageIdentifier] -- ^ Available dependencies. Pass 'Nothing' if this
                               -- is unknown.
  -> String -- ^ OS-name
  -> String -- ^ Arch-name
  -> (String, Version) -- ^ Compiler + Version
  -> GenericPackageDescription
  -> Either [Dependency]
            (PackageDescription, [(String,Bool)])
	     -- ^ Either missing dependencies or the resolved package
	     -- description along with the flag assignments chosen.
finalizePackageDescription userflags mpkgs os arch impl 
        (GenericPackageDescription pkg flags mlib0 exes0) =
    case resolveFlags of 
      Right ((mlib, exes'), deps, flagVals) ->
        Right ( pkg { library = mlib                            
                    , executables = exes'
                    , buildDepends = nub deps
                    }
              , flagVals )
      Left missing -> Left $ nub missing
  where
    -- Combine lib and exes into one list of @CondTree@s with tagged data
    condTrees = maybeToList (fmap (mapTreeData Lib) mlib0 )
                ++ map (\(name,tree) -> mapTreeData (Exe name) tree) exes0

    untagRslts = foldr untag (Nothing, [])
      where
        untag (Lib _) (Just _, _) = bug "Only one library expected"
        untag (Lib l) (Nothing, exes) = (Just l, exes)
        untag (Exe n e) (mlib, exes)
         | any ((== n) . fst) exes = bug "Exe with same name found"
         | otherwise = (mlib, exes ++ [(n, e)])
        untag PDNull x = x  -- actually this should not happen, but let's be liberal

    resolveFlags =
        case resolveWithFlags flagChoices os arch impl condTrees check of
          Right (as, ds, fs) ->
              let (mlib, exes) = untagRslts as in
              Right ( (fmap libFillInDefaults mlib,
                       map (\(n,e) -> (exeFillInDefaults e) { exeName = n }) exes),
                     ds, fs)
          Left missing      -> Left missing

    flagChoices  = map (\(MkFlag n _ d) -> (n, d2c n d)) flags
    d2c n b      = maybe [b, not b] (\x -> [x]) $ lookup n userflags
    --flagDefaults = map (\(n,x:_) -> (n,x)) flagChoices 
    check ds     = if all satisfyDep ds
                   then DepOk
                   else MissingDeps $ filter (not . satisfyDep) ds
    -- if we don't know which packages are present, we just accept any
    -- dependency
    satisfyDep   = maybe (const True) 
                         (\pkgs -> isJust . satisfyDependency pkgs) 
                         mpkgs


-- | Flatten a generic package description by ignoring all conditions and just
-- join the field descriptors into on package description.  Note, however,
-- that this may lead to inconsistent field values, since all values are
-- joined into one field, which may not be possible in the original package
-- description, due to the use of exclusive choices (if ... else ...).
--
-- XXX: One particularly tricky case is defaulting.  In the original package
-- description, e.g., the source dirctory might either be the default or a
-- certain, explicitly set path.  Since defaults are filled in only after the
-- package has been resolved and when no explicit value has been set, the
-- default path will be missing from the package description returned by this
-- function.
flattenPackageDescription :: GenericPackageDescription -> PackageDescription
flattenPackageDescription (GenericPackageDescription pkg _ mlib0 exes0) =
    pkg { library = mlib
        , executables = reverse exes
        , buildDepends = nub $ ldeps ++ reverse edeps
        }
  where
    (mlib, ldeps) = case mlib0 of
        Just lib -> let (l,ds) = ignoreConditions lib in 
                    (Just (libFillInDefaults l), ds)
        Nothing -> (Nothing, [])
    (exes, edeps) = foldr flattenExe ([],[]) exes0
    flattenExe (n, t) (es, ds) = 
        let (e, ds') = ignoreConditions t in
        ( (exeFillInDefaults $ e { exeName = n }) : es, ds' ++ ds ) 

-- This is in fact rather a hack.  The original version just overrode the
-- default values, however, when adding conditions we had to switch to a
-- modifier-based approach.  There, nothing is ever overwritten, but only
-- joined together.
--
-- This is the cleanest way i could think of, that doesn't require
-- changing all field parsing functions to return modifiers instead.
libFillInDefaults :: Library -> Library
libFillInDefaults lib@(Library { libBuildInfo = bi }) = 
    lib { libBuildInfo = biFillInDefaults bi }

exeFillInDefaults :: Executable -> Executable
exeFillInDefaults exe@(Executable { buildInfo = bi }) = 
    exe { buildInfo = biFillInDefaults bi }

biFillInDefaults :: BuildInfo -> BuildInfo
biFillInDefaults bi =
    if null (hsSourceDirs bi)
    then bi { hsSourceDirs = [currentDir] }
    else bi

-- ------------------------------------------------------------
-- * Utils
-- ------------------------------------------------------------

satisfyDependency :: [PackageIdentifier] -> Dependency
	-> Maybe PackageIdentifier
satisfyDependency pkgs (Dependency pkgname vrange) =
  case filter ok pkgs of
    [] -> Nothing 
    qs -> Just (maximumBy versions qs)
  where
	ok p = pkgName p == pkgname && pkgVersion p `withinRange` vrange
        versions a b = pkgVersion a `compare` pkgVersion b

-- |Select options for a particular Haskell compiler.
hcOptions :: CompilerFlavor -> [(CompilerFlavor, [String])] -> [String]
hcOptions hc hc_opts = [opt | (hc',opts) <- hc_opts, hc' == hc, opt <- opts]

-- |The name of the auto-generated module associated with a package
autogenModuleName :: PackageDescription -> String
autogenModuleName pkg_descr =
    "Paths_" ++ map fixchar (pkgName (package pkg_descr))
  where fixchar '-' = '_'
        fixchar c   = c

haddockName :: PackageDescription -> FilePath
haddockName pkg_descr = pkgName (package pkg_descr) <.> "haddock"

setupMessage :: Verbosity -> String -> PackageDescription -> IO ()
setupMessage verbosity msg pkg_descr =
    notice verbosity (msg ++ ' ':showPackageId (package pkg_descr) ++ "...")

bug :: String -> a
bug msg = error $ msg ++ ". Consider this a bug."

-- ------------------------------------------------------------
-- * Testing
-- ------------------------------------------------------------
#ifdef DEBUG

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
    PackageDescription 
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
          libBuildInfo = BuildInfo {
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
    case finalizePackageDescription [] Nothing "" "" ("",Version [] []) gpd of
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
        (warns, ers) <- sanityCheckPackage emptyPackageDescription
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

test :: IO Counts
test = runTestTT (TestList hunitTests)
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
       case finalizePackageDescription [("debug",True)] (Just pkgs) os arch impl ppd of
         Right (pd,fs) -> do putStrLn $ showPackageDescription pd
                             print fs
         Left missing -> putStrLn $ "missing: " ++ show missing
       putStrLn $ showPackageDescription $ 
                flattenPackageDescription ppd
  where
    pkgs = [ PackageIdentifier "blub" (Version [1,0] []) 
           --, PackageIdentifier "hunit" (Version [1,1] []) 
           , PackageIdentifier "blab" (Version [0,1] []) 
           ]
    os = "win32"
    arch = "amd64"
    impl = ("ghc", Version [6,6] [])


#endif
