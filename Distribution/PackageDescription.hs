-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.PackageDescription
-- Copyright   :  Isaac Jones 2003-2004
-- 
-- Maintainer  :  Isaac Jones <ijones@syntaxpolice.org>
-- Stability   :  alpha
-- Portability :  
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
	PackageDescription(..),
	emptyPackageDescription,
        readPackageDescription,
	parseDescription,
        writePackageDescription,
	showPackageDescription,
        unionPackageDescription,
	basicStanzaFields,
        setupMessage,
        withLib,
        hasLibs,
        BuildInfo(..),
        emptyBuildInfo,
        Executable(..),
        Library(..),
        emptyExecutable,
        exeModules,
        libModules,
        hcOptions,
#ifdef DEBUG
        hunitTests,
        test
#endif
  ) where

import Control.Monad(foldM, when)
import Data.Char
import Data.Maybe(fromMaybe, fromJust)
import Text.PrettyPrint.HughesPJ(text, render, ($$), empty, space, vcat, fsep)
import System.Directory(doesFileExist)

import Distribution.ParseUtils
import Distribution.Package(PackageIdentifier(..),showPackageId,
			    parsePackageName)
import Distribution.Version(Version(..), VersionRange(..),
                            showVersion, parseVersion)
import Distribution.License(License(..))
import Distribution.Version(Dependency(..))
import Distribution.Extension(Extension(..))
import Distribution.Setup(CompilerFlavor(..))
import Distribution.Simple.Utils(currentDir, die)

import Distribution.Compat.ReadP as ReadP hiding (get)

#ifdef DEBUG
import Control.Monad	(liftM)
import HUnit (Test(..), assertBool, Assertion, runTestTT)
import Distribution.ParseUtils	(runP)
#endif

-- | This data type is the internal representation of the file @pkg.descr@.
-- It contains two kinds of information about the package: information
-- which is needed for all packages, such as the package name and version, and 
-- information which is needed for the simple build system only, such as 
-- the compiler options and library name.
-- 
data PackageDescription
    =  PackageDescription {
	-- the following are required by all packages:
	package        :: PackageIdentifier,
        license        :: License,
        copyright      :: String,
        maintainer     :: String,
	author         :: String,
        stability      :: String,
	testedWith     :: [(CompilerFlavor,VersionRange)],
	homepage       :: String,
	pkgUrl         :: String,
	description    :: String,
	category       :: String,
        buildDepends      :: [Dependency],
	-- possibly system-dependent build parameters
	buildPackage   :: Bool,		-- ^ package is buildable here
	ccOptions      :: [String],	-- ^ options for C compiler
	ldOptions      :: [String],	-- ^ options for linker
	frameworks     :: [String],
	-- components
        library        :: Maybe Library,
        executables    :: [Executable]
    }
    deriving (Show, Read, Eq)

data Library = Library { exposedModules    :: [String],
	                 hiddenModules     :: [String],
                         libBuildInfo      :: BuildInfo }
             deriving (Show, Eq, Read)

emptyLibrary :: Library
emptyLibrary = Library [] [] emptyBuildInfo

emptyPackageDescription :: PackageDescription
emptyPackageDescription
    =  PackageDescription {package      = PackageIdentifier "" (Version [] []),
                      license      = AllRightsReserved,
                      copyright    = "",
                      maintainer   = "",
		      author       = "",
                      stability    = "",
		      testedWith   = [],
                      buildDepends = [],
		      homepage     = "",
		      pkgUrl       = "",
		      description  = "",
		      category     = "",
		      buildPackage = True,
		      ccOptions    = [],
		      ldOptions    = [],
		      frameworks   = [],
                      library      = Nothing,
                      executables  = []
                     }

-- |Get all the module names from the libraries in this package
libModules :: PackageDescription -> [String]
libModules PackageDescription{library=lib}
    = (maybe [] exposedModules lib)
      ++ (maybe [] hiddenModules lib)

-- |Get all the module names from the exes in this package
exeModules :: PackageDescription -> [String]
exeModules PackageDescription{executables=execs}
    = concatMap executableModules execs

-- |does this package have any libraries?
hasLibs :: PackageDescription -> Bool
hasLibs p = case library p of
            Just l  -> if null (cSources $ libBuildInfo l)
                          && null (hiddenModules l)
                          && null (exposedModules l)
                       then False else True
            Nothing -> False
            
-- Consider refactoring into executable and library versions.
data BuildInfo = BuildInfo {
        cSources          :: [FilePath],
        hsSourceDir       :: FilePath,
        extensions        :: [Extension],
        extraLibs         :: [String],
        extraLibDirs      :: [String],
        includeDirs       :: [FilePath],
        includes          :: [FilePath],
        options           :: [(CompilerFlavor,[String])]
    }
    deriving (Show,Read,Eq)

emptyBuildInfo :: BuildInfo
emptyBuildInfo = BuildInfo {
		      cSources          = [],
		      hsSourceDir       = currentDir,
                      extensions        = [],
                      extraLibs         = [],
                      extraLibDirs      = [],
                      includeDirs       = [],
                      includes          = [],
                      options           = []
                     }

data Executable = Executable {
        exeName    :: String,
        executableModules :: [String],
        modulePath :: FilePath,
        buildInfo  :: BuildInfo
    }
    deriving (Show, Read, Eq)

emptyExecutable :: Executable
emptyExecutable = Executable {
                      exeName = "",
                      modulePath = "",
                      buildInfo = emptyBuildInfo,
                      executableModules=[]
                     }

-- ------------------------------------------------------------
-- * Utils
-- ------------------------------------------------------------

-- |If the package description has a library section, call the given
--  function with the library build info as argument.
withLib :: PackageDescription -> a -> (Library -> IO a) -> IO a
withLib pkg_descr a f = if hasLibs pkg_descr
                        then f (fromJust (library pkg_descr))
                        else return a

setupMessage :: String -> PackageDescription -> IO ()
setupMessage msg pkg_descr = 
   putStrLn (msg ++ ' ':showPackageId (package pkg_descr) ++ "...")

-- |This isn't quite the right way to go about this.  For one thing,
-- the Right Thing for excutables isn't exactly clear.  For another
-- thing, it's hard to tell whether or not the field was provided at
-- all in /p1/.  The only way to guess (as the parser is currently
-- implemented) is to compare it with the 'emptyPackageDescription'
-- variable, though it's possible that they will be equal, but the
-- user actually did provide that field.  Another question is what to
-- do about the "required," static fields which should not be in /p2/.
-- We should definitely check to be sure they're in /p1/, and not in
-- /p2/, though not in this function.
-- FIXME: executables not implemented correctly, library (buildinfo)
-- not yet implemented.

unionPackageDescription :: PackageDescription -> PackageDescription -> PackageDescription
unionPackageDescription p1 p2
    = p1{ -- simple fields
         license       = override license "license",
         copyright     = override copyright "copyright",
         maintainer    = override maintainer "maintainer",
	 author        = override author "author",
         stability     = override stability "stability",
	 homepage      = override homepage "homepage",
	 pkgUrl        = override pkgUrl "package-url",
	 description   = override description "description",
	 category      = override category "category",
	 buildPackage  = override buildPackage "build-package",
         -- combine fields:
	 ccOptions     = combine ccOptions,
	 ldOptions     = combine ldOptions,
	 frameworks    = combine frameworks,
         testedWith    = combine testedWith,
         -- it's not obvious what to do with executables:
         executables   = combine executables,

         -- complex fields
         package       = unionPackageIdent (package p1) (package p2),
         library       = makeLib (library p1) (library p2)
        }
      where
      override :: (Eq a) => (PackageDescription -> a)
               -> String -- Field name
               -> a
      override f s
          | f p1 == f p2 = f p1
          | f p1 /= f emptyPackageDescription
            && f p2 /= f emptyPackageDescription
                = error $ "union: Two non-empty fields found in union attempt:" ++ s
          | f p1 == f emptyPackageDescription = f p2
          | otherwise = f p1
      combine :: (Eq a) => (PackageDescription -> [a])
               -> [a]
      combine f = f p1 ++ f p2
      makeLib :: Maybe Library -> Maybe Library -> Maybe Library
      makeLib Nothing Nothing     = Nothing
      makeLib Nothing j           = j
      makeLib j Nothing           = j
      makeLib (Just b1) (Just b2) = Just $ unionLibrary b1 b2

unionLibrary :: Library -> Library -> Library
unionLibrary l1 l2
    = l1{ exposedModules    = combine exposedModules,
	  hiddenModules     = combine hiddenModules,
          libBuildInfo      = unionBuildInfo (libBuildInfo l1) (libBuildInfo l2)
        }
      where 
      combine :: (Eq a) => (Library -> [a]) -> [a]
      combine f = f l1 ++ f l2

unionBuildInfo :: BuildInfo -> BuildInfo -> BuildInfo
unionBuildInfo b1 b2
    = b1{cSources          = combine cSources,
         hsSourceDir       = override hsSourceDir "hs-source-dir" currentDir,
         extensions        = combine extensions,
         extraLibs         = combine extraLibs,
         extraLibDirs      = combine extraLibDirs,
         includeDirs       = combine includeDirs,
         includes          = combine includes,
         options           = combine options
        }
      where 
      combine :: (Eq a) => (BuildInfo -> [a]) -> [a]
      combine f = f b1 ++ f b2
      override :: (Eq a)
	=> (BuildInfo -> a)	-- ^ field extractor
	-> String		-- ^ field name
	-> a			-- ^ default value
	-> a
      override f s def
	| v1 == def = v2
	| v2 == def = v1
	| otherwise = error $ "union: Two non-empty fields found in union attempt: " ++ s
        where v1 = f b1
	      v2 = f b2

unionPackageIdent :: PackageIdentifier -> PackageIdentifier -> PackageIdentifier
unionPackageIdent p1 p2
    = p1{pkgName = override pkgName "name",
         pkgVersion = override pkgVersion "version"}
      where
      override :: (Eq a) => (PackageIdentifier -> a)
               -> String -- Field name
               -> a
      override f s
          | f p1 == f p2 = f p1
          | f p1 /= f emptyIdent
            && f p2 /= f emptyIdent
                = error $ "union: Two non-empty fields found in union attempt:" ++ s
          | f p1 == f emptyIdent = f p2
          | otherwise = f p1
      emptyIdent = PackageIdentifier "" (Version [] [])

-- |Select options for a particular Haskell compiler.
hcOptions :: CompilerFlavor -> [(CompilerFlavor, [String])] -> [String]
hcOptions hc hc_opts = [opt | (hc',opts) <- hc_opts, hc' == hc, opt <- opts]

-- ------------------------------------------------------------
-- * Parsing & Pretty printing
-- ------------------------------------------------------------

basicStanzaFields :: [StanzaField PackageDescription]
basicStanzaFields =
 [ simpleField "name"
                           text                   parsePackageName
                           (pkgName . package)    (\name pkg -> pkg{package=(package pkg){pkgName=name}})
 , simpleField "version"
                           (text . showVersion)   parseVersion 
                           (pkgVersion . package) (\ver pkg -> pkg{package=(package pkg){pkgVersion=ver}})
 , licenseField "license" False
                           license                (\l pkg -> pkg{license=l})
 , licenseField "license-file" True
                           license                (\l pkg -> pkg{license=l})
 , simpleField "copyright"
                           showFreeText           (munch (const True))
                           copyright              (\val pkg -> pkg{copyright=val})
 , simpleField "maintainer"
                           showFreeText           (munch (const True))
                           maintainer             (\val pkg -> pkg{maintainer=val})
 , listField   "build-depends"   
                           showDependency         parseDependency
                           buildDepends           (\xs    pkg -> pkg{buildDepends=xs})
 , simpleField "stability"
                           showFreeText           (munch (const True))
                           stability              (\val pkg -> pkg{stability=val})
 , simpleField "homepage"
                           showFreeText           (munch (const True))
                           homepage               (\val pkg -> pkg{homepage=val})
 , simpleField "package-url"
                           showFreeText           (munch (const True))
                           pkgUrl                 (\val pkg -> pkg{pkgUrl=val})
 , simpleField "description"
                           showFreeText           (munch (const True))
                           description            (\val pkg -> pkg{description=val})
 , simpleField "category"
                           showFreeText           (munch (const True))
                           category               (\val pkg -> pkg{category=val})
 , simpleField "author"
                           showFreeText           (munch (const True))
                           author                 (\val pkg -> pkg{author=val})
 , listField "tested-with"
                           showTestedWith         parseTestedWithQ
                           testedWith             (\val pkg -> pkg{testedWith=val})
 , simpleField "build-package"
                           (text . show)          parseReadS
                           buildPackage           (\val pkg -> pkg{buildPackage=val})
 , simpleField "cc-options"
                           (fsep . map text)      (fmap words (munch (const True)))
                           ccOptions              (\val pkg -> pkg{ccOptions=val})
 , simpleField "ld-options"
                           (fsep . map text)      (fmap words (munch (const True)))
                           ldOptions              (\val pkg -> pkg{ldOptions=val})
 , simpleField "frameworks"
                           (fsep . map text)      (fmap words (munch (const True)))
                           frameworks             (\val pkg -> pkg{frameworks=val})

 , listField   "hidden-modules"         
                           text               parseModuleNameQ
                           (\p -> maybe [] hiddenModules (library p))
                           (\xs    pkg -> let lib = fromMaybe emptyLibrary (library pkg) in
                                              pkg{library = Just lib{hiddenModules=xs}})
 , listField   "exposed-modules"
                           text               parseModuleNameQ
                           (\p -> maybe [] exposedModules (library p))
                           (\xs    pkg -> let lib = fromMaybe emptyLibrary (library pkg) in
                                              pkg{library = Just lib{exposedModules=xs}})
 ]

executableStanzaFields :: [StanzaField Executable]
executableStanzaFields =
 [ simpleField "executable"
                           showFreeText       (munch (const True))
                           exeName            (\xs    exe -> exe{exeName=xs})
 , simpleField "main-is"
                           showFilePath       parseFilePathQ
                           modulePath         (\xs    exe -> exe{modulePath=xs})
 , listField   "executable-modules"
                           text               parseModuleNameQ
                           executableModules  (\xs    exe -> exe{executableModules=xs})
 ]

binfoFields :: [StanzaField BuildInfo]
binfoFields =
 [ listField   "c-sources"
                           showFilePath       parseFilePathQ
                           cSources           (\paths binfo -> binfo{cSources=paths})
 , listField   "extensions"
                           (text . show)      parseExtensionQ
                           extensions         (\exts  binfo -> binfo{extensions=exts})
 , listField   "extra-libs"
                           text               parseLibNameQ
                           extraLibs          (\xs    binfo -> binfo{extraLibs=xs})
 , listField   "extra-lib-dirs"
                           text               parseLibNameQ
                           extraLibDirs       (\xs    binfo -> binfo{extraLibDirs=xs})
 , listField   "includes"
                           showFilePath       parseFilePathQ
                           includes           (\paths binfo -> binfo{includes=paths})
 , listField   "include-dirs"
                           showFilePath       parseFilePathQ
                           includeDirs        (\paths binfo -> binfo{includeDirs=paths})
 , simpleField "hs-source-dir"
                           showFilePath       parseFilePathQ
                           hsSourceDir        (\path  binfo -> binfo{hsSourceDir=path})
 , optsField   "options-ghc"  GHC
                           options            (\path  binfo -> binfo{options=path})
 , optsField   "options-hugs" Hugs
                           options            (\path  binfo -> binfo{options=path})
 , optsField   "options-nhc"  NHC
                           options            (\path  binfo -> binfo{options=path})
 ]

-- --------------------------------------------
-- ** Parsing

-- |Parse the given package file.
readPackageDescription :: FilePath -> IO PackageDescription
readPackageDescription fpath = do 
  exists <- doesFileExist fpath
  when (not exists) (die $ "Error: description file \"" ++ fpath ++ "\" doesn't exist. Cannot continue.")
  str <- readFile fpath
  case parseDescription str of
    ParseFailed e -> error (showError e) -- FIXME
--    ParseOk PackageDescription{library=Nothing, executables=[]} -> error "no library listed, and no executable stanza."
    ParseOk x -> return x

parseDescription :: String -> ParseResult PackageDescription
parseDescription inp = do (st:sts) <- splitStanzas inp
                          pkg <- foldM (parseBasicStanza basicStanzaFields) emptyPackageDescription st
                          exes <- mapM parseExecutableStanza sts
                          return pkg{executables=exes}
  where -- The basic stanza, with library building info
        parseBasicStanza ((StanzaField name _ _ set):fields) pkg (lineNo, f, val)
          | name == f = set lineNo val pkg
          | otherwise = parseBasicStanza fields pkg (lineNo, f, val)
        parseBasicStanza [] pkg (lineNo, f, val) = do
          let lib = fromMaybe emptyLibrary (library pkg)
	  bi <- parseBInfoField binfoFields (libBuildInfo lib) (lineNo, f, val)
          return pkg{library=Just lib{libBuildInfo=bi}}

        parseExecutableStanza st@((_, "executable",eName):_) =
          case lookupField "main-is" st of
	    Just (_,_) -> foldM (parseExecutableField executableStanzaFields) emptyExecutable st
	    Nothing           -> fail $ "No 'Main-Is' field found for " ++ eName ++ " stanza"
        parseExecutableStanza ((lineNo, f,_):_) = 
          myError lineNo $ "'Executable' stanza starting with field '" ++ f ++ "'"
        parseExecutableStanza _ = error "This shouldn't happen!"

        parseExecutableField ((StanzaField name _ _ set):fields) exe (lineNo, f, val)
	  | name == f = set lineNo val exe
	  | otherwise = parseExecutableField fields exe (lineNo, f, val)
	parseExecutableField [] exe (lineNo, f, val) = do
	  binfo <- parseBInfoField binfoFields (buildInfo exe) (lineNo, f, val)
          return exe{buildInfo=binfo}

        parseBInfoField ((StanzaField name _ _ set):fields) binfo (lineNo, f, val)
	  | name == f = set lineNo val binfo
	  | otherwise = parseBInfoField fields binfo (lineNo, f, val)
	parseBInfoField [] _ (lineNo, f, _) =
	  myError lineNo $ "Unknown field '" ++ f ++ "'"
        -- ...
        lookupField :: String -> Stanza -> Maybe (LineNo,String)
        lookupField _ [] = Nothing
        lookupField x ((n,f,v):st)
          | x == f      = Just (n,v)
          | otherwise   = lookupField x st


-- --------------------------------------------
-- ** Pretty printing

writePackageDescription :: FilePath -> PackageDescription -> IO ()
writePackageDescription fpath pkg = writeFile fpath (showPackageDescription pkg)

showPackageDescription :: PackageDescription -> String
showPackageDescription pkg = render $
  ppFields pkg basicStanzaFields $$
  (case library pkg of
     Nothing  -> empty
     Just lib -> ppFields (libBuildInfo lib) binfoFields) $$
  vcat (map ppExecutable (executables pkg))
  where
    ppExecutable exe =
      space $$
      ppFields exe executableStanzaFields $$
      ppFields (buildInfo exe) binfoFields

    ppFields _ [] = empty
    ppFields pkg' ((StanzaField _ get _ _):flds) =
           get pkg' $$ ppFields pkg' flds
        
-- ------------------------------------------------------------
-- * Testing
-- ------------------------------------------------------------
#ifdef DEBUG
testPkgDesc = unlines [
        "-- Required",
        "Name: Cabal",
        "Version: 0.1.1.1.1-rain",
        "License: LGPL",
        "Copyright: Free Text String",
        "-- Optional - may be in source?",
        "Author: Happy Haskell Hacker",
        "Homepage: http://www.haskell.org/foo",
        "Package-url: http://www.haskell.org/foo",
        "Description: a nice package!",
        "Category: tools",
        "Build-Package: True",
        "CC-OPTIONS: -g -o",
        "LD-OPTIONS: -BStatic -dn",
        "Frameworks: foo",
        "Tested-with: GHC",
        "Stability: Free Text String",
        "Build-Depends: haskell-src, HUnit>=1.0.0-rain",
        "Hidden-Modules: Distribution.Package, Distribution.Version,",
        "                Distribution.Simple.GHCPackageConfig",
        "C-Sources: not/even/rain.c, such/small/hands",
        "HS-Source-Dir: src",
        "Exposed-Modules: Distribution.Void, Foo.Bar",
        "Extensions: OverlappingInstances, TypeSynonymInstances",
        "Extra-Libs: libfoo, bar, bang",
	"Extra-Lib-Dirs: \"/usr/local/libs\"",
        "Include-Dirs: your/slightest, look/will",
        "Includes: /easily/unclose, /me, \"funky, path\\\\name\"",
        "Options-ghc: -fTH -fglasgow-exts",
        "Options-hugs: +TH",
        "",
        "-- Next is an executable",
        "Executable: somescript",
        "Main-is: SomeFile.hs",
        "Executable-Modules: Foo1, Util, Main",
        "HS-Source-Dir: scripts",
        "Extensions: OverlappingInstances"
        ]

testPkgDescAnswer = 
 PackageDescription {package = PackageIdentifier {pkgName = "Cabal",
                                                 pkgVersion = Version {versionBranch = [0,1,1,1,1],
                                                 versionTags = ["rain"]}},
                    license = LGPL,
                    copyright = "Free Text String",
                    author  = "Happy Haskell Hacker",
                    homepage = "http://www.haskell.org/foo",
                    pkgUrl   = "http://www.haskell.org/foo",
                    description = "a nice package!",
                    category = "tools",
                    buildPackage = True,
                     buildDepends = [Dependency "haskell-src" AnyVersion,
                                     Dependency "HUnit"
                                     (UnionVersionRanges (ThisVersion (Version [1,0,0] ["rain"]))
                                      (LaterVersion (Version [1,0,0] ["rain"])))],
                    ccOptions = ["-g", "-o"],
                    ldOptions = ["-BStatic", "-dn"],
                    frameworks = ["foo"],
                    testedWith=[(GHC, AnyVersion)],
                    maintainer = "",
                    stability = "Free Text String",

                    library = Just $ Library {
                        hiddenModules = ["Distribution.Package","Distribution.Version",
                                         "Distribution.Simple.GHCPackageConfig"],
                        exposedModules = ["Distribution.Void", "Foo.Bar"],
                        libBuildInfo=BuildInfo {
                           cSources = ["not/even/rain.c", "such/small/hands"],
                           hsSourceDir = "src",
                           extensions = [OverlappingInstances, TypeSynonymInstances],
                           extraLibs = ["libfoo", "bar", "bang"],
                           extraLibDirs = ["/usr/local/libs"],
                           includeDirs = ["your/slightest", "look/will"],
                           includes = ["/easily/unclose", "/me", "funky, path\\name"],
                           -- Note reversed order:
                           options = [(Hugs,["+TH"]), (GHC,["-fTH","-fglasgow-exts"])]}
                    },
                    executables = [Executable "somescript" 
                       ["Foo1","Util","Main"] "SomeFile.hs" (
                      emptyBuildInfo{
                        
                        hsSourceDir = "scripts",
                        extensions = [OverlappingInstances]
                      })]
}

hunitTests :: [Test]
hunitTests = [
              TestLabel "license parsers" $ TestCase $
                 sequence_ [assertParseOk ("license " ++ show lVal) lVal
                                        (runP 1 "license" parseLicenseQ (show lVal))
                           | lVal <- [GPL,LGPL,BSD3,BSD4]],

              TestLabel "Required fields" $ TestCase $
                 do assertParseOk "some fields"
                       emptyPackageDescription{package=(PackageIdentifier "foo"
                                                        (Version [0,0] ["asdf"]))}
                       (parseDescription "Name: foo\nVersion: 0.0-asdf")

                    assertParseOk "more fields foo"
                       emptyPackageDescription{package=(PackageIdentifier "foo"
                                                        (Version [0,0]["asdf"])),
                                               license=GPL}
                       (parseDescription "Name: foo\nVersion:0.0-asdf\nLicense: GPL")

                    assertParseOk "required fields for foo"
                       emptyPackageDescription{package=(PackageIdentifier "foo"
                                                        (Version [0,0]["asdf"])),
                                        license=GPL, copyright="2004 isaac jones"}
                       (parseDescription "Name: foo\nVersion:0.0-asdf\nCopyright: 2004 isaac jones\nLicense: GPL"),
                                          
             TestCase $ assertParseOk "no library" Nothing
                        (library `liftM` parseDescription "Name: foo\nVersion: 1\nLicense: GPL\nMaintainer: someone\n\nExecutable: script\nMain-is: SomeFile.hs\n"),

             TestLabel "Package description" $ TestCase $ 
                assertParseOk "entire package description" testPkgDescAnswer
                                                         (parseDescription testPkgDesc),
             TestLabel "Package description pretty" $ TestCase $ 
                case parseDescription testPkgDesc of
                 ParseFailed _ -> assertBool "can't parse description" False
                 ParseOk d -> assertParseOk "parse . show . parse not identity"
                             testPkgDescAnswer (parseDescription $ showPackageDescription d)
             ]


assertParseOk :: (Eq val) => String -> val -> ParseResult val -> Assertion
assertParseOk mes expected actual
    =  assertBool mes
           (case actual of
             ParseOk v -> v == expected
             _         -> False)

test = runTestTT (TestList hunitTests)
#endif
