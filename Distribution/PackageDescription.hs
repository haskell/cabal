{-# OPTIONS -cpp #-}
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
	basicStanzaFields,
        setupMessage,
        withLib,
        hasLibs,
        BuildInfo(..),
        emptyBuildInfo,
        Executable(..),
        emptyExecutable,
        libModules,
        exeModules,
        biModules,
#ifdef DEBUG
        hunitTests,
        test
#endif
  ) where

import Control.Monad(foldM, when)
import Data.Char
import Data.List(concatMap)
import Data.Maybe(fromMaybe, fromJust)
import Text.PrettyPrint.HughesPJ
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
import HUnit (Test(..), (~:), (~=?), assertEqual, assertBool, Assertion, runTestTT)
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
        library        :: Maybe BuildInfo,
        executables    :: [Executable]
    }
    deriving (Show, Read, Eq)

emptyPackageDescription :: PackageDescription
emptyPackageDescription
    =  PackageDescription {package      = PackageIdentifier "" (Version [] []),
                      license      = AllRightsReserved,
                      copyright    = "",
                      maintainer   = "",
		      author       = "",
                      stability    = "",
		      testedWith   = [],
		      homepage     = "",
		      pkgUrl       = "",
		      description  = "",
		      category     = "",
                      library      = Nothing,
                      executables  = []
                     }

-- |All the modules listed in this BuildInfo.
biModules :: BuildInfo -> [String]
biModules BuildInfo{executableModules=exeMods,
                    exposedModules=expMods,
                    hiddenModules=hMods}
    = exeMods ++ hMods ++ expMods

-- |Get all the module names from the libraries in this package
libModules :: PackageDescription -> [String]
libModules PackageDescription{library=lib}
    = (maybe [] exposedModules lib)
      ++ (maybe [] hiddenModules lib)

-- |Get all the module names from the exes in this package
exeModules :: PackageDescription -> [String]
exeModules PackageDescription{executables=execs}
    = concatMap (\e -> executableModules $ buildInfo e) execs

-- |Set the name for this package. Convenience function.
setPkgName :: String -> PackageDescription -> PackageDescription
setPkgName n desc@PackageDescription{package=pkgIdent}
    = desc{package=pkgIdent{pkgName=n}}

-- |Set the version for this package. Convenience function.
setPkgVersion :: Version -> PackageDescription -> PackageDescription
setPkgVersion v desc@PackageDescription{package=pkgIdent}
    = desc{package=pkgIdent{pkgVersion=v}}

-- |does this package have any libraries?
hasLibs :: PackageDescription -> Bool
hasLibs p = case library p of
            Just l  -> if null (cSources l)
                          && null (hiddenModules l)
                          && null (exposedModules l)
                       then False else True
            Nothing -> False

            
-- Consider refactoring into executable and library versions.
data BuildInfo = BuildInfo {
        buildDepends      :: [Dependency],
        executableModules :: [String], -- Only used for executables
	exposedModules    :: [String], -- Only used for libs
	hiddenModules     :: [String], -- Only used for libs
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
                      buildDepends      = [],
                      executableModules = [], -- Only used for executables
		      exposedModules    = [], -- Only used for libs
		      hiddenModules     = [], -- Only used for libs
		      cSources          = [],
		      hsSourceDir       = currentDir,
                      extensions        = [],
                      extraLibs         = [],
                      extraLibDirs      = [],
                      includeDirs       = [],
                      includes          = [],
                      options           = []
                     }
                     
-- |Add options for a specific compiler. Convenience function.
setOptions :: CompilerFlavor -> [String] -> BuildInfo -> BuildInfo
setOptions c xs desc@BuildInfo{options=opts}
    = desc{options=(c,xs):opts}


data Executable = Executable {
        exeName    :: String,
        modulePath :: FilePath,
        buildInfo  :: BuildInfo
    }
    deriving (Show, Read, Eq)

emptyExecutable :: Executable
emptyExecutable = Executable {
                      exeName = "",
                      modulePath = "",
                      buildInfo = emptyBuildInfo
                     }

-- ------------------------------------------------------------
-- * Utils
-- ------------------------------------------------------------

-- |If the package description has a library section, call the given
--  function with the library build info as argument.
withLib :: PackageDescription -> (BuildInfo -> IO ()) -> IO ()
withLib pkg_descr f = when (hasLibs pkg_descr) $ f (fromJust (library pkg_descr))

setupMessage :: String -> PackageDescription -> IO ()
setupMessage msg pkg_descr = 
   putStrLn (msg ++ ' ':showPackageId (package pkg_descr) ++ "...")


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
                           text                   (munch (const True))
                           copyright              (\val pkg -> pkg{copyright=val})
 , simpleField "maintainer"
                           text                   (munch (const True))
                           maintainer             (\val pkg -> pkg{maintainer=val})
 , simpleField "stability"
                           text                   (munch (const True))
                           stability              (\val pkg -> pkg{stability=val})
 , simpleField "homepage"
                           text                   (munch (const True))
                           homepage               (\val pkg -> pkg{homepage=val})
 , simpleField "package-url"
                           text                   (munch (const True))
                           pkgUrl                 (\val pkg -> pkg{pkgUrl=val})
 , simpleField "description"
                           text                   (munch (const True))
                           description            (\val pkg -> pkg{description=val})
 , simpleField "category"
                           text                   (munch (const True))
                           category               (\val pkg -> pkg{category=val})
 , simpleField "author"
                           text                   (munch (const True))
                           author                 (\val pkg -> pkg{author=val})
 , listField "tested-with"
                           showTestedWith         parseTestedWithQ
                           testedWith             (\val pkg -> pkg{testedWith=val})

 ]

executableStanzaFields :: [StanzaField Executable]
executableStanzaFields =
 [ simpleField "executable"
                           text               (munch (const True))
                           exeName            (\xs    exe -> exe{exeName=xs})
 , simpleField "main-is"
                           showFilePath       parseFilePathQ
                           modulePath         (\xs    exe -> exe{modulePath=xs})
 ]

binfoFields :: [StanzaField BuildInfo]
binfoFields =
 [ listField   "build-depends"   
                           showDependency     parseDependency
                           buildDepends       (\xs    binfo -> binfo{buildDepends=xs})
 , listField   "hidden-modules"         
                           text               parseModuleNameQ
                           hiddenModules      (\xs    binfo -> binfo{hiddenModules=xs})
 , listField   "exposed-modules"
                           text               parseModuleNameQ
                           exposedModules     (\xs    binfo -> binfo{exposedModules=xs})
 , listField   "executable-modules"
                           text               parseModuleNameQ
                           exposedModules     (\xs    binfo -> binfo{executableModules=xs})
 , listField   "c-sources"
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
                           includes           (\paths binfo -> binfo{includeDirs=paths})
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
    Left  e -> error (showError e) -- FIXME
    Right PackageDescription{library=Nothing, executables=[]} -> error "no library listed, and no executable stanza."
    Right x -> return x

parseDescription :: String -> Either PError PackageDescription
parseDescription inp = do (st:sts) <- splitStanzas inp
                          pkg <- foldM (parseBasicStanza basicStanzaFields) emptyPackageDescription st
                          exes <- mapM parseExecutableStanza sts
                          return pkg{executables=exes}
  where -- The basic stanza, with library building info
        parseBasicStanza ((StanzaField name _ _ set):fields) pkg (lineNo, f, val)
          | name == f = set lineNo val pkg
          | otherwise = parseBasicStanza fields pkg (lineNo, f, val)
        parseBasicStanza [] pkg (lineNo, f, val) = do
          let lib = fromMaybe emptyBuildInfo (library pkg)
	  lib' <- parseBInfoField binfoFields lib (lineNo, f, val)
          return pkg{library=Just lib'}

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
	parseBInfoField [] binfo (lineNo, f, _) =
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
     Just lib -> ppFields lib binfoFields) $$
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
                    testedWith=[(GHC, AnyVersion)],
                    maintainer = "",
                    stability = "Free Text String",

                    library = Just $ BuildInfo {
                        buildDepends = [Dependency "haskell-src" AnyVersion,
                                        Dependency "HUnit"
                                         (UnionVersionRanges (ThisVersion (Version [1,0,0] ["rain"]))
                                          (LaterVersion (Version [1,0,0] ["rain"])))],

                        hiddenModules = ["Distribution.Package","Distribution.Version",
                                         "Distribution.Simple.GHCPackageConfig"],
                        executableModules=[],
                        cSources = ["not/even/rain.c", "such/small/hands"],
                        hsSourceDir = "src",
                        exposedModules = ["Distribution.Void", "Foo.Bar"],
                        extensions = [OverlappingInstances, TypeSynonymInstances],
                        extraLibs = ["libfoo", "bar", "bang"],
                        extraLibDirs = ["/usr/local/libs"],
                        includeDirs = ["your/slightest", "look/will"],
                        includes = ["/easily/unclose", "/me", "funky, path\\name"],
                        -- Note reversed order:
                        options = [(Hugs,["+TH"]), (GHC,["-fTH","-fglasgow-exts"])]
                    },
                    executables = [Executable "somescript" "SomeFile.hs" (
                      emptyBuildInfo{
                        executableModules = ["Foo1","Util","Main"],
                        hsSourceDir = "scripts",
                        extensions = [OverlappingInstances]
                      })]
}

hunitTests :: [Test]
hunitTests = [
              TestLabel "license parsers" $ TestCase $
                 sequence_ [assertRight ("license " ++ show lVal) lVal
                                        (runP 1 "license" parseLicenseQ (show lVal))
                           | lVal <- [GPL,LGPL,BSD3,BSD4]],

              TestLabel "Required fields" $ TestCase $
                 do assertRight "some fields"
                       emptyPackageDescription{package=(PackageIdentifier "foo"
                                                        (Version [0,0] ["asdf"]))}
                       (parseDescription "Name: foo\nVersion: 0.0-asdf")

                    assertRight "more fields foo"
                       emptyPackageDescription{package=(PackageIdentifier "foo"
                                                        (Version [0,0]["asdf"])),
                                               license=GPL}
                       (parseDescription "Name: foo\nVersion:0.0-asdf\nLicense: GPL")

                    assertRight "required fields for foo"
                       emptyPackageDescription{package=(PackageIdentifier "foo"
                                                        (Version [0,0]["asdf"])),
                                        license=GPL, copyright="2004 isaac jones"}
                       (parseDescription "Name: foo\nVersion:0.0-asdf\nCopyright: 2004 isaac jones\nLicense: GPL"),
                                          
             TestCase $ assertRight "no library" Nothing
                        (library `liftM` parseDescription "Name: foo\nVersion: 1\nLicense: GPL\nMaintainer: someone\n\nExecutable: script\nMain-is: SomeFile.hs\n"),

             TestLabel "Package description" $ TestCase $ 
                assertRight "entire package description" testPkgDescAnswer
                                                         (parseDescription testPkgDesc)

             ]


assertRight :: (Eq val) => String -> val -> (Either a val) -> Assertion
assertRight mes expected actual
    =  assertBool mes
           (case actual of
             (Right v) -> v == expected
             _         -> False)

isError (Left _) = True
isError _        = False

test = runTestTT (TestList hunitTests)
#endif
