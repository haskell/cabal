{-# OPTIONS -cpp #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Package
-- Copyright   :  Isaac Jones 2003-2004
-- 
-- Maintainer  :  Isaac Jones <ijones@syntaxpolice.org>
-- Stability   :  alpha
-- Portability :  
--
-- Explanation: Package description and parsing

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

module Distribution.Package (
	PackageIdentifier(..), 
	showPackageId,
	PackageDescription(..),
        BuildInfo(..),
        Executable(..),
	emptyPackageDescription,
        parsePackageDesc,
        hasLibs,
#ifdef DEBUG
        hunitTests,
        test
#endif
  ) where

import Control.Monad(foldM)
import Data.Char
import Data.List(isPrefixOf)
import Data.Maybe(fromMaybe)

import Distribution.Version(Version(..), VersionRange(..),
                            showVersion, parseVersion, parseVersionRange)
import Distribution.Misc(License(..), Dependency(..), Extension(..))
import Distribution.Setup(CompilerFlavor(..))

import System.IO(openFile, IOMode(..), hGetContents)

import Compat.H98
import Compat.ReadP

#ifdef DEBUG
import HUnit (Test(..), (~:), (~=?), assertEqual, assertBool, Assertion, runTestTT)
#endif

data PackageIdentifier
    = PackageIdentifier {pkgName::String, pkgVersion::Version}
      deriving (Read, Show, Eq)

showPackageId :: PackageIdentifier -> String
showPackageId (PackageIdentifier n (Version [] _)) = n -- if no version, don't show version.
showPackageId pkgid = 
  pkgName pkgid ++ '-': showVersion (pkgVersion pkgid)

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
        stability      :: String,
        library        :: Maybe BuildInfo,
        executables    :: [Executable]
    }
    deriving (Show, Read, Eq)

data Executable = Executable {
        exeName    :: String,
        modulePath :: FilePath,
        buildInfo  :: BuildInfo
    }
    deriving (Show, Read, Eq)

data BuildInfo = BuildInfo {
        buildDepends    :: [Dependency],
        modules         :: [String],
	exposedModules  :: [String],
        cSources        :: [FilePath],
        hsSourceDir     :: FilePath,
        extensions      :: [Extension],
        extraLibs       :: [String],
        includeDirs     :: [FilePath],
        includes        :: [FilePath],
        options         :: [(CompilerFlavor,[String])]
    }
    deriving (Show,Read,Eq)

-- |Set the name for this package. Convenience function.
setPkgName :: String -> PackageDescription -> PackageDescription
setPkgName n desc@PackageDescription{package=pkgIdent}
    = desc{package=pkgIdent{pkgName=n}}

-- |Set the version for this package. Convenience function.
setPkgVersion :: Version -> PackageDescription -> PackageDescription
setPkgVersion v desc@PackageDescription{package=pkgIdent}
    = desc{package=pkgIdent{pkgVersion=v}}

emptyPackageDescription :: PackageDescription
emptyPackageDescription
    =  PackageDescription {package      = PackageIdentifier "" (Version [] []),
                      license      = AllRightsReserved,
                      copyright    = "",
                      maintainer   = "",
                      stability    = "",
                      library      = Nothing,
                      executables  = []
                     }

emptyBuildInfo :: BuildInfo
emptyBuildInfo = BuildInfo {
                      buildDepends   = [],
                      modules        = [],
		      exposedModules = [], -- Only used for libs
		      cSources       = [],
		      hsSourceDir    = ".", -- FIX: FileUtils.currentDir
                      extensions     = [],
                      extraLibs      = [],
                      includeDirs    = [],
                      includes       = [],
                      options        = []
                     }
                        
-- |Add options for a specific compiler. Convenience function.
setOptions :: CompilerFlavor -> [String] -> BuildInfo -> BuildInfo
setOptions c xs desc@BuildInfo{options=opts}
    = desc{options=(c,xs):opts}

-- |does this package have any libraries?
hasLibs :: PackageDescription -> Bool
hasLibs p = case library p of
            Just l  -> if null (cSources l) && null (modules l)
                       then False else True
            Nothing -> False


-- ------------------------------------------------------------
-- * Parsing
-- ------------------------------------------------------------

-- |Parse the given package file.
parsePackageDesc :: FilePath -> IO PackageDescription
parsePackageDesc p = do h <- openFile p ReadMode
                        str <- hGetContents h
                        case parseDescription str of
                          Left  e -> error (showError e) -- FIXME
                          Right PackageDescription{library=Nothing,
                                                   executables=[]}
                              -> error "no library listed, and no executable stanza."
                          Right x -> return x

data PError = AmbigousParse | NoParse | FromString String
        deriving Show

instance Error PError where
        strMsg = FromString

showError :: PError -> String
showError AmbigousParse  = "Ambigous parse"
showError NoParse        = "No parse"
showError (FromString s) = s

parseDescription :: String -> Either PError PackageDescription
parseDescription inp = do let (st:sts) = splitStanzas inp
                          pkg <- foldM parseBasicStanza emptyPackageDescription st
                          exes <- mapM parseExecutableStanza sts
                          return pkg{executables=exes}
  where -- The basic stanza, with library building info
        parseBasicStanza pkg ("name",      val) = return (setPkgName val pkg)
        parseBasicStanza pkg ("version",   val) =
          do v <- runP parseVersion val
             return (setPkgVersion v pkg)
        parseBasicStanza pkg ("copyright", val) = return pkg{copyright=val}
        parseBasicStanza pkg ("license",   val) =
          do l <- runP parseLicense val
             return pkg{license=l}
        parseBasicStanza pkg ("license-file", val) =
          do path <- runP parseFilePath val
             return pkg{license=OtherLicense path}
        parseBasicStanza pkg ("maintainer", val) = return pkg{maintainer=val}
        parseBasicStanza pkg ("stability",  val) = return pkg{stability=val}
        parseBasicStanza pkg (field, val) =
          do let lib = fromMaybe emptyBuildInfo (library pkg)
             lib' <- parseExeHelp lib (field, val)
             return pkg{library=Just lib'}
        -- Stanzas for executables
        parseExecutableStanza (("executable",eName):st) =
          case lookup "main-is" st of
            Just xs -> do path <- runP parseFilePath xs
                          binfo <- foldM parseExeHelp emptyBuildInfo st
                          return $ Executable eName path binfo
            Nothing -> fail $
                "No 'Main-Is' field found for " ++ eName ++ " stanza"
        parseExecutableStanza ((f,_):_) = fail $
                "'Executable' stanza starting with field '" ++ f ++ "'"
        parseExecutableStanza _ = error "This shouldn't happen!"
        parseExeHelp binfo ("main-is", _) = return binfo
        parseExeHelp binfo ("extra-libs", val) =
          do xs <- runP (parseCommaList parseLibName) val
             return binfo{extraLibs=xs}
        parseExeHelp binfo ("build-depends", val) =
          do xs <- runP (parseCommaList parseDependency) val
             return binfo{buildDepends=xs}
        -- Paths and stuff
        parseExeHelp binfo ("c-sources", val) =
          do paths <- runP (parseCommaList parseFilePath) val
             return binfo{cSources=paths}
        parseExeHelp binfo ("include-dirs", val) =
          do paths <- runP (parseCommaList parseFilePath) val
             return binfo{includeDirs=paths}
        parseExeHelp binfo ("includes", val) =
          do paths <- runP (parseCommaList parseFilePath) val
             return binfo{includes=paths}
        parseExeHelp binfo ("hs-source-dir", val) =
          do path <- runP parseFilePath val
             return binfo{hsSourceDir=path}
        -- Module related
        parseExeHelp binfo ("modules", val) =
          do xs <- runP (parseCommaList parseModuleName) val
             return binfo{modules=xs}
        parseExeHelp binfo ("exposed-modules", val) =
          do xs <- runP (parseCommaList parseModuleName) val
             return binfo{exposedModules=xs}
        parseExeHelp binfo ("extensions", val) =
          do exts <- runP (parseCommaList parseExtension) val
             return binfo{extensions=exts}
        parseExeHelp binfo (f, val) | "options-" `isPrefixOf` f =
          let compilers = [("ghc",GHC),("nhc",NHC),("hugs",Hugs)] -- FIXME
           in case lookup (drop (length "options-") f) compilers of
                Just c  -> return (setOptions c (words val) binfo)
                Nothing -> error $ "Unknown compiler (" ++ drop 8 f ++ ")"
        parseExeHelp _binfo (field, _val) = error $ "Unknown field :: " ++ field
        -- ...

runP :: ReadP a a -> String -> Either PError a
runP p s = case [ x | (x,"") <- readP_to_S p s ] of
             [a] -> Right a
             []  -> Left NoParse
             _   -> Left AmbigousParse

type Stanza = [(String,String)]

-- |Split a string into blank line-separated stanzas of
-- "Field: value" groups
splitStanzas :: String -> [Stanza]
splitStanzas = map merge . groupStanzas . filter validLine . lines
  where validLine s = case dropWhile isSpace s of
                        '-':'-':_ -> False      -- Comment
                        _         -> True
        groupStanzas [] = []
        groupStanzas xs = let (ys,zs) = break (all isSpace) xs
                           in ys : groupStanzas (dropWhile (all isSpace) zs)
        merge (x:(' ':s):ys) = case dropWhile isSpace s of
                                 "." -> merge ((x++"\n"):ys)
                                 s'  -> merge ((x++"\n"++s'):ys)
        merge (x:ys) = brk x : merge ys
        merge []     = []
        brk xs = case break (==':') xs of
                   (fld, ':':val) -> (map toLower fld, dropWhile isSpace val)
                   (fld, _)       -> error $ "Parser error: Line '"
                                        ++ fld ++ "' has no colon"

-- |parse a module name
parseModuleName :: ReadP r String
parseModuleName = do c <- satisfy isUpper
                     cs <- munch (\x -> isAlphaNum x || x `elem` "_'.")
                     return (c:cs)

parseFilePath :: ReadP r FilePath
parseFilePath = parseReadS <++ (munch1 (\x -> isAlphaNum x || x `elem` "-+/_."))

parseReadS :: Read a => ReadP r a
parseReadS = readS_to_P reads

parseDependency :: ReadP r Dependency
parseDependency = do name <- munch1 (\x -> isAlphaNum x || x `elem` "-_")
                     skipSpaces
                     ver <- parseVersionRange <++ return AnyVersion
                     skipSpaces
                     return $ Dependency name ver

parseLicense :: ReadP r License
parseLicense = parseReadS

parseExtension :: ReadP r Extension
parseExtension = parseReadS

parseLibName :: ReadP r String
parseLibName = munch1 (\x -> not (isSpace x) && x /= ',')

parseCommaList :: ReadP r a -- ^The parser for the stuff between commas
               -> ReadP r [a]
parseCommaList p = sepBy1 p separator
    where separator = skipSpaces >> char ',' >> skipSpaces


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
        "Stability: Free Text String",
        "Build-Depends: haskell-src, HUnit>=1.0.0-rain",
        "Modules: Distribution.Package, Distribution.Version, Distribution.Simple.GHCPackageConfig",
        "C-Sources: not/even/rain.c, such/small/hands",
        "HS-Source-Dir: src",
        "Exposed-Modules: Distribution.Void, Foo.Bar",
        "Extensions: OverlappingInstances, TypeSynonymInstances",
        "Extra-Libs: libfoo, bar, bang",
        "Include-Dirs: your/slightest, look/will",
        "Includes: /easily/unclose, /me, \"funky, path\\\\name\"",
        "Options-ghc: -fTH -fglasgow-exts",
        "Options-hugs: +TH",
        "",
        "-- Next is an executable",
        "Executable: somescript",
        "Main-is: SomeFile.hs",
        "Modules: Foo1, Util, Main",
        "HS-Source-Dir: scripts",
        "Extensions: OverlappingInstances"
        ]

testPkgDescAnswer = 
 PackageDescription {package = PackageIdentifier {pkgName = "Cabal",
                                                 pkgVersion = Version {versionBranch = [0,1,1,1,1],
                                                 versionTags = ["rain"]}},
                    license = LGPL,
                    copyright = "Free Text String",
                    maintainer = "",
                    stability = "Free Text String",

                    library = Just $ BuildInfo {
                        buildDepends = [Dependency "haskell-src" AnyVersion,
                                        Dependency "HUnit"
                                         (UnionVersionRanges (ThisVersion (Version [1,0,0] ["rain"]))
                                          (LaterVersion (Version [1,0,0] ["rain"])))],

                        modules = ["Distribution.Package","Distribution.Version",
                                      "Distribution.Simple.GHCPackageConfig"],

                        cSources = ["not/even/rain.c", "such/small/hands"],
                        hsSourceDir = "src",
                        exposedModules = ["Distribution.Void", "Foo.Bar"],
                        extensions = [OverlappingInstances, TypeSynonymInstances],
                        extraLibs = ["libfoo", "bar", "bang"],
                        includeDirs = ["your/slightest", "look/will"],
                        includes = ["/easily/unclose", "/me", "funky, path\\name"],
                        -- Note reversed order:
                        options = [(Hugs,["+TH"]), (GHC,["-fTH","-fglasgow-exts"])]
                    },
                    executables = [Executable "somescript" "SomeFile.hs" (
                      emptyBuildInfo{
                        modules = ["Foo1","Util","Main"],
                        hsSourceDir = "scripts",
                        extensions = [OverlappingInstances]
                      })]
}

hunitTests :: [Test]
hunitTests = [
              TestLabel "license parsers" $ TestCase $
                 sequence_ [assertRight ("license " ++ show lVal) lVal
                                        (runP parseLicense (show lVal))
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
