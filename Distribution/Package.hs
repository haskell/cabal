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
        emptyBuildInfo,
        Executable(..),
	emptyPackageDescription,
        parsePackageDesc,
        hasLibs,
#ifdef DEBUG
        hunitTests,
        test
#endif
  ) where

import Control.Monad(foldM, liftM)
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

type LineNo = Int
type Stanza = [(LineNo,String,String)]

data PError = AmbigousParse String LineNo
            | NoParse String LineNo
            | FromString String (Maybe LineNo)
        deriving Show

instance Error PError where
        strMsg s = FromString s Nothing

showError :: PError -> String
showError (AmbigousParse f n)     = "Line "++show n++": Ambigous parse in field '"++f++"'"
showError (NoParse f n)           = "Line "++show n++": Parse of field '"++f++"' failed"
showError (FromString s (Just n)) = "Line "++show n++": " ++ s
showError (FromString s Nothing)  = s

myError :: LineNo -> String -> Either PError a
myError n s = Left $ FromString s (Just n)

parseDescription :: String -> Either PError PackageDescription
parseDescription inp = do let (st:sts) = splitStanzas inp
                          pkg <- foldM parseBasicStanza emptyPackageDescription st
                          exes <- mapM parseExecutableStanza sts
                          return pkg{executables=exes}
  where -- The basic stanza, with library building info
        parseBasicStanza pkg (lineNo, f@"name", val) =
          do name <- runP lineNo f parsePackageName val
             return (setPkgName name pkg)
        parseBasicStanza pkg (lineNo, f@"version", val) =
          do v <- runP lineNo f parseVersion val
             return (setPkgVersion v pkg)
        parseBasicStanza pkg (lineNo, "copyright", val) = return pkg{copyright=val}
        parseBasicStanza pkg (lineNo, f@"license", val) =
          do l <- runP lineNo f parseLicense val
             return pkg{license=l}
        parseBasicStanza pkg (lineNo, f@"license-file", val) =
          do path <- runP lineNo f parseFilePath val
             return pkg{license=OtherLicense path}
        parseBasicStanza pkg (lineNo, "maintainer", val) = return pkg{maintainer=val}
        parseBasicStanza pkg (lineNo, "stability",  val) = return pkg{stability=val}
        parseBasicStanza pkg (lineNo, field, val) =
          do let lib = fromMaybe emptyBuildInfo (library pkg)
             lib' <- parseBInfoField lib (lineNo, field, val)
             return pkg{library=Just lib'}
        -- Stanzas for executables
        parseExecutableStanza ((lineNo, f@"executable",eName):st) =
          case lookupField "main-is" st of
            Just (lineNo,val) -> do path <- runP lineNo f parseFilePath val
                                    binfo <- foldM parseBInfoField emptyBuildInfo st
                                    return $ Executable eName path binfo
            Nothing -> fail $
                "No 'Main-Is' field found for " ++ eName ++ " stanza"
        parseExecutableStanza ((lineNo, f,_):_) = myError lineNo $
                "'Executable' stanza starting with field '" ++ f ++ "'"
        parseExecutableStanza _ = error "This shouldn't happen!"
        parseBInfoField binfo (lineNo, "main-is", _) = return binfo
        parseBInfoField binfo (lineNo, f@"extra-libs", val) =
          do xs <- runP lineNo f (parseCommaList parseLibName) val
             return binfo{extraLibs=xs}
        parseBInfoField binfo (lineNo, f@"build-depends", val) =
          do xs <- runP lineNo f (parseCommaList parseDependency) val
             return binfo{buildDepends=xs}
        -- Paths and stuff
        parseBInfoField binfo (lineNo, f@"c-sources", val) =
          do paths <- runP lineNo f (parseCommaList parseFilePath) val
             return binfo{cSources=paths}
        parseBInfoField binfo (lineNo, f@"include-dirs", val) =
          do paths <- runP lineNo f (parseCommaList parseFilePath) val
             return binfo{includeDirs=paths}
        parseBInfoField binfo (lineNo, f@"includes", val) =
          do paths <- runP lineNo f (parseCommaList parseFilePath) val
             return binfo{includes=paths}
        parseBInfoField binfo (lineNo, f@"hs-source-dir", val) =
          do path <- runP lineNo f parseFilePath val
             return binfo{hsSourceDir=path}
        -- Module related
        parseBInfoField binfo (lineNo, f@"modules", val) =
          do xs <- runP lineNo f (parseCommaList parseModuleName) val
             return binfo{modules=xs}
        parseBInfoField binfo (lineNo, f@"exposed-modules", val) =
          do xs <- runP lineNo f (parseCommaList parseModuleName) val
             return binfo{exposedModules=xs}
        parseBInfoField binfo (lineNo, f@"extensions", val) =
          do exts <- runP lineNo f (parseCommaList parseExtension) val
             return binfo{extensions=exts}
        parseBInfoField binfo (lineNo, f, val) | "options-" `isPrefixOf` f =
          let compilers = [("ghc",GHC),("nhc",NHC),("hugs",Hugs)] -- FIXME
           in case lookup (drop (length "options-") f) compilers of
                Just c  -> return (setOptions c (words val) binfo)
                Nothing -> myError lineNo $ "Unknown compiler '" ++ drop 8 f ++ "'"
        parseBInfoField _binfo (lineNo, field, _val) =
                myError lineNo $ "Unknown field '" ++ field ++ "'"
        -- ...
        lookupField :: String -> Stanza -> Maybe (LineNo,String)
        lookupField x [] = Nothing
        lookupField x ((n,f,v):st)
          | x == f      = Just (n,v)
          | otherwise   = lookupField x st

runP :: LineNo -> String -> ReadP a a -> String -> Either PError a
runP lineNo field p s =
  case [ x | (x,"") <- results ] of
    [a] -> Right a
    []  -> case [ x | (x,ys) <- results, all isSpace ys ] of
             [a] -> Right a
             []  -> Left (NoParse field lineNo)
             _   -> Left (AmbigousParse field lineNo)
    _   -> Left (AmbigousParse field lineNo)
  where results = readP_to_S p s

-- |Split a string into blank line-separated stanzas of
-- "Field: value" groups
splitStanzas :: String -> [Stanza]
splitStanzas = map merge . groupStanzas . filter validLine . zip [1..] . lines
  where validLine (_,s) = case dropWhile isSpace s of
                            '-':'-':_ -> False      -- Comment
                            _         -> True
        allSpaces (_,xs) = all isSpace xs
        groupStanzas :: [(Int,String)] -> [[(Int,String)]]
        groupStanzas [] = []
        groupStanzas xs = let (ys,zs) = break allSpaces xs
                           in ys : groupStanzas (dropWhile allSpaces zs)
        merge ((n,x):(_,' ':s):ys) = case dropWhile isSpace s of
                                       ('.':s') -> merge ((n,x++"\n"++s'):ys)
                                       s'       -> merge ((n,x++"\n"++s'):ys)
        merge ((n,x):ys) = brk n x : merge ys
        merge []         = []
        brk n xs = case break (==':') xs of
                     (fld, ':':val) -> (n, map toLower fld, dropWhile isSpace val)
                     (fld, _)       -> error $ "Line "++show n++": Invalid syntax (no colon after field name)"

-- |parse a module name
parseModuleName :: ReadP r String
parseModuleName = do c <- satisfy isUpper
                     cs <- munch (\x -> isAlphaNum x || x `elem` "_'.")
                     return (c:cs)

parseFilePath :: ReadP r FilePath
parseFilePath = parseReadS <++ (munch1 (\x -> isAlphaNum x || x `elem` "-+/_."))

parseReadS :: Read a => ReadP r a
parseReadS = readS_to_P reads

parsePackageName :: ReadP r String
parsePackageName = do n <- satisfy isAlpha
                      name <- munch1 (\x -> isAlphaNum x || x `elem` "-")
                      return (n:name)

parseDependency :: ReadP r Dependency
parseDependency = do name <- parsePackageName
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
        "Modules: Distribution.Package, Distribution.Version,",
        "         Distribution.Simple.GHCPackageConfig",
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
                                        (runP 1 "license" parseLicense (show lVal))
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
