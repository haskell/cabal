{-# OPTIONS -cpp #-}
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
        emptyPackageDescription,
        readPackageDescription,
        writePackageDescription,
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
        exeModules,

	-- ** Parsing
        FieldDescr(..),
        LineNo,

	-- ** Sanity checking
        sanityCheckPackage,

        -- * Build information
        BuildInfo(..),
        emptyBuildInfo,
        mapBuildInfo,

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

import Control.Monad(liftM, foldM, when)
import Data.Char
import Data.Maybe(fromMaybe, isNothing, catMaybes)
import Data.List (nub,maximumBy)
import Text.PrettyPrint.HughesPJ as Pretty
import System.Directory(doesFileExist)

import Distribution.ParseUtils
import Distribution.Package(PackageIdentifier(..),showPackageId,
                            parsePackageName)
import Distribution.Version(Version(..), VersionRange(..), withinRange,
                            showVersion, parseVersion, showVersionRange, parseVersionRange)
import Distribution.License(License(..))
import Distribution.Version(Dependency(..))
import Distribution.Verbosity
import Distribution.Compiler(CompilerFlavor(..))
import Distribution.Simple.Utils(currentDir, die, dieWithLocation, warn)
import Language.Haskell.Extension(Extension(..))

import Distribution.Compat.ReadP as ReadP hiding (get)
import System.FilePath((<.>))

#ifdef DEBUG
import HUnit (Test(..), assertBool, Assertion, runTestTT, Counts, assertEqual)
import Data.List (sortBy)
#endif

-- We only get our own version number when we're building with ourselves
cabalVersion :: Version
#ifdef CABAL_VERSION
cabalVersion = Version [CABAL_VERSION] []
#else
cabalVersion = error "Cabal was not bootstrapped correctly"
#endif

-- -----------------------------------------------------------------------------
-- The PackageDescription type

-- | This data type is the internal representation of the file @pkg.cabal@.
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
        licenseFile    :: FilePath,
        copyright      :: String,
        maintainer     :: String,
        author         :: String,
        stability      :: String,
        testedWith     :: [(CompilerFlavor,VersionRange)],
        homepage       :: String,
        pkgUrl         :: String,
        synopsis       :: String, -- ^A one-line summary of this package
        description    :: String, -- ^A more verbose description of this package
        category       :: String,
        buildDepends   :: [Dependency],
        descCabalVersion :: VersionRange, -- ^If this package depends on a specific version of Cabal, give that here.
        buildType      :: BuildType,
        -- components
        library        :: Maybe Library,
        executables    :: [Executable],
        dataFiles      :: [FilePath],
        extraSrcFiles  :: [FilePath],
        extraTmpFiles  :: [FilePath]
    }
    deriving (Show, Read, Eq)

emptyPackageDescription :: PackageDescription
emptyPackageDescription
    =  PackageDescription {package      = PackageIdentifier "" (Version [] []),
                      license      = AllRightsReserved,
                      licenseFile  = "",
                      descCabalVersion = AnyVersion,
                      buildType    = Custom,
                      copyright    = "",
                      maintainer   = "",
                      author       = "",
                      stability    = "",
                      testedWith   = [],
                      buildDepends = [],
                      homepage     = "",
                      pkgUrl       = "",
                      synopsis     = "",
                      description  = "",
                      category     = "",
                      library      = Nothing,
                      executables  = [],
                      dataFiles    = [],
                      extraSrcFiles = [],
                      extraTmpFiles = []
                     }

-- | The type of build system used by this package.
data BuildType
  = Simple      -- ^ calls @Distribution.Simple.defaultMain@
  | Configure   -- ^ calls @Distribution.Simple.defaultMainWithHooks defaultUserHooks@,
                -- which invokes @configure@ to generate additional build
                -- information used by later phases.
  | Make        -- ^ calls @Distribution.Make.defaultMain@
  | Custom      -- ^ uses user-supplied @Setup.hs@ or @Setup.lhs@ (default)
                deriving (Show, Read, Eq)

-- the strings for the required fields are necessary here, and so we
-- don't repeat ourselves, I name them:
reqNameName	  :: String
reqNameName       = "name"
reqNameVersion	  :: String
reqNameVersion    = "version"
reqNameCopyright  :: String
reqNameCopyright  = "copyright"
reqNameMaintainer :: String
reqNameMaintainer = "maintainer"
reqNameSynopsis   :: String
reqNameSynopsis   = "synopsis"

pkgDescrFieldDescrs :: [FieldDescr PackageDescription]
pkgDescrFieldDescrs =
 [ simpleField reqNameName
           text                   parsePackageName
           (pkgName . package)    (\name pkg -> pkg{package=(package pkg){pkgName=name}})
 , simpleField reqNameVersion
           (text . showVersion)   parseVersion
           (pkgVersion . package) (\ver pkg -> pkg{package=(package pkg){pkgVersion=ver}})
 , simpleField "cabal-version"
           (text . showVersionRange) parseVersionRange
           descCabalVersion       (\v pkg -> pkg{descCabalVersion=v})
 , simpleField "build-type"
           (text . show)          parseReadSQ
           buildType              (\t pkg -> pkg{buildType=t})
 , simpleField "license"
           (text . show)          parseLicenseQ
           license                (\l pkg -> pkg{license=l})
 , simpleField "license-file"
           showFilePath           parseFilePathQ
           licenseFile            (\l pkg -> pkg{licenseFile=l})
 , simpleField reqNameCopyright
           showFreeText           (munch (const True))
           copyright              (\val pkg -> pkg{copyright=val})
 , simpleField reqNameMaintainer
           showFreeText           (munch (const True))
           maintainer             (\val pkg -> pkg{maintainer=val})
 , commaListField  "build-depends"
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
 , simpleField reqNameSynopsis
           showFreeText           (munch (const True))
           synopsis               (\val pkg -> pkg{synopsis=val})
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
 , listField "data-files"  
           showFilePath           parseFilePathQ
           dataFiles              (\val pkg -> pkg{dataFiles=val})
 , listField "extra-source-files" 
           showFilePath    parseFilePathQ
           extraSrcFiles          (\val pkg -> pkg{extraSrcFiles=val})
 , listField "extra-tmp-files" 
           showFilePath       parseFilePathQ
           extraTmpFiles          (\val pkg -> pkg{extraTmpFiles=val})
 ]

-- ---------------------------------------------------------------------------
-- The Library type

data Library = Library {
        exposedModules    :: [String],
        libBuildInfo      :: BuildInfo
    }
    deriving (Show, Eq, Read)

emptyLibrary :: Library
emptyLibrary = Library [] emptyBuildInfo

-- |does this package have any libraries?
hasLibs :: PackageDescription -> Bool
hasLibs p = maybe False (buildable . libBuildInfo) (library p)

-- |'Maybe' version of 'hasLibs'
maybeHasLibs :: PackageDescription -> Maybe Library
maybeHasLibs p =
   library p >>= (\lib -> toMaybe (buildable (libBuildInfo lib)) lib)

-- |If the package description has a library section, call the given
--  function with the library build info as argument.
withLib :: PackageDescription -> a -> (Library -> IO a) -> IO a
withLib pkg_descr a f =
   maybe (return a) f (maybeHasLibs pkg_descr)

-- |Get all the module names from the libraries in this package
libModules :: PackageDescription -> [String]
libModules PackageDescription{library=lib}
    = maybe [] exposedModules lib
       ++ maybe [] (otherModules . libBuildInfo) lib

libFieldDescrs :: [FieldDescr Library]
libFieldDescrs = map biToLib binfoFieldDescrs
  ++ [
      listField "exposed-modules" text parseModuleNameQ
	 exposedModules (\mods lib -> lib{exposedModules=mods})
     ]
  where biToLib = liftField libBuildInfo (\bi lib -> lib{libBuildInfo=bi})

-- ---------------------------------------------------------------------------
-- The Executable type

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

-- | Perform the action on each buildable 'Executable' in the package
-- description.
withExe :: PackageDescription -> (Executable -> IO a) -> IO ()
withExe pkg_descr f =
  sequence_ [f exe | exe <- executables pkg_descr, buildable (buildInfo exe)]

-- |Get all the module names from the exes in this package
exeModules :: PackageDescription -> [String]
exeModules PackageDescription{executables=execs}
    = concatMap (otherModules . buildInfo) execs

executableFieldDescrs :: [FieldDescr Executable]
executableFieldDescrs = 
  [ -- note ordering: configuration must come first, for
    -- showPackageDescription.
    simpleField "executable"
                           showToken          parseTokenQ
                           exeName            (\xs    exe -> exe{exeName=xs})
  , simpleField "main-is"
                           showFilePath       parseFilePathQ
                           modulePath         (\xs    exe -> exe{modulePath=xs})
  ]
  ++ map biToExe binfoFieldDescrs
  where biToExe = liftField buildInfo (\bi exe -> exe{buildInfo=bi})

-- ---------------------------------------------------------------------------
-- The BuildInfo type

-- Consider refactoring into executable and library versions.
data BuildInfo = BuildInfo {
        buildable         :: Bool,      -- ^ component is buildable here
        cpphsOptions      :: [String],  -- ^ options for cpphs
        ccOptions         :: [String],  -- ^ options for C compiler
        ldOptions         :: [String],  -- ^ options for linker
        frameworks        :: [String], -- ^support frameworks for Mac OS X
        cSources          :: [FilePath],
        hsSourceDirs      :: [FilePath], -- ^ where to look for the haskell module hierarchy
        otherModules      :: [String], -- ^ non-exposed or non-main modules
        extensions        :: [Extension],
        extraLibs         :: [String], -- ^ what libraries to link with when compiling a program that uses your package
        extraLibDirs      :: [String],
        includeDirs       :: [FilePath], -- ^directories to find .h files
        includes          :: [FilePath], -- ^ The .h files to be found in includeDirs
	installIncludes   :: [FilePath], -- ^ .h files to install with the package
        options           :: [(CompilerFlavor,[String])],
        ghcProfOptions    :: [String]
    }
    deriving (Show,Read,Eq)

emptyBuildInfo :: BuildInfo
emptyBuildInfo = BuildInfo {
                      buildable         = True,
                      cpphsOptions      = [],
                      ccOptions         = [],
                      ldOptions         = [],
                      frameworks        = [],
                      cSources          = [],
                      hsSourceDirs      = [currentDir],
                      otherModules      = [],
                      extensions        = [],
                      extraLibs         = [],
                      extraLibDirs      = [],
                      includeDirs       = [],
                      includes          = [],
                      installIncludes   = [],
                      options           = [],
                      ghcProfOptions       = []
                     }

-- | Modify all the 'BuildInfo's in a package description.
mapBuildInfo :: (BuildInfo -> BuildInfo) ->
                PackageDescription -> PackageDescription
mapBuildInfo f pkg = pkg {
    library = liftM mapLibBuildInfo (library pkg),
    executables = map mapExeBuildInfo (executables pkg) }
  where
    mapLibBuildInfo lib = lib { libBuildInfo = f (libBuildInfo lib) }
    mapExeBuildInfo exe = exe { buildInfo = f (buildInfo exe) }

type HookedBuildInfo = (Maybe BuildInfo, [(String, BuildInfo)])

emptyHookedBuildInfo :: HookedBuildInfo
emptyHookedBuildInfo = (Nothing, [])

binfoFieldDescrs :: [FieldDescr BuildInfo]
binfoFieldDescrs =
 [ simpleField "buildable"
           (text . show)      parseReadS
           buildable          (\val binfo -> binfo{buildable=val})
 , listField "cpphs-options"
           showToken          parseTokenQ
           cpphsOptions       (\val binfo -> binfo{cpphsOptions=val})
 , listField "cc-options"
           showToken          parseTokenQ
           ccOptions          (\val binfo -> binfo{ccOptions=val})
 , listField "ld-options"
           showToken          parseTokenQ
           ldOptions          (\val binfo -> binfo{ldOptions=val})
 , listField "frameworks"
           showToken          parseTokenQ
           frameworks         (\val binfo -> binfo{frameworks=val})
 , listField   "c-sources"
           showFilePath       parseFilePathQ
           cSources           (\paths binfo -> binfo{cSources=paths})
 , listField   "extensions"
           (text . show)      parseExtensionQ
           extensions         (\exts  binfo -> binfo{extensions=exts})
 , listField   "extra-libraries"
           showToken          parseTokenQ
           extraLibs          (\xs    binfo -> binfo{extraLibs=xs})
 , listField   "extra-lib-dirs"
           showFilePath       parseFilePathQ
           extraLibDirs       (\xs    binfo -> binfo{extraLibDirs=xs})
 , listField   "includes"
           showFilePath       parseFilePathQ
           includes           (\paths binfo -> binfo{includes=paths})
 , listField   "install-includes"
           showFilePath       parseFilePathQ
           installIncludes    (\paths binfo -> binfo{installIncludes=paths})
 , listField   "include-dirs"
           showFilePath       parseFilePathQ
           includeDirs        (\paths binfo -> binfo{includeDirs=paths})
 , listField   "hs-source-dirs"
           showFilePath       parseFilePathQ
           hsSourceDirs       (\paths binfo -> binfo{hsSourceDirs=paths})
 , listField   "other-modules"         
           text               parseModuleNameQ
           otherModules       (\val binfo -> binfo{otherModules=val})
 , listField   "ghc-prof-options"         
           text               parseTokenQ
           ghcProfOptions        (\val binfo -> binfo{ghcProfOptions=val})
 , optsField   "ghc-options"  GHC
           options            (\path  binfo -> binfo{options=path})
 , optsField   "hugs-options" Hugs
           options            (\path  binfo -> binfo{options=path})
 , optsField   "nhc-options"  NHC
           options            (\path  binfo -> binfo{options=path})
 , optsField   "jhc-options"  JHC
           options            (\path  binfo -> binfo{options=path})
 ]

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

-- |Update the given package description with the output from the
-- pre-hooks.

updatePackageDescription :: HookedBuildInfo -> PackageDescription -> PackageDescription
updatePackageDescription (mb_lib_bi, exe_bi) p
    = p{ executables = updateExecutables exe_bi    (executables p)
       , library     = updateLibrary     mb_lib_bi (library     p)
       }
    where
      updateLibrary :: Maybe BuildInfo -> Maybe Library -> Maybe Library
      updateLibrary (Just bi) (Just lib) = Just (lib{libBuildInfo = unionBuildInfo bi (libBuildInfo lib)})
      updateLibrary Nothing   mb_lib     = mb_lib

       --the lib only exists in the buildinfo file.  FIX: Is this
       --wrong?  If there aren't any exposedModules, then the library
       --won't build anyway.  add to sanity checker?
      updateLibrary (Just bi) Nothing     = Just emptyLibrary{libBuildInfo=bi}

      updateExecutables :: [(String, BuildInfo)] -- ^[(exeName, new buildinfo)]
                        -> [Executable]          -- ^list of executables to update
                        -> [Executable]          -- ^list with exeNames updated
      updateExecutables exe_bi' executables' = foldr updateExecutable executables' exe_bi'
      
      updateExecutable :: (String, BuildInfo) -- ^(exeName, new buildinfo)
                       -> [Executable]        -- ^list of executables to update
                       -> [Executable]        -- ^libst with exeName updated
      updateExecutable _                 []         = []
      updateExecutable exe_bi'@(name,bi) (exe:exes)
        | exeName exe == name = exe{buildInfo = unionBuildInfo bi (buildInfo exe)} : exes
        | otherwise           = exe : updateExecutable exe_bi' exes

unionBuildInfo :: BuildInfo -> BuildInfo -> BuildInfo
unionBuildInfo b1 b2
    = b1{buildable         = buildable b1 && buildable b2,
         cpphsOptions      = combine cpphsOptions,
         ccOptions         = combine ccOptions,
         ldOptions         = combine ldOptions,
         frameworks        = combine frameworks,
         cSources          = combine cSources,
         hsSourceDirs      = combine hsSourceDirs,
         otherModules      = combine otherModules,
         extensions        = combine extensions,
         extraLibs         = combine extraLibs,
         extraLibDirs      = combine extraLibDirs,
         includeDirs       = combine includeDirs,
         includes          = combine includes,
         installIncludes   = combine installIncludes,
         options           = combine options
        }
      where 
      combine :: (Eq a) => (BuildInfo -> [a]) -> [a]
      combine f = nub $ f b1 ++ f b2

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
    when (verbosity >= normal) $
        putStrLn (msg ++ ' ':showPackageId (package pkg_descr) ++ "...")

-- ---------------------------------------------------------------
-- Parsing

-- | Given a parser and a filename, return the parse of the file,
-- after checking if the file exists.
readAndParseFile :: Verbosity -> (String -> ParseResult a) -> FilePath -> IO a
readAndParseFile verbosity parser fpath = do
  exists <- doesFileExist fpath
  when (not exists) (die $ "Error Parsing: file \"" ++ fpath ++ "\" doesn't exist. Cannot continue.")
  str <- readFile fpath
  case parser str of
    ParseFailed e -> do
        let (lineNo, message) = locatedErrorMsg e
        dieWithLocation fpath lineNo message
    ParseOk ws x -> do
        mapM_ (warn verbosity) ws
        return x

readHookedBuildInfo :: Verbosity -> FilePath -> IO HookedBuildInfo
readHookedBuildInfo verbosity = readAndParseFile verbosity parseHookedBuildInfo

-- |Parse the given package file.
readPackageDescription :: Verbosity -> FilePath -> IO PackageDescription
readPackageDescription verbosity = readAndParseFile verbosity parseDescription
parseDescription :: String -> ParseResult PackageDescription
parseDescription str = do 
  all_fields0 <- readFields str
  all_fields <- mapM deprecField all_fields0
  let (st:sts) = stanzas all_fields
  pkg <- parseFields basic_field_descrs emptyPackageDescription st
  foldM parseExtraStanza pkg sts
  where
        parseExtraStanza pkg st@((_lineNo, "executable",_eName):_) = do
		exe <- parseFields executableFieldDescrs emptyExecutable st
		return pkg{executables= executables pkg ++ [exe]}
        parseExtraStanza _ x = error ("This shouldn't happen!" ++ show x)

basic_field_descrs :: [FieldDescr PackageDescription]
basic_field_descrs = pkgDescrFieldDescrs ++ map liftToPkg libFieldDescrs
  where liftToPkg = liftField (fromMaybe emptyLibrary . library)
			      (\lib pkg -> pkg{library = Just lib})

stanzas :: [Field] -> [[Field]]
stanzas [] = []
stanzas (f:fields) = (f:this) : stanzas rest
  where (this, rest) = break isStanzaHeader fields

isStanzaHeader :: Field -> Bool
isStanzaHeader (_,f,_) = f == "executable"

parseFields :: [FieldDescr a] -> a  -> [Field] -> ParseResult a
parseFields descrs ini fields = foldM (parseField descrs) ini fields

parseField :: [FieldDescr a] -> a -> Field -> ParseResult a
parseField ((FieldDescr name _ parse):fields) a (lineNo, f, val)
  | name == f = parse lineNo val a
  | otherwise = parseField fields a (lineNo, f, val)
-- ignore "x-" extension fields without a warning
parseField [] a (_, 'x':'-':_, _) = return a
parseField [] a (_, f, _) = do
          warning $ "Unknown field '" ++ f ++ "'"
          return a

-- Handle deprecated fields
deprecField :: Field -> ParseResult Field
deprecField (line,fld,val) = do
  fld' <- case fld of
	     "hs-source-dir"
		-> do warning "The field \"hs-source-dir\" is deprecated, please use hs-source-dirs."
		      return "hs-source-dirs"
	     "other-files"
		-> do warning "The field \"other-files\" is deprecated, please use extra-source-files."
		      return "extra-source-files"
	     _ -> return fld
  return (line,fld',val)


parseHookedBuildInfo :: String -> ParseResult HookedBuildInfo
parseHookedBuildInfo inp = do
  fields <- readFields inp
  let ss@(mLibFields:exes) = stanzas fields
  mLib <- parseLib mLibFields
  biExes <- mapM parseExe (maybe ss (const exes) mLib)
  return (mLib, biExes)
  where
    parseLib :: [Field] -> ParseResult (Maybe BuildInfo)
    parseLib (bi@((_, inFieldName, _):_))
        | map toLower inFieldName /= "executable" = liftM Just (parseBI bi)
    parseLib _ = return Nothing

    parseExe :: [Field] -> ParseResult (String, BuildInfo)
    parseExe ((lineNo, inFieldName, mName):bi)
        | map toLower inFieldName == "executable"
            = do bis <- parseBI bi
                 return (mName, bis)
        | otherwise = syntaxError lineNo "expecting 'executable' at top of stanza"
    parseExe [] = syntaxError 0 "error in parsing buildinfo file. Expected executable stanza"

    parseBI st = parseFields binfoFieldDescrs emptyBuildInfo st

-- ---------------------------------------------------------------------------
-- Pretty printing

writePackageDescription :: FilePath -> PackageDescription -> IO ()
writePackageDescription fpath pkg = writeFile fpath (showPackageDescription pkg)

showPackageDescription :: PackageDescription -> String
showPackageDescription pkg = render $
  ppFields pkg pkgDescrFieldDescrs $$
  (case library pkg of
     Nothing  -> empty
     Just lib -> ppFields lib libFieldDescrs) $$
  vcat (map ppExecutable (executables pkg))
  where
    ppExecutable exe = space $$ ppFields exe executableFieldDescrs

writeHookedBuildInfo :: FilePath -> HookedBuildInfo -> IO ()
writeHookedBuildInfo fpath pbi = writeFile fpath (showHookedBuildInfo pbi)

showHookedBuildInfo :: HookedBuildInfo -> String
showHookedBuildInfo (mb_lib_bi, ex_bi) = render $
  (case mb_lib_bi of
     Nothing -> empty
     Just bi -> ppFields bi binfoFieldDescrs) $$
  vcat (map ppExeBuildInfo ex_bi)
  where
    ppExeBuildInfo (name, bi) =
      space $$
      text "executable:" <+> text name $$
      ppFields bi binfoFieldDescrs

ppFields :: a -> [FieldDescr a] -> Doc
ppFields _ [] = empty
ppFields pkg' ((FieldDescr name get _):flds) =
     ppField name (get pkg') $$ ppFields pkg' flds

ppField :: String -> Doc -> Doc
ppField name fielddoc = text name <> colon <+> fielddoc

-- ------------------------------------------------------------
-- * Sanity Checking
-- ------------------------------------------------------------

-- |Sanity check this description file.

-- FIX: add a sanity check for missing haskell files? That's why its
-- in the IO monad.

sanityCheckPackage :: PackageDescription -> IO ([String] -- Warnings
                                               ,[String])-- Errors
sanityCheckPackage pkg_descr
    = let libSane   = sanityCheckLib (library pkg_descr)
          nothingToDo = checkSanity
                        (null (executables pkg_descr) && isNothing (library pkg_descr))
                        "No executables and no library found. Nothing to do."
          noModules = checkSanity (hasMods pkg_descr)
                      "No exposed modules or executables in this package."
          allRights = checkSanity (license pkg_descr == AllRightsReserved)
                      "Package is copyright All Rights Reserved"
          noLicenseFile = checkSanity (null $ licenseFile pkg_descr)
                          "No license-file field."
          goodCabal = let v = (descCabalVersion pkg_descr)
                          in checkSanity (not $ cabalVersion  `withinRange` v)
                                 ("This package requires Cabal version: " ++ (showVersionRange v) ++ ".")
         in return $ ( catMaybes [nothingToDo, noModules, allRights, noLicenseFile],
                       catMaybes (libSane:goodCabal: checkMissingFields pkg_descr
			  ++ map sanityCheckExe (executables pkg_descr)) )

toMaybe :: Bool -> a -> Maybe a
toMaybe b x = if b then Just x else Nothing

checkMissingFields :: PackageDescription -> [Maybe String]
checkMissingFields pkg_descr = 
    [missingField (pkgName . package)    reqNameName
    ,missingField (versionBranch .pkgVersion .package) reqNameVersion
    ]
    where missingField :: (PackageDescription -> [a]) -- Field accessor
                       -> String -- Name of field
                       -> Maybe String -- error message
          missingField f n
              = toMaybe (null (f pkg_descr)) ("Missing field: " ++ n)

sanityCheckLib :: Maybe Library -> Maybe String
sanityCheckLib ml =
   ml >>= (\l ->
      toMaybe (null $ exposedModules l)
              ("Non-empty library, but empty exposed modules list. " ++
               "Cabal may not build this library correctly"))

sanityCheckExe :: Executable -> Maybe String
sanityCheckExe exe
   = if null (modulePath exe)
	then Just ("No 'Main-Is' field found for executable " ++ exeName exe)
	else Nothing

checkSanity :: Bool -> String -> Maybe String
checkSanity = toMaybe

hasMods :: PackageDescription -> Bool
hasMods pkg_descr =
   null (executables pkg_descr) &&
      maybe True (null . exposedModules) (library pkg_descr)


-- ------------------------------------------------------------
-- * Testing
-- ------------------------------------------------------------
#ifdef DEBUG
testPkgDesc :: String
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

testPkgDescAnswer :: PackageDescription
testPkgDescAnswer = 
 PackageDescription {package = PackageIdentifier {pkgName = "Cabal",
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
                    descCabalVersion=LaterVersion (Version [1,1,1] []),
                    buildType=Custom,
                    buildDepends = [Dependency "haskell-src" AnyVersion,
                                     Dependency "HUnit"
                                     (UnionVersionRanges (ThisVersion (Version [1,0,0] ["rain"]))
                                      (LaterVersion (Version [1,0,0] ["rain"])))],
                    testedWith=[(GHC, AnyVersion)],
                    maintainer = "",
                    stability = "Free Text String",
                    extraTmpFiles=["file1", "file2"],
                    extraSrcFiles=["file1", "file2"],
                    dataFiles=[],

                    library = Just $ Library {
                        exposedModules = ["Distribution.Void", "Foo.Bar"],
                        libBuildInfo=BuildInfo {
                           buildable = True,
                           cpphsOptions = [],
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
                           options = [(GHC,["-fTH","-fglasgow-exts"]),(Hugs,["+TH"]),(NHC,[]),(JHC,[])]
                    }},
                    executables = [Executable "somescript" 
                       "SomeFile.hs" (
                      emptyBuildInfo{
                        otherModules=["Foo1","Util","Main"],
                        hsSourceDirs = ["scripts"],
                        extensions = [OverlappingInstances],
                        options = [(GHC,[]),(Hugs,[]),(NHC,[]),(JHC,[])]
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
                 ParseOk _ d -> case parseDescription $ showPackageDescription d of
                                ParseFailed _ ->
                                    assertBool "can't parse description after pretty print!" False
                                ParseOk _ d' -> 
                                    assertBool ("parse . show . parse not identity."
                                                ++"   Incorrect fields:\n"
                                                ++ (unlines $ comparePackageDescriptions d d'))
                                               (d == d'),
            TestLabel "Sanity checker" $ TestCase $ do
              (warns, ers) <- sanityCheckPackage emptyPackageDescription
              assertEqual "Wrong number of errors"   2 (length ers)
              assertEqual "Wrong number of warnings" 4 (length warns)
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
                          in toMaybe (e1 /= e2)
                                     (er ++ " Expected: " ++ show e1
                                              ++ " Got: " ++ show e2)

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
#endif

