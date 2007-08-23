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
        GenericPackageDescription(..),
        finalizePackageDescription,
        flattenPackageDescription,
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
import Data.Maybe(isNothing, isJust, catMaybes, listToMaybe, maybeToList)
import Data.List (nub, maximumBy, unfoldr, partition)
import Text.PrettyPrint.HughesPJ as Pretty
import System.Directory(doesFileExist)

import Distribution.ParseUtils
import Distribution.Package(PackageIdentifier(..),showPackageId,
                            parsePackageName)
import Distribution.Version(Version(..), VersionRange(..), withinRange,
                            showVersion, parseVersion, showVersionRange,
                            parseVersionRange, isAnyVersion)
import Distribution.License(License(..))
import Distribution.Version(Dependency(..))
import Distribution.Verbosity
import Distribution.Compiler(CompilerFlavor(..))
import Distribution.Configuration
import Distribution.Simple.Utils(currentDir, die, dieWithLocation, warn)
import Language.Haskell.Extension(Extension(..))

import Distribution.Compat.ReadP as ReadP hiding (get)
import System.FilePath((<.>))

import Data.Monoid

#ifdef DEBUG
import Data.List ( sortBy )
import Test.HUnit (Test(..), assertBool, Assertion, runTestTT, Counts, assertEqual)
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

data GenericPackageDescription = 
    GenericPackageDescription {
        packageDescription :: PackageDescription,
        genPackageFlags       :: [Flag],
        condLibrary        :: Maybe (CondTree ConfVar [Dependency] Library),
        condExecutables    :: [(String, CondTree ConfVar [Dependency] Executable)]
      }
    --deriving (Show)

-- XXX: I think we really want a PPrint or Pretty or ShowPretty class.
instance Show GenericPackageDescription where
    show (GenericPackageDescription pkg flgs mlib exes) =
        showPackageDescription pkg ++ "\n" ++
        (render $ vcat $ map ppFlag flgs) ++ "\n" ++
        render (maybe empty (\l -> text "Library:" $+$ 
                                   nest 2 (ppCondTree l showDeps)) mlib)
        ++ "\n" ++
        (render $ vcat $ 
            map (\(n,ct) -> (text ("Executable: " ++ n) $+$ 
                             nest 2 (ppCondTree ct showDeps))) exes)
      where
        ppFlag (MkFlag name desc dflt) =
            (text ("Flag: " ++ name) <> colon) $+$ 
            nest 2 
              ((if (null desc) then empty else 
                   text ("Description: " ++ desc)) $+$
              text ("Default: " ++ show dflt))
        showDeps = fsep . punctuate comma . map showDependency
         

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
  -> Maybe [PackageIdentifier] -- ^ Available dependencies
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

instance Monoid Library where
    mempty = nullLibrary
    mappend = unionLibrary

emptyLibrary :: Library
emptyLibrary = Library [] emptyBuildInfo

nullLibrary :: Library
nullLibrary = Library [] nullBuildInfo

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

unionLibrary :: Library -> Library -> Library
unionLibrary l1 l2 =
    l1 { exposedModules = combine exposedModules
       , libBuildInfo = unionBuildInfo (libBuildInfo l1) (libBuildInfo l2)
       }
  where combine f = f l1 ++ f l2

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

-- ---------------------------------------------------------------------------
-- The Executable type

data Executable = Executable {
        exeName    :: String,
        modulePath :: FilePath,
        buildInfo  :: BuildInfo
    }
    deriving (Show, Read, Eq)

instance Monoid Executable where
    mempty = nullExecutable
    mappend = unionExecutable

emptyExecutable :: Executable
emptyExecutable = Executable {
                      exeName = "",
                      modulePath = "",
                      buildInfo = emptyBuildInfo
                     }

nullExecutable :: Executable
nullExecutable = emptyExecutable { buildInfo = nullBuildInfo }

-- note comment at libFillInDefaults
exeFillInDefaults :: Executable -> Executable
exeFillInDefaults exe@(Executable { buildInfo = bi }) = 
    exe { buildInfo = biFillInDefaults bi }


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

unionExecutable :: Executable -> Executable -> Executable
unionExecutable e1 e2 =
    e1 { exeName = combine exeName
       , modulePath = combine modulePath
       , buildInfo = unionBuildInfo (buildInfo e1) (buildInfo e2)
       }
  where combine f = case (f e1, f e2) of
                      ("","") -> ""
                      ("", x) -> x
                      (x, "") -> x
                      (x, y) -> error $ "Ambiguous values for executable field: '"
                                  ++ x ++ "' and '" ++ y ++ "'"
  
-- ---------------------------------------------------------------------------
-- The BuildInfo type

-- Consider refactoring into executable and library versions.
data BuildInfo = BuildInfo {
        buildable         :: Bool,      -- ^ component is buildable here
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

nullBuildInfo :: BuildInfo
nullBuildInfo = BuildInfo {
                      buildable         = True,
                      ccOptions         = [],
                      ldOptions         = [],
                      frameworks        = [],
                      cSources          = [],
                      hsSourceDirs      = [],
                      otherModules      = [],
                      extensions        = [],
                      extraLibs         = [],
                      extraLibDirs      = [],
                      includeDirs       = [],
                      includes          = [],
                      installIncludes   = [],
                      options           = [],
                      ghcProfOptions    = []
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

emptyBuildInfo :: BuildInfo
emptyBuildInfo = nullBuildInfo { hsSourceDirs = [currentDir] }

-- see comment at libFillInDefaults
biFillInDefaults :: BuildInfo -> BuildInfo
biFillInDefaults bi =
    if null (hsSourceDirs bi)
    then bi { hsSourceDirs = [currentDir] }
    else bi

type HookedBuildInfo = (Maybe BuildInfo, [(String, BuildInfo)])

emptyHookedBuildInfo :: HookedBuildInfo
emptyHookedBuildInfo = (Nothing, [])

binfoFieldDescrs :: [FieldDescr BuildInfo]
binfoFieldDescrs =
 [ simpleField "buildable"
           (text . show)      parseReadS
           buildable          (\val binfo -> binfo{buildable=val})
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

------------------------------------------------------------------------------

flagFieldDescrs :: [FieldDescr Flag]
flagFieldDescrs =
    [ simpleField "description"
        showFreeText     (munch (const True))
        flagDescription  (\val fl -> fl{ flagDescription = val })
    , simpleField "default"
        (text . show)    parseReadS
        flagDefault      (\val fl -> fl{ flagDefault = val })
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
        let (line, message) = locatedErrorMsg e
        dieWithLocation fpath line message
    ParseOk ws x -> do
        mapM_ (warn verbosity) (reverse ws)
        return x

readHookedBuildInfo :: Verbosity -> FilePath -> IO HookedBuildInfo
readHookedBuildInfo verbosity = readAndParseFile verbosity parseHookedBuildInfo

-- |Parse the given package file.
-- readPackageDescription :: Int -> FilePath -> IO PackageDescription
-- readPackageDescription verbosity = readAndParseFile verbosity parseDescription 
readPackageDescription :: Verbosity -> FilePath -> IO GenericPackageDescription
readPackageDescription verbosity =
    readAndParseFile verbosity parseDescription
{-
parseDescription :: String -> ParseResult PackageDescription
parseDescription str = do 
  all_fields0 <- readFields str
--  detectCabalFormat all_fields0
  all_fields <- mapM deprecField all_fields0
  let (st:sts) = stanzas all_fields
  pkg <- parseFields basic_field_descrs emptyPackageDescription st
  foldM parseExtraStanza pkg sts
  where
        parseExtraStanza pkg st@((F _lineNo "executable" _eName):_) = do
		exe <- parseFields executableFieldDescrs emptyExecutable st
		return pkg{executables= executables pkg ++ [exe]}
        parseExtraStanza _ x = error ("This shouldn't happen!" ++ show x)

basic_field_descrs :: [FieldDescr PackageDescription]
basic_field_descrs = pkgDescrFieldDescrs ++ map liftToPkg libFieldDescrs
  where liftToPkg = liftField (fromMaybe emptyLibrary . library)
			      (\lib pkg -> pkg{library = Just lib})
-}
stanzas :: [Field] -> [[Field]]
stanzas [] = []
stanzas (f:fields) = (f:this) : stanzas rest
  where 
    (this, rest) = break isStanzaHeader fields

isStanzaHeader :: Field -> Bool
isStanzaHeader (F _ f _) = f == "executable"
isStanzaHeader _ = False

------------------------------------------------------------------------------


mapSimpleFields :: (Field -> ParseResult Field) -> [Field] 
                -> ParseResult [Field]
mapSimpleFields f fs = mapM walk fs
  where
    walk fld@(F _ _ _) = f fld
    walk (IfBlock l c fs1 fs2) = do 
      fs1' <- mapM walk fs1 
      fs2' <- mapM walk fs2
      return (IfBlock l c fs1' fs2')
    walk (Section ln n l fs1) = do
      fs1' <-  mapM walk fs1
      return (Section ln n l fs1')

-- prop_isMapM fs = mapSimpleFields return fs == return fs
      

-- names of fields that represents dependencies, thus consrca
constraintFieldNames :: [String]
constraintFieldNames = ["build-depends"]

-- Possible refactoring would be to have modifiers be explicit about what
-- they add and define an accessor that specifies what the dependencies
-- are.  This way we would completely reuse the parsing knowledge from the
-- field descriptor.
parseConstraint :: Field -> ParseResult [Dependency]
parseConstraint (F l n v) 
    | n == "build-depends" = runP l n (parseCommaList parseDependency) v 
parseConstraint f = bug $ "Constraint was expected (got: " ++ show f ++ ")"

{-
headerFieldNames :: [String]
headerFieldNames = filter (\n -> not (n `elem` constraintFieldNames)) 
                 . map fieldName $ pkgDescrFieldDescrs
-}

libFieldNames :: [String]
libFieldNames = map fieldName libFieldDescrs 
                ++ buildInfoNames ++ constraintFieldNames

-- exeFieldNames :: [String]
-- exeFieldNames = map fieldName executableFieldDescrs 
--                 ++ buildInfoNames

buildInfoNames :: [String]
buildInfoNames = map fieldName binfoFieldDescrs
                ++ map fst deprecatedFieldsBuildInfo


{-
-- Just to make the structure explicit
data CabalFile = MkCabalFile
    { headerFields :: [Field]
    , cfFlags      :: [Flag]
    , exeFields    :: [(String,CondTree ConfVar Dependency Field)]
    , libFields    :: CondTree ConfVar Dependency Field
    } -- deriving Show
-}

-- A minimal implementation of the StateT monad transformer to avoid depending
-- on the 'mtl' package.
newtype StT s m a = StT { runStT :: s -> m (a,s) }

instance Monad m => Monad (StT s m) where
    return a = StT (\s -> return (a,s))
    StT f >>= g = StT $ \s -> do
                        (a,s') <- f s
                        runStT (g a) s'

get :: Monad m => StT s m s
get = StT $ \s -> return (s, s)

modify :: Monad m => (s -> s) -> StT s m ()
modify f = StT $ \s -> return ((),f s)

lift :: Monad m => m a -> StT s m a
lift m = StT $ \s -> m >>= \a -> return (a,s)

evalStT :: Monad m => StT s m a -> s -> m a
evalStT st s = runStT st s >>= return . fst

-- Our monad for parsing a list/tree of fields.
--
-- The state represents the remaining fields to be processed.
type PM a = StT [Field] ParseResult a



-- return look-ahead field or nothing if we're at the end of the file
peekField :: PM (Maybe Field) 
peekField = get >>= return . listToMaybe

-- Unconditionally discard the first field in our state.  Will error when it
-- reaches end of file.  (Yes, that's evil.)
skipField :: PM ()
skipField = modify tail

-- | Parses the given file into a 'GenericPackageDescription'.
--
-- In Cabal 1.2 the syntax for package descriptions was changed to a format
-- with sections and possibly indented property descriptions.  
parseDescription :: String -> ParseResult GenericPackageDescription
parseDescription file = do
    let tabs = findIndentTabs file

    fields0 <- readFields file `catchParseError` \err ->
                 case err of
                   -- In case of a TabsError report them all at once.
                   TabsError _ -> reportTabsError tabs
                   _ -> parseFail err

    -- Parsing might have been successful, but if the new syntax was used with
    -- tabs we can't be quite sure the parse was correct.  (It is possible to
    -- allow tabs in non-indented fields, but that would be inconsistent so we
    -- disallow tabs as indentation alltogether.)
    when (not (oldSyntax fields0) && not (null tabs)) $
      reportTabsError tabs

    let sf = sectionizeFields fields0
    fields <- mapSimpleFields deprecField sf

    flip evalStT fields $ do
      hfs <- getHeader []
      pkg <- lift $ parseFields pkgDescrFieldDescrs emptyPackageDescription hfs
      (flags, mlib, exes) <- getBody
      warnIfRest
      when (not (oldSyntax fields0)) $
        maybeWarnCabalVersion pkg
      return (GenericPackageDescription pkg flags mlib exes)

  where
    oldSyntax flds = all isSimpleField flds
    reportTabsError tabs =
        syntaxError (fst (head tabs)) $
          "Do not use tabs for indentation (use spaces instead)\n"
          ++ "  Tabs were used at (line,column): " ++ show tabs
    maybeWarnCabalVersion pkg =
        when (pkgName (package pkg) /= "Cabal" -- supress warning for Cabal
	   && isAnyVersion (descCabalVersion pkg)) $
          lift $ warning $
            "A package using section syntax should require\n" 
            ++ "\"Cabal-Version: >= 1.2\" or equivalent."

    -- "Sectionize" an old-style Cabal file.  A sectionized file has:
    --
    --  * all global fields at the beginning, followed by
    --  * all flag declarations, followed by
    --  * an optional library section, and
    --  * an arbitrary number of executable sections.
    --
    -- The current implementatition just gathers all library-specific fields
    -- in a library section and wraps all executable stanzas in an executable
    -- section.
    sectionizeFields fs
        | oldSyntax fs =
            let (hdr0, exes0) = break ((=="executable") . fName) fs
                (hdr, libfs0) = partition (not . (`elem` libFieldNames) . fName) hdr0

                -- XXX: In traditional cabal files, dependencies are global.
                -- However, we now have library dependencies and
                -- per-executable dependencies, of which only the library
                -- dependencies are used for flag resolution.
                --
                -- The right solution would be to add global dependencies to
                -- each non-empty section and resolve dependencies for each.
                -- The workaround, for now, is to allow library sections that
                -- only consist of dependency specifications.
                --
                (deps, libfs1) = partition ((`elem` constraintFieldNames) . fName) libfs0
                libfs = if null libfs1 && not (null deps)
                       -- mark library as not buildable 
                        then [F (lineNo (head deps)) "buildable" "False"]
                        else libfs1

                exes = unfoldr toExe exes0
                toExe [] = Nothing
                toExe (F l e n : r) 
                 | e == "executable" = 
                     let (efs, r') = break ((=="executable") . fName) r
                     in Just (Section l "executable" n (deps ++ efs), r')
                toExe _ = bug "unexpeced input to 'toExe'"
            in hdr 
               ++ 
               (if null libfs then [] 
               else [Section (lineNo (head libfs)) "library" "" (deps ++ libfs)])
               ++
               exes
        | otherwise = fs

    isSimpleField (F _ _ _) = True
    isSimpleField _ = False

    -- warn if there's something at the end of the file
    warnIfRest :: PM ()
    warnIfRest = do 
      s <- get
      case s of 
        [] -> return ()
        _ -> lift $ warning "Ignoring trailing declarations."  -- add line no.

    -- all simple fields at the beginning of the file are (considered) header
    -- fields
    getHeader acc = peekField >>= \mf -> case mf of
        Just f@(F _ _ _) -> skipField >> getHeader (f:acc)
        _ -> return (reverse acc)
      
    --
    -- body ::= [ flags ] { library | executable }+   -- at most one lib
    --        
    getBody = do
      mf <- peekField
      case mf of
      --  Just f@(F l n v) 
      --    | n `elem` libFieldNames -> compatParse f -- old-style format
      --    | n == "executable" -> compatParse f
      --    | otherwise -> error $ "???" ++ show f -- XXX
        Just (Section l sn _label _fields) 
          | sn == "flag"    -> do 
              -- don't skipField here.  it's simpler to let getFlags do it
              -- itself
              flags <- getFlags []
              (lib, exes) <- getLibOrExe (Lit True)
              return (flags, lib, exes)
          | sn `elem` ["library", "executable"] -> do 
              (lib,exes) <- getLibOrExe (Lit True)
              return ([], lib, exes)
          | otherwise -> 
              lift $ syntaxError l $ "Unknown section type: " ++ sn
        Nothing -> do lift $ warning "No library or executable specified"
                      return ([], Nothing, [])
        Just f -> lift $ syntaxError (lineNo f) $ 
               "Construct not supported at this position: " ++ show f
    
    -- 
    -- flags ::= "flag:" name { flag_prop } 
    --
    getFlags acc = peekField >>= \mf -> case mf of
        Just (Section _ sn sl fs) 
          | sn == "flag" -> do
              fl <- lift $ parseFields
                      flagFieldDescrs 
                      (MkFlag (map toLower sl) "" True)
                      fs 
              skipField >> getFlags (fl : acc)
        _ -> return (reverse acc)
              
    getLibOrExe cond = peekField >>= \mf -> case mf of
        Just (Section _ sn sl fs)
          | sn == "executable" -> do
              flds <- collectFields parseExeFields fs
              skipField
              (lib, exes) <- getLibOrExe cond
              return (lib, exes ++ [(sl, flds)])
          | sn == "library" -> do
              flds <- collectFields parseLibFields fs
              skipField
              (lib, exes) <- getLibOrExe cond
              return (maybe (Just flds)
                            (const (error "Multiple libraries specified"))
                            lib
                     , exes)
        Just x -> lift $ syntaxError (lineNo x) $ 
                     "Library or Executable section expected."
        Nothing -> return (Nothing, [])

    -- extracts all fields in a block, possibly add dependencies to the
    -- guard condition
    collectFields :: ([Field] -> PM a) -> [Field] 
                  -> PM (CondTree ConfVar [Dependency] a)
    collectFields parser allflds = do
        a <- parser dataFlds
        deps <- liftM concat . mapM (lift . parseConstraint) $ depFlds
        ifs <- mapM processIfs condFlds
        return (CondNode a deps ifs)
      where
        (depFlds, dataFlds) = partition isConstraint simplFlds
        simplFlds = [ F l n v | F l n v <- allflds ]
        condFlds = [ f | f@(IfBlock _ _ _ _) <- allflds ]
        isConstraint (F _ n _) = n `elem` constraintFieldNames
        isConstraint _ = False
        processIfs (IfBlock l c t e) = do
            cnd <- lift $ runP l "if" parseCondition c
            t' <- collectFields parser t
            e' <- case e of
                   [] -> return Nothing
                   es -> do fs <- collectFields parser es
                            return (Just fs)
            return (cnd, t', e')
        processIfs _ = bug "processIfs called with wrong field type"

    parseLibFields = lift . parseFields libFieldDescrs nullLibrary 

    parseExeFields = lift . parseFields executableFieldDescrs nullExecutable


parseFields :: [FieldDescr a] -> a  -> [Field] -> ParseResult a
parseFields descrs ini fields = 
    do (a, unknowns) <- foldM (parseField descrs) (ini, []) fields
       when (not (null unknowns)) $ do
         warning $ render $ 
           text "Unknown fields:" <+> 
                commaSep (map (\(l,u) -> u ++ " (line " ++ show l ++ ")") 
                              (reverse unknowns)) 
           $+$
           text "Fields allowed in this section:" $$ 
             nest 4 (commaSep $ map fieldName descrs)
       return a
  where
    commaSep = fsep . punctuate comma . map text

parseField :: [FieldDescr a] -> (a,[(Int,String)]) -> Field -> ParseResult (a, [(Int,String)])
parseField ((FieldDescr name _ parse):fields) (a, us) (F line f val)
  | name == f = parse line val a >>= \a' -> return (a',us)
  | otherwise = parseField fields (a,us) (F line f val)
-- ignore "x-" extension fields without a warning
parseField [] (a,us) (F _ ('x':'-':_) _) = return (a, us)
parseField [] (a,us) (F l f _) = do
          return (a, ((l,f):us))
parseField _ _ _ = error "'parseField' called on a non-field.  This is a bug."

deprecatedFields :: [(String,String)]
deprecatedFields = 
    deprecatedFieldsPkgDescr ++ deprecatedFieldsBuildInfo

deprecatedFieldsPkgDescr :: [(String,String)]
deprecatedFieldsPkgDescr = [ ("other-files", "extra-source-files") ]

deprecatedFieldsBuildInfo :: [(String,String)]
deprecatedFieldsBuildInfo = [ ("hs-source-dir","hs-source-dirs") ]

-- Handle deprecated fields
deprecField :: Field -> ParseResult Field
deprecField (F line fld val) = do
  fld' <- case lookup fld deprecatedFields of
            Nothing -> return fld
            Just newName -> do
              warning $ "The field \"" ++ fld
                      ++ "\" is deprecated, please use \"" ++ newName ++ "\""
              return newName
  return (F line fld' val)
deprecField _ = error "'deprecField' called on a non-field.  This is a bug."

   
parseHookedBuildInfo :: String -> ParseResult HookedBuildInfo
parseHookedBuildInfo inp = do
  fields <- readFields inp
  let ss@(mLibFields:exes) = stanzas fields
  mLib <- parseLib mLibFields
  biExes <- mapM parseExe (maybe ss (const exes) mLib)
  return (mLib, biExes)
  where
    parseLib :: [Field] -> ParseResult (Maybe BuildInfo)
    parseLib (bi@((F _ inFieldName _):_))
        | map toLower inFieldName /= "executable" = liftM Just (parseBI bi)
    parseLib _ = return Nothing

    parseExe :: [Field] -> ParseResult (String, BuildInfo)
    parseExe ((F line inFieldName mName):bi)
        | map toLower inFieldName == "executable"
            = do bis <- parseBI bi
                 return (mName, bis)
        | otherwise = syntaxError line "expecting 'executable' at top of stanza"
    parseExe (_:_) = error "`parseExe' called on a non-field.  This is a bug."
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
ppFields pkg' ((FieldDescr name getter _):flds) =
     ppField name (getter pkg') $$ ppFields pkg' flds

ppField :: String -> Doc -> Doc
ppField name fielddoc = text name <> colon <+> fielddoc

-- replace all tabs used as indentation with whitespace, also return where
-- tabs were found
findIndentTabs :: String -> [(Int,Int)]
findIndentTabs = concatMap checkLine
               . zip [1..]
               . lines
    where
      checkLine (lineno, l) =
          let (indent, _content) = span isSpace l
              tabCols = map fst . filter ((== '\t') . snd) . zip [0..]
              addLineNo = map (\col -> (lineno,col))
          in addLineNo (tabCols indent)

#ifdef DEBUG
test_findIndentTabs = findIndentTabs $ unlines $
    [ "foo", "  bar", " \t baz", "\t  biz\t", "\t\t \t mib" ]
#endif

-- ------------------------------------------------------------
-- * Sanity Checking
-- ------------------------------------------------------------

-- |Sanity check this description file.

-- FIX: add a sanity check for missing haskell files? That's why its
-- in the IO monad.

sanityCheckPackage :: PackageDescription -> IO ([String] -- Warnings
                                               ,[String])-- Errors
sanityCheckPackage pkg_descr = 
    let libSane   = sanityCheckLib (library pkg_descr)
        nothingToDo = checkSanity
                        (null (executables pkg_descr) 
                         && isNothing (library pkg_descr))
                        "No executables and no library found. Nothing to do."
        noModules = checkSanity (hasMods pkg_descr)
                      "No exposed modules or executables in this package."
        noLicenseFile = checkSanity (null $ licenseFile pkg_descr)
                          "No license-file field."
        goodCabal = let v = (descCabalVersion pkg_descr)
                    in checkSanity (not $ cabalVersion  `withinRange` v)
                           ("This package requires Cabal version: " 
                              ++ (showVersionRange v) ++ ".")
    in return $ ( catMaybes [nothingToDo, noModules, noLicenseFile],
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
      toMaybe (buildable (libBuildInfo l) && null (exposedModules l))
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

bug :: String -> a
bug msg = error $ msg ++ ". Consider this a bug."


-- ------------------------------------------------------------
-- * Testing
-- ------------------------------------------------------------
#ifdef DEBUG
-- disabled for now

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
      buildType = Custom,
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
    gpd <- parseDescription descr
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
------------------------------------------------------------------------------

test_stanzas' = parseDescription testFile
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
          ppd <- parseDescription' fs
          let Right (pd,_) = finalizePackageDescription [] (Just pkgs) os arch ppd
          pdold <- parseDescription testPkgDesc
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
    case parseDescription testFile of
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

