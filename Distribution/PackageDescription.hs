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
        emptyPackageDescription,
        readPackageDescription,
        writePackageDescription,
        parsePackageDescription,
        showPackageDescription,
        BuildType(..),
        knownBuildTypes,

        -- ** Libraries
        Library(..),
        emptyLibrary,
        withLib,
        hasLibs,
        libModules,

        -- ** Executables
        Executable(..),
        emptyExecutable,
        withExe,
        hasExes,
        exeModules,

        -- ** Parsing
        FieldDescr(..),
        LineNo,

        -- * Build information
        BuildInfo(..),
        emptyBuildInfo,
        allBuildInfo,
        hcOptions,

        -- ** Supplementary build information
        HookedBuildInfo,
        emptyHookedBuildInfo,
        updatePackageDescription,

        -- * package configuration
        Flag(..), FlagName(..), FlagAssignment,
        CondTree(..), ConfVar(..), Condition(..),
        freeVars,

        -- ** Supplementary build information
        readHookedBuildInfo,
        parseHookedBuildInfo,
        writeHookedBuildInfo,
        showHookedBuildInfo,        

        -- * Deprecated compat stuff
        ParseResult(..),
        setupMessage,
        cabalVersion,
  ) where

import Data.Maybe (listToMaybe)
import Data.List  (nub, unfoldr, partition, (\\))
import Data.Monoid (Monoid(mempty, mappend))
import Text.PrettyPrint.HughesPJ as Disp
import qualified Distribution.Compat.ReadP as Parse
import Distribution.Compat.ReadP ((+++))
import qualified Data.Char as Char (isAlphaNum, isSpace)
import Control.Monad (liftM, foldM, when, unless)
import System.Directory (doesFileExist)

import Distribution.Package
         ( PackageIdentifier(..), Dependency, Package(..)
         , parsePackageName, packageName, packageVersion )
import Distribution.Version
         ( Version(Version), VersionRange(AnyVersion)
         , isAnyVersion, withinRange )
import Distribution.License  (License(AllRightsReserved))
import Distribution.Compiler (CompilerFlavor(..))
import Distribution.System   (OS, Arch)
import Distribution.ParseUtils
import Distribution.Text
         ( Text(..), display, simpleParse )
import Distribution.Simple.Utils
         ( notice, die, dieWithLocation, warn, intercalate
         , lowercase, cabalVersion, readUTF8File, writeUTF8File )
import Language.Haskell.Extension (Extension)
import Distribution.Verbosity (Verbosity)


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
        customFieldsPD :: [(String,String)], -- ^Custom fields starting 
                                             -- with x-, stored in a 
                                             -- simple assoc-list.
        buildDepends   :: [Dependency],
        descCabalVersion :: VersionRange, -- ^If this package depends on a specific version of Cabal, give that here.
        buildType      :: Maybe BuildType,
        -- components
        library        :: Maybe Library,
        executables    :: [Executable],
        dataFiles      :: [FilePath],
        dataDir        :: FilePath,
        extraSrcFiles  :: [FilePath],
        extraTmpFiles  :: [FilePath]
    }
    deriving (Show, Read, Eq)

instance Package PackageDescription where
  packageId = package

emptyPackageDescription :: PackageDescription
emptyPackageDescription
    =  PackageDescription {package      = PackageIdentifier "" (Version [] []),
                      license      = AllRightsReserved,
                      licenseFile  = "",
                      descCabalVersion = AnyVersion,
                      buildType    = Nothing,
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
                      customFieldsPD = [],
                      library      = Nothing,
                      executables  = [],
                      dataFiles    = [],
                      dataDir      = "",
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
  | UnknownBuildType String
                -- ^ a package that uses an unknown build type cannot actually
                --   be built. Doing it this way rather than just giving a
                --   parse error means we get better error messages and allows
                --   you to inspect the rest of the package description.
                deriving (Show, Read, Eq)

knownBuildTypes :: [BuildType]
knownBuildTypes = [Simple, Configure, Make, Custom]

instance Text BuildType where
  disp (UnknownBuildType other) = Disp.text other
  disp other                    = Disp.text (show other)

  parse = do
    name <- Parse.munch1 Char.isAlphaNum
    return $ case name of
      "Simple"    -> Simple
      "Configure" -> Configure
      "Custom"    -> Custom
      "Make"      -> Make
      _           -> UnknownBuildType name

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
   library p >>= \lib -> if buildable (libBuildInfo lib)
                           then Just lib
                           else Nothing

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

unionLibrary :: Library -> Library -> Library
unionLibrary l1 l2 =
    l1 { exposedModules = combine exposedModules
       , libBuildInfo = unionBuildInfo (libBuildInfo l1) (libBuildInfo l2)
       }
  where combine f = f l1 ++ f l2

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

-- |does this package have any executables?
hasExes :: PackageDescription -> Bool
hasExes p = any (buildable . buildInfo) (executables p)

-- | Perform the action on each buildable 'Executable' in the package
-- description.
withExe :: PackageDescription -> (Executable -> IO a) -> IO ()
withExe pkg_descr f =
  sequence_ [f exe | exe <- executables pkg_descr, buildable (buildInfo exe)]

-- |Get all the module names from the exes in this package
exeModules :: PackageDescription -> [String]
exeModules PackageDescription{executables=execs}
    = concatMap (otherModules . buildInfo) execs

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
        buildTools        :: [Dependency], -- ^ tools needed to build this bit
	cppOptions        :: [String],  -- ^ options for pre-processing Haskell code
        ccOptions         :: [String],  -- ^ options for C compiler
        ldOptions         :: [String],  -- ^ options for linker
        pkgconfigDepends  :: [Dependency], -- ^ pkg-config packages that are used
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
        ghcProfOptions    :: [String],
        ghcSharedOptions  :: [String],
        customFieldsBI    :: [(String,String)]  -- ^Custom fields starting
                                                -- with x-, stored in a
                                                -- simple assoc-list.  
    }
    deriving (Show,Read,Eq)

instance Monoid BuildInfo where
    mempty = nullBuildInfo
    mappend = unionBuildInfo

nullBuildInfo :: BuildInfo
nullBuildInfo = BuildInfo {
                      buildable         = True,
                      buildTools        = [],
                      cppOptions        = [],
                      ccOptions         = [],
                      ldOptions         = [],
                      pkgconfigDepends  = [],
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
                      ghcProfOptions    = [],
                      ghcSharedOptions  = [],
                      customFieldsBI    = []
                     }

emptyBuildInfo :: BuildInfo
emptyBuildInfo = nullBuildInfo

-- | The 'BuildInfo' for the library (if there is one and it's buildable) and
-- all the buildable executables. Useful for gathering dependencies.
allBuildInfo :: PackageDescription -> [BuildInfo]
allBuildInfo pkg_descr = [ bi | Just lib <- [library pkg_descr]
                              , let bi = libBuildInfo lib
                              , buildable bi ]
                      ++ [ bi | exe <- executables pkg_descr
                              , let bi = buildInfo exe
                              , buildable bi ]

type HookedBuildInfo = (Maybe BuildInfo, [(String, BuildInfo)])

emptyHookedBuildInfo :: HookedBuildInfo
emptyHookedBuildInfo = (Nothing, [])

-- |Select options for a particular Haskell compiler.
hcOptions :: CompilerFlavor -> BuildInfo -> [String]
hcOptions hc bi = [ opt | (hc',opts) <- options bi
                        , hc' == hc
                        , opt <- opts ]

-- ------------------------------------------------------------
-- * Utils
-- ------------------------------------------------------------

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
    = BuildInfo {
         buildable         = buildable b1 && buildable b2,
         buildTools        = combineNub buildTools,
         cppOptions        = combine    cppOptions,
         ccOptions         = combine    ccOptions,
         ldOptions         = combine    ldOptions,
         pkgconfigDepends  = combineNub pkgconfigDepends,
         frameworks        = combineNub frameworks,
         cSources          = combineNub cSources,
         hsSourceDirs      = combineNub hsSourceDirs,
         otherModules      = combineNub otherModules,
         extensions        = combineNub extensions,
         extraLibs         = combine    extraLibs,
         extraLibDirs      = combineNub extraLibDirs,
         includeDirs       = combineNub includeDirs,
         includes          = combineNub includes,
         installIncludes   = combineNub installIncludes,
         options           = combine    options,
         ghcProfOptions    = combine    ghcProfOptions,
         ghcSharedOptions  = combine    ghcSharedOptions,
         customFieldsBI    = combine    customFieldsBI
        }
  where
    combine    f = f b1 ++ f b2
    combineNub f = nub (combine f)

-- ---------------------------------------------------------------------------
-- The GenericPackageDescription type

data GenericPackageDescription =
    GenericPackageDescription {
        packageDescription :: PackageDescription,
        genPackageFlags       :: [Flag],
        condLibrary        :: Maybe (CondTree ConfVar [Dependency] Library),
        condExecutables    :: [(String, CondTree ConfVar [Dependency] Executable)]
      }
    deriving (Show)

instance Package GenericPackageDescription where
  packageId = packageId . packageDescription

{-
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
-}

-- | A flag can represent a feature to be included, or a way of linking
--   a target against its dependencies, or in fact whatever you can think of.
data Flag = MkFlag
    { flagName        :: FlagName
    , flagDescription :: String
    , flagDefault     :: Bool
    }
    deriving Show

-- | A 'FlagName' is the name of a user-defined configuration flag
newtype FlagName = FlagName String
    deriving (Eq, Ord, Show, Read)

-- | A 'FlagAssignment' is a total or partial mapping of 'FlagName's to
-- 'Bool' flag values. It represents the flags chosen by the user or
-- discovered during configuration. For example @--flags=foo --flags=-bar@
-- becomes @[("foo", True), ("bar", False)]@
--
type FlagAssignment = [(FlagName, Bool)]

-- | A @ConfVar@ represents the variable type used.
data ConfVar = OS OS
             | Arch Arch
             | Flag FlagName
             | Impl CompilerFlavor VersionRange
    deriving (Eq, Show)

--instance Text ConfVar where
--    disp (OS os) = "os(" ++ display os ++ ")"
--    disp (Arch arch) = "arch(" ++ display arch ++ ")"
--    disp (Flag (ConfFlag f)) = "flag(" ++ f ++ ")"
--    disp (Impl c v) = "impl(" ++ display c
--                       ++ " " ++ display v ++ ")"

-- | A boolean expression parameterized over the variable type used.
data Condition c = Var c
                 | Lit Bool
                 | CNot (Condition c)
                 | COr (Condition c) (Condition c)
                 | CAnd (Condition c) (Condition c)
    deriving Show

--instance Text c => Text (Condition c) where
--  disp (Var x) = text (show x)
--  disp (Lit b) = text (show b)
--  disp (CNot c) = char '!' <> parens (ppCond c)
--  disp (COr c1 c2) = parens $ sep [ppCond c1, text "||" <+> ppCond c2]
--  disp (CAnd c1 c2) = parens $ sep [ppCond c1, text "&&" <+> ppCond c2]

data CondTree v c a = CondNode
    { condTreeData        :: a
    , condTreeConstraints :: c
    , condTreeComponents  :: [( Condition v
                              , CondTree v c a
                              , Maybe (CondTree v c a))]
    }
    deriving Show

--instance (Text v, Text c) => Text (CondTree v c a) where
--  disp (CondNode _dat cs ifs) =
--    (text "build-depends: " <+>
--      disp cs)
--    $+$
--    (vcat $ map ppIf ifs)
--  where
--    ppIf (c,thenTree,mElseTree) =
--        ((text "if" <+> ppCond c <> colon) $$
--          nest 2 (ppCondTree thenTree disp))
--        $+$ (maybe empty (\t -> text "else: " $$ nest 2 (ppCondTree t disp))
--                   mElseTree)

freeVars :: CondTree ConfVar c a  -> [FlagName]
freeVars t = [ f | Flag f <- freeVars' t ]
  where
    freeVars' (CondNode _ _ ifs) = concatMap compfv ifs
    compfv (c, ct, mct) = condfv c ++ freeVars' ct ++ maybe [] freeVars' mct
    condfv c = case c of
      Var v      -> [v]
      Lit _      -> []
      CNot c'    -> condfv c'
      COr c1 c2  -> condfv c1 ++ condfv c2
      CAnd c1 c2 -> condfv c1 ++ condfv c2

-- ---------------------------------------------------------------------------
-- Deprecated compat stuff

{-# DEPRECATED setupMessage "it's exported from the Utils module now" #-}
setupMessage :: Verbosity -> String -> PackageDescription -> IO ()
setupMessage verbosity msg pkg_descr =
    notice verbosity (msg ++ ' ': display (packageId pkg_descr) ++ "...")

-- -----------------------------------------------------------------------------
-- The PackageDescription type

pkgDescrFieldDescrs :: [FieldDescr PackageDescription]
pkgDescrFieldDescrs =
    [ simpleField "name"
           text                   parsePackageName
           packageName            (\name pkg -> pkg{package=(package pkg){pkgName=name}})
 , simpleField "version"
           disp                   parse
           packageVersion         (\ver pkg -> pkg{package=(package pkg){pkgVersion=ver}})
 , simpleField "cabal-version"
           disp                   parse
           descCabalVersion       (\v pkg -> pkg{descCabalVersion=v})
 , simpleField "build-type"
           (maybe empty disp)     (fmap Just parse)
           buildType              (\t pkg -> pkg{buildType=t})
 , simpleField "license"
           disp                   parseLicenseQ
           license                (\l pkg -> pkg{license=l})
 , simpleField "license-file"
           showFilePath           parseFilePathQ
           licenseFile            (\l pkg -> pkg{licenseFile=l})
 , simpleField "copyright"
           showFreeText           (Parse.munch (const True))
           copyright              (\val pkg -> pkg{copyright=val})
 , simpleField "maintainer"
           showFreeText           (Parse.munch (const True))
           maintainer             (\val pkg -> pkg{maintainer=val})
 , commaListField  "build-depends"
           disp                   parse
           buildDepends           (\xs    pkg -> pkg{buildDepends=xs})
 , simpleField "stability"
           showFreeText           (Parse.munch (const True))
           stability              (\val pkg -> pkg{stability=val})
 , simpleField "homepage"
           showFreeText           (Parse.munch (const True))
           homepage               (\val pkg -> pkg{homepage=val})
 , simpleField "package-url"
           showFreeText           (Parse.munch (const True))
           pkgUrl                 (\val pkg -> pkg{pkgUrl=val})
 , simpleField "synopsis"
           showFreeText           (Parse.munch (const True))
           synopsis               (\val pkg -> pkg{synopsis=val})
 , simpleField "description"
           showFreeText           (Parse.munch (const True))
           description            (\val pkg -> pkg{description=val})
 , simpleField "category"
           showFreeText           (Parse.munch (const True))
           category               (\val pkg -> pkg{category=val})
 , simpleField "author"
           showFreeText           (Parse.munch (const True))
           author                 (\val pkg -> pkg{author=val})
 , listField "tested-with"
           showTestedWith         parseTestedWithQ
           testedWith             (\val pkg -> pkg{testedWith=val})
 , listField "data-files"  
           showFilePath           parseFilePathQ
           dataFiles              (\val pkg -> pkg{dataFiles=val})
 , simpleField "data-dir"
           showFilePath           parseFilePathQ
           dataDir                (\val pkg -> pkg{dataDir=val})
 , listField "extra-source-files" 
           showFilePath    parseFilePathQ
           extraSrcFiles          (\val pkg -> pkg{extraSrcFiles=val})
 , listField "extra-tmp-files" 
           showFilePath       parseFilePathQ
           extraTmpFiles          (\val pkg -> pkg{extraTmpFiles=val})
 ]

-- | Store any fields beginning with "x-" in the customFields field of
--   a PackageDescription.  All other fields will generate a warning.
storeXFieldsPD :: UnrecFieldParser PackageDescription
storeXFieldsPD (f@('x':'-':_),val) pkg = Just pkg{ customFieldsPD = (f,val):(customFieldsPD pkg) }
storeXFieldsPD _ _ = Nothing

-- ---------------------------------------------------------------------------
-- The Library type

libFieldDescrs :: [FieldDescr Library]
libFieldDescrs = map biToLib binfoFieldDescrs
  ++ [
      listField "exposed-modules" text parseModuleNameQ
	 exposedModules (\mods lib -> lib{exposedModules=mods})
     ]
  where biToLib = liftField libBuildInfo (\bi lib -> lib{libBuildInfo=bi})

storeXFieldsLib :: UnrecFieldParser Library
storeXFieldsLib (f@('x':'-':_), val) l@(Library { libBuildInfo = bi }) = 
    Just $ l {libBuildInfo = bi{ customFieldsBI = (f,val):(customFieldsBI bi) }}
storeXFieldsLib _ _ = Nothing

-- ---------------------------------------------------------------------------
-- The Executable type


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

storeXFieldsExe :: UnrecFieldParser Executable
storeXFieldsExe (f@('x':'-':_), val) e@(Executable { buildInfo = bi }) =
    Just $ e {buildInfo = bi{ customFieldsBI = (f,val):(customFieldsBI bi)}}
storeXFieldsExe _ _ = Nothing
  
-- ---------------------------------------------------------------------------
-- The BuildInfo type


binfoFieldDescrs :: [FieldDescr BuildInfo]
binfoFieldDescrs =
 [ boolField "buildable"
           buildable          (\val binfo -> binfo{buildable=val})
 , commaListField  "build-tools"
           disp               parseBuildTool
           buildTools         (\xs  binfo -> binfo{buildTools=xs})
 , listField "cpp-options"
           showToken          parseTokenQ
           cppOptions          (\val binfo -> binfo{cppOptions=val})
 , listField "cc-options"
           showToken          parseTokenQ
           ccOptions          (\val binfo -> binfo{ccOptions=val})
 , listField "ld-options"
           showToken          parseTokenQ
           ldOptions          (\val binfo -> binfo{ldOptions=val})
 , commaListField  "pkgconfig-depends"
           disp               parsePkgconfigDependency
           pkgconfigDepends   (\xs  binfo -> binfo{pkgconfigDepends=xs})
 , listField "frameworks"
           showToken          parseTokenQ
           frameworks         (\val binfo -> binfo{frameworks=val})
 , listField   "c-sources"
           showFilePath       parseFilePathQ
           cSources           (\paths binfo -> binfo{cSources=paths})
 , listField   "extensions"
           disp               parseExtensionQ
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
 , listField   "ghc-shared-options"
           text               parseTokenQ
           ghcProfOptions        (\val binfo -> binfo{ghcSharedOptions=val})
 , optsField   "ghc-options"  GHC
           options            (\path  binfo -> binfo{options=path})
 , optsField   "hugs-options" Hugs
           options            (\path  binfo -> binfo{options=path})
 , optsField   "nhc98-options"  NHC
           options            (\path  binfo -> binfo{options=path})
 , optsField   "jhc-options"  JHC
           options            (\path  binfo -> binfo{options=path})
 ]

storeXFieldsBI :: UnrecFieldParser BuildInfo
storeXFieldsBI (f@('x':'-':_),val) bi = Just bi{ customFieldsBI = (f,val):(customFieldsBI bi) }
storeXFieldsBI _ _ = Nothing

------------------------------------------------------------------------------

flagFieldDescrs :: [FieldDescr Flag]
flagFieldDescrs =
    [ simpleField "description"
        showFreeText     (Parse.munch (const True))
        flagDescription  (\val fl -> fl{ flagDescription = val })
    , boolField "default"
        flagDefault      (\val fl -> fl{ flagDefault = val })
    ]

-- ---------------------------------------------------------------
-- Parsing

-- | Given a parser and a filename, return the parse of the file,
-- after checking if the file exists.
readAndParseFile :: (FilePath -> IO String)
                 -> (String -> ParseResult a)
                 -> Verbosity
                 -> FilePath -> IO a
readAndParseFile readFile' parser verbosity fpath = do
  exists <- doesFileExist fpath
  when (not exists) (die $ "Error Parsing: file \"" ++ fpath ++ "\" doesn't exist. Cannot continue.")
  str <- readFile' fpath
  case parser str of
    ParseFailed e -> do
        let (line, message) = locatedErrorMsg e
        dieWithLocation fpath line message
    ParseOk warnings x -> do
        mapM_ (warn verbosity . showPWarning fpath) $ reverse warnings
        return x

readHookedBuildInfo :: Verbosity -> FilePath -> IO HookedBuildInfo
readHookedBuildInfo =
    readAndParseFile readFile parseHookedBuildInfo

-- |Parse the given package file.
readPackageDescription :: Verbosity -> FilePath -> IO GenericPackageDescription
readPackageDescription =
    readAndParseFile readUTF8File parsePackageDescription

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
    | n == "build-depends" = runP l n (parseCommaList parse) v
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
parsePackageDescription :: String -> ParseResult GenericPackageDescription
parsePackageDescription file = do
    let tabs = findIndentTabs file

    fields0 <- readFields file `catchParseError` \err ->
                 case err of
                   -- In case of a TabsError report them all at once.
                   TabsError tabLineNo -> reportTabsError
                   -- but only report the ones including and following
                   -- the one that caused the actual error
                                            [ t | t@(lineNo',_) <- tabs
                                                , lineNo' >= tabLineNo ]
                   _ -> parseFail err

    let cabalVersionNeeded =
          head $ [ versionRange
                 | Just versionRange <- [ simpleParse v
                                        | F _ "cabal-version" v <- fields0 ] ]
              ++ [AnyVersion]
    handleFutureVersionParseFailure cabalVersionNeeded $ do

      let sf = sectionizeFields fields0
      fields <- mapSimpleFields deprecField sf

      flip evalStT fields $ do
        hfs <- getHeader []
        pkg <- lift $ parseFields pkgDescrFieldDescrs storeXFieldsPD emptyPackageDescription hfs
        (flags, mlib, exes) <- getBody
        warnIfRest
        when (not (oldSyntax fields0)) $
          maybeWarnCabalVersion pkg
        checkForUndefinedFlags flags mlib exes
        return (GenericPackageDescription pkg flags mlib exes)

  where
    oldSyntax flds = all isSimpleField flds
    reportTabsError tabs =
        syntaxError (fst (head tabs)) $
          "Do not use tabs for indentation (use spaces instead)\n"
          ++ "  Tabs were used at (line,column): " ++ show tabs
    maybeWarnCabalVersion pkg =
        when (packageName pkg /= "Cabal" -- supress warning for Cabal
	   && isAnyVersion (descCabalVersion pkg)) $
          lift $ warning $
            "A package using section syntax should require\n" 
            ++ "\"Cabal-Version: >= 1.2\" or equivalent."

    handleFutureVersionParseFailure cabalVersionNeeded parseBody =
      (unless versionOk (warning message) >> parseBody)
        `catchParseError` \parseError -> case parseError of
        TabsError _   -> parseFail parseError
        _ | versionOk -> parseFail parseError
          | otherwise -> fail message
      where versionOk = cabalVersion `withinRange` cabalVersionNeeded
            message   = "This package requires Cabal version: "
                     ++ display cabalVersionNeeded

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
    sectionizeFields :: [Field] -> [Field]
    sectionizeFields fs
      | oldSyntax fs =
          let 
            -- "build-depends" is a local field now.  To be backwards
            -- compatible, we still allow it as a global field in old-style
            -- package description files and translate it to a local field by
            -- adding it to every non-empty section
            (hdr0, exes0) = break ((=="executable") . fName) fs
            (hdr, libfs0) = partition (not . (`elem` libFieldNames) . fName) hdr0

            (deps, libfs) = partition ((== "build-depends") . fName)
                                       libfs0

            exes = unfoldr toExe exes0
            toExe [] = Nothing
            toExe (F l e n : r) 
              | e == "executable" = 
                  let (efs, r') = break ((=="executable") . fName) r
                  in Just (Section l "executable" n (deps ++ efs), r')
            toExe _ = bug "unexpeced input to 'toExe'"
          in 
            hdr ++ 
           (if null libfs then [] 
            else [Section (lineNo (head libfs)) "library" "" (deps ++ libfs)])
            ++ exes
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
    getHeader :: [Field] -> PM [Field]
    getHeader acc = peekField >>= \mf -> case mf of
        Just f@(F _ _ _) -> skipField >> getHeader (f:acc)
        _ -> return (reverse acc)
      
    --
    -- body ::= flag* { library | executable }+   -- at most one lib
    --        
    -- The body consists of an optional sequence of flag declarations and after
    -- that an arbitrary number of executables and an optional library.  The 
    -- order of the latter doesn't play a role.
    getBody :: PM ([Flag]
                  ,Maybe (CondTree ConfVar [Dependency] Library)
                  ,[(String, CondTree ConfVar [Dependency] Executable)])
    getBody = do
      mf <- peekField
      case mf of
        Just (Section _ sn _label _fields) 
          | sn == "flag"    -> do 
              -- don't skipField here.  it's simpler to let getFlags do it
              -- itself
              flags <- getFlags []
              (lib, exes) <- getLibOrExe
              return (flags, lib, exes)
          | otherwise -> do 
              (lib,exes) <- getLibOrExe
              return ([], lib, exes)
        Nothing -> do lift $ warning "No library or executable specified"
                      return ([], Nothing, [])
        Just f -> lift $ syntaxError (lineNo f) $ 
               "Construct not supported at this position: " ++ show f
    
    -- 
    -- flags ::= "flag:" name { flag_prop } 
    --
    getFlags :: [Flag] -> StT [Field] ParseResult [Flag]
    getFlags acc = peekField >>= \mf -> case mf of
        Just (Section _ sn sl fs) 
          | sn == "flag" -> do
              fl <- lift $ parseFields
                      flagFieldDescrs
                      warnUnrec
                      (MkFlag (FlagName (lowercase sl)) "" True)
                      fs 
              skipField >> getFlags (fl : acc)
        _ -> return (reverse acc)

    getLibOrExe :: PM (Maybe (CondTree ConfVar [Dependency] Library)
                      ,[(String, CondTree ConfVar [Dependency] Executable)])
    getLibOrExe = peekField >>= \mf -> case mf of
        Just (Section n sn sl fs)
          | sn == "executable" -> do
              when (null sl) $ lift $
                syntaxError n "'executable' needs one argument (the executable's name)"
              exename <- lift $ runP n "executable" parseTokenQ sl
              flds <- collectFields parseExeFields fs
              skipField
              (lib, exes) <- getLibOrExe
              return (lib, exes ++ [(exename, flds)])
          | sn == "library" -> do
              when (not (null sl)) $ lift $
                syntaxError n "'library' expects no argument"
              flds <- collectFields parseLibFields fs
              skipField
              (lib, exes) <- getLibOrExe
              return (maybe (Just flds)
                            (const (error "Multiple libraries specified"))
                            lib
                     , exes)
          | otherwise -> do
              lift $ warning $ "Unknown section type: " ++ sn ++ " ignoring..."
              return (Nothing, []) -- yep
        Just x -> lift $ syntaxError (lineNo x) $ "Section expected."
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

    parseLibFields :: [Field] -> StT s ParseResult Library
    parseLibFields = lift . parseFields libFieldDescrs storeXFieldsLib emptyLibrary 

    parseExeFields :: [Field] -> StT s ParseResult Executable
    parseExeFields = lift . parseFields executableFieldDescrs storeXFieldsExe emptyExecutable

    checkForUndefinedFlags ::
        [Flag] ->
        Maybe (CondTree ConfVar [Dependency] Library) ->
        [(String, CondTree ConfVar [Dependency] Executable)] ->
        PM ()
    checkForUndefinedFlags flags mlib exes = do
        let definedFlags = map flagName flags
        maybe (return ()) (checkCondTreeFlags definedFlags) mlib
        mapM_ (checkCondTreeFlags definedFlags . snd) exes

    checkCondTreeFlags :: [FlagName] -> CondTree ConfVar c a -> PM ()
    checkCondTreeFlags definedFlags ct = do
        let fv = nub $ freeVars ct
        when (not . all (`elem` definedFlags) $ fv) $
            fail $ "These flags are used without having been defined: "
                ++ intercalate ", " [ n | FlagName n <- fv \\ definedFlags ]


-- | Parse a list of fields, given a list of field descriptions,
--   a structure to accumulate the parsed fields, and a function
--   that can decide what to do with fields which don't match any
--   of the field descriptions.  
parseFields :: [FieldDescr a]      -- ^ list of parseable fields
            -> UnrecFieldParser a  -- ^ possibly do something with
                                   --   unrecognized fields
            -> a                   -- ^ accumulator
            -> [Field]             -- ^ fields to be parsed
            -> ParseResult a
parseFields descrs unrec ini fields = 
    do (a, unknowns) <- foldM (parseField descrs unrec) (ini, []) fields
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

parseField :: [FieldDescr a]     -- ^ list of parseable fields
           -> UnrecFieldParser a -- ^ possibly do something with
                                 --   unrecognized fields
           -> (a,[(Int,String)]) -- ^ accumulated result and warnings
           -> Field              -- ^ the field to be parsed
           -> ParseResult (a, [(Int,String)])
parseField ((FieldDescr name _ parser):fields) unrec (a, us) (F line f val)
  | name == f = parser line val a >>= \a' -> return (a',us)
  | otherwise = parseField fields unrec (a,us) (F line f val)
parseField [] unrec (a,us) (F l f val) = return $
  case unrec (f,val) a of        -- no fields matched, see if the 'unrec'
    Just a' -> (a',us)           -- function wants to do anything with it
    Nothing -> (a, ((l,f):us))
parseField _ _ _ _ = bug "'parseField' called on a non-field"

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
deprecField _ = bug "'deprecField' called on a non-field"

   
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
        | lowercase inFieldName /= "executable" = liftM Just (parseBI bi)
    parseLib _ = return Nothing

    parseExe :: [Field] -> ParseResult (String, BuildInfo)
    parseExe ((F line inFieldName mName):bi)
        | lowercase inFieldName == "executable"
            = do bis <- parseBI bi
                 return (mName, bis)
        | otherwise = syntaxError line "expecting 'executable' at top of stanza"
    parseExe (_:_) = bug "`parseExe' called on a non-field"
    parseExe [] = syntaxError 0 "error in parsing buildinfo file. Expected executable stanza"

    parseBI st = parseFields binfoFieldDescrs storeXFieldsBI emptyBuildInfo st

-- | Parse a configuration condition from a string.
parseCondition :: Parse.ReadP r (Condition ConfVar)
parseCondition = condOr
  where
    condOr   = Parse.sepBy1 condAnd (oper "||") >>= return . foldl1 COr
    condAnd  = Parse.sepBy1 cond (oper "&&")>>= return . foldl1 CAnd
    cond     = sp >> (boolLiteral +++ inparens condOr +++ notCond +++ osCond
                      +++ archCond +++ flagCond +++ implCond )
    inparens   = Parse.between (Parse.char '(' >> sp) (sp >> Parse.char ')' >> sp)
    notCond  = Parse.char '!' >> sp >> cond >>= return . CNot
    osCond   = Parse.string "os" >> sp >> inparens osIdent >>= return . Var
    archCond = Parse.string "arch" >> sp >> inparens archIdent >>= return . Var
    flagCond = Parse.string "flag" >> sp >> inparens flagIdent >>= return . Var
    implCond = Parse.string "impl" >> sp >> inparens implIdent >>= return . Var
    boolLiteral   = fmap Lit  parse
    archIdent     = fmap Arch parse
    osIdent       = fmap OS   parse
    flagIdent     = fmap (Flag . FlagName . lowercase) (Parse.munch1 isIdentChar)
    isIdentChar c = Char.isAlphaNum c || c == '_' || c == '-'
    oper s        = sp >> Parse.string s >> sp
    sp            = Parse.skipSpaces
    implIdent     = do i <- parse
                       vr <- sp >> Parse.option AnyVersion parse
                       return $ Impl i vr

-- ---------------------------------------------------------------------------
-- Pretty printing

writePackageDescription :: FilePath -> PackageDescription -> IO ()
writePackageDescription fpath pkg = writeUTF8File fpath (showPackageDescription pkg)

showPackageDescription :: PackageDescription -> String
showPackageDescription pkg = render $
  ppFields pkg pkgDescrFieldDescrs $$
  ppCustomFields (customFieldsPD pkg) $$
  (case library pkg of
     Nothing  -> empty
     Just lib -> ppFields lib libFieldDescrs) $$
  vcat (map ppExecutable (executables pkg))
  where
    ppExecutable exe = space $$ ppFields exe executableFieldDescrs

ppCustomFields :: [(String,String)] -> Doc
ppCustomFields flds = vcat (map ppCustomField flds)

ppCustomField :: (String,String) -> Doc
ppCustomField (name,val) = text name <> colon <+> showFreeText val

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
      ppFields bi binfoFieldDescrs $$
      ppCustomFields (customFieldsBI bi)

-- replace all tabs used as indentation with whitespace, also return where
-- tabs were found
findIndentTabs :: String -> [(Int,Int)]
findIndentTabs = concatMap checkLine
               . zip [1..]
               . lines
    where
      checkLine (lineno, l) =
          let (indent, _content) = span Char.isSpace l
              tabCols = map fst . filter ((== '\t') . snd) . zip [0..]
              addLineNo = map (\col -> (lineno,col))
          in addLineNo (tabCols indent)

--test_findIndentTabs = findIndentTabs $ unlines $
--    [ "foo", "  bar", " \t baz", "\t  biz\t", "\t\t \t mib" ]

bug :: String -> a
bug msg = error $ msg ++ ". Consider this a bug."
