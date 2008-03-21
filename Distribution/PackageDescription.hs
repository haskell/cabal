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
        BuildType(..),

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
        GenericPackageDescription(..),
        Flag(..), CondTree(..), ConfVar(..), ConfFlag(..), Condition(..),
  ) where

import Data.List   (nub)
import Data.Monoid (Monoid(mempty, mappend))
import Text.PrettyPrint.HughesPJ

import Distribution.Package
         ( PackageIdentifier(PackageIdentifier), Dependency, Package(..) )
import Distribution.Version  (Version(Version), VersionRange(AnyVersion))
import Distribution.License  (License(AllRightsReserved))
import Distribution.Compiler (CompilerFlavor, showCompilerFlavor)
import Distribution.System   (OS, Arch)
import Distribution.Text
         ( display )
import Distribution.Simple.Utils  (currentDir)
import Language.Haskell.Extension (Extension)

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
emptyBuildInfo = nullBuildInfo { hsSourceDirs = [currentDir] }

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
    = b1{buildable         = buildable b1 && buildable b2,
         buildTools        = combine buildTools,
         cppOptions         = combine cppOptions,
         ccOptions         = combine ccOptions,
         ldOptions         = combine ldOptions,
         pkgconfigDepends  = combine pkgconfigDepends,
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
         options           = combine options,
         customFieldsBI    = combine customFieldsBI
        }
      where 
      combine :: (Eq a) => (BuildInfo -> [a]) -> [a]
      combine f = nub $ f b1 ++ f b2

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
    { flagName        :: String
    , flagDescription :: String
    , flagDefault     :: Bool
    }

instance Show Flag where show (MkFlag n _ _) = n

-- | A @ConfFlag@ represents an user-defined flag
data ConfFlag = ConfFlag String
    deriving Eq

-- | A @ConfVar@ represents the variable type used.
data ConfVar = OS OS
             | Arch Arch
             | Flag ConfFlag
             | Impl CompilerFlavor VersionRange
               deriving Eq

instance Show ConfVar where
    show (OS os) = "os(" ++ display os ++ ")"
    show (Arch arch) = "arch(" ++ display arch ++ ")"
    show (Flag (ConfFlag f)) = "flag(" ++ f ++ ")"
    show (Impl c v) = "impl(" ++ showCompilerFlavor c
                       ++ " " ++ display v ++ ")"

-- | A boolean expression parameterized over the variable type used.
data Condition c = Var c
                 | Lit Bool
                 | CNot (Condition c)
                 | COr (Condition c) (Condition c)
                 | CAnd (Condition c) (Condition c)

instance Show c => Show (Condition c) where
    show c = render $ ppCond c

-- | Pretty print a @Condition@.
ppCond :: Show c => Condition c -> Doc
ppCond (Var x) = text (show x)
ppCond (Lit b) = text (show b)
ppCond (CNot c) = char '!' <> parens (ppCond c)
ppCond (COr c1 c2) = parens $ sep [ppCond c1, text "||" <+> ppCond c2]
ppCond (CAnd c1 c2) = parens $ sep [ppCond c1, text "&&" <+> ppCond c2]

data CondTree v c a = CondNode
    { condTreeData        :: a
    , condTreeConstraints :: c
    , condTreeComponents  :: [( Condition v
                              , CondTree v c a
                              , Maybe (CondTree v c a))]
    }
    deriving Show

{-
instance (Show v, Show c) => Show (CondTree v c a) where
    show t = render $ ppCondTree t (text . show)

ppCondTree :: Show v => CondTree v c a -> (c -> Doc) -> Doc
ppCondTree (CondNode _dat cs ifs) ppD =
    (text "build-depends: " <+>
      ppD cs)
    $+$
    (vcat $ map ppIf ifs)
  where
    ppIf (c,thenTree,mElseTree) =
        ((text "if" <+> ppCond c <> colon) $$
          nest 2 (ppCondTree thenTree ppD))
        $+$ (maybe empty (\t -> text "else: " $$ nest 2 (ppCondTree t ppD))
                   mElseTree)
-}
