{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Distribution.Types.BuildInfo
  ( BuildInfo
  , BuildInfoAnn
  , BuildInfoWith (..)
  , unannotateBuildInfo
  , unannotateDependencyAnn
  , emptyBuildInfo
  , allLanguages
  , allExtensions
  , usedExtensions
  , usesTemplateHaskellOrQQ
  , hcOptions
  , hcProfOptions
  , hcSharedOptions
  , hcProfSharedOptions
  , hcStaticOptions
  ) where

import qualified Data.Semigroup as Semigroup (Last(..))
import Data.Monoid (All (..))

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Trivia
import Distribution.Types.Dependency
import Distribution.Types.ExeDependency
import Distribution.Types.LegacyExeDependency
import Distribution.Types.Mixin
import Distribution.Types.PkgconfigDependency
import Distribution.Utils.Path

import Distribution.Compiler
import Distribution.ModuleName
import Language.Haskell.Extension

import Data.Kind

import Distribution.Types.Modify (Annotate, AnnotateWith, AttachPositions, PreserveGrouping, AttachPosition)
import qualified Distribution.Types.Modify as Mod

type BuildInfo = BuildInfoWith Mod.HasNoAnn
type BuildInfoAnn = BuildInfoWith Mod.HasAnn

-- type family TargetBuildDepends (mod :: Mod.HasAnnotation) where
--   TargetBuildDepends Mod.HasAnn = [(Positions, [DependencyWith Mod.HasAnn])]
--   TargetBuildDepends Mod.HasNoAnn = [DependencyWith Mod.HasNoAnn]

-- Consider refactoring into executable and library versions.
data BuildInfoWith (m :: Mod.HasAnnotation) = BuildInfo
  { buildable :: AnnotateWith Positions m Bool
  -- ^ component is buildable here
  , buildTools :: PreserveGrouping m (AttachPositions m [AttachPosition m (Annotate m LegacyExeDependency)])
  -- ^ Tools needed to build this bit.
  --
  -- This is a legacy field that 'buildToolDepends' largely supersedes.
  --
  -- Unless use are very sure what you are doing, use the functions in
  -- "Distribution.Simple.BuildToolDepends" rather than accessing this
  -- field directly.
  , buildToolDepends :: PreserveGrouping m (AttachPositions m [AttachPosition m (Annotate m ExeDependency)])
  -- ^ Haskell tools needed to build this bit
  --
  -- This field is better than 'buildTools' because it allows one to
  -- precisely specify an executable in a package.
  --
  -- Unless use are very sure what you are doing, use the functions in
  -- "Distribution.Simple.BuildToolDepends" rather than accessing this
  -- field directly.
  , cppOptions :: PreserveGrouping m (AttachPositions m [AttachPosition m (Annotate m String)])
  -- ^ options for pre-processing Haskell code
  , asmOptions :: PreserveGrouping m (AttachPositions m [AttachPosition m (Annotate m String)])
  -- ^ options for assembler
  , cmmOptions :: PreserveGrouping m (AttachPositions m [AttachPosition m (Annotate m String)])
  -- ^ options for C-- compiler
  , ccOptions :: PreserveGrouping m (AttachPositions m [AttachPosition m (Annotate m String)])
  -- ^ options for C compiler
  , cxxOptions :: PreserveGrouping m (AttachPositions m [AttachPosition m (Annotate m String)])
  -- ^ options for C++ compiler
  , jsppOptions :: PreserveGrouping m (AttachPositions m [AttachPosition m (Annotate m String)])
  -- ^ options for pre-processing JavaScript code @since 3.16.0.0
  , ldOptions :: PreserveGrouping m (AttachPositions m [AttachPosition m (Annotate m String)])
  -- ^ options for linker
  , hsc2hsOptions :: PreserveGrouping m (AttachPositions m [AttachPosition m (Annotate m String)])
  -- ^ options for hsc2hs
  , pkgconfigDepends :: PreserveGrouping m (AttachPositions m [AttachPosition m (Annotate m PkgconfigDependency)])
  -- ^ pkg-config packages that are used
  , frameworks :: PreserveGrouping m (AttachPositions m [AttachPosition m (Annotate m (RelativePath Framework File))])
  -- ^ support frameworks for Mac OS X
  , extraFrameworkDirs :: PreserveGrouping m (AttachPositions m [AttachPosition m (Annotate m (SymbolicPath Pkg (Dir Framework)))])
  -- ^ extra locations to find frameworks.
  , asmSources :: PreserveGrouping m (AttachPositions m [AttachPosition m (Annotate m (SymbolicPath Pkg File))])
  -- ^ Assembly files.
  , cmmSources :: PreserveGrouping m (AttachPositions m [AttachPosition m (Annotate m (SymbolicPath Pkg File))])
  -- ^ C-- files.
  , cSources :: PreserveGrouping m (AttachPositions m [AttachPosition m (Annotate m (SymbolicPath Pkg File))])
  , cxxSources :: PreserveGrouping m (AttachPositions m [AttachPosition m (Annotate m (SymbolicPath Pkg File))])
  , jsSources :: PreserveGrouping m (AttachPositions m [AttachPosition m (Annotate m (SymbolicPath Pkg File))])
  , hsSourceDirs :: PreserveGrouping m (AttachPositions m [AttachPosition m (Annotate m (SymbolicPath Pkg (Dir Source)))])
  -- ^ where to look for the Haskell module hierarchy
  , -- NB: these are symbolic paths are not relative paths,
    -- because autogenerated modules might end up in an absolute path
    otherModules :: PreserveGrouping m (AttachPositions m [AttachPosition m (Annotate m ModuleName)])
  -- ^ non-exposed or non-main modules
  , virtualModules :: [ModuleName]
  -- ^ exposed modules that do not have a source file (e.g. @GHC.Prim@ from @ghc-prim@ package)
  , autogenModules :: [ModuleName]
  -- ^ not present on sdist, Paths_* or user-generated with a custom Setup.hs
  , defaultLanguage :: Maybe Language
  -- ^ language used when not explicitly specified
  , otherLanguages :: [Language]
  -- ^ other languages used within the package
  , defaultExtensions :: [Extension]
  -- ^ language extensions used by all modules
  , otherExtensions :: [Extension]
  -- ^ other language extensions used within the package
  , oldExtensions :: [Extension]
  -- ^ the old extensions field, treated same as 'defaultExtensions'
  , extraLibs :: [String]
  -- ^ what libraries to link with when compiling a program that uses your package
  , extraLibsStatic :: [String]
  -- ^ what libraries to link with when compiling a program fully statically that uses your package
  , extraGHCiLibs :: [String]
  -- ^ if present, overrides extraLibs when package is loaded with GHCi.
  , extraBundledLibs :: [String]
  -- ^ if present, adds libs to hs-libraries, which become part of the package.
  --   Example 1: the Cffi library shipping with the rts, alongside the HSrts-1.0.a,.o,...
  --   Example 2: a library that is being built by a foreign tool (e.g. rust)
  --              and copied and registered together with this library.  The
  --              logic on how this library is built will have to be encoded in a
  --              custom Setup for now.  Otherwise cabal would need to learn how to
  --              call arbitrary library builders.
  , extraLibFlavours :: [String]
  -- ^ Hidden Flag.  This set of strings, will be appended to all libraries when
  --   copying. E.g. [libHS<name>_<flavour> | flavour <- extraLibFlavours]. This
  --   should only be needed in very specific cases, e.g. the `rts` package, where
  --   there are multiple copies of slightly differently built libs.
  , extraDynLibFlavours :: [String]
  -- ^ Hidden Flag. This set of strings will be appended to all /dynamic/
  --   libraries when copying. This is particularly useful with the `rts` package,
  --   where we want different dynamic flavours of the RTS library to be installed.
  , extraLibDirs :: [SymbolicPath Pkg (Dir Lib)]
  , extraLibDirsStatic :: [SymbolicPath Pkg (Dir Lib)]
  , includeDirs :: [SymbolicPath Pkg (Dir Include)]
  -- ^ directories to find .h files
  , includes :: [SymbolicPath Include File]
  -- ^ The .h files to be found in includeDirs
  , autogenIncludes :: [RelativePath Include File]
  -- ^ The .h files to be generated (e.g. by @autoconf@)
  , installIncludes :: [RelativePath Include File]
  -- ^ .h files to install with the package
  , options :: PerCompilerFlavor [String]
  , profOptions :: PerCompilerFlavor [String]
  , sharedOptions :: PerCompilerFlavor [String]
  , profSharedOptions :: PerCompilerFlavor [String]
  , staticOptions :: PerCompilerFlavor [String]
  , customFieldsBI :: [(String, String)]
  -- ^ Custom fields starting
  --  with x-, stored in a
  --  simple assoc-list.
  , targetBuildDepends :: PreserveGrouping m (AttachPositions m [AttachPosition m (Annotate m (DependencyWith m))])
  -- ^ Dependencies specific to a library or executable target
  , mixins :: [Mixin]
  }
  deriving (Generic)

deriving instance Show BuildInfo
deriving instance Read BuildInfo
deriving instance Eq BuildInfo
deriving instance Ord BuildInfo
deriving instance Data BuildInfo

instance Binary BuildInfo
instance Structured BuildInfo
instance NFData BuildInfo where rnf = genericRnf

unannotateBuildInfo :: BuildInfoAnn -> BuildInfo
unannotateBuildInfo bi =
  let unannotateMonoidalField = map (unAnn . snd) . join . map snd
  in
  bi
    { buildable = unAnn $ buildable bi
    , buildTools = unannotateMonoidalField $ buildTools bi
    , buildToolDepends = unannotateMonoidalField $ buildToolDepends bi
    , cppOptions = unannotateMonoidalField $ cppOptions bi
    , asmOptions = unannotateMonoidalField $ asmOptions bi
    , cmmOptions = unannotateMonoidalField $ cmmOptions bi
    , ccOptions = unannotateMonoidalField $ ccOptions bi
    , cxxOptions = unannotateMonoidalField $ cxxOptions bi
    , jsppOptions = unannotateMonoidalField $ jsppOptions bi
    , ldOptions = unannotateMonoidalField $ ldOptions bi
    , hsc2hsOptions = unannotateMonoidalField $ hsc2hsOptions bi
    , pkgconfigDepends = unannotateMonoidalField $ pkgconfigDepends bi
    , frameworks = unannotateMonoidalField $ frameworks bi
    , extraFrameworkDirs = unannotateMonoidalField $ extraFrameworkDirs bi
    , asmSources = unannotateMonoidalField $ asmSources bi
    , cmmSources = unannotateMonoidalField $ cmmSources bi
    , cSources = unannotateMonoidalField $ cSources bi
    , cxxSources = unannotateMonoidalField $ cxxSources bi
    , jsSources = unannotateMonoidalField $ jsSources bi
    , hsSourceDirs = unannotateMonoidalField $ hsSourceDirs bi
    , otherModules = unannotateMonoidalField $ otherModules bi
    , -- TODO(leana8959): add more fields here

      -- [(Positions, (Position, Ann t dep))]
      targetBuildDepends = map (unannotateDependencyAnn . unAnn . snd) $ join $ map snd $ targetBuildDepends bi
    }

instance Monoid BuildInfo where
  mempty =
    BuildInfo
      { buildable = True
      , buildTools = []
      , buildToolDepends = []
      , cppOptions = []
      , asmOptions = []
      , cmmOptions = []
      , ccOptions = []
      , cxxOptions = []
      , jsppOptions = []
      , ldOptions = []
      , hsc2hsOptions = []
      , pkgconfigDepends = []
      , frameworks = []
      , extraFrameworkDirs = []
      , asmSources = []
      , cmmSources = []
      , cSources = []
      , cxxSources = []
      , jsSources = []
      , hsSourceDirs = []
      , otherModules = []
      , virtualModules = []
      , autogenModules = []
      , defaultLanguage = Nothing
      , otherLanguages = []
      , defaultExtensions = []
      , otherExtensions = []
      , oldExtensions = []
      , extraLibs = []
      , extraLibsStatic = []
      , extraGHCiLibs = []
      , extraBundledLibs = []
      , extraLibFlavours = []
      , extraDynLibFlavours = []
      , extraLibDirs = []
      , extraLibDirsStatic = []
      , includeDirs = []
      , includes = []
      , autogenIncludes = []
      , installIncludes = []
      , options = mempty
      , profOptions = mempty
      , sharedOptions = mempty
      , profSharedOptions = mempty
      , staticOptions = mempty
      , customFieldsBI = []
      , targetBuildDepends = []
      , mixins = []
      }
  mappend = (<>)

instance Monoid (BuildInfoWith Mod.HasAnn) where
  mempty = emptyBuildInfo'

instance Semigroup BuildInfo where
  a <> b =
    BuildInfo
      { buildable = buildable a && buildable b
      , buildTools = combine buildTools
      , buildToolDepends = combine buildToolDepends
      , cppOptions = combine cppOptions
      , asmOptions = combine asmOptions
      , cmmOptions = combine cmmOptions
      , ccOptions = combine ccOptions
      , cxxOptions = combine cxxOptions
      , jsppOptions = combine jsppOptions
      , ldOptions = combine ldOptions
      , hsc2hsOptions = combine hsc2hsOptions
      , pkgconfigDepends = combine pkgconfigDepends
      , frameworks = combineNub frameworks
      , extraFrameworkDirs = combineNub extraFrameworkDirs
      , asmSources = combineNub asmSources
      , cmmSources = combineNub cmmSources
      , cSources = combineNub cSources
      , cxxSources = combineNub cxxSources
      , jsSources = combineNub jsSources
      , hsSourceDirs = combineNub hsSourceDirs
      , otherModules = combineNub otherModules
      , virtualModules = combineNub virtualModules
      , autogenModules = combineNub autogenModules
      , defaultLanguage = combineMby defaultLanguage
      , otherLanguages = combineNub otherLanguages
      , defaultExtensions = combineNub defaultExtensions
      , otherExtensions = combineNub otherExtensions
      , oldExtensions = combineNub oldExtensions
      , extraLibs = combine extraLibs
      , extraLibsStatic = combine extraLibsStatic
      , extraGHCiLibs = combine extraGHCiLibs
      , extraBundledLibs = combine extraBundledLibs
      , extraLibFlavours = combine extraLibFlavours
      , extraDynLibFlavours = combine extraDynLibFlavours
      , extraLibDirs = combineNub extraLibDirs
      , extraLibDirsStatic = combineNub extraLibDirsStatic
      , includeDirs = combineNub includeDirs
      , includes = combineNub includes
      , autogenIncludes = combineNub autogenIncludes
      , installIncludes = combineNub installIncludes
      , options = combine options
      , profOptions = combine profOptions
      , sharedOptions = combine sharedOptions
      , profSharedOptions = combine profSharedOptions
      , staticOptions = combine staticOptions
      , customFieldsBI = combine customFieldsBI
      , targetBuildDepends = combineNub targetBuildDepends
      , mixins = combine mixins
      }
    where
      combine :: Monoid a => (BuildInfo -> a) -> a
      combine field = field a `mappend` field b
      combineNub field = nub (combine field)
      combineMby field = field b `mplus` field a

instance Semigroup (BuildInfoWith Mod.HasAnn) where
  a <> b =
    BuildInfo
      { buildable =
          mapAnnA (fmap Semigroup.getLast) getAll $
              mapAnnA (fmap Semigroup.Last) All (buildable a)
              <> mapAnnA (fmap Semigroup.Last) All (buildable b)
      , buildTools = combine buildTools
      , buildToolDepends = combine buildToolDepends
      , cppOptions = combine cppOptions
      , asmOptions = combine asmOptions
      , cmmOptions = combine cmmOptions
      , ccOptions = combine ccOptions
      , cxxOptions = combine cxxOptions
      , jsppOptions = combine jsppOptions
      , ldOptions = combine ldOptions
      , hsc2hsOptions = combine hsc2hsOptions
      , pkgconfigDepends = combine pkgconfigDepends
      , frameworks = combineNub frameworks
      , extraFrameworkDirs = combineNub extraFrameworkDirs
      , asmSources = combineNub asmSources
      , cmmSources = combineNub cmmSources
      , cSources = combineNub cSources
      , cxxSources = combineNub cxxSources
      , jsSources = combineNub jsSources
      , hsSourceDirs = combineNub hsSourceDirs
      , otherModules = combineNub otherModules
      , virtualModules = combineNub virtualModules
      , autogenModules = combineNub autogenModules
      , defaultLanguage = combineMby defaultLanguage
      , otherLanguages = combineNub otherLanguages
      , defaultExtensions = combineNub defaultExtensions
      , otherExtensions = combineNub otherExtensions
      , oldExtensions = combineNub oldExtensions
      , extraLibs = combine extraLibs
      , extraLibsStatic = combine extraLibsStatic
      , extraGHCiLibs = combine extraGHCiLibs
      , extraBundledLibs = combine extraBundledLibs
      , extraLibFlavours = combine extraLibFlavours
      , extraDynLibFlavours = combine extraDynLibFlavours
      , extraLibDirs = combineNub extraLibDirs
      , extraLibDirsStatic = combineNub extraLibDirsStatic
      , includeDirs = combineNub includeDirs
      , includes = combineNub includes
      , autogenIncludes = combineNub autogenIncludes
      , installIncludes = combineNub installIncludes
      , options = combine options
      , profOptions = combine profOptions
      , sharedOptions = combine sharedOptions
      , profSharedOptions = combine profSharedOptions
      , staticOptions = combine staticOptions
      , customFieldsBI = combine customFieldsBI
      , targetBuildDepends = combineNub targetBuildDepends
      , mixins = combine mixins
      }
    where
      combine :: Monoid a => (BuildInfoWith Mod.HasAnn -> a) -> a
      combine field = field a `mappend` field b
      combineNub field = nub (combine field)
      combineMby field = field b `mplus` field a

emptyBuildInfo :: BuildInfo
emptyBuildInfo = mempty

emptyBuildInfo' :: BuildInfoWith Mod.HasAnn
emptyBuildInfo' =
  BuildInfo
    { buildable = Ann NoTrivia True
    , buildTools = []
    , buildToolDepends = []
    , cppOptions = []
    , asmOptions = []
    , cmmOptions = []
    , ccOptions = []
    , cxxOptions = []
    , jsppOptions = []
    , ldOptions = []
    , hsc2hsOptions = []
    , pkgconfigDepends = []
    , frameworks = []
    , extraFrameworkDirs = []
    , asmSources = []
    , cmmSources = []
    , cSources = []
    , cxxSources = []
    , jsSources = []
    , hsSourceDirs = []
    , otherModules = []
    , virtualModules = []
    , autogenModules = []
    , defaultLanguage = Nothing
    , otherLanguages = []
    , defaultExtensions = []
    , otherExtensions = []
    , oldExtensions = []
    , extraLibs = []
    , extraLibsStatic = []
    , extraGHCiLibs = []
    , extraBundledLibs = []
    , extraLibFlavours = []
    , extraDynLibFlavours = []
    , extraLibDirs = []
    , extraLibDirsStatic = []
    , includeDirs = []
    , includes = []
    , autogenIncludes = []
    , installIncludes = []
    , options = mempty
    , profOptions = mempty
    , sharedOptions = mempty
    , profSharedOptions = mempty
    , staticOptions = mempty
    , customFieldsBI = []
    , targetBuildDepends = []
    , mixins = []
    }

-- | The 'Language's used by this component
allLanguages :: BuildInfo -> [Language]
allLanguages bi =
  maybeToList (defaultLanguage bi)
    ++ otherLanguages bi

-- | The 'Extension's that are used somewhere by this component
allExtensions :: BuildInfo -> [Extension]
allExtensions bi =
  usedExtensions bi
    ++ otherExtensions bi

-- | The 'Extensions' that are used by all modules in this component
usedExtensions :: BuildInfo -> [Extension]
usedExtensions bi =
  oldExtensions bi
    ++ defaultExtensions bi

-- | Whether any modules in this component use Template Haskell or
-- Quasi Quotes
usesTemplateHaskellOrQQ :: BuildInfo -> Bool
usesTemplateHaskellOrQQ bi = any p (allExtensions bi)
  where
    p ex =
      ex
        `elem` [EnableExtension TemplateHaskell, EnableExtension QuasiQuotes]

-- | Select options for a particular Haskell compiler.
hcOptions :: CompilerFlavor -> BuildInfo -> [String]
hcOptions = lookupHcOptions options

hcProfOptions :: CompilerFlavor -> BuildInfo -> [String]
hcProfOptions = lookupHcOptions profOptions

hcSharedOptions :: CompilerFlavor -> BuildInfo -> [String]
hcSharedOptions = lookupHcOptions sharedOptions

hcProfSharedOptions :: CompilerFlavor -> BuildInfo -> [String]
hcProfSharedOptions = lookupHcOptions profSharedOptions

hcStaticOptions :: CompilerFlavor -> BuildInfo -> [String]
hcStaticOptions = lookupHcOptions staticOptions

lookupHcOptions
  :: (BuildInfo -> PerCompilerFlavor [String])
  -> CompilerFlavor
  -> BuildInfo
  -> [String]
lookupHcOptions f hc bi = case f bi of
  PerCompilerFlavor ghc ghcjs
    | hc == GHC -> ghc
    | hc == GHCJS -> ghcjs
    | otherwise -> mempty
