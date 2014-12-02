{-# LANGUAGE DeriveGeneric #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Compiler
-- Copyright   :  Isaac Jones 2003-2004
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This should be a much more sophisticated abstraction than it is. Currently
-- it's just a bit of data about the compiler, like it's flavour and name and
-- version. The reason it's just data is because currently it has to be in
-- 'Read' and 'Show' so it can be saved along with the 'LocalBuildInfo'. The
-- only interesting bit of info it contains is a mapping between language
-- extensions and compiler command line flags. This module also defines a
-- 'PackageDB' type which is used to refer to package databases. Most compilers
-- only know about a single global package collection but GHC has a global and
-- per-user one and it lets you create arbitrary other package databases. We do
-- not yet fully support this latter feature.

module Distribution.Simple.Compiler (
        -- * Haskell implementations
        module Distribution.Compiler,
        Compiler(..),
        showCompilerId, compilerFlavor, compilerVersion,
        compilerInfo,

        -- * Support for package databases
        PackageDB(..),
        PackageDBStack,
        registrationPackageDB,
        absolutePackageDBPaths,
        absolutePackageDBPath,

        -- * Support for optimisation levels
        OptimisationLevel(..),
        flagToOptimisationLevel,

        -- * Support for language extensions
        Flag,
        languageToFlags,
        unsupportedLanguages,
        extensionsToFlags,
        unsupportedExtensions,
        parmakeSupported,
        reexportedModulesSupported,
        renamingPackageFlagsSupported,
        packageKeySupported
  ) where

import Distribution.Compiler
import Distribution.Version (Version(..))
import Distribution.Text (display)
import Language.Haskell.Extension (Language(Haskell98), Extension)

import Control.Monad (liftM)
import Data.Binary (Binary)
import Data.List (nub)
import qualified Data.Map as M (Map, lookup)
import Data.Maybe (catMaybes, isNothing)
import GHC.Generics (Generic)
import System.Directory (canonicalizePath)

data Compiler = Compiler {
        compilerId              :: CompilerId,
        -- ^ Compiler flavour and version.
        compilerAbiTag          :: AbiTag,
        -- ^ Tag for distinguishing incompatible ABI's on the same architecture/os.
        compilerCompat          :: [CompilerId],
        -- ^ Other implementations that this compiler claims to be compatible with.
        compilerLanguages       :: [(Language, Flag)],
        -- ^ Supported language standards.
        compilerExtensions      :: [(Extension, Flag)],
        -- ^ Supported extensions.
        compilerProperties      :: M.Map String String
        -- ^ A key-value map for properties not covered by the above fields.
    }
    deriving (Generic, Show, Read)

instance Binary Compiler

showCompilerId :: Compiler -> String
showCompilerId = display . compilerId

compilerFlavor ::  Compiler -> CompilerFlavor
compilerFlavor = (\(CompilerId f _) -> f) . compilerId

compilerVersion :: Compiler -> Version
compilerVersion = (\(CompilerId _ v) -> v) . compilerId

compilerInfo :: Compiler -> CompilerInfo
compilerInfo c = CompilerInfo (compilerId c)
                              (compilerAbiTag c)
                              (Just . compilerCompat $ c)
                              (Just . map fst . compilerLanguages $ c)
                              (Just . map fst . compilerExtensions $ c)

-- ------------------------------------------------------------
-- * Package databases
-- ------------------------------------------------------------

-- |Some compilers have a notion of a database of available packages.
-- For some there is just one global db of packages, other compilers
-- support a per-user or an arbitrary db specified at some location in
-- the file system. This can be used to build isloated environments of
-- packages, for example to build a collection of related packages
-- without installing them globally.
--
data PackageDB = GlobalPackageDB
               | UserPackageDB
               | SpecificPackageDB FilePath
    deriving (Eq, Generic, Ord, Show, Read)

instance Binary PackageDB

-- | We typically get packages from several databases, and stack them
-- together. This type lets us be explicit about that stacking. For example
-- typical stacks include:
--
-- > [GlobalPackageDB]
-- > [GlobalPackageDB, UserPackageDB]
-- > [GlobalPackageDB, SpecificPackageDB "package.conf.inplace"]
--
-- Note that the 'GlobalPackageDB' is invariably at the bottom since it
-- contains the rts, base and other special compiler-specific packages.
--
-- We are not restricted to using just the above combinations. In particular
-- we can use several custom package dbs and the user package db together.
--
-- When it comes to writing, the top most (last) package is used.
--
type PackageDBStack = [PackageDB]

-- | Return the package that we should register into. This is the package db at
-- the top of the stack.
--
registrationPackageDB :: PackageDBStack -> PackageDB
registrationPackageDB []  = error "internal error: empty package db set"
registrationPackageDB dbs = last dbs

-- | Make package paths absolute


absolutePackageDBPaths :: PackageDBStack -> IO PackageDBStack
absolutePackageDBPaths = mapM absolutePackageDBPath

absolutePackageDBPath :: PackageDB -> IO PackageDB
absolutePackageDBPath GlobalPackageDB        = return GlobalPackageDB
absolutePackageDBPath UserPackageDB          = return UserPackageDB
absolutePackageDBPath (SpecificPackageDB db) =
  SpecificPackageDB `liftM` canonicalizePath db

-- ------------------------------------------------------------
-- * Optimisation levels
-- ------------------------------------------------------------

-- | Some compilers support optimising. Some have different levels.
-- For compilers that do not the level is just capped to the level
-- they do support.
--
data OptimisationLevel = NoOptimisation
                       | NormalOptimisation
                       | MaximumOptimisation
    deriving (Bounded, Enum, Eq, Generic, Read, Show)

instance Binary OptimisationLevel

flagToOptimisationLevel :: Maybe String -> OptimisationLevel
flagToOptimisationLevel Nothing  = NormalOptimisation
flagToOptimisationLevel (Just s) = case reads s of
  [(i, "")]
    | i >= fromEnum (minBound :: OptimisationLevel)
   && i <= fromEnum (maxBound :: OptimisationLevel)
                -> toEnum i
    | otherwise -> error $ "Bad optimisation level: " ++ show i
                        ++ ". Valid values are 0..2"
  _             -> error $ "Can't parse optimisation level " ++ s

-- ------------------------------------------------------------
-- * Languages and Extensions
-- ------------------------------------------------------------

unsupportedLanguages :: Compiler -> [Language] -> [Language]
unsupportedLanguages comp langs =
  [ lang | lang <- langs
         , isNothing (languageToFlag comp lang) ]

languageToFlags :: Compiler -> Maybe Language -> [Flag]
languageToFlags comp = filter (not . null)
                     . catMaybes . map (languageToFlag comp)
                     . maybe [Haskell98] (\x->[x])

languageToFlag :: Compiler -> Language -> Maybe Flag
languageToFlag comp ext = lookup ext (compilerLanguages comp)


-- |For the given compiler, return the extensions it does not support.
unsupportedExtensions :: Compiler -> [Extension] -> [Extension]
unsupportedExtensions comp exts =
  [ ext | ext <- exts
        , isNothing (extensionToFlag comp ext) ]

type Flag = String

-- |For the given compiler, return the flags for the supported extensions.
extensionsToFlags :: Compiler -> [Extension] -> [Flag]
extensionsToFlags comp = nub . filter (not . null)
                       . catMaybes . map (extensionToFlag comp)

extensionToFlag :: Compiler -> Extension -> Maybe Flag
extensionToFlag comp ext = lookup ext (compilerExtensions comp)

-- | Does this compiler support parallel --make mode?
parmakeSupported :: Compiler -> Bool
parmakeSupported = ghcSupported "Support parallel --make"

-- | Does this compiler support reexported-modules?
reexportedModulesSupported :: Compiler -> Bool
reexportedModulesSupported = ghcSupported "Support reexported-modules"

-- | Does this compiler support thinning/renaming on package flags?
renamingPackageFlagsSupported :: Compiler -> Bool
renamingPackageFlagsSupported = ghcSupported "Support thinning and renaming package flags"

-- | Does this compiler support package keys?
packageKeySupported :: Compiler -> Bool
packageKeySupported = ghcSupported "Uses package keys"

-- | Utility function for GHC only features
ghcSupported :: String -> Compiler -> Bool
ghcSupported key comp =
  case compilerFlavor comp of
    GHC -> case M.lookup key (compilerProperties comp) of
      Just "YES" -> True
      _          -> False
    _   -> False
