{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}

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
-- it's just a bit of data about the compiler, like its flavour and name and
-- version. The reason it's just data is because currently it has to be in
-- 'Read' and 'Show' so it can be saved along with the 'LocalBuildInfo'. The
-- only interesting bit of info it contains is a mapping between language
-- extensions and compiler command line flags. This module also defines a
-- 'PackageDB' type which is used to refer to package databases. Most compilers
-- only know about a single global package collection but GHC has a global and
-- per-user one and it lets you create arbitrary other package databases. We do
-- not yet fully support this latter feature.
module Distribution.Simple.Compiler
  ( -- * Haskell implementations
      module Distribution.Compiler
  , Compiler (..)
  , showCompilerId
  , showCompilerIdWithAbi
  , compilerFlavor
  , compilerVersion
  , compilerCompatFlavor
  , compilerCompatVersion
  , compilerInfo

    -- * Support for package databases
  , PackageDB
  , PackageDBStack
  , PackageDBCWD
  , PackageDBStackCWD
  , PackageDBX (..)
  , PackageDBStackX
  , PackageDBS
  , PackageDBStackS
  , registrationPackageDB
  , absolutePackageDBPaths
  , absolutePackageDBPath
  , interpretPackageDB
  , interpretPackageDBStack
  , coercePackageDB
  , coercePackageDBStack
  , readPackageDb

    -- * Support for optimisation levels
  , OptimisationLevel (..)
  , flagToOptimisationLevel

    -- * Support for debug info levels
  , DebugInfoLevel (..)
  , flagToDebugInfoLevel

    -- * Support for language extensions
  , CompilerFlag
  , languageToFlags
  , unsupportedLanguages
  , extensionsToFlags
  , unsupportedExtensions
  , parmakeSupported
  , reexportedModulesSupported
  , renamingPackageFlagsSupported
  , unifiedIPIDRequired
  , packageKeySupported
  , unitIdSupported
  , coverageSupported
  , profilingSupported
  , profilingDynamicSupported
  , profilingDynamicSupportedOrUnknown
  , profilingVanillaSupported
  , profilingVanillaSupportedOrUnknown
  , dynamicSupported
  , backpackSupported
  , arResponseFilesSupported
  , arDashLSupported
  , libraryDynDirSupported
  , libraryVisibilitySupported
  , jsemSupported

    -- * Support for profiling detail levels
  , ProfDetailLevel (..)
  , knownProfDetailLevels
  , flagToProfDetailLevel
  , showProfDetailLevel
  ) where

import Distribution.Compat.CharParsing
import Distribution.Compat.Prelude
import Distribution.Parsec
import Distribution.Pretty
import Prelude ()

import Distribution.Compiler
import Distribution.Simple.Utils
import Distribution.Utils.Path
import Distribution.Version

import Language.Haskell.Extension

import Data.Bool (bool)
import qualified Data.Map as Map (lookup)
import System.Directory (canonicalizePath)

data Compiler = Compiler
  { compilerId :: CompilerId
  -- ^ Compiler flavour and version.
  , compilerAbiTag :: AbiTag
  -- ^ Tag for distinguishing incompatible ABI's on the same
  -- architecture/os.
  , compilerCompat :: [CompilerId]
  -- ^ Other implementations that this compiler claims to be
  -- compatible with.
  , compilerLanguages :: [(Language, CompilerFlag)]
  -- ^ Supported language standards.
  , compilerExtensions :: [(Extension, Maybe CompilerFlag)]
  -- ^ Supported extensions.
  , compilerProperties :: Map String String
  -- ^ A key-value map for properties not covered by the above fields.
  }
  deriving (Eq, Generic, Show, Read)

instance Binary Compiler
instance Structured Compiler

showCompilerId :: Compiler -> String
showCompilerId = prettyShow . compilerId

showCompilerIdWithAbi :: Compiler -> String
showCompilerIdWithAbi comp =
  prettyShow (compilerId comp)
    ++ case compilerAbiTag comp of
      NoAbiTag -> []
      AbiTag xs -> '-' : xs

compilerFlavor :: Compiler -> CompilerFlavor
compilerFlavor = (\(CompilerId f _) -> f) . compilerId

compilerVersion :: Compiler -> Version
compilerVersion = (\(CompilerId _ v) -> v) . compilerId

-- | Is this compiler compatible with the compiler flavour we're interested in?
--
-- For example this checks if the compiler is actually GHC or is another
-- compiler that claims to be compatible with some version of GHC, e.g. GHCJS.
--
-- > if compilerCompatFlavor GHC compiler then ... else ...
compilerCompatFlavor :: CompilerFlavor -> Compiler -> Bool
compilerCompatFlavor flavor comp =
  flavor == compilerFlavor comp
    || flavor `elem` [flavor' | CompilerId flavor' _ <- compilerCompat comp]

-- | Is this compiler compatible with the compiler flavour we're interested in,
-- and if so what version does it claim to be compatible with.
--
-- For example this checks if the compiler is actually GHC-7.x or is another
-- compiler that claims to be compatible with some GHC-7.x version.
--
-- > case compilerCompatVersion GHC compiler of
-- >   Just (Version (7:_)) -> ...
-- >   _                    -> ...
compilerCompatVersion :: CompilerFlavor -> Compiler -> Maybe Version
compilerCompatVersion flavor comp
  | compilerFlavor comp == flavor = Just (compilerVersion comp)
  | otherwise =
      listToMaybe [v | CompilerId fl v <- compilerCompat comp, fl == flavor]

compilerInfo :: Compiler -> CompilerInfo
compilerInfo c =
  CompilerInfo
    (compilerId c)
    (compilerAbiTag c)
    (Just . compilerCompat $ c)
    (Just . map fst . compilerLanguages $ c)
    (Just . map fst . compilerExtensions $ c)

-- ------------------------------------------------------------

-- * Package databases

-- ------------------------------------------------------------

-- | Some compilers have a notion of a database of available packages.
--  For some there is just one global db of packages, other compilers
--  support a per-user or an arbitrary db specified at some location in
--  the file system. This can be used to build isolated environments of
--  packages, for example to build a collection of related packages
--  without installing them globally.
--
--  Abstracted over
data PackageDBX fp
  = GlobalPackageDB
  | UserPackageDB
  | -- | NB: the path might be relative or it might be absolute
    SpecificPackageDB fp
  deriving (Eq, Generic, Ord, Show, Read, Functor, Foldable, Traversable)

instance Binary fp => Binary (PackageDBX fp)
instance Structured fp => Structured (PackageDBX fp)

-- | Parse a PackageDB stack entry
--
-- @since 3.7.0.0
readPackageDb :: String -> Maybe PackageDB
readPackageDb "clear" = Nothing
readPackageDb "global" = Just GlobalPackageDB
readPackageDb "user" = Just UserPackageDB
readPackageDb other = Just (SpecificPackageDB (makeSymbolicPath other))

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
type PackageDBStackX from = [PackageDBX from]

type PackageDB = PackageDBX (SymbolicPath Pkg (Dir PkgDB))
type PackageDBStack = PackageDBStackX (SymbolicPath Pkg (Dir PkgDB))

type PackageDBS from = PackageDBX (SymbolicPath from (Dir PkgDB))
type PackageDBStackS from = PackageDBStackX (SymbolicPath from (Dir PkgDB))

type PackageDBCWD = PackageDBX FilePath
type PackageDBStackCWD = PackageDBStackX FilePath

-- | Return the package that we should register into. This is the package db at
-- the top of the stack.
registrationPackageDB :: PackageDBStackX from -> PackageDBX from
registrationPackageDB dbs = case safeLast dbs of
  Nothing -> error "internal error: empty package db set"
  Just p -> p

-- | Make package paths absolute
absolutePackageDBPaths
  :: Maybe (SymbolicPath CWD (Dir Pkg))
  -> PackageDBStack
  -> IO PackageDBStack
absolutePackageDBPaths mbWorkDir = traverse $ absolutePackageDBPath mbWorkDir

absolutePackageDBPath
  :: Maybe (SymbolicPath CWD (Dir Pkg))
  -> PackageDB
  -> IO PackageDB
absolutePackageDBPath _ GlobalPackageDB = return GlobalPackageDB
absolutePackageDBPath _ UserPackageDB = return UserPackageDB
absolutePackageDBPath mbWorkDir (SpecificPackageDB db) = do
  let db' =
        case symbolicPathRelative_maybe db of
          Nothing -> getSymbolicPath db
          Just rel_path -> interpretSymbolicPath mbWorkDir rel_path
  SpecificPackageDB . makeSymbolicPath <$> canonicalizePath db'

interpretPackageDB :: Maybe (SymbolicPath CWD (Dir Pkg)) -> PackageDB -> PackageDBCWD
interpretPackageDB _ GlobalPackageDB = GlobalPackageDB
interpretPackageDB _ UserPackageDB = UserPackageDB
interpretPackageDB mbWorkDir (SpecificPackageDB db) =
  SpecificPackageDB (interpretSymbolicPath mbWorkDir db)

interpretPackageDBStack :: Maybe (SymbolicPath CWD (Dir Pkg)) -> PackageDBStack -> PackageDBStackCWD
interpretPackageDBStack mbWorkDir = map (interpretPackageDB mbWorkDir)

-- | Transform a package db using a FilePath into one using symbolic paths.
coercePackageDB :: PackageDBCWD -> PackageDBX (SymbolicPath CWD (Dir PkgDB))
coercePackageDB GlobalPackageDB = GlobalPackageDB
coercePackageDB UserPackageDB = UserPackageDB
coercePackageDB (SpecificPackageDB db) = SpecificPackageDB (makeSymbolicPath db)

coercePackageDBStack
  :: [PackageDBCWD]
  -> [PackageDBX (SymbolicPath CWD (Dir PkgDB))]
coercePackageDBStack = map coercePackageDB

-- ------------------------------------------------------------

-- * Optimisation levels

-- ------------------------------------------------------------

-- | Some compilers support optimising. Some have different levels.
-- For compilers that do not the level is just capped to the level
-- they do support.
data OptimisationLevel
  = NoOptimisation
  | NormalOptimisation
  | MaximumOptimisation
  deriving (Bounded, Enum, Eq, Generic, Read, Show)

instance Binary OptimisationLevel
instance Structured OptimisationLevel

instance Parsec OptimisationLevel where
  parsec = parsecOptimisationLevel

parsecOptimisationLevel :: CabalParsing m => m OptimisationLevel
parsecOptimisationLevel = boolParser <|> intParser
  where
    boolParser = (bool NoOptimisation NormalOptimisation) <$> parsec
    intParser = intToOptimisationLevel <$> integral

flagToOptimisationLevel :: Maybe String -> OptimisationLevel
flagToOptimisationLevel Nothing = NormalOptimisation
flagToOptimisationLevel (Just s) = case reads s of
  [(i, "")] -> intToOptimisationLevel i
  _ -> error $ "Can't parse optimisation level " ++ s

intToOptimisationLevel :: Int -> OptimisationLevel
intToOptimisationLevel i
  | i >= fromEnum (minBound :: OptimisationLevel)
      && i <= fromEnum (maxBound :: OptimisationLevel) =
      toEnum i
  | otherwise =
      error $
        "Bad optimisation level: "
          ++ show i
          ++ ". Valid values are 0..2"

-- ------------------------------------------------------------

-- * Debug info levels

-- ------------------------------------------------------------

-- | Some compilers support emitting debug info. Some have different
-- levels.  For compilers that do not the level is just capped to the
-- level they do support.
data DebugInfoLevel
  = NoDebugInfo
  | MinimalDebugInfo
  | NormalDebugInfo
  | MaximalDebugInfo
  deriving (Bounded, Enum, Eq, Generic, Read, Show)

instance Binary DebugInfoLevel
instance Structured DebugInfoLevel

instance Parsec DebugInfoLevel where
  parsec = parsecDebugInfoLevel

parsecDebugInfoLevel :: CabalParsing m => m DebugInfoLevel
parsecDebugInfoLevel = flagToDebugInfoLevel <$> pure <$> parsecToken

flagToDebugInfoLevel :: Maybe String -> DebugInfoLevel
flagToDebugInfoLevel Nothing = NormalDebugInfo
flagToDebugInfoLevel (Just s) = case reads s of
  [(i, "")]
    | i >= fromEnum (minBound :: DebugInfoLevel)
        && i <= fromEnum (maxBound :: DebugInfoLevel) ->
        toEnum i
    | otherwise ->
        error $
          "Bad debug info level: "
            ++ show i
            ++ ". Valid values are 0..3"
  _ -> error $ "Can't parse debug info level " ++ s

-- ------------------------------------------------------------

-- * Languages and Extensions

-- ------------------------------------------------------------

unsupportedLanguages :: Compiler -> [Language] -> [Language]
unsupportedLanguages comp langs =
  [ lang | lang <- langs, isNothing (languageToFlag comp lang)
  ]

languageToFlags :: Compiler -> Maybe Language -> [CompilerFlag]
languageToFlags comp =
  filter (not . null)
    . catMaybes
    . map (languageToFlag comp)
    . maybe [Haskell98] (\x -> [x])

languageToFlag :: Compiler -> Language -> Maybe CompilerFlag
languageToFlag comp ext = lookup ext (compilerLanguages comp)

-- | For the given compiler, return the extensions it does not support.
unsupportedExtensions :: Compiler -> [Extension] -> [Extension]
unsupportedExtensions comp exts =
  [ ext | ext <- exts, isNothing (extensionToFlag' comp ext)
  ]

type CompilerFlag = String

-- | For the given compiler, return the flags for the supported extensions.
extensionsToFlags :: Compiler -> [Extension] -> [CompilerFlag]
extensionsToFlags comp =
  nub
    . filter (not . null)
    . catMaybes
    . map (extensionToFlag comp)

-- | Looks up the flag for a given extension, for a given compiler.
-- Ignores the subtlety of extensions which lack associated flags.
extensionToFlag :: Compiler -> Extension -> Maybe CompilerFlag
extensionToFlag comp ext = join (extensionToFlag' comp ext)

-- | Looks up the flag for a given extension, for a given compiler.
-- However, the extension may be valid for the compiler but not have a flag.
-- For example, NondecreasingIndentation is enabled by default on GHC 7.0.4,
-- hence it is considered a supported extension but not an accepted flag.
--
-- The outer layer of Maybe indicates whether the extensions is supported, while
-- the inner layer indicates whether it has a flag.
-- When building strings, it is often more convenient to use 'extensionToFlag',
-- which ignores the difference.
extensionToFlag' :: Compiler -> Extension -> Maybe (Maybe CompilerFlag)
extensionToFlag' comp ext = lookup ext (compilerExtensions comp)

-- | Does this compiler support parallel --make mode?
parmakeSupported :: Compiler -> Bool
parmakeSupported = ghcSupported "Support parallel --make"

-- | Does this compiler support reexported-modules?
reexportedModulesSupported :: Compiler -> Bool
reexportedModulesSupported = ghcSupported "Support reexported-modules"

-- | Does this compiler support thinning/renaming on package flags?
renamingPackageFlagsSupported :: Compiler -> Bool
renamingPackageFlagsSupported =
  ghcSupported
    "Support thinning and renaming package flags"

-- | Does this compiler have unified IPIDs (so no package keys)
unifiedIPIDRequired :: Compiler -> Bool
unifiedIPIDRequired = ghcSupported "Requires unified installed package IDs"

-- | Does this compiler support package keys?
packageKeySupported :: Compiler -> Bool
packageKeySupported = ghcSupported "Uses package keys"

-- | Does this compiler support unit IDs?
unitIdSupported :: Compiler -> Bool
unitIdSupported = ghcSupported "Uses unit IDs"

-- | Does this compiler support Backpack?
backpackSupported :: Compiler -> Bool
backpackSupported = ghcSupported "Support Backpack"

-- | Does this compiler support the -jsem option?
jsemSupported :: Compiler -> Bool
jsemSupported comp = case compilerFlavor comp of
  GHC -> v >= mkVersion [9, 7]
  _ -> False
  where
    v = compilerVersion comp

-- | Does this compiler support a package database entry with:
-- "dynamic-library-dirs"?
libraryDynDirSupported :: Compiler -> Bool
libraryDynDirSupported comp = case compilerFlavor comp of
  GHC ->
    -- Not just v >= mkVersion [8,0,1,20161022], as there
    -- are many GHC 8.1 nightlies which don't support this.
    ( (v >= mkVersion [8, 0, 1, 20161022] && v < mkVersion [8, 1])
        || v >= mkVersion [8, 1, 20161021]
    )
  _ -> False
  where
    v = compilerVersion comp

-- | Does this compiler's "ar" command supports response file
-- arguments (i.e. @file-style arguments).
arResponseFilesSupported :: Compiler -> Bool
arResponseFilesSupported = ghcSupported "ar supports at file"

-- | Does this compiler's "ar" command support llvm-ar's -L flag,
-- which compels the archiver to add an input archive's members
-- rather than adding the archive itself.
arDashLSupported :: Compiler -> Bool
arDashLSupported = ghcSupported "ar supports -L"

-- | Does this compiler support Haskell program coverage?
coverageSupported :: Compiler -> Bool
coverageSupported comp =
  case compilerFlavor comp of
    GHC -> True
    GHCJS -> True
    _ -> False

-- | Does this compiler support profiling?
profilingSupported :: Compiler -> Bool
profilingSupported comp =
  case compilerFlavor comp of
    GHC -> True
    GHCJS -> True
    _ -> False

-- | Returns Just if we can certainly determine whether a way is supported
-- if we don't know, return Nothing
waySupported :: String -> Compiler -> Maybe Bool
waySupported way comp =
  case compilerFlavor comp of
    GHC ->
      -- Information about compiler ways is only accurately reported after
      -- 9.10.1. Which is useful as this is before profiling dynamic support
      -- was introduced. (See GHC #24881)
      if compilerVersion comp >= mkVersion [9, 10, 1]
        then case Map.lookup "RTS ways" (compilerProperties comp) of
          Just ways -> Just (way `elem` words ways)
          Nothing -> Just False
        else Nothing
    _ -> Nothing

-- | Either profiling is definitely supported or we don't know (so assume
-- it is)
profilingVanillaSupportedOrUnknown :: Compiler -> Bool
profilingVanillaSupportedOrUnknown comp = profilingVanillaSupported comp `elem` [Just True, Nothing]

-- | Is the compiler distributed with profiling libraries
profilingVanillaSupported :: Compiler -> Maybe Bool
profilingVanillaSupported comp = waySupported "p" comp

-- | Is the compiler distributed with profiling dynamic libraries
profilingDynamicSupported :: Compiler -> Maybe Bool
profilingDynamicSupported comp =
  -- Certainly not before this version, as it was not implemented yet.
  if compilerVersion comp <= mkVersion [9, 11, 0]
    then Just False
    else waySupported "p_dyn" comp

-- | Either profiling dynamic is definitely supported or we don't know (so assume
-- it is)
profilingDynamicSupportedOrUnknown :: Compiler -> Bool
profilingDynamicSupportedOrUnknown comp =
  profilingDynamicSupported comp `elem` [Just True, Nothing]

-- | Is the compiler distributed with dynamic libraries
dynamicSupported :: Compiler -> Maybe Bool
dynamicSupported comp = waySupported "dyn" comp

-- | Does this compiler support a package database entry with:
-- "visibility"?
libraryVisibilitySupported :: Compiler -> Bool
libraryVisibilitySupported comp = case compilerFlavor comp of
  GHC -> v >= mkVersion [8, 8]
  _ -> False
  where
    v = compilerVersion comp

-- | Utility function for GHC only features
ghcSupported :: String -> Compiler -> Bool
ghcSupported key comp =
  case compilerFlavor comp of
    GHC -> checkProp
    GHCJS -> checkProp
    _ -> False
  where
    checkProp =
      case Map.lookup key (compilerProperties comp) of
        Just "YES" -> True
        _ -> False

-- ------------------------------------------------------------

-- * Profiling detail level

-- ------------------------------------------------------------

-- | Some compilers (notably GHC) support profiling and can instrument
-- programs so the system can account costs to different functions. There are
-- different levels of detail that can be used for this accounting.
-- For compilers that do not support this notion or the particular detail
-- levels, this is either ignored or just capped to some similar level
-- they do support.
data ProfDetailLevel
  = ProfDetailNone
  | ProfDetailDefault
  | ProfDetailExportedFunctions
  | ProfDetailToplevelFunctions
  | ProfDetailAllFunctions
  | ProfDetailTopLate
  | ProfDetailOther String
  deriving (Eq, Generic, Read, Show)

instance Binary ProfDetailLevel
instance Structured ProfDetailLevel

instance Parsec ProfDetailLevel where
  parsec = parsecProfDetailLevel

parsecProfDetailLevel :: CabalParsing m => m ProfDetailLevel
parsecProfDetailLevel = flagToProfDetailLevel <$> parsecToken

flagToProfDetailLevel :: String -> ProfDetailLevel
flagToProfDetailLevel "" = ProfDetailDefault
flagToProfDetailLevel s =
  case lookup
    (lowercase s)
    [ (name, value)
    | (primary, aliases, value) <- knownProfDetailLevels
    , name <- primary : aliases
    ] of
    Just value -> value
    Nothing -> ProfDetailOther s

knownProfDetailLevels :: [(String, [String], ProfDetailLevel)]
knownProfDetailLevels =
  [ ("default", [], ProfDetailDefault)
  , ("none", [], ProfDetailNone)
  , ("exported-functions", ["exported"], ProfDetailExportedFunctions)
  , ("toplevel-functions", ["toplevel", "top"], ProfDetailToplevelFunctions)
  , ("all-functions", ["all"], ProfDetailAllFunctions)
  , ("late-toplevel", ["late"], ProfDetailTopLate)
  ]

showProfDetailLevel :: ProfDetailLevel -> String
showProfDetailLevel dl = case dl of
  ProfDetailNone -> "none"
  ProfDetailDefault -> "default"
  ProfDetailExportedFunctions -> "exported-functions"
  ProfDetailToplevelFunctions -> "toplevel-functions"
  ProfDetailAllFunctions -> "all-functions"
  ProfDetailTopLate -> "late-toplevel"
  ProfDetailOther other -> other
