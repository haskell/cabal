{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.Simple.MHS
-- Copyright   :  Cabal Devs 2026
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This module contains most of the MicroHs-specific code for configuring,
-- building and installing packages.
--
-- MicroHs (@mhs@, <https://github.com/augustss/MicroHs>) is a small Haskell
-- compiler that compiles to combinators. See Note [MicroHs package databases]
-- for how its packages are mapped onto the package databases of Cabal.
module Distribution.Simple.MHS
  ( configure
  , getInstalledPackages
  , getInstalledPackagesMonitorFiles
  , buildLib
  , buildExe
  , installLib
  , installExe
  , createPackageDB
  , registerPackage
  , rewritePackageDescription
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.InstalledPackageInfo
  ( ExposedModule (..)
  , InstalledPackageInfo
  , emptyInstalledPackageInfo
  , parseInstalledPackageInfo
  , showInstalledPackageInfo
  )
import qualified Distribution.InstalledPackageInfo as IPI
import Distribution.ModuleName (ModuleName)
import Distribution.Package hiding (installedUnitId)
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration (transformAllBuildDepends)
import Distribution.Parsec (simpleParsec)
import Distribution.Pretty
import Distribution.Simple.Build.Macros (generateCabalMacrosHeader)
import Distribution.Simple.BuildPaths
import Distribution.Simple.Compiler
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.PackageIndex (InstalledPackageIndex)
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Simple.Program
import Distribution.Simple.Utils
import Distribution.System (Platform)
import Distribution.Types.MungedPackageId (computeCompatPackageId)
import Distribution.Types.MungedPackageName (MungedPackageName (..), decodeCompatPackageName)
import Distribution.Utils.Path
import Distribution.Verbosity
import Distribution.Version
import Language.Haskell.Extension

import Distribution.Compat.Lens (over, view)
import qualified Distribution.Types.BuildInfo.Lens as L

import qualified Data.ByteString as BS
import Data.List (stripPrefix)
import qualified Data.Map as Map (fromList)
import qualified Data.Text as Text
import System.Directory (doesDirectoryExist, getDirectoryContents, getHomeDirectory, makeAbsolute)
import System.FilePath (takeDirectory, takeExtension, takeFileName)

{- Note [MicroHs package databases]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
MicroHs compiles a library into a single package file, <name>-<version>.pkg,
which 'mhs -P<name>-<version>' writes. When compiling, mhs loads the package
files it is told to with -p<pkgfile>, and finds the ones it is not told about
through its package search path: directories of the layout that
'mhs -Q <pkgfile> <dir>' creates,

  <dir>/packages/<name>-<version>.pkg   the compiled package
  <dir>/<Module>/<Name>.txt             one index file per exposed module,
                                        naming the package that provides it

A package database of Cabal is a directory of registration files,
<dir>/<unit-id>.conf, each the InstalledPackageInfo of a library, whose
package file stays where the library was built or installed: in its
library-dirs. When building a component, every package in the dependency
closure of the component is preloaded from there and the search path of mhs
is cleared, so that the component sees exactly the packages it depends on, as
it would with GHC. A library rebuilt in place is thus picked up by its
dependents without being registered again.

GlobalPackageDB is the default search path of mhs, as 'mhs -L' reports it; it
holds the packages that come with mhs, such as base. UserPackageDB is
~/.mcabal/mhs-<version>, where MicroCabal, the package manager of MicroHs,
installs packages for the user. The packages of these directories have no
registration file: they are described by what 'mhs -L<pkgfile>' reports about
them, with <name>-<version> as their unit id. A directory given as
SpecificPackageDB may hold such packages too.

The base of mhs is versioned like mhs itself (base-0.16.9.0, say), and it
bundles the modules of packages that are separate on Hackage: bytestring,
text, deepseq, stm, directory, process, hashable and integer-logarithms.
Packages from Hackage depend on base-4.x and on those packages by name, so
Cabal describes the base of mhs the way MicroCabal does: as base at a GHC-era
version, and the bundled packages as installed packages of their own that
depend on base and have no package file, so that base.pkg is what gets
loaded. See 'compatPackages'. The unit id of base stays <name>-<version> as
mhs knows it, which is also the name of its package file.
-}

{- Note [MicroHs rewrites of packages]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
MicroCabal rewrites the dependencies of every package it builds with mhs, and
so does cabal-install, see 'rewritePackageDescription': it rewrites the
packages it gives to the solver, so that the solver chooses, and cabal-install
builds, the packages mhs needs, as far as they are on Hackage or installed.
That is enough since cabal-install calls Cabal in-library (#11703): it
configures a package with the description of its plan, which is rewritten
already, rather than having a Setup script read the .cabal file of the
package again. Only a Custom setup script still would, and mhs cannot compile
one.

  * ghc-compat holds modules that are part of base in GHC but not in mhs,
    such as GHC.Stack, GHC.Exts and Language.Haskell.TH.Syntax. Packages
    written for GHC import them while depending only on base, so ghc-compat
    becomes a dependency of every component, except in base, ghc-compat and
    the packages of MicroHs itself; a component that depends on it already
    keeps its own bounds.
  * mhs cannot compile some packages from Hackage, and MicroHs has forks of
    them on Hackage that it can: array-mhs and random-mhs. A dependency on the
    original becomes one on the fork, of any version, as the bounds on the
    original are about its own versions, and a re-export that names the
    original names the fork. The version macros of the fork are defined under
    the name of the original too, see 'cabalMacrosOptions'.
-}

-- -----------------------------------------------------------------------------
-- Configuring

configure
  :: Verbosity
  -> Maybe FilePath
  -> ProgramDb
  -> IO (Compiler, Maybe Platform, ProgramDb)
configure verbosity hcPath progdb = do
  (_mhsProg, mhsVersion, progdb') <-
    requireProgramVersion
      verbosity
      mhsProgram
      (orLaterVersion (mkVersion [0, 15]))
      (userMaybeSpecifyPath "mhs" hcPath progdb)

  let comp =
        Compiler
          { compilerId = CompilerId MHS mhsVersion
          , compilerAbiTag = NoAbiTag
          , compilerCompat = []
          , compilerLanguages = mhsLanguages
          , compilerExtensions = mhsLanguageExtensions
          , compilerProperties =
              -- mhs sees all the modules of the packages it loads, which
              -- include the dependencies of the dependencies of a component, so
              -- the modules a package re-exports are visible to the components
              -- depending on it, as long as they are not renamed, which
              -- 'buildLib' checks.
              Map.fromList [("Support reexported-modules", "YES")]
          , compilerWiredInUnitIds = Nothing
          }
      compPlatform = Nothing
  return (comp, compPlatform, progdb')

-- | MicroHs has all the extensions it supports enabled at all times, so it
-- accepts every language.
mhsLanguages :: [(Language, CompilerFlag)]
mhsLanguages = [(lang, "") | lang <- knownLanguages]

-- | The extensions that MicroHs accepts: all of them. The extensions that mhs
-- implements are always on, except for CPP, and it ignores the LANGUAGE
-- pragmas of the others, so a package is not rejected for the extensions it
-- declares, which are often used only on the GHC side of a conditional. A
-- package that does need an extension that mhs does not implement fails when
-- mhs meets the syntax. The extensions that mhs implements are listed at
-- <https://github.com/augustss/MicroHs/wiki/Language>.
mhsLanguageExtensions :: [(Extension, Maybe CompilerFlag)]
mhsLanguageExtensions =
  (EnableExtension CPP, Just "-XCPP")
    : [(EnableExtension ext, Nothing) | ext <- knownExtensions, ext /= CPP]
    ++ [(DisableExtension ext, Nothing) | ext <- knownExtensions]
    ++ [(UnknownExtension ext, Nothing) | ext <- mhsSpecific]
  where
    -- Extensions of MicroHs that GHC does not have.
    mhsSpecific = ["ExtraInstanceBindings", "ImportShadowing", "TypeLits"]

-- -----------------------------------------------------------------------------
-- Package descriptions

-- | The package description as MicroCabal builds the package with mhs, as
-- far as the packages it uses instead are available, which the predicate says.
-- See Note [MicroHs rewrites of packages].
rewritePackageDescription
  :: (PackageName -> Bool) -> GenericPackageDescription -> GenericPackageDescription
rewritePackageDescription available gpd0
  | packageName gpd0 `elem` withoutGhcCompat || not (available ghcCompat) = gpd
  | otherwise =
      gpd
        { condLibrary = fmap addGhcCompat (condLibrary gpd)
        , condSubLibraries = map (fmap addGhcCompat) (condSubLibraries gpd)
        , condForeignLibs = map (fmap addGhcCompat) (condForeignLibs gpd)
        , condExecutables = map (fmap addGhcCompat) (condExecutables gpd)
        , condTestSuites = map (fmap addGhcCompat) (condTestSuites gpd)
        , condBenchmarks = map (fmap addGhcCompat) (condBenchmarks gpd)
        }
  where
    gpd =
      forked
        { condLibrary = fmap (fmap useForkReexports) (condLibrary forked)
        , condSubLibraries = map (fmap (fmap useForkReexports)) (condSubLibraries forked)
        }
    forked = transformAllBuildDepends useFork gpd0
    useFork dep@(Dependency name _ libs) =
      case lookup name forkedPackages of
        Just fork | available fork -> Dependency fork anyVersion libs
        _ -> dep
    -- A re-export that names the package of a module names the fork too.
    useForkReexports lib = lib{reexportedModules = map useForkReexport (reexportedModules lib)}
    useForkReexport reexport@ModuleReexport{moduleReexportOriginalPackage = Just name}
      | Just fork <- lookup name forkedPackages, available fork = reexport{moduleReexportOriginalPackage = Just fork}
    useForkReexport reexport = reexport
    ghcCompatDependency = Dependency ghcCompat anyVersion mainLibSet
    -- Every component gets the dependency at its root, unless it depends on
    -- ghc-compat there already, with bounds of its own. A dependency under a
    -- condition, or in another component, is no reason to leave it out.
    addGhcCompat :: L.HasBuildInfo a => CondTree v a -> CondTree v a
    addGhcCompat node@(CondNode component branches)
      | any ((== ghcCompat) . depPkgName) (view L.targetBuildDepends component) = node
      | otherwise = CondNode (over L.targetBuildDepends (++ [ghcCompatDependency]) component) branches

ghcCompat :: PackageName
ghcCompat = mkPackageName "ghc-compat"

-- | The packages that are not built with ghc-compat, as MicroCabal has them:
-- base and ghc-compat themselves, and those of MicroHs.
withoutGhcCompat :: [PackageName]
withoutGhcCompat = map mkPackageName ["base", "ghc-compat", "MicroHs", "MicroCabal", "canvhs"]

-- | The forks of packages from Hackage that mhs can compile, as MicroCabal
-- uses them.
forkedPackages :: [(PackageName, PackageName)]
forkedPackages =
  [ (mkPackageName "array", mkPackageName "array-mhs")
  , (mkPackageName "random", mkPackageName "random-mhs")
  ]

-- -----------------------------------------------------------------------------
-- Installed packages

getInstalledPackages
  :: Verbosity
  -> Compiler
  -> Maybe (SymbolicPath CWD (Dir from))
  -> PackageDBStackX (SymbolicPath from (Dir PkgDB))
  -> ProgramDb
  -> IO InstalledPackageIndex
getInstalledPackages verbosity comp mbWorkDir packagedbs progdb = do
  dirs <- packageDbDirs verbosity comp progdb mbWorkDir packagedbs
  pkgs <- concat <$> traverse (readPackageDbDir verbosity progdb) dirs
  return (PackageIndex.fromList pkgs)

-- | What changes when the packages of the databases do: the directories of
-- the databases, where registration files and the packages subdirectory come
-- and go, the packages subdirectories, where package files come and go, and
-- the files themselves, which are rewritten in place when a package is
-- registered or installed again. See Note [MicroHs package databases].
getInstalledPackagesMonitorFiles
  :: Verbosity
  -> Compiler
  -> ProgramDb
  -> Maybe (SymbolicPath CWD (Dir from))
  -> PackageDBStackX (SymbolicPath from (Dir PkgDB))
  -> IO [FilePath]
getInstalledPackagesMonitorFiles verbosity comp progdb mbWorkDir packagedbs = do
  dirs <- packageDbDirs verbosity comp progdb mbWorkDir packagedbs
  fmap concat $ for dirs $ \dir -> do
    registrations <- filesWithExtension ".conf" dir
    pkgFiles <- filesWithExtension ".pkg" (dir </> "packages")
    return (dir : (dir </> "packages") : registrations ++ pkgFiles)

-- | The directories of the given package databases, in order.
-- See Note [MicroHs package databases].
packageDbDirs
  :: Verbosity
  -> Compiler
  -> ProgramDb
  -> Maybe (SymbolicPath CWD (Dir from))
  -> PackageDBStackX (SymbolicPath from (Dir PkgDB))
  -> IO [FilePath]
packageDbDirs verbosity comp progdb mbWorkDir packagedbs =
  ordNub . concat <$> traverse dirsOf packagedbs
  where
    dirsOf GlobalPackageDB = globalPackageDbDirs verbosity progdb
    dirsOf UserPackageDB = (: []) <$> userPackageDbDir comp
    dirsOf (SpecificPackageDB dir) = return [interpretSymbolicPath mbWorkDir dir]

-- | The default package search path of mhs, as @mhs -L@ lists it:
--
-- > /path/to/mhs-0.16.9.0/packages:
-- >   MicroHs-0.16.9.0
-- >   base-0.16.9.0
globalPackageDbDirs :: Verbosity -> ProgramDb -> IO [FilePath]
globalPackageDbDirs verbosity progdb = do
  output <- getDbProgramOutput verbosity mhsProgram progdb ["-L"]
  -- The listing shows the packages subdirectories.
  return
    [ takeDirectory (dropWhileEnd (== ':') line)
    | line@(c : _) <- lines output
    , not (isSpace c)
    , ":" `isSuffixOf` line
    ]

-- | Where MicroCabal installs packages for the user.
userPackageDbDir :: Compiler -> IO FilePath
userPackageDbDir comp = do
  home <- getHomeDirectory
  return (home </> ".mcabal" </> prettyShow (compilerId comp))

-- | The packages of a package database directory.
-- See Note [MicroHs package databases].
readPackageDbDir :: Verbosity -> ProgramDb -> FilePath -> IO [InstalledPackageInfo]
readPackageDbDir verbosity progdb dir = do
  raw <- traverse (readRegistrationFile verbosity) =<< filesWithExtension ".conf" dir
  let registeredPkgFiles = map mhsPackageFileName raw
      -- A base that Cabal registered, built from the sources of MicroHs, is
      -- the base of mhs as much as the one that comes with it.
      registered = concatMap withCompat raw
  pkgFiles <- filesWithExtension ".pkg" (dir </> "packages")
  unregistered <-
    traverse (describePackageFile verbosity progdb) $
      [pkgFile | pkgFile <- pkgFiles, takeFileName pkgFile `notElem` registeredPkgFiles]
  return (registered ++ concat unregistered)

-- | The paths of the files with the given extension in a directory, if the
-- directory exists.
filesWithExtension :: String -> FilePath -> IO [FilePath]
filesWithExtension ext dir = do
  exists <- doesDirectoryExist dir
  if exists
    then do
      files <- getDirectoryContents dir
      return [dir </> file | file <- sort files, takeExtension file == ext]
    else return []

-- | Read a registration file written by 'registerPackage'.
readRegistrationFile :: Verbosity -> FilePath -> IO InstalledPackageInfo
readRegistrationFile verbosity file = do
  contents <- BS.readFile file
  case parseInstalledPackageInfo contents of
    Left errs -> dieWithException verbosity $ MhsRegistrationFileParseError file (toList errs)
    Right (warnings, ipi) -> do
      traverse_ (warn verbosity) warnings
      return ipi

-- | Describe a package that mhs or MicroCabal installed, from what
-- @mhs -L<pkgfile>@ reports about it:
--
-- > name: base
-- > version: 0.16.9.0
-- > compiler: mhs-0.16.9.0
-- > depends:
-- > linker opts: -lm
-- > exposed-modules:
-- >   Control.Applicative
-- >   ...
-- > other-modules:
-- >   ...
--
-- The base of mhs is described as several packages, see 'compatPackages'.
describePackageFile :: Verbosity -> ProgramDb -> FilePath -> IO [InstalledPackageInfo]
describePackageFile verbosity progdb pkgFile = do
  output <- getDbProgramOutput verbosity mhsProgram progdb ["-L" ++ pkgFile]
  let fields = parseFields output
      field name = fromMaybe [] (lookup name fields)
      modules :: String -> [ModuleName]
      modules name = mapMaybe simpleParsec (field name)
  case (field "name", field "version") of
    ([name], [versionStr])
      | Just version <- simpleParsec versionStr -> do
          let MungedPackageName pname libname = decodeCompatPackageName (mkPackageName name)
              pkgId = PackageIdentifier pname version
              unitId = mkUnitId (mhsPackageId pkgId libname)
              pkg =
                emptyInstalledPackageInfo
                  { IPI.sourcePackageId = pkgId
                  , IPI.sourceLibName = libname
                  , IPI.installedUnitId = unitId
                  , IPI.installedComponentId_ = mkComponentId (unUnitId unitId)
                  , IPI.exposed = True
                  , IPI.exposedModules = [ExposedModule m Nothing | m <- modules "exposed-modules"]
                  , IPI.hiddenModules = modules "other-modules"
                  , IPI.depends = map mkUnitId (field "depends")
                  , IPI.libraryDirs = [takeDirectory pkgFile]
                  , IPI.ldOptions = field "linker opts"
                  }
          return (withCompat pkg)
    _ -> do
      warn verbosity $
        "Ignoring the MicroHs package file " ++ pkgFile ++ ", which mhs cannot describe."
      return []

-- | Parse the fields of what @mhs -L<pkgfile>@ reports: a @name: value@ line
-- starts a field, and the indented lines that follow continue it. The value
-- of a field is its words.
parseFields :: String -> [(String, [String])]
parseFields = go . lines
  where
    go [] = []
    go (line : rest)
      | (name, ':' : value) <- break (== ':') line
      , not (isIndented line) =
          let (continuation, rest') = span isIndented rest
           in (name, words value ++ concatMap words continuation) : go rest'
      | otherwise = go rest
    isIndented (c : _) = isSpace c
    isIndented [] = False

-- | The version at which the base of mhs is presented, and the packages of
-- Hackage whose modules it bundles, with the version at which each is
-- presented and the modules (given by prefix) that are theirs.
--
-- The versions are those that MicroCabal uses for the same purpose; in its
-- words, "the version numbers are totally fake; they are from GHC". They say
-- which API the modules roughly implement. The exception is hashable, which
-- MicroCabal has at 1.0.0.0, "very rudimentary": the Data.Hashable of mhs has
-- the API of the 1.4 series (Data.Hashable.Lifted, Hashable1 with an Eq1
-- superclass), and the packages that depend on hashable require at least that
-- (unordered-containers 1.4, these 1.4.4), which MicroCabal can ignore, as it
-- does not check bounds, but Cabal cannot.
baseCompatVersion :: Version
baseCompatVersion = mkVersion [4, 19, 1, 0]

bundledPackages :: [(String, [Int], [String])]
bundledPackages =
  [ ("bytestring", [0, 12, 1, 0], ["Data.ByteString"])
  , ("deepseq", [1, 4, 4, 0], ["Control.DeepSeq"])
  , ("directory", [1, 3, 8, 1], ["System.Directory"])
  , ("hashable", [1, 4, 7, 0], ["Data.Hashable"])
  , ("integer-logarithms", [1, 0, 4], ["Math.NumberTheory"])
  , ("process", [1, 6, 18, 0], ["System.Process"])
  , ("stm", [2, 5, 3, 0], ["Control.Concurrent.STM", "Control.Monad.STM"])
  , ("text", [2, 1, 1], ["Data.Text"])
  ]

-- | The packages that an installed package is presented as: the base of mhs
-- as 'compatPackages', any other package as itself.
withCompat :: InstalledPackageInfo -> [InstalledPackageInfo]
withCompat ipi
  | pkgName (IPI.sourcePackageId ipi) == mkPackageName "base" && IPI.sourceLibName ipi == LMainLibName = compatPackages ipi
  | otherwise = [ipi]

-- | Present the base of mhs the way packages from Hackage expect it: at
-- 'baseCompatVersion', and with the 'bundledPackages' as installed packages
-- of their own, which depend on base and have no package file of their own.
-- See Note [MicroHs package databases].
compatPackages :: InstalledPackageInfo -> [InstalledPackageInfo]
compatPackages base =
  base
    { IPI.sourcePackageId = PackageIdentifier (packageName base) baseCompatVersion
    , IPI.exposedModules = [m | m <- IPI.exposedModules base, not (any (bundles m) prefixesOf)]
    }
    : [ emptyInstalledPackageInfo
        { IPI.sourcePackageId = pkgId
        , IPI.installedUnitId = mkUnitId (prettyShow pkgId)
        , IPI.installedComponentId_ = mkComponentId (prettyShow pkgId)
        , IPI.exposed = True
        , IPI.exposedModules = [m | m <- IPI.exposedModules base, bundles m prefixes]
        , IPI.depends = [IPI.installedUnitId base]
        }
      | (name, version, prefixes) <- bundledPackages
      , let pkgId = PackageIdentifier (mkPackageName name) (mkVersion version)
      ]
  where
    prefixesOf = [prefixes | (_, _, prefixes) <- bundledPackages]
    bundles (ExposedModule m _) prefixes =
      any (\root -> moduleName == root || (root ++ ".") `isPrefixOf` moduleName) prefixes
      where
        moduleName = prettyShow m

-- | The name under which mhs knows a library: @<name>-<version>@, with the
-- munged name of the library, so that the sub-libraries of a package get
-- distinct names.
mhsPackageId :: PackageId -> LibraryName -> String
mhsPackageId pkgId libname = prettyShow (computeCompatPackageId pkgId libname)

-- | The name of the package file of a library built by Cabal,
-- @<name>-<version>.pkg@.
mhsPackageFileName :: InstalledPackageInfo -> FilePath
mhsPackageFileName ipi = mhsPackageId (IPI.sourcePackageId ipi) (IPI.sourceLibName ipi) <.> "pkg"

-- | The package file of a library, in one of its library directories: named
-- after its unit id for a package that comes with mhs, see
-- 'describePackageFile', and after its munged package id for a library built
-- by Cabal, see 'buildLib', as it was built: the compat package key of its
-- registration, which is the name and version Cabal built it at, even when
-- it is presented at another version, see 'compatPackages'. A library
-- without library directories has no
-- package file: it has no modules, or mhs provides it as part of base, see
-- 'compatPackages'. A library without modules is registered with its
-- extra-lib-dirs as its library directories, where its package file is not:
-- its linker options are taken from its registration instead, see 'runMhs'.
packageFile :: Verbosity -> InstalledPackageInfo -> IO (Maybe FilePath)
packageFile verbosity ipi
  | null dirs = return Nothing
  | otherwise = do
      mbFile <- findFirstFile id [dir </> file | dir <- dirs, file <- files]
      when (isNothing mbFile && not (null (IPI.exposedModules ipi) && null (IPI.hiddenModules ipi))) $
        dieWithException verbosity $
          MhsPackageFileNotFound (IPI.installedUnitId ipi) dirs
      return mbFile
  where
    dirs = IPI.libraryDirs ipi
    files =
      ordNub $
        [unUnitId (IPI.installedUnitId ipi) <.> "pkg", mhsPackageFileName ipi]
          ++ [IPI.compatPackageKey ipi <.> "pkg" | not (null (IPI.compatPackageKey ipi))]

-- -----------------------------------------------------------------------------
-- Building

buildLib
  :: Verbosity
  -> PackageDescription
  -> LocalBuildInfo
  -> Library
  -> ComponentLocalBuildInfo
  -> IO ()
buildLib verbosity pkg_descr lbi lib clbi = do
  let bi = libBuildInfo lib
      pkgId = mhsPackageId (packageId pkg_descr) (libName lib)
      targetDir = componentBuildDir lbi clbi
      pkgFile = targetDir </> makeRelativePathEx (pkgId <.> "pkg")
      -- The modules given to mhs are the ones that the package exposes; it
      -- finds the other modules through the imports. A library without exposed
      -- modules has to expose its other modules to be of any use.
      modules
        | null (exposedModules lib) = otherModules bi
        | otherwise = exposedModules lib
      -- mhs provides the modules of a package under their own names only.
      renamed = [r | r <- reexportedModules lib, moduleReexportName r /= moduleReexportOriginalName r]
  unless (null renamed) $
    dieWithException verbosity $
      MhsRenamedReexports renamed
  createDirectoryIfMissingVerbose verbosity True (interpretSymbolicPathLBI lbi targetDir)
  unless (null (cSources bi)) $
    warn verbosity "MicroHs cannot include C sources in a library; ignoring them."
  runMhs verbosity pkg_descr lbi bi clbi targetDir $
    ["-P" ++ pkgId, "-o", interpretSymbolicPathCWD pkgFile]
      ++ map prettyShow modules

buildExe
  :: Verbosity
  -> PackageDescription
  -> LocalBuildInfo
  -> Executable
  -> ComponentLocalBuildInfo
  -> IO ()
buildExe verbosity pkg_descr lbi exe clbi = do
  let bi = buildInfo exe
      exeName' = prettyShow (exeName exe)
      targetDir = buildDir lbi </> makeRelativePathEx exeName'
      exeFile = targetDir </> makeRelativePathEx (exeName' <.> exeExtension (hostPlatform lbi))
      u = interpretSymbolicPathCWD -- See Note [Symbolic paths] in Distribution.Utils.Path
      -- The main module is in the build directory of the executable when a
      -- preprocessor generated it.
      generatedDir = exeBuildDir lbi exe
  srcMainPath <-
    findFileCwd verbosity (mbWorkDirLBI lbi) (coerceSymbolicPath generatedDir : sourceDirs bi) (modulePath exe)
  createDirectoryIfMissingVerbose verbosity True (interpretSymbolicPathLBI lbi targetDir)
  runMhs verbosity pkg_descr lbi bi clbi generatedDir $
    -- Compress the combinator code of the program, as MicroCabal does.
    ["-z"]
      ++ map u (cSources bi)
      ++ ["-o", u exeFile, u srcMainPath]

-- | Run mhs with the arguments common to libraries and executables, followed
-- by the given ones.
runMhs
  :: Verbosity
  -> PackageDescription
  -> LocalBuildInfo
  -> BuildInfo
  -> ComponentLocalBuildInfo
  -> SymbolicPath Pkg (Dir Build)
  -- ^ where the preprocessors put the modules they generated
  -> [String]
  -> IO ()
runMhs verbosity pkg_descr lbi bi clbi generatedDir args = do
  let comp = compiler lbi
      mbWorkDir = mbWorkDirLBI lbi
      progdb = withPrograms lbi
      u = interpretSymbolicPathCWD -- See Note [Symbolic paths] in Distribution.Utils.Path
  pkgFiles <- dependencyPackageFiles verbosity lbi clbi
  -- The extra-lib-dirs are made absolute, as the package file of a library
  -- keeps the linker options for the programs linked elsewhere.
  libDirs <- traverse (makeAbsolute . interpretSymbolicPathLBI lbi) (extraLibDirs bi)
  let commonArgs =
        mhsVerbosityFlags verbosity
          -- The module search path: the source directories of the component
          -- instead of the default, the current directory.
          ++ ["-i"]
          ++ ["-i" ++ u dir | dir <- sourceDirs bi]
          -- The modules that preprocessors generated, from .hsc files say,
          -- are in the build directory of the component.
          ++ ["-i" ++ u generatedDir]
          ++ ["-i" ++ u (autogenComponentModulesDir lbi clbi)]
          ++ ["-i" ++ u (autogenPackageModulesDir lbi)]
          -- The packages: the dependencies of the component, preloaded, instead
          -- of whatever is on the package search path.
          -- See Note [MicroHs package databases].
          ++ ["-a"]
          ++ ["-p" ++ pkgFile | pkgFile <- pkgFiles]
          ++ languageToFlags comp (defaultLanguage bi)
          ++ extensionsToFlags comp (usedExtensions bi)
          -- mhs passes only -D and -I options on to cpphs, so the macros of
          -- cabal_macros.h are defined one by one rather than included. The
          -- header is found for the modules that include it themselves.
          ++ cabalMacrosOptions pkg_descr lbi clbi
          ++ ["-I" ++ u (autogenComponentModulesDir lbi clbi)]
          -- The headers that hsc2hs generates beside the modules, for the C
          -- it generates with them.
          ++ ["-I" ++ u generatedDir]
          ++ [opt | opt <- map Text.unpack (cppOptions bi), any (`isPrefixOf` opt) ["-D", "-I"]]
          ++ ["-I" ++ u dir | dir <- includeDirs bi]
          ++ map Text.unpack (hcOptions MHS bi)
          ++ concat [["-optl", opt] | opt <- linkerOptions]
      linkerOptions =
        ["-L" ++ dir | dir <- libDirs]
          ++ ["-l" ++ Text.unpack lib | lib <- extraLibs bi]
          ++ map Text.unpack (ldOptions bi)
  runDbProgramCwd verbosity mbWorkDir mhsProgram progdb (commonArgs ++ args)

-- | The package files of the dependencies of a component, closed under the
-- dependencies of the dependencies.
dependencyPackageFiles :: Verbosity -> LocalBuildInfo -> ComponentLocalBuildInfo -> IO [FilePath]
dependencyPackageFiles verbosity lbi clbi = do
  -- The index of configure, with the libraries built since threaded in by
  -- build, has the dependencies, unless they were registered since the package
  -- was configured: then the package databases are read again.
  closure <- case PackageIndex.dependencyClosure (installedPkgs lbi) deps of
    Left closure -> return (PackageIndex.allPackages closure)
    Right _ -> do
      index <-
        getInstalledPackages verbosity (compiler lbi) (mbWorkDirLBI lbi) (withPackageDB lbi) (withPrograms lbi)
      case PackageIndex.dependencyClosure index deps of
        Left closure -> return (PackageIndex.allPackages closure)
        Right broken ->
          dieWithException verbosity $
            MhsMissingDependencies $
              ordNub $
                [dep | dep <- deps, isNothing (PackageIndex.lookupUnitId index dep)]
                  ++ concatMap snd broken
  catMaybes <$> traverse (packageFile verbosity) closure
  where
    deps = map fst (componentPackageDeps clbi)

-- | The macros of @cabal_macros.h@ as @-D@ options of cpphs, to which mhs passes
-- on only the @-D@ and @-I@ options it is given, so that it cannot be told to
-- include the header: every @#define@ of the header, with its continuation
-- lines joined, becomes an option, and the first definition of a macro wins,
-- as the @#ifndef@ guards of the header make it. The macros of a fork of
-- 'forkedPackages' are also defined under the name of the package it forks,
-- as MicroCabal does: a package that depends on random tests
-- @MIN_VERSION_random@, which is then the version of random-mhs.
cabalMacrosOptions :: PackageDescription -> LocalBuildInfo -> ComponentLocalBuildInfo -> [String]
cabalMacrosOptions pkg_descr lbi clbi =
  [ "-D" ++ lhs ++ "=" ++ rhs
  | (_, lhs, rhs) <- ordNubBy (\(name, _, _) -> name) (defines ++ forkDefines)
  ]
  where
    header = generateCabalMacrosHeader pkg_descr lbi clbi
    forkDefines =
      [ (macro ++ mangle original, macro ++ mangle original ++ drop (length name) lhs, rhs)
      | (original, fork) <- forkedPackages
      , (name, lhs, rhs) <- defines
      , macro <- ["VERSION_", "MIN_VERSION_"]
      , name == macro ++ mangle fork
      ]
    mangle = map (\c -> if c == '-' then '_' else c) . unPackageName
    defines =
      [ (takeWhile (/= '(') lhs, lhs, dropWhile isSpace rhs)
      | line <- joinContinuations (lines header)
      , Just definition <- [stripPrefix "#define " line]
      , let (lhs, rhs) = break isSpace definition
      ]
    joinContinuations (line : rest)
      | '\\' : reversedLine <- reverse line =
          case joinContinuations rest of
            next : rest' -> (reverse reversedLine ++ next) : rest'
            [] -> [reverse reversedLine]
      | otherwise = line : joinContinuations rest
    joinContinuations [] = []

-- | The source directories of a component; the package directory if none are
-- given.
sourceDirs :: BuildInfo -> [SymbolicPath Pkg (Dir Source)]
sourceDirs bi
  | null (hsSourceDirs bi) = [sameDirectory]
  | otherwise = ordNub (hsSourceDirs bi)

mhsVerbosityFlags :: Verbosity -> [String]
mhsVerbosityFlags verbosity
  | verbosityLevel verbosity >= Deafening = ["-v", "-v"]
  | verbosityLevel verbosity >= Verbose = ["-v"]
  | verbosityLevel verbosity < Normal = ["-q"]
  | otherwise = []

-- -----------------------------------------------------------------------------
-- Installation

installLib
  :: Verbosity
  -> LocalBuildInfo
  -> FilePath
  -- ^ install location
  -> FilePath
  -- ^ install location for dynamic libraries (unused)
  -> FilePath
  -- ^ build location
  -> PackageDescription
  -> Library
  -> ComponentLocalBuildInfo
  -> IO ()
installLib verbosity _lbi targetDir _dynlibTargetDir builtDir pkg lib _clbi = do
  let pkgFile = mhsPackageId (packageId pkg) (libName lib) <.> "pkg"
  createDirectoryIfMissingVerbose verbosity True targetDir
  installOrdinaryFile verbosity (builtDir </> pkgFile) (targetDir </> pkgFile)

installExe
  :: Verbosity
  -> LocalBuildInfo
  -> FilePath
  -- ^ install location
  -> FilePath
  -- ^ build location
  -> (FilePath, FilePath)
  -- ^ executable (prefix, suffix)
  -> PackageDescription
  -> Executable
  -> IO ()
installExe verbosity lbi binDir buildPref (progprefix, progsuffix) _pkg exe = do
  let exeName' = prettyShow (exeName exe)
      ext = exeExtension (hostPlatform lbi)
  createDirectoryIfMissingVerbose verbosity True binDir
  installExecutableFile
    verbosity
    (buildPref </> exeName' </> exeName' <.> ext)
    (binDir </> progprefix ++ exeName' ++ progsuffix <.> ext)

-- -----------------------------------------------------------------------------
-- Registering

-- | An empty package database is an empty directory.
-- See Note [MicroHs package databases].
createPackageDB :: Verbosity -> FilePath -> IO ()
createPackageDB verbosity dbPath =
  createDirectoryIfMissingVerbose verbosity True dbPath

-- | Register a library by writing its registration file into the database.
-- See Note [MicroHs package databases].
registerPackage
  :: Verbosity
  -> Maybe (SymbolicPath CWD (Dir from))
  -> Compiler
  -> ProgramDb
  -> PackageDBStackS from
  -> InstalledPackageInfo
  -> IO ()
registerPackage verbosity mbWorkDir comp progdb packageDbs installedPkgInfo = do
  dbDir <- case registrationPackageDB packageDbs of
    GlobalPackageDB -> do
      dirs <- globalPackageDbDirs verbosity progdb
      case dirs of
        dir : _ -> return dir
        [] -> dieWithException verbosity MhsNoGlobalPackageDb
    UserPackageDB -> userPackageDbDir comp
    SpecificPackageDB dir -> return (interpretSymbolicPath mbWorkDir dir)
  createDirectoryIfMissingVerbose verbosity True dbDir
  writeUTF8File
    (dbDir </> prettyShow (IPI.installedUnitId installedPkgInfo) <.> "conf")
    (showInstalledPackageInfo installedPkgInfo)

-- -----------------------------------------------------------------------------
-- Errors

-- | The errors of building with MicroHs.
data MhsException
  = MhsNoGlobalPackageDb
  | MhsPackageFileNotFound UnitId [FilePath]
  | MhsRegistrationFileParseError FilePath [String]
  | MhsMissingDependencies [UnitId]
  | MhsRenamedReexports [ModuleReexport]
  deriving (Show)

instance Exception (VerboseException MhsException) where
  displayException (VerboseException stack timestamp verb err) =
    withOutputMarker verb ("Error: [Cabal-" ++ show (mhsExceptionCode err) ++ "]\n")
      ++ exceptionWithMetadata stack timestamp verb (mhsExceptionMessage err)

mhsExceptionCode :: MhsException -> Int
mhsExceptionCode err = case err of
  MhsNoGlobalPackageDb{} -> 3201
  MhsPackageFileNotFound{} -> 3202
  MhsRegistrationFileParseError{} -> 3203
  MhsMissingDependencies{} -> 3204
  MhsRenamedReexports{} -> 3205

mhsExceptionMessage :: MhsException -> String
mhsExceptionMessage err = case err of
  MhsNoGlobalPackageDb ->
    "mhs does not report a package directory to register the package in (see 'mhs -L')."
  MhsPackageFileNotFound unitId dirs ->
    "Cannot find the MicroHs package file of " ++ prettyShow unitId ++ " in " ++ intercalate ", " dirs
  MhsRegistrationFileParseError file errs ->
    "Failed to parse the package registration file " ++ file ++ ":\n" ++ unlines errs
  MhsMissingDependencies unitIds ->
    "Failed to find the installed units "
      ++ intercalate ", " (map prettyShow unitIds)
      ++ " in the package database stack."
  MhsRenamedReexports reexports ->
    "MicroHs provides the modules of a package under their own names only, so a library cannot re-export modules under other names: "
      ++ intercalate ", " (map prettyShow reexports)
