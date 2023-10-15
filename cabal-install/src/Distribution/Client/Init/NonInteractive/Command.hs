{-# LANGUAGE LambdaCase #-}

module Distribution.Client.Init.NonInteractive.Command
  ( genPkgDescription
  , genLibTarget
  , genExeTarget
  , genTestTarget
  , createProject
  , packageTypeHeuristics
  , authorHeuristics
  , emailHeuristics
  , cabalVersionHeuristics
  , packageNameHeuristics
  , versionHeuristics
  , mainFileHeuristics
  , testDirsHeuristics
  , initializeTestSuiteHeuristics
  , exposedModulesHeuristics
  , libOtherModulesHeuristics
  , exeOtherModulesHeuristics
  , testOtherModulesHeuristics
  , buildToolsHeuristics
  , dependenciesHeuristics
  , otherExtsHeuristics
  , licenseHeuristics
  , homepageHeuristics
  , synopsisHeuristics
  , categoryHeuristics
  , extraDocFileHeuristics
  , appDirsHeuristics
  , srcDirsHeuristics
  , languageHeuristics
  , noCommentsHeuristics
  , minimalHeuristics
  , overwriteHeuristics
  ) where

import Distribution.Client.Init.Types

import Distribution.Client.Compat.Prelude hiding (getLine, head, last, putStr, putStrLn)
import Prelude ()

import Data.List (last)
import qualified Data.List.NonEmpty as NEL

import Distribution.CabalSpecVersion (CabalSpecVersion (..))
import Distribution.Client.Init.Defaults
import Distribution.Client.Init.FlagExtractors
import Distribution.Client.Init.NonInteractive.Heuristics
import Distribution.Client.Init.Utils
import Distribution.Client.Types (SourcePackageDb (..))
import Distribution.ModuleName (ModuleName, components)
import Distribution.Simple.PackageIndex (InstalledPackageIndex)
import Distribution.Simple.Setup (Flag (..), fromFlagOrDefault)
import Distribution.Solver.Types.PackageIndex (elemByPackageName)
import Distribution.Types.Dependency (Dependency (..))
import Distribution.Types.PackageName (PackageName, unPackageName)
import Distribution.Utils.Generic (safeHead)
import Distribution.Verbosity
import Distribution.Version (Version)

import Language.Haskell.Extension (Extension (..), Language (..))

import qualified Data.Set as Set
import Distribution.FieldGrammar.Newtypes
import Distribution.Simple.Compiler
import System.FilePath (splitDirectories, (</>))

-- | Main driver for interactive prompt code.
createProject
  :: Interactive m
  => Compiler
  -> Verbosity
  -> InstalledPackageIndex
  -> SourcePackageDb
  -> InitFlags
  -> m ProjectSettings
createProject comp v pkgIx srcDb initFlags = do
  -- The workflow is as follows:
  --
  --  1. Get the package type, supplied as either a program input or
  --     via user prompt. This determines what targets will be built
  --     in later steps.
  --
  --  2. Determine whether we generate simple targets or prompt the
  --     user for inputs when not supplied as a flag. In general,
  --     flag inputs are preferred, and "simple" here means
  --     reasonable defaults defined in @Defaults.hs@.
  --
  --  3. Generate package description and the targets specified by
  --     the package type. Once this is done, a prompt for building
  --     test suites is initiated, and this determines if we build
  --     test targets as well. Then we ask if the user wants to
  --     comment their .cabal file with pretty comments.
  --
  --  4. The targets are passed to the file creator script, and associated
  --     directories/files/modules are created, with the a .cabal file
  --     being generated as a final result.
  --

  pkgType <- packageTypeHeuristics initFlags
  isMinimal <- getMinimal initFlags
  doOverwrite <- getOverwrite initFlags
  pkgDir <- packageDirHeuristics initFlags
  pkgDesc <- fixupDocFiles v =<< genPkgDescription initFlags srcDb
  comments <- noCommentsHeuristics initFlags

  let pkgName = _pkgName pkgDesc
      cabalSpec = _pkgCabalVersion pkgDesc
      mkOpts cs =
        WriteOpts
          doOverwrite
          isMinimal
          cs
          v
          pkgDir
          pkgType
          pkgName

  case pkgType of
    Library -> do
      libTarget <- genLibTarget initFlags comp pkgIx cabalSpec
      testTarget <-
        addLibDepToTest pkgName
          <$> genTestTarget initFlags comp pkgIx cabalSpec

      return $
        ProjectSettings
          (mkOpts comments cabalSpec)
          pkgDesc
          (Just libTarget)
          Nothing
          testTarget
    Executable -> do
      exeTarget <- genExeTarget initFlags comp pkgIx cabalSpec

      return $
        ProjectSettings
          (mkOpts comments cabalSpec)
          pkgDesc
          Nothing
          (Just exeTarget)
          Nothing
    LibraryAndExecutable -> do
      libTarget <- genLibTarget initFlags comp pkgIx cabalSpec
      exeTarget <-
        addLibDepToExe pkgName
          <$> genExeTarget initFlags comp pkgIx cabalSpec
      testTarget <-
        addLibDepToTest pkgName
          <$> genTestTarget initFlags comp pkgIx cabalSpec

      return $
        ProjectSettings
          (mkOpts comments cabalSpec)
          pkgDesc
          (Just libTarget)
          (Just exeTarget)
          testTarget
    TestSuite -> do
      testTarget <- genTestTarget initFlags comp pkgIx cabalSpec

      return $
        ProjectSettings
          (mkOpts comments cabalSpec)
          pkgDesc
          Nothing
          Nothing
          testTarget

genPkgDescription
  :: Interactive m
  => InitFlags
  -> SourcePackageDb
  -> m PkgDescription
genPkgDescription flags srcDb =
  PkgDescription
    <$> cabalVersionHeuristics flags
    <*> packageNameHeuristics srcDb flags
    <*> versionHeuristics flags
    <*> licenseHeuristics flags
    <*> authorHeuristics flags
    <*> emailHeuristics flags
    <*> homepageHeuristics flags
    <*> synopsisHeuristics flags
    <*> categoryHeuristics flags
    <*> getExtraSrcFiles flags
    <*> extraDocFileHeuristics flags

genLibTarget
  :: Interactive m
  => InitFlags
  -> Compiler
  -> InstalledPackageIndex
  -> CabalSpecVersion
  -> m LibTarget
genLibTarget flags comp pkgs v = do
  srcDirs <- srcDirsHeuristics flags
  let srcDir = fromMaybe defaultSourceDir $ safeHead srcDirs
  LibTarget srcDirs
    <$> languageHeuristics flags comp
    <*> exposedModulesHeuristics flags
    <*> libOtherModulesHeuristics flags
    <*> otherExtsHeuristics flags srcDir
    <*> dependenciesHeuristics flags srcDir pkgs
    <*> buildToolsHeuristics flags srcDir v

genExeTarget
  :: Interactive m
  => InitFlags
  -> Compiler
  -> InstalledPackageIndex
  -> CabalSpecVersion
  -> m ExeTarget
genExeTarget flags comp pkgs v = do
  appDirs <- appDirsHeuristics flags
  let appDir = fromMaybe defaultApplicationDir $ safeHead appDirs
  ExeTarget
    <$> mainFileHeuristics flags
    <*> pure appDirs
    <*> languageHeuristics flags comp
    <*> exeOtherModulesHeuristics flags
    <*> otherExtsHeuristics flags appDir
    <*> dependenciesHeuristics flags appDir pkgs
    <*> buildToolsHeuristics flags appDir v

genTestTarget
  :: Interactive m
  => InitFlags
  -> Compiler
  -> InstalledPackageIndex
  -> CabalSpecVersion
  -> m (Maybe TestTarget)
genTestTarget flags comp pkgs v = do
  initialized <- initializeTestSuiteHeuristics flags
  testDirs' <- testDirsHeuristics flags
  let testDir = fromMaybe defaultTestDir $ safeHead testDirs'
  if not initialized
    then return Nothing
    else
      fmap Just $
        TestTarget
          <$> testMainHeuristics flags
          <*> pure testDirs'
          <*> languageHeuristics flags comp
          <*> testOtherModulesHeuristics flags
          <*> otherExtsHeuristics flags testDir
          <*> dependenciesHeuristics flags testDir pkgs
          <*> buildToolsHeuristics flags testDir v

-- -------------------------------------------------------------------- --
-- Get flags from init config

minimalHeuristics :: Interactive m => InitFlags -> m Bool
minimalHeuristics = getMinimal

overwriteHeuristics :: Interactive m => InitFlags -> m Bool
overwriteHeuristics = getOverwrite

packageDirHeuristics :: Interactive m => InitFlags -> m FilePath
packageDirHeuristics = getPackageDir

-- | Get the version of the cabal spec to use.
--   The spec version can be specified by the InitFlags cabalVersion field. If
--   none is specified then the default version is used.
cabalVersionHeuristics :: Interactive m => InitFlags -> m CabalSpecVersion
cabalVersionHeuristics flags = getCabalVersion flags guessCabalSpecVersion

-- | Get the package name: use the package directory (supplied, or the current
--   directory by default) as a guess. It looks at the SourcePackageDb to avoid
--   using an existing package name.
packageNameHeuristics :: Interactive m => SourcePackageDb -> InitFlags -> m PackageName
packageNameHeuristics sourcePkgDb flags = getPackageName flags $ do
  defName <-
    guessPackageName =<< case packageDir flags of
      Flag a -> return a
      NoFlag -> last . splitDirectories <$> getCurrentDirectory

  when (isPkgRegistered defName) $
    putStrLn (inUseMsg defName)

  return defName
  where
    isPkgRegistered = elemByPackageName (packageIndex sourcePkgDb)

    inUseMsg pn =
      "The name "
        ++ unPackageName pn
        ++ " is already in use by another package on Hackage."

-- | Package version: use 0.1.0.0 as a last resort
versionHeuristics :: Interactive m => InitFlags -> m Version
versionHeuristics flags = getVersion flags $ return defaultVersion

-- | Choose a license for the package.
-- The license can come from Initflags (license field), if it is not present
-- then prompt the user from a predefined list of licenses.
licenseHeuristics :: Interactive m => InitFlags -> m SpecLicense
licenseHeuristics flags = getLicense flags $ guessLicense flags

-- | The author's name. Prompt, or try to guess from an existing
--   git repo.
authorHeuristics :: Interactive m => InitFlags -> m String
authorHeuristics flags =
  guessAuthorName
    >>= maybe (getAuthor flags $ return "Unknown") (getAuthor flags . return)

-- | The author's email. Prompt, or try to guess from an existing
--   git repo.
emailHeuristics :: Interactive m => InitFlags -> m String
emailHeuristics flags =
  guessAuthorEmail
    >>= maybe (getEmail flags $ return "Unknown") (getEmail flags . return)

-- | Prompt for a homepage URL for the package.
homepageHeuristics :: Interactive m => InitFlags -> m String
homepageHeuristics flags = getHomepage flags $ return ""

-- | Prompt for a project synopsis.
synopsisHeuristics :: Interactive m => InitFlags -> m String
synopsisHeuristics flags = getSynopsis flags $ return ""

-- | Prompt for a package category.
--   Note that it should be possible to do some smarter guessing here too, i.e.
--   look at the name of the top level source directory.
categoryHeuristics :: Interactive m => InitFlags -> m String
categoryHeuristics flags = getCategory flags $ return ""

-- | Try to guess extra source files.
extraDocFileHeuristics :: Interactive m => InitFlags -> m (Maybe (Set FilePath))
extraDocFileHeuristics flags = case extraDoc flags of
  Flag x -> return $ Just $ Set.fromList x
  _ -> guessExtraDocFiles flags

-- | Try to guess if the project builds a library, an executable, or both.
packageTypeHeuristics :: Interactive m => InitFlags -> m PackageType
packageTypeHeuristics flags = getPackageType flags $ guessPackageType flags

-- | Try to guess the main file, if nothing is found, fallback
--   to a default value.
mainFileHeuristics :: Interactive m => InitFlags -> m HsFilePath
mainFileHeuristics flags = do
  appDirs <- appDirsHeuristics flags
  let appDir = case appDirs of
        [] -> error "impossible: appDirsHeuristics returned empty list of dirs"
        (appDir' : _) -> appDir'
  getMainFile flags . guessMainFile $ appDir

testMainHeuristics :: Interactive m => InitFlags -> m HsFilePath
testMainHeuristics flags = do
  testDirs' <- testDirsHeuristics flags
  let testDir = case testDirs' of
        [] -> error "impossible: testDirsHeuristics returned empty list of dirs"
        (testDir' : _) -> testDir'
  guessMainFile testDir

initializeTestSuiteHeuristics :: Interactive m => InitFlags -> m Bool
initializeTestSuiteHeuristics flags = getInitializeTestSuite flags $ return False

testDirsHeuristics :: Interactive m => InitFlags -> m [String]
testDirsHeuristics flags = getTestDirs flags $ return [defaultTestDir]

-- | Ask for the Haskell base language of the package.
languageHeuristics :: Interactive m => InitFlags -> Compiler -> m Language
languageHeuristics flags comp = getLanguage flags $ guessLanguage comp

-- | Ask whether to generate explanatory comments.
noCommentsHeuristics :: Interactive m => InitFlags -> m Bool
noCommentsHeuristics flags = getNoComments flags $ return False

-- | Ask for the application root directory.
appDirsHeuristics :: Interactive m => InitFlags -> m [String]
appDirsHeuristics flags = getAppDirs flags $ guessApplicationDirectories flags

-- | Ask for the source (library) root directory.
srcDirsHeuristics :: Interactive m => InitFlags -> m [String]
srcDirsHeuristics flags = getSrcDirs flags $ guessSourceDirectories flags

-- | Retrieve the list of exposed modules
exposedModulesHeuristics :: Interactive m => InitFlags -> m (NonEmpty ModuleName)
exposedModulesHeuristics flags = do
  mods <- case exposedModules flags of
    Flag x -> return x
    NoFlag -> do
      srcDir <- fromMaybe defaultSourceDir . safeHead <$> srcDirsHeuristics flags

      exists <- doesDirectoryExist srcDir

      if exists
        then do
          modules <- filter isHaskell <$> listFilesRecursive srcDir
          modulesNames <- catMaybes <$> traverse retrieveModuleName modules

          otherModules' <- libOtherModulesHeuristics flags
          return $ filter (`notElem` otherModules') modulesNames
        else return []

  return $
    if null mods
      then myLibModule NEL.:| []
      else NEL.fromList mods

-- | Retrieve the list of other modules for Libraries, filtering them
--   based on the last component of the module name
libOtherModulesHeuristics :: Interactive m => InitFlags -> m [ModuleName]
libOtherModulesHeuristics flags = case otherModules flags of
  Flag x -> return x
  NoFlag -> do
    let otherCandidates = ["Internal", "Utils"]
        srcDir = case sourceDirs flags of
          Flag x -> fromMaybe defaultSourceDir $ safeHead x
          NoFlag -> defaultSourceDir

    libDir <-
      (</> srcDir) <$> case packageDir flags of
        Flag x -> return x
        NoFlag -> getCurrentDirectory

    exists <- doesDirectoryExist libDir
    if exists
      then do
        otherModules' <- filter isHaskell <$> listFilesRecursive libDir
        filter ((`elem` otherCandidates) . last . components)
          . catMaybes
          <$> traverse retrieveModuleName otherModules'
      else return []

-- | Retrieve the list of other modules for Executables, it lists everything
--   that is a Haskell file within the application directory, excluding the main file
exeOtherModulesHeuristics :: Interactive m => InitFlags -> m [ModuleName]
exeOtherModulesHeuristics flags = case otherModules flags of
  Flag x -> return x
  NoFlag -> do
    let appDir = case applicationDirs flags of
          Flag x -> fromMaybe defaultApplicationDir $ safeHead x
          NoFlag -> defaultApplicationDir

    exeDir <-
      (</> appDir) <$> case packageDir flags of
        Flag x -> return x
        NoFlag -> getCurrentDirectory

    exists <- doesDirectoryExist exeDir
    if exists
      then do
        otherModules' <-
          filter (\f -> not (isMain f) && isHaskell f)
            <$> listFilesRecursive exeDir
        catMaybes <$> traverse retrieveModuleName otherModules'
      else return []

-- | Retrieve the list of other modules for Tests, it lists everything
--   that is a Haskell file within the tests directory, excluding the main file
testOtherModulesHeuristics :: Interactive m => InitFlags -> m [ModuleName]
testOtherModulesHeuristics flags = case otherModules flags of
  Flag x -> return x
  NoFlag -> do
    let testDir = case testDirs flags of
          Flag x -> fromMaybe defaultTestDir $ safeHead x
          NoFlag -> defaultTestDir

    testDir' <-
      (</> testDir) <$> case packageDir flags of
        Flag x -> return x
        NoFlag -> getCurrentDirectory

    exists <- doesDirectoryExist testDir'
    if exists
      then do
        otherModules' <-
          filter (\f -> not (isMain f) && isHaskell f)
            <$> listFilesRecursive testDir'
        catMaybes <$> traverse retrieveModuleName otherModules'
      else return []

-- | Retrieve the list of build tools
buildToolsHeuristics
  :: Interactive m
  => InitFlags
  -> FilePath
  -> CabalSpecVersion
  -> m [Dependency]
buildToolsHeuristics flags fp v = case buildTools flags of
  Flag{} -> getBuildTools flags
  NoFlag -> retrieveBuildTools v fp

-- | Retrieve the list of dependencies
dependenciesHeuristics :: Interactive m => InitFlags -> FilePath -> InstalledPackageIndex -> m [Dependency]
dependenciesHeuristics flags fp pkgIx = getDependencies flags $ do
  sources <- retrieveSourceFiles fp

  let mods = case exposedModules flags of
        Flag x -> x
        NoFlag -> map moduleName sources

      groupedDeps = concatMap (\s -> map (\i -> (moduleName s, i)) (imports s)) sources
      filteredDeps = filter ((`notElem` mods) . snd) groupedDeps
      preludeNub = nubBy (\a b -> snd a == snd b) $ (fromString "Prelude", fromString "Prelude") : filteredDeps

  retrieveDependencies (fromFlagOrDefault normal $ initVerbosity flags) flags preludeNub pkgIx

-- | Retrieve the list of extensions
otherExtsHeuristics :: Interactive m => InitFlags -> FilePath -> m [Extension]
otherExtsHeuristics flags fp = case otherExts flags of
  Flag x -> return x
  NoFlag -> do
    exists <- doesDirectoryExist fp
    if exists
      then do
        sources <- listFilesRecursive fp
        extensions' <- traverse retrieveModuleExtensions . filter isHaskell $ sources

        return $ nub . join $ extensions'
      else return []
