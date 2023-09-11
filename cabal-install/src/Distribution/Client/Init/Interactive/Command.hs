{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.Client.Init.Command
-- Copyright   :  (c) Brent Yorgey 2009
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Implementation of the 'cabal init' command, which creates an initial .cabal
-- file for a project.
module Distribution.Client.Init.Interactive.Command
  ( -- * Commands
    createProject

    -- ** Target generation
  , genPkgDescription
  , genLibTarget
  , genExeTarget
  , genTestTarget

    -- ** Prompts
  , cabalVersionPrompt
  , packageNamePrompt
  , versionPrompt
  , licensePrompt
  , authorPrompt
  , emailPrompt
  , homepagePrompt
  , synopsisPrompt
  , categoryPrompt
  , mainFilePrompt
  , testDirsPrompt
  , languagePrompt
  , noCommentsPrompt
  , appDirsPrompt
  , dependenciesPrompt
  , srcDirsPrompt
  ) where

import Distribution.Client.Compat.Prelude hiding (getLine, last, putStr, putStrLn)
import Prelude ()

import Distribution.CabalSpecVersion (CabalSpecVersion (..), showCabalSpecVersion)
import Distribution.Client.Init.Defaults
import Distribution.Client.Init.FlagExtractors
import Distribution.Client.Init.NonInteractive.Heuristics (guessAuthorEmail, guessAuthorName)
import Distribution.Client.Init.Prompt
import Distribution.Client.Init.Types
import Distribution.Client.Init.Utils
import Distribution.Client.Types (SourcePackageDb (..))
import Distribution.FieldGrammar.Newtypes (SpecLicense (..))
import qualified Distribution.SPDX as SPDX
import Distribution.Simple.PackageIndex (InstalledPackageIndex)
import Distribution.Simple.Setup (Flag (..), fromFlagOrDefault)
import Distribution.Solver.Types.PackageIndex (elemByPackageName)
import Distribution.Types.PackageName (PackageName, unPackageName)
import Distribution.Version (Version)

import Distribution.License (knownLicenses)
import Distribution.Parsec (simpleParsec')
import Language.Haskell.Extension (Language (..))

-- | Main driver for interactive prompt code.
createProject
  :: Interactive m
  => Verbosity
  -> InstalledPackageIndex
  -> SourcePackageDb
  -> InitFlags
  -> m ProjectSettings
createProject v pkgIx srcDb initFlags = do
  -- The workflow is as follows:
  --
  --  1. Get the package type, supplied as either a program input or
  --     via user prompt. This determines what targets will be built
  --     in later steps.
  --
  --  2. Generate package description and the targets specified by
  --     the package type. Once this is done, a prompt for building
  --     test suites is initiated, and this determines if we build
  --     test targets as well. Then we ask if the user wants to
  --     comment their .cabal file with pretty comments.
  --
  --  3. The targets are passed to the file creator script, and associated
  --     directories/files/modules are created, with the a .cabal file
  --     being generated as a final result.
  --

  pkgType <- packageTypePrompt initFlags
  isMinimal <- getMinimal initFlags
  doOverwrite <- overwritePrompt initFlags
  pkgDir <- getPackageDir initFlags
  pkgDesc <- fixupDocFiles v =<< genPkgDescription initFlags srcDb

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
      initFlags' = initFlags{cabalVersion = Flag cabalSpec}

  case pkgType of
    Library -> do
      libTarget <- genLibTarget initFlags' pkgIx
      testTarget <-
        addLibDepToTest pkgName
          <$> genTestTarget initFlags' pkgIx

      comments <- noCommentsPrompt initFlags'

      return $
        ProjectSettings
          (mkOpts comments cabalSpec)
          pkgDesc
          (Just libTarget)
          Nothing
          testTarget
    Executable -> do
      exeTarget <- genExeTarget initFlags' pkgIx
      comments <- noCommentsPrompt initFlags'

      return $
        ProjectSettings
          (mkOpts comments cabalSpec)
          pkgDesc
          Nothing
          (Just exeTarget)
          Nothing
    LibraryAndExecutable -> do
      libTarget <- genLibTarget initFlags' pkgIx

      exeTarget <-
        addLibDepToExe pkgName
          <$> genExeTarget initFlags' pkgIx

      testTarget <-
        addLibDepToTest pkgName
          <$> genTestTarget initFlags' pkgIx

      comments <- noCommentsPrompt initFlags'

      return $
        ProjectSettings
          (mkOpts comments cabalSpec)
          pkgDesc
          (Just libTarget)
          (Just exeTarget)
          testTarget
    TestSuite -> do
      -- the line below is necessary because if both package type and test flags
      -- are *not* passed, the user will be prompted for a package type (which
      -- includes TestSuite in the list). It prevents that the user end up with a
      -- TestSuite target with initializeTestSuite set to NoFlag, thus avoiding the prompt.
      let initFlags'' = initFlags'{initializeTestSuite = Flag True}
      testTarget <- genTestTarget initFlags'' pkgIx

      comments <- noCommentsPrompt initFlags''

      return $
        ProjectSettings
          (mkOpts comments cabalSpec)
          pkgDesc
          Nothing
          Nothing
          testTarget

-- -------------------------------------------------------------------- --
-- Target and pkg description generation

-- | Extract flags relevant to a package description and interactively
-- generate a 'PkgDescription' object for creation. If the user specifies
-- the generation of a simple package, then a simple target with defaults
-- is generated.
genPkgDescription
  :: Interactive m
  => InitFlags
  -> SourcePackageDb
  -> m PkgDescription
genPkgDescription flags' srcDb = do
  csv <- cabalVersionPrompt flags'
  let flags = flags'{cabalVersion = Flag csv}
  PkgDescription csv
    <$> packageNamePrompt srcDb flags
    <*> versionPrompt flags
    <*> licensePrompt flags
    <*> authorPrompt flags
    <*> emailPrompt flags
    <*> homepagePrompt flags
    <*> synopsisPrompt flags
    <*> categoryPrompt flags
    <*> getExtraSrcFiles flags
    <*> getExtraDocFiles flags

-- | Extract flags relevant to a library target and interactively
-- generate a 'LibTarget' object for creation. If the user specifies
-- the generation of a simple package, then a simple target with defaults
-- is generated.
genLibTarget
  :: Interactive m
  => InitFlags
  -> InstalledPackageIndex
  -> m LibTarget
genLibTarget flags pkgs =
  LibTarget
    <$> srcDirsPrompt flags
    <*> languagePrompt flags "library"
    <*> getExposedModules flags
    <*> getOtherModules flags
    <*> getOtherExts flags
    <*> dependenciesPrompt pkgs flags
    <*> getBuildTools flags

-- | Extract flags relevant to a executable target and interactively
-- generate a 'ExeTarget' object for creation. If the user specifies
-- the generation of a simple package, then a simple target with defaults
-- is generated.
genExeTarget
  :: Interactive m
  => InitFlags
  -> InstalledPackageIndex
  -> m ExeTarget
genExeTarget flags pkgs =
  ExeTarget
    <$> mainFilePrompt flags
    <*> appDirsPrompt flags
    <*> languagePrompt flags "executable"
    <*> getOtherModules flags
    <*> getOtherExts flags
    <*> dependenciesPrompt pkgs flags
    <*> getBuildTools flags

-- | Extract flags relevant to a test target and interactively
-- generate a 'TestTarget' object for creation. If the user specifies
-- the generation of a simple package, then a simple target with defaults
-- is generated.
--
-- Note: this workflow is only enabled if the user answers affirmatively
-- when prompted, or if the user passes in the flag to enable
-- test suites at command line.
genTestTarget
  :: Interactive m
  => InitFlags
  -> InstalledPackageIndex
  -> m (Maybe TestTarget)
genTestTarget flags pkgs = initializeTestSuitePrompt flags >>= go
  where
    go initialized
      | not initialized = return Nothing
      | otherwise =
          fmap Just $
            TestTarget
              <$> testMainPrompt
              <*> testDirsPrompt flags
              <*> languagePrompt flags "test suite"
              <*> getOtherModules flags
              <*> getOtherExts flags
              <*> dependenciesPrompt pkgs flags
              <*> getBuildTools flags

-- -------------------------------------------------------------------- --
-- Prompts

overwritePrompt :: Interactive m => InitFlags -> m Bool
overwritePrompt flags = do
  con <- getPackageDir flags >>= listDirectory

  -- Do not ask useless overwrite question if directory is empty.
  if null con
    then return False
    else do
      isOverwrite <- getOverwrite flags
      promptYesNo
        "Do you wish to overwrite existing files (backups will be created) (y/n)"
        (DefaultPrompt isOverwrite)

cabalVersionPrompt :: Interactive m => InitFlags -> m CabalSpecVersion
cabalVersionPrompt flags = getCabalVersion flags $ do
  v <-
    promptList
      "Please choose version of the Cabal specification to use"
      ppVersions
      (DefaultPrompt ppDefault)
      (Just takeVersion)
      False
  -- take just the version numbers for convenience
  return $ parseCabalVersion (takeVersion v)
  where
    -- only used when presenting the default in prompt
    takeVersion = takeWhile (/= ' ')

    ppDefault = displayCabalVersion defaultCabalVersion
    ppVersions = displayCabalVersion <$> defaultCabalVersions

    parseCabalVersion :: String -> CabalSpecVersion
    parseCabalVersion "1.24" = CabalSpecV1_24
    parseCabalVersion "2.0" = CabalSpecV2_0
    parseCabalVersion "2.2" = CabalSpecV2_2
    parseCabalVersion "2.4" = CabalSpecV2_4
    parseCabalVersion "3.0" = CabalSpecV3_0
    parseCabalVersion "3.4" = CabalSpecV3_4
    parseCabalVersion _ = defaultCabalVersion -- 2.4
    displayCabalVersion :: CabalSpecVersion -> String
    displayCabalVersion v = case v of
      CabalSpecV2_0 -> "2.0   (support for Backpack, internal sub-libs, '^>=' operator)"
      CabalSpecV2_2 -> "2.2   (+ support for 'common', 'elif', redundant commas, SPDX)"
      CabalSpecV2_4 -> "2.4   (+ support for '**' globbing)"
      CabalSpecV3_0 -> "3.0   (+ set notation for ==, common stanzas in ifs, more redundant commas, better pkgconfig-depends)"
      CabalSpecV3_4 -> "3.4   (+ sublibraries in 'mixins', optional 'default-language')"
      _ -> showCabalSpecVersion v

packageNamePrompt :: Interactive m => SourcePackageDb -> InitFlags -> m PackageName
packageNamePrompt srcDb flags = getPackageName flags $ do
  defName <- case packageDir flags of
    Flag b -> filePathToPkgName b
    NoFlag -> currentDirPkgName

  go $ DefaultPrompt defName
  where
    go defName =
      prompt "Package name" defName >>= \n ->
        if isPkgRegistered n
          then do
            don'tUseName <- promptYesNo (promptOtherNameMsg n) (DefaultPrompt True)
            if don'tUseName
              then go defName
              else return n
          else return n

    isPkgRegistered = elemByPackageName (packageIndex srcDb)

    inUseMsg pn =
      "The name "
        ++ unPackageName pn
        ++ " is already in use by another package on Hackage."

    promptOtherNameMsg pn = inUseMsg pn ++ " Do you want to choose a different name (y/n)"

versionPrompt :: Interactive m => InitFlags -> m Version
versionPrompt flags = getVersion flags go
  where
    go = do
      vv <- promptStr "Package version" (DefaultPrompt $ prettyShow defaultVersion)
      case simpleParsec vv of
        Nothing -> do
          putStrLn $
            "Version must be a valid PVP format (e.g. 0.1.0.0): "
              ++ vv
          go
        Just v -> return v

licensePrompt :: Interactive m => InitFlags -> m SpecLicense
licensePrompt flags = getLicense flags $ do
  let csv = fromFlagOrDefault defaultCabalVersion (cabalVersion flags)
  l <-
    promptList
      "Please choose a license"
      (licenses csv)
      (DefaultPrompt "BSD-3-Clause")
      Nothing
      True

  case simpleParsec' csv l of
    Nothing -> do
      putStrLn
        ( "The license must be a valid SPDX expression:"
            ++ "\n - On the SPDX License List: https://spdx.org/licenses/"
            ++ "\n - NONE, if you do not want to grant any license"
            ++ "\n - LicenseRef-( alphanumeric | - | . )+"
        )
      licensePrompt flags
    Just l' -> return l'
  where
    licenses csv =
      if csv >= CabalSpecV2_2
        then SPDX.licenseId <$> defaultLicenseIds
        else fmap prettyShow knownLicenses

authorPrompt :: Interactive m => InitFlags -> m String
authorPrompt flags = getAuthor flags $ guessAuthorName >>= promptOrDefault "Author name"

emailPrompt :: Interactive m => InitFlags -> m String
emailPrompt flags = getEmail flags $ guessAuthorEmail >>= promptOrDefault "Maintainer email"

homepagePrompt :: Interactive m => InitFlags -> m String
homepagePrompt flags =
  getHomepage flags $
    promptStr "Project homepage URL" OptionalPrompt

synopsisPrompt :: Interactive m => InitFlags -> m String
synopsisPrompt flags =
  getSynopsis flags $
    promptStr "Project synopsis" OptionalPrompt

categoryPrompt :: Interactive m => InitFlags -> m String
categoryPrompt flags =
  getCategory flags $
    promptList
      "Project category"
      defaultCategories
      (DefaultPrompt "")
      (Just matchNone)
      True
  where
    matchNone s
      | null s = "(none)"
      | otherwise = s

mainFilePrompt :: Interactive m => InitFlags -> m HsFilePath
mainFilePrompt flags = getMainFile flags go
  where
    defaultMainIs' = show defaultMainIs
    go = do
      fp <-
        promptList
          "What is the main module of the executable"
          [defaultMainIs', "Main.lhs"]
          (DefaultPrompt defaultMainIs')
          Nothing
          True

      let hs = toHsFilePath fp

      case _hsFileType hs of
        InvalidHsPath -> do
          putStrLn $
            concat
              [ "Main file "
              , show hs
              , " is not a valid haskell file. Source files must end in .hs or .lhs."
              ]
          go
        _ -> return hs

testDirsPrompt :: Interactive m => InitFlags -> m [String]
testDirsPrompt flags = getTestDirs flags $ do
  dir <- promptStr "Test directory" (DefaultPrompt defaultTestDir)
  return [dir]

languagePrompt :: Interactive m => InitFlags -> String -> m Language
languagePrompt flags pkgType = getLanguage flags $ do
  let h2010 = "Haskell2010"
      h98 = "Haskell98"
      ghc2021 = "GHC2021 (requires at least GHC 9.2)"

  l <-
    promptList
      ("Choose a language for your " ++ pkgType)
      [h2010, h98, ghc2021]
      (DefaultPrompt h2010)
      Nothing
      True

  if
      | l == h2010 -> return Haskell2010
      | l == h98 -> return Haskell98
      | l == ghc2021 -> return GHC2021
      | otherwise -> return $ UnknownLanguage l

noCommentsPrompt :: Interactive m => InitFlags -> m Bool
noCommentsPrompt flags = getNoComments flags $ do
  doComments <-
    promptYesNo
      "Add informative comments to each field in the cabal file. (y/n)"
      (DefaultPrompt True)

  --
  -- if --no-comments is flagged, then we choose not to generate comments
  -- for fields in the cabal file, but it's a nicer UX to present the
  -- affirmative question which must be negated.
  --

  return (not doComments)

-- | Ask for the application root directory.
appDirsPrompt :: Interactive m => InitFlags -> m [String]
appDirsPrompt flags = getAppDirs flags $ do
  dir <-
    promptList
      promptMsg
      [defaultApplicationDir, "exe", "src-exe"]
      (DefaultPrompt defaultApplicationDir)
      Nothing
      True

  return [dir]
  where
    promptMsg = case mainIs flags of
      Flag p -> "Application (" ++ p ++ ") directory"
      NoFlag -> "Application directory"

-- | Ask for the source (library) root directory.
srcDirsPrompt :: Interactive m => InitFlags -> m [String]
srcDirsPrompt flags = getSrcDirs flags $ do
  dir <-
    promptList
      "Library source directory"
      [defaultSourceDir, "lib", "src-lib"]
      (DefaultPrompt defaultSourceDir)
      Nothing
      True

  return [dir]

promptOrDefault :: Interactive m => String -> Maybe String -> m String
promptOrDefault s = maybe (promptStr s MandatoryPrompt) (promptStr s . DefaultPrompt)
