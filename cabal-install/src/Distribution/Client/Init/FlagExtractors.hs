{-# LANGUAGE LambdaCase #-}

module Distribution.Client.Init.FlagExtractors
  ( -- * Flag extractors
    getPackageDir
  , getSimpleProject
  , getMinimal
  , getCabalVersion
  , getCabalVersionNoPrompt
  , getPackageName
  , getVersion
  , getLicense
  , getAuthor
  , getEmail
  , getHomepage
  , getSynopsis
  , getCategory
  , getExtraSrcFiles
  , getExtraDocFiles
  , getPackageType
  , getMainFile
  , getInitializeTestSuite
  , getTestDirs
  , getLanguage
  , getNoComments
  , getAppDirs
  , getSrcDirs
  , getExposedModules
  , getBuildTools
  , getDependencies
  , getOtherExts
  , getOverwrite
  , getOtherModules

    -- * Shared prompts
  , simpleProjectPrompt
  , initializeTestSuitePrompt
  , packageTypePrompt
  , testMainPrompt
  , dependenciesPrompt
  ) where

import Distribution.Client.Compat.Prelude hiding (getLine, last, putStr, putStrLn)
import Prelude ()

import qualified Data.List.NonEmpty as NEL

import Distribution.CabalSpecVersion (CabalSpecVersion (..))
import Distribution.Client.Init.Defaults
import Distribution.Client.Init.Types
import Distribution.FieldGrammar.Newtypes (SpecLicense)
import Distribution.ModuleName (ModuleName)
import Distribution.Simple.Flag (flagElim)
import Distribution.Simple.Setup (Flag (..), flagToMaybe, fromFlagOrDefault)
import Distribution.Types.Dependency (Dependency (..))
import Distribution.Types.PackageName (PackageName)
import Distribution.Version (Version)

import qualified Data.Set as Set
import Distribution.Client.Init.Prompt
import Distribution.Client.Init.Utils
import Distribution.Simple.PackageIndex
import Language.Haskell.Extension (Extension (..), Language (..))

-- -------------------------------------------------------------------- --
-- Flag extraction

getPackageDir :: Interactive m => InitFlags -> m FilePath
getPackageDir = flagElim getCurrentDirectory return . packageDir

-- | Ask if a simple project with sensible defaults should be created.
getSimpleProject :: Interactive m => InitFlags -> m Bool -> m Bool
getSimpleProject flags = fromFlagOrPrompt (simpleProject flags)

-- | Extract minimal cabal file flag (implies nocomments)
getMinimal :: Interactive m => InitFlags -> m Bool
getMinimal = return . fromFlagOrDefault False . minimal

-- | Get the version of the cabal spec to use.
--
-- The spec version can be specified by the InitFlags cabalVersion field. If
-- none is specified then the user is prompted to pick from a list of
-- supported versions (see code below).
getCabalVersion :: Interactive m => InitFlags -> m CabalSpecVersion -> m CabalSpecVersion
getCabalVersion flags = fromFlagOrPrompt (cabalVersion flags)

getCabalVersionNoPrompt :: InitFlags -> CabalSpecVersion
getCabalVersionNoPrompt = fromFlagOrDefault defaultCabalVersion . cabalVersion

-- | Get the package name: use the package directory (supplied, or the current
--   directory by default) as a guess. It looks at the SourcePackageDb to avoid
--   using an existing package name.
getPackageName :: Interactive m => InitFlags -> m PackageName -> m PackageName
getPackageName flags = fromFlagOrPrompt (packageName flags)

-- | Package version: use 0.1.0.0 as a last resort, but try prompting the user
--  if possible.
getVersion :: Interactive m => InitFlags -> m Version -> m Version
getVersion flags = fromFlagOrPrompt (version flags)

-- | Choose a license for the package.
-- The license can come from Initflags (license field), if it is not present
-- then prompt the user from a predefined list of licenses.
getLicense :: Interactive m => InitFlags -> m SpecLicense -> m SpecLicense
getLicense flags = fromFlagOrPrompt (license flags)

-- | The author's name. Prompt, or try to guess from an existing
--   darcs repo.
getAuthor :: Interactive m => InitFlags -> m String -> m String
getAuthor flags = fromFlagOrPrompt (author flags)

-- | The author's email. Prompt, or try to guess from an existing
--   darcs repo.
getEmail :: Interactive m => InitFlags -> m String -> m String
getEmail flags = fromFlagOrPrompt (email flags)

-- | Prompt for a homepage URL for the package.
getHomepage :: Interactive m => InitFlags -> m String -> m String
getHomepage flags = fromFlagOrPrompt (homepage flags)

-- | Prompt for a project synopsis.
getSynopsis :: Interactive m => InitFlags -> m String -> m String
getSynopsis flags = fromFlagOrPrompt (synopsis flags)

-- | Prompt for a package category.
--   Note that it should be possible to do some smarter guessing here too, i.e.
--   look at the name of the top level source directory.
getCategory :: Interactive m => InitFlags -> m String -> m String
getCategory flags = fromFlagOrPrompt (category flags)

-- | Try to guess extra source files (don't prompt the user).
getExtraSrcFiles :: Interactive m => InitFlags -> m (Set String)
getExtraSrcFiles = pure . flagElim mempty Set.fromList . extraSrc

-- | Try to guess extra source files (don't prompt the user).
getExtraDocFiles :: Interactive m => InitFlags -> m (Maybe (Set String))
getExtraDocFiles =
  pure
    . Just
    . flagElim (Set.singleton defaultChangelog) Set.fromList
    . extraDoc

-- | Ask whether the project builds a library or executable.
getPackageType :: Interactive m => InitFlags -> m PackageType -> m PackageType
getPackageType
  InitFlags
    { initializeTestSuite = Flag True
    , packageType = NoFlag
    }
  _ = return TestSuite
getPackageType flags act = fromFlagOrPrompt (packageType flags) act

getMainFile :: Interactive m => InitFlags -> m HsFilePath -> m HsFilePath
getMainFile flags act = case mainIs flags of
  Flag a
    | isHsFilePath a -> return $ toHsFilePath a
    | otherwise -> act
  NoFlag -> act

getInitializeTestSuite :: Interactive m => InitFlags -> m Bool -> m Bool
getInitializeTestSuite flags = fromFlagOrPrompt (initializeTestSuite flags)

getTestDirs :: Interactive m => InitFlags -> m [String] -> m [String]
getTestDirs flags = fromFlagOrPrompt (testDirs flags)

-- | Ask for the Haskell base language of the package.
getLanguage :: Interactive m => InitFlags -> m Language -> m Language
getLanguage flags = fromFlagOrPrompt (language flags)

-- | Ask whether to generate explanatory comments.
getNoComments :: Interactive m => InitFlags -> m Bool -> m Bool
getNoComments flags = fromFlagOrPrompt (noComments flags)

-- | Ask for the application root directory.
getAppDirs :: Interactive m => InitFlags -> m [String] -> m [String]
getAppDirs flags = fromFlagOrPrompt (applicationDirs flags)

-- | Ask for the source (library) root directory.
getSrcDirs :: Interactive m => InitFlags -> m [String] -> m [String]
getSrcDirs flags = fromFlagOrPrompt (sourceDirs flags)

-- | Retrieve the list of exposed modules
getExposedModules :: Interactive m => InitFlags -> m (NonEmpty ModuleName)
getExposedModules =
  return
    . fromMaybe (myLibModule NEL.:| [])
    . join
    . flagToMaybe
    . fmap NEL.nonEmpty
    . exposedModules

-- | Retrieve the list of other modules
getOtherModules :: Interactive m => InitFlags -> m [ModuleName]
getOtherModules = return . fromFlagOrDefault [] . otherModules

-- | Retrieve the list of build tools
getBuildTools :: Interactive m => InitFlags -> m [Dependency]
getBuildTools = flagElim (return []) (foldM go []) . buildTools
  where
    go acc dep = case eitherParsec dep of
      Left e -> do
        putStrLn $ "Failed to parse dependency: " ++ e
        putStrLn "Skipping..."

        return acc
      Right d -> return $ acc ++ [d]

-- | Retrieve the list of dependencies
getDependencies
  :: Interactive m
  => InitFlags
  -> m [Dependency]
  -> m [Dependency]
getDependencies flags = fromFlagOrPrompt (dependencies flags)

-- | Retrieve the list of extensions
getOtherExts :: Interactive m => InitFlags -> m [Extension]
getOtherExts = return . fromFlagOrDefault [] . otherExts

-- | Tell whether to overwrite files on write
getOverwrite :: Interactive m => InitFlags -> m Bool
getOverwrite = return . fromFlagOrDefault False . overwrite

-- -------------------------------------------------------------------- --
-- Shared prompts

simpleProjectPrompt :: Interactive m => InitFlags -> m Bool
simpleProjectPrompt flags =
  getSimpleProject flags $
    promptYesNo
      "Should I generate a simple project with sensible defaults"
      (DefaultPrompt True)

initializeTestSuitePrompt :: Interactive m => InitFlags -> m Bool
initializeTestSuitePrompt flags =
  getInitializeTestSuite flags $
    promptYesNo
      "Should I generate a test suite for the library"
      (DefaultPrompt True)

packageTypePrompt :: Interactive m => InitFlags -> m PackageType
packageTypePrompt flags = getPackageType flags $ do
  pt <-
    promptList
      "What does the package build"
      packageTypes
      (DefaultPrompt "Executable")
      Nothing
      False

  return $ fromMaybe Executable (parsePackageType pt)
  where
    packageTypes =
      [ "Library"
      , "Executable"
      , "Library and Executable"
      , "Test suite"
      ]

    parsePackageType = \case
      "Library" -> Just Library
      "Executable" -> Just Executable
      "Library and Executable" -> Just LibraryAndExecutable
      "Test suite" -> Just TestSuite
      _ -> Nothing

testMainPrompt :: Interactive m => m HsFilePath
testMainPrompt = do
  fp <-
    promptList
      "What is the main module of the test suite?"
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
      testMainPrompt
    _ -> return hs
  where
    defaultMainIs' = show defaultMainIs

dependenciesPrompt
  :: Interactive m
  => InstalledPackageIndex
  -> InitFlags
  -> m [Dependency]
dependenciesPrompt pkgIx flags = getDependencies flags (getBaseDep pkgIx flags)

-- -------------------------------------------------------------------- --
-- utilities

-- | If a flag is defined, return its value or else execute
-- an interactive action.
fromFlagOrPrompt
  :: Interactive m
  => Flag a
  -> m a
  -> m a
fromFlagOrPrompt flag action = flagElim action return flag
