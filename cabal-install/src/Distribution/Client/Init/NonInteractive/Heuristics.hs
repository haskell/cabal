{-# LANGUAGE LambdaCase #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.Client.Init.NonInteractive.Heuristics
-- Copyright   :  (c) Benedikt Huber 2009
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Heuristics for creating initial cabal files.
module Distribution.Client.Init.NonInteractive.Heuristics
  ( guessPackageName
  , guessMainFile
  , guessLicense
  , guessExtraDocFiles
  , guessAuthorName
  , guessAuthorEmail
  , guessCabalSpecVersion
  , guessLanguage
  , guessPackageType
  , guessSourceDirectories
  , guessApplicationDirectories
  ) where

import Distribution.Client.Compat.Prelude hiding (many, readFile, (<|>))

import Distribution.Simple.Setup (fromFlagOrDefault)

import qualified Data.List as L
import qualified Data.Set as Set
import Distribution.CabalSpecVersion
import Distribution.Client.Init.Defaults
import Distribution.Client.Init.FlagExtractors (getCabalVersionNoPrompt)
import Distribution.Client.Init.Types
import Distribution.Client.Init.Utils
import Distribution.FieldGrammar.Newtypes
import Distribution.Simple.Compiler
import Distribution.Types.PackageName (PackageName)
import Distribution.Version
import Language.Haskell.Extension
import System.FilePath

-- | Guess the main file, returns a default value if none is found.
guessMainFile :: Interactive m => FilePath -> m HsFilePath
guessMainFile pkgDir = do
  exists <- doesDirectoryExist pkgDir
  if exists
    then do
      files <- filter isMain <$> listFilesRecursive pkgDir
      return $
        if null files
          then defaultMainIs
          else toHsFilePath $ L.head files
    else return defaultMainIs

-- | Juggling characters around to guess the desired cabal version based on
--   the system's cabal version.
guessCabalSpecVersion :: Interactive m => m CabalSpecVersion
guessCabalSpecVersion = do
  (_, verString, _) <- readProcessWithExitCode "cabal" ["--version"] ""
  case simpleParsec $ takeWhile (not . isSpace) $ dropWhile (not . isDigit) verString of
    Just v -> pure $ fromMaybe defaultCabalVersion $ case versionNumbers v of
      [x, y, _, _] -> cabalSpecFromVersionDigits [x, y]
      [x, y, _] -> cabalSpecFromVersionDigits [x, y]
      _ -> Just defaultCabalVersion
    Nothing -> pure defaultCabalVersion

-- | Guess the language specification based on the GHC version
guessLanguage :: Interactive m => Compiler -> m Language
guessLanguage Compiler{compilerId = CompilerId GHC ver} =
  return $
    if ver < mkVersion [7, 0, 1]
      then Haskell98
      else Haskell2010
guessLanguage _ = return defaultLanguage

-- | Guess the package name based on the given root directory.
guessPackageName :: Interactive m => FilePath -> m PackageName
guessPackageName = filePathToPkgName

-- | Try to guess the license from an already existing @LICENSE@ file in
--   the package directory, comparing the file contents with the ones
--   listed in @Licenses.hs@, for now it only returns a default value.
guessLicense :: Interactive m => InitFlags -> m SpecLicense
guessLicense flags = return . defaultLicense $ getCabalVersionNoPrompt flags

guessExtraDocFiles :: Interactive m => InitFlags -> m (Maybe (Set FilePath))
guessExtraDocFiles flags = do
  pkgDir <- fromFlagOrDefault getCurrentDirectory $ return <$> packageDir flags
  files <- getDirectoryContents pkgDir

  let extraDocCandidates = ["CHANGES", "CHANGELOG", "README"]
      extraDocs = [y | x <- extraDocCandidates, y <- files, x == map toUpper (takeBaseName y)]

  return $
    Just $
      if null extraDocs
        then Set.singleton defaultChangelog
        else Set.fromList extraDocs

-- | Try to guess the package type from the files in the package directory,
--   looking for unique characteristics from each type, defaults to Executable.
guessPackageType :: Interactive m => InitFlags -> m PackageType
guessPackageType flags = do
  if fromFlagOrDefault False (initializeTestSuite flags)
    then return TestSuite
    else do
      let lastDir dirs = L.last . splitDirectories $ dirs
          srcCandidates = [defaultSourceDir, "src", "source"]
          testCandidates = [defaultTestDir, "test", "tests"]

      pkgDir <- fromFlagOrDefault getCurrentDirectory $ return <$> packageDir flags
      files <- listFilesInside (\x -> return $ lastDir x `notElem` testCandidates) pkgDir
      files' <-
        filter (not . null . map (`elem` testCandidates) . splitDirectories)
          <$> listFilesRecursive pkgDir

      let hasExe = not $ null [f | f <- files, isMain $ takeFileName f]
          hasLib = not $ null [f | f <- files, lastDir f `elem` srcCandidates]
          hasTest = not $ null [f | f <- files', isMain $ takeFileName f]

      return $ case (hasLib, hasExe, hasTest) of
        (True, True, _) -> LibraryAndExecutable
        (True, False, _) -> Library
        (False, False, True) -> TestSuite
        _ -> Executable

-- | Try to guess the application directories from the package directory,
--   using a default value as fallback.
guessApplicationDirectories :: Interactive m => InitFlags -> m [FilePath]
guessApplicationDirectories flags = do
  pkgDirs <-
    fromFlagOrDefault
      getCurrentDirectory
      (return <$> packageDir flags)
  pkgDirsContents <- listDirectory pkgDirs

  let candidates = [defaultApplicationDir, "app", "src-exe"]
   in return $ case [y | x <- candidates, y <- pkgDirsContents, x == y] of
        [] -> [defaultApplicationDir]
        x -> map (</> pkgDirs) . nub $ x

-- | Try to guess the source directories, using a default value as fallback.
guessSourceDirectories :: Interactive m => InitFlags -> m [FilePath]
guessSourceDirectories flags = do
  pkgDir <- fromFlagOrDefault getCurrentDirectory $ return <$> packageDir flags

  doesDirectoryExist (pkgDir </> "src")
    >>= return . \case
      False -> [defaultSourceDir]
      True -> ["src"]

-- | Guess author and email using git configuration options.
guessAuthorName :: Interactive m => m (Maybe String)
guessAuthorName = guessGitInfo "user.name"

guessAuthorEmail :: Interactive m => m (Maybe String)
guessAuthorEmail = guessGitInfo "user.email"

guessGitInfo :: Interactive m => String -> m (Maybe String)
guessGitInfo target = do
  localInfo <- readProcessWithExitCode "git" ["config", "--local", target] ""
  if null $ snd' localInfo
    then do
      globalInfo <- readProcessWithExitCode "git" ["config", "--global", target] ""
      case fst' globalInfo of
        ExitSuccess -> return $ Just (trim $ snd' globalInfo)
        _ -> return Nothing
    else return $ Just (trim $ snd' localInfo)
  where
    fst' (x, _, _) = x
    snd' (_, x, _) = x
