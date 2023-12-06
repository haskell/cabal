{-# LANGUAGE RecordWildCards #-}

module Distribution.Client.Init.Utils
  ( SourceFileEntry (..)
  , retrieveSourceFiles
  , retrieveModuleName
  , retrieveModuleImports
  , retrieveModuleExtensions
  , retrieveBuildTools
  , retrieveDependencies
  , isMain
  , isHaskell
  , isSourceFile
  , trim
  , currentDirPkgName
  , filePathToPkgName
  , mkPackageNameDep
  , fixupDocFiles
  , mkStringyDep
  , getBaseDep
  , addLibDepToExe
  , addLibDepToTest
  ) where

import Distribution.Client.Compat.Prelude hiding (Parsec, empty, many, putStrLn, readFile)
import Distribution.Utils.Generic (isInfixOf, safeLast)
import qualified Prelude ()

import Control.Monad (forM)

import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Language.Haskell.Extension (Extension (..))
import System.FilePath

import Distribution.CabalSpecVersion (CabalSpecVersion (..))
import Distribution.Client.Init.Defaults
import Distribution.Client.Init.Types
import Distribution.Client.Utils (pvpize)
import qualified Distribution.Compat.NonEmptySet as NES
import Distribution.InstalledPackageInfo (InstalledPackageInfo, exposed)
import Distribution.ModuleName (ModuleName)
import qualified Distribution.Package as P
import Distribution.Simple.PackageIndex (InstalledPackageIndex, moduleNameIndex)
import Distribution.Simple.Setup (Flag (..))
import Distribution.Types.Dependency (Dependency, mkDependency)
import Distribution.Types.LibraryName
import Distribution.Types.PackageName
import Distribution.Utils.String (trim)
import Distribution.Verbosity (silent)
import Distribution.Version

-- | Data type of source files found in the working directory
data SourceFileEntry = SourceFileEntry
  { relativeSourcePath :: FilePath
  , moduleName :: ModuleName
  , fileExtension :: String
  , imports :: [ModuleName]
  , extensions :: [Extension]
  }
  deriving (Show)

-- Unfortunately we cannot use the version exported by Distribution.Simple.Program
knownSuffixHandlers :: CabalSpecVersion -> String -> String
knownSuffixHandlers v s
  | v < CabalSpecV3_0 = case s of
      ".gc" -> "greencard"
      ".chs" -> "chs"
      ".hsc" -> "hsc2hs"
      ".x" -> "alex"
      ".y" -> "happy"
      ".ly" -> "happy"
      ".cpphs" -> "cpp"
      _ -> ""
  | otherwise = case s of
      ".gc" -> "greencard:greencard"
      ".chs" -> "chs:chs"
      ".hsc" -> "hsc2hs:hsc2hs"
      ".x" -> "alex:alex"
      ".y" -> "happy:happy"
      ".ly" -> "happy:happy"
      ".cpphs" -> "cpp:cpp"
      _ -> ""

-- | Check if a given file has main file characteristics
isMain :: String -> Bool
isMain f =
  (isInfixOf "Main" f || isInfixOf "main" f)
    && isSuffixOf ".hs" f
    || isSuffixOf ".lhs" f

-- | Check if a given file has a Haskell extension
isHaskell :: String -> Bool
isHaskell f = isSuffixOf ".hs" f || isSuffixOf ".lhs" f

isBuildTool :: CabalSpecVersion -> String -> Bool
isBuildTool v = not . null . knownSuffixHandlers v . takeExtension

retrieveBuildTools :: Interactive m => CabalSpecVersion -> FilePath -> m [Dependency]
retrieveBuildTools v fp = do
  exists <- doesDirectoryExist fp
  if exists
    then do
      files <- fmap takeExtension <$> listFilesRecursive fp

      let tools =
            [ mkStringyDep (knownSuffixHandlers v f)
            | f <- files
            , isBuildTool v f
            ]

      return tools
    else return []

retrieveSourceFiles :: Interactive m => FilePath -> m [SourceFileEntry]
retrieveSourceFiles fp = do
  exists <- doesDirectoryExist fp
  if exists
    then do
      files <- filter isHaskell <$> listFilesRecursive fp

      entries <- forM files $ \f -> do
        exists' <- doesFileExist f
        if exists'
          then do
            maybeModuleName <- retrieveModuleName f
            case maybeModuleName of
              Nothing -> return Nothing
              Just moduleName -> do
                let fileExtension = takeExtension f
                relativeSourcePath <- makeRelative f <$> getCurrentDirectory
                imports <- retrieveModuleImports f
                extensions <- retrieveModuleExtensions f

                return . Just $ SourceFileEntry{..}
          else return Nothing

      return . catMaybes $ entries
    else return []

-- | Given a module, retrieve its name
retrieveModuleName :: Interactive m => FilePath -> m (Maybe ModuleName)
retrieveModuleName m = do
  rawModule <- trim . grabModuleName <$> readFile m

  if isInfixOf rawModule (dirToModuleName m)
    then return $ Just $ fromString rawModule
    else do
      putStrLn $
        "Warning: found module that doesn't match directory structure: "
          ++ rawModule
      return Nothing
  where
    dirToModuleName = map (\x -> if x == '/' || x == '\\' then '.' else x)

    stop c = (c /= '\n') && (c /= ' ')

    grabModuleName [] = []
    grabModuleName ('-' : '-' : xs) = grabModuleName $ dropWhile' (/= '\n') xs
    grabModuleName ('m' : 'o' : 'd' : 'u' : 'l' : 'e' : ' ' : xs) = takeWhile' stop xs
    grabModuleName (_ : xs) = grabModuleName xs

-- | Given a module, retrieve all of its imports
retrieveModuleImports :: Interactive m => FilePath -> m [ModuleName]
retrieveModuleImports m = do
  map (fromString . trim) . grabModuleImports <$> readFile m
  where
    stop c = (c /= '\n') && (c /= ' ') && (c /= '(')

    grabModuleImports [] = []
    grabModuleImports ('-' : '-' : xs) = grabModuleImports $ dropWhile' (/= '\n') xs
    grabModuleImports ('i' : 'm' : 'p' : 'o' : 'r' : 't' : ' ' : xs) = case trim xs of -- in case someone uses a weird formatting
      ('q' : 'u' : 'a' : 'l' : 'i' : 'f' : 'i' : 'e' : 'd' : ' ' : ys) -> takeWhile' stop ys : grabModuleImports (dropWhile' stop ys)
      _ -> takeWhile' stop xs : grabModuleImports (dropWhile' stop xs)
    grabModuleImports (_ : xs) = grabModuleImports xs

-- | Given a module, retrieve all of its language pragmas
retrieveModuleExtensions :: Interactive m => FilePath -> m [Extension]
retrieveModuleExtensions m = do
  catMaybes <$> map (simpleParsec . trim) . grabModuleExtensions <$> readFile m
  where
    stop c = (c /= '\n') && (c /= ' ') && (c /= ',') && (c /= '#')

    grabModuleExtensions [] = []
    grabModuleExtensions ('-' : '-' : xs) = grabModuleExtensions $ dropWhile' (/= '\n') xs
    grabModuleExtensions ('L' : 'A' : 'N' : 'G' : 'U' : 'A' : 'G' : 'E' : xs) = takeWhile' stop xs : grabModuleExtensions' (dropWhile' stop xs)
    grabModuleExtensions (_ : xs) = grabModuleExtensions xs

    grabModuleExtensions' [] = []
    grabModuleExtensions' ('#' : xs) = grabModuleExtensions xs
    grabModuleExtensions' (',' : xs) = takeWhile' stop xs : grabModuleExtensions' (dropWhile' stop xs)
    grabModuleExtensions' (_ : xs) = grabModuleExtensions xs

takeWhile' :: (Char -> Bool) -> String -> String
takeWhile' p = takeWhile p . trim

dropWhile' :: (Char -> Bool) -> String -> String
dropWhile' p = dropWhile p . trim

-- | Check whether a potential source file is located in one of the
--   source directories.
isSourceFile :: Maybe [FilePath] -> SourceFileEntry -> Bool
isSourceFile Nothing sf = isSourceFile (Just ["."]) sf
isSourceFile (Just srcDirs) sf = any (equalFilePath (relativeSourcePath sf)) srcDirs

retrieveDependencies :: Interactive m => Verbosity -> InitFlags -> [(ModuleName, ModuleName)] -> InstalledPackageIndex -> m [P.Dependency]
retrieveDependencies v flags mods' pkgIx = do
  let mods = mods'

      modMap :: M.Map ModuleName [InstalledPackageInfo]
      modMap = M.map (filter exposed) $ moduleNameIndex pkgIx

      modDeps :: [(ModuleName, ModuleName, Maybe [InstalledPackageInfo])]
      modDeps = map (\(mn, ds) -> (mn, ds, M.lookup ds modMap)) mods
  -- modDeps = map (id &&& flip M.lookup modMap) mods

  message v Info "Guessing dependencies..."
  nub . catMaybes <$> traverse (chooseDep v flags) modDeps

-- Given a module and a list of installed packages providing it,
-- choose a dependency (i.e. package + version range) to use for that
-- module.
chooseDep
  :: Interactive m
  => Verbosity
  -> InitFlags
  -> (ModuleName, ModuleName, Maybe [InstalledPackageInfo])
  -> m (Maybe P.Dependency)
chooseDep v flags (importer, m, mipi) = case mipi of
  -- We found some packages: group them by name.
  Just ps@(_ : _) ->
    case NE.groupBy (\x y -> P.pkgName x == P.pkgName y) $ map P.packageId ps of
      -- if there's only one group, i.e. multiple versions of a single package,
      -- we make it into a dependency, choosing the latest-ish version.

      -- Given a list of available versions of the same package, pick a dependency.
      [grp] -> fmap Just $ case grp of
        -- If only one version, easy. We change e.g. 0.4.2  into  0.4.*
        (pid :| []) ->
          return $
            P.Dependency
              (P.pkgName pid)
              (pvpize desugar . P.pkgVersion $ pid)
              P.mainLibSet -- TODO sublibraries

        -- Otherwise, choose the latest version and issue a warning.
        pids -> do
          message v Warning ("multiple versions of " ++ prettyShow (P.pkgName . NE.head $ pids) ++ " provide " ++ prettyShow m ++ ", choosing the latest.")
          return $
            P.Dependency
              (P.pkgName . NE.head $ pids)
              (pvpize desugar . maximum . fmap P.pkgVersion $ pids)
              P.mainLibSet -- TODO take into account sublibraries

      -- if multiple packages are found, we refuse to choose between
      -- different packages and make the user do it
      grps -> do
        message v Warning ("multiple packages found providing " ++ prettyShow m ++ ": " ++ intercalate ", " (fmap (prettyShow . P.pkgName . NE.head) grps))
        message v Warning "You will need to pick one and manually add it to the build-depends field."
        return Nothing
  _ -> do
    message v Warning ("no package found providing " ++ prettyShow m ++ " in " ++ prettyShow importer ++ ".")
    return Nothing
  where
    -- desugar if cabal version lower than 2.0
    desugar = case cabalVersion flags of
      Flag x -> x < CabalSpecV2_0
      NoFlag -> defaultCabalVersion < CabalSpecV2_0

filePathToPkgName :: Interactive m => FilePath -> m P.PackageName
filePathToPkgName =
  fmap (mkPackageName . repair . fromMaybe "" . safeLast . splitDirectories)
    . canonicalizePathNoThrow
  where
    -- Treat each span of non-alphanumeric characters as a hyphen. Each
    -- hyphenated component of a package name must contain at least one
    -- alphabetic character. An arbitrary character ('x') will be prepended if
    -- this is not the case for the first component, and subsequent components
    -- will simply be run together. For example, "1+2_foo-3" will become
    -- "x12-foo3".
    repair = repair' ('x' :) id
    repair' invalid valid x = case dropWhile (not . isAlphaNum) x of
      "" -> repairComponent ""
      x' ->
        let (c, r) = first repairComponent $ span isAlphaNum x'
         in c ++ repairRest r
      where
        repairComponent c
          | all isDigit c = invalid c
          | otherwise = valid c
    repairRest = repair' id ('-' :)

currentDirPkgName :: Interactive m => m P.PackageName
currentDirPkgName = filePathToPkgName =<< getCurrentDirectory

mkPackageNameDep :: PackageName -> Dependency
mkPackageNameDep pkg = mkDependency pkg anyVersion (NES.singleton LMainLibName)

-- when cabal-version < 1.18, extra-doc-files is not supported
-- so whatever the user wants as doc files should be dumped into
-- extra-src-files.
--
fixupDocFiles :: Interactive m => Verbosity -> PkgDescription -> m PkgDescription
fixupDocFiles v pkgDesc
  | _pkgCabalVersion pkgDesc < CabalSpecV1_18 = do
      message v Warning $
        concat
          [ "Cabal spec versions < 1.18 do not support extra-doc-files. "
          , "Doc files will be treated as extra-src-files."
          ]

      return $
        pkgDesc
          { _pkgExtraSrcFiles =
              _pkgExtraSrcFiles pkgDesc
                <> fromMaybe mempty (_pkgExtraDocFiles pkgDesc)
          , _pkgExtraDocFiles = Nothing
          }
  | otherwise = return pkgDesc

mkStringyDep :: String -> Dependency
mkStringyDep = mkPackageNameDep . mkPackageName

getBaseDep :: Interactive m => InstalledPackageIndex -> InitFlags -> m [Dependency]
getBaseDep pkgIx flags =
  retrieveDependencies
    silent
    flags
    [(fromString "Prelude", fromString "Prelude")]
    pkgIx

-- Add package name as dependency of test suite
--
addLibDepToTest :: PackageName -> Maybe TestTarget -> Maybe TestTarget
addLibDepToTest _ Nothing = Nothing
addLibDepToTest n (Just t) =
  Just $
    t
      { _testDependencies = _testDependencies t ++ [mkPackageNameDep n]
      }

-- Add package name as dependency of executable
--
addLibDepToExe :: PackageName -> ExeTarget -> ExeTarget
addLibDepToExe n exe =
  exe
    { _exeDependencies = _exeDependencies exe ++ [mkPackageNameDep n]
    }
