{-# LANGUAGE RecordWildCards #-}

module Distribution.Client.Init.Utils
( SourceFileEntry(..)
, retrieveSourceFiles
, retrieveModuleName
, retrieveModuleImports
, retrieveModuleExtensions
, retrieveBuildTools
, retrieveDependencies
, isMain
, isHaskell
, isSourceFile
, versionParser
, trim
, currentDirPkgName
, filePathToPkgName
, mkPackageNameDep
, fixupDocFiles
) where


import qualified Prelude
import Distribution.Client.Compat.Prelude hiding (empty, readFile, Parsec, many)
import Distribution.Utils.Generic (isInfixOf)

import Control.Monad (forM)

import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Language.Haskell.Extension (Extension(..))
import System.FilePath

import Distribution.CabalSpecVersion (CabalSpecVersion(..))
import Distribution.ModuleName (ModuleName)
import Distribution.InstalledPackageInfo (InstalledPackageInfo, exposed)
import qualified Distribution.Package as P
import qualified Distribution.Types.PackageName as PN
import Distribution.Simple.PackageIndex (InstalledPackageIndex, moduleNameIndex)
import Distribution.Simple.Setup (Flag(..), fromFlagOrDefault)
import Distribution.Verbosity
import Distribution.Version
import Distribution.Client.Init.Defaults
import Distribution.Client.Init.Types
import Text.Parsec
import Distribution.Types.PackageName
import Distribution.Types.Dependency (Dependency, mkDependency)
import qualified Distribution.Compat.NonEmptySet as NES
import Distribution.Types.LibraryName


-- |Data type of source files found in the working directory
data SourceFileEntry = SourceFileEntry
    { relativeSourcePath :: FilePath
    , moduleName         :: ModuleName
    , fileExtension      :: String
    , imports            :: [ModuleName]
    , extensions         :: [Extension]
    } deriving Show

-- Unfortunately we cannot use the version exported by Distribution.Simple.Program
knownSuffixHandlers :: String -> String
knownSuffixHandlers ".gc"    = "greencard"
knownSuffixHandlers ".chs"   = "chs"
knownSuffixHandlers ".hsc"   = "hsc2hs"
knownSuffixHandlers ".x"     = "alex"
knownSuffixHandlers ".y"     = "happy"
knownSuffixHandlers ".ly"    = "happy"
knownSuffixHandlers ".cpphs" = "cpp"
knownSuffixHandlers _       = ""


-- | Check if a given file has main file characteristics
isMain :: String -> Bool
isMain f = (isInfixOf "Main" f || isInfixOf "main" f)
         && isSuffixOf ".hs" f || isSuffixOf ".lhs" f

-- | Check if a given file has a Haskell extension
isHaskell :: String -> Bool
isHaskell f = isSuffixOf ".hs" f || isSuffixOf ".lhs" f

isBuildTool :: String -> Bool
isBuildTool f = not . null . knownSuffixHandlers $ takeExtension f

retrieveBuildTools :: Interactive m => FilePath -> m [String]
retrieveBuildTools fp = do
  files <- map takeExtension <$> listFilesRecursive fp

  return [knownSuffixHandlers f | f <- files, isBuildTool f]

retrieveSourceFiles :: Interactive m => FilePath -> m [SourceFileEntry]
retrieveSourceFiles fp = do
  files <- filter isHaskell <$> listFilesRecursive fp

  forM files $ \f -> do
    let fileExtension   = takeExtension f
    relativeSourcePath <- makeRelative f <$> getCurrentDirectory
    moduleName         <- retrieveModuleName f
    imports            <- retrieveModuleImports f
    extensions         <- retrieveModuleExtensions f

    return $ SourceFileEntry {..}

-- | Given a module, retrieve its name
retrieveModuleName :: Interactive m => FilePath -> m ModuleName
retrieveModuleName m = do
  fromString . trim . grabModuleName <$> readFile m

  where
    stop c = (c /= '\\') && (c /= ' ')

    grabModuleName [] = []
    grabModuleName ('m':'o':'d':'u':'l':'e':' ':xs) = takeWhile' stop xs
    grabModuleName (_:xs) = grabModuleName xs

-- | Given a module, retrieve all of its imports
retrieveModuleImports :: Interactive m => FilePath -> m [ModuleName]
retrieveModuleImports m = do
  map (fromString . trim) . grabModuleImports <$> readFile m

  where
    stop c = (c /= '\\') && (c /= ' ') && (c /= '(')

    grabModuleImports [] = []
    grabModuleImports ('i':'m':'p':'o':'r':'t':' ':xs) = case trim xs of -- in case someone uses a weird formatting
      ('q':'u':'a':'l':'i':'f':'i':'e':'d':' ':ys) -> takeWhile' stop ys : grabModuleImports (dropWhile' stop ys)
      _                                            -> takeWhile' stop xs : grabModuleImports (dropWhile' stop xs)
    grabModuleImports (_:xs) = grabModuleImports xs

-- | Given a module, retrieve all of its language pragmas
retrieveModuleExtensions :: Interactive m => FilePath -> m [Extension]
retrieveModuleExtensions m = do
  catMaybes <$> map (simpleParsec . trim) . grabModuleExtensions <$> readFile m

  where
    stop c = (c /= '\\') && (c /= ' ') && (c /= ',')

    grabModuleExtensions [] = []
    grabModuleExtensions ('L':'A':'N':'G':'U':'A':'G':'E':' ':xs) = takeWhile' stop xs : grabModuleExtensions (dropWhile' stop xs)
    grabModuleExtensions (',':xs) = takeWhile' stop xs : grabModuleExtensions (dropWhile' stop xs)
    grabModuleExtensions (_:xs) = grabModuleExtensions xs

takeWhile' :: (Char -> Bool) -> String -> String
takeWhile' p = takeWhile p . trim

dropWhile' :: (Char -> Bool) -> String -> String
dropWhile' p = dropWhile p . trim

trim :: String -> String
trim = removeLeadingSpace . reverse . removeLeadingSpace . reverse
  where
    removeLeadingSpace  = dropWhile isSpace

-- | Check whether a potential source file is located in one of the
--   source directories.
isSourceFile :: Maybe [FilePath] -> SourceFileEntry -> Bool
isSourceFile Nothing        sf = isSourceFile (Just ["."]) sf
isSourceFile (Just srcDirs) sf = any (equalFilePath (relativeSourcePath sf)) srcDirs

retrieveDependencies :: Interactive m => InitFlags -> [ModuleName] -> InstalledPackageIndex -> m [P.Dependency]
retrieveDependencies flags mods' pkgIx = do
  let mods = mods'

      modMap :: M.Map ModuleName [InstalledPackageInfo]
      modMap  = M.map (filter exposed) $ moduleNameIndex pkgIx

      modDeps :: [(ModuleName, Maybe [InstalledPackageInfo])]
      modDeps = map (\mn -> (mn, M.lookup mn modMap)) mods
      -- modDeps = map (id &&& flip M.lookup modMap) mods

  message (fromFlagOrDefault silent $ initVerbosity flags) "\nGuessing dependencies..."
  nub . catMaybes <$> traverse (chooseDep flags) modDeps

-- Given a module and a list of installed packages providing it,
-- choose a dependency (i.e. package + version range) to use for that
-- module.
chooseDep
  :: Interactive m
  => InitFlags
  -> (ModuleName, Maybe [InstalledPackageInfo])
  -> m (Maybe P.Dependency)
chooseDep flags (m, mipi) = case mipi of
  -- We found some packages: group them by name.
  Just ps@(_:_) ->
    case NE.groupBy (\x y -> P.pkgName x == P.pkgName y) $ map P.packageId ps of
    -- if there's only one group, i.e. multiple versions of a single package,
    -- we make it into a dependency, choosing the latest-ish version.

      -- Given a list of available versions of the same package, pick a dependency.
      [grp] -> fmap Just $ case grp of

        -- If only one version, easy. We change e.g. 0.4.2  into  0.4.*
        (pid:|[]) ->
          return $ P.Dependency
              (P.pkgName pid)
              (pvpize desugar . P.pkgVersion $ pid)
              P.mainLibSet --TODO sublibraries

        -- Otherwise, choose the latest version and issue a warning.
        pids -> do
          message v ("\nWarning: multiple versions of " ++ prettyShow (P.pkgName . NE.head $ pids) ++ " provide " ++ prettyShow m ++ ", choosing the latest.")
          return $ P.Dependency
              (P.pkgName . NE.head $ pids)
              (pvpize desugar . maximum . fmap P.pkgVersion $ pids)
              P.mainLibSet --TODO take into account sublibraries

      -- if multiple packages are found, we refuse to choose between
      -- different packages and make the user do it
      grps     -> do
        message v ("\nWarning: multiple packages found providing " ++ prettyShow m ++ ": " ++ intercalate ", " (fmap (prettyShow . P.pkgName . NE.head) grps))
        message v "You will need to pick one and manually add it to the build-depends field."
        return Nothing

  _ -> do
    message v ("\nWarning: no package found providing " ++ prettyShow m ++ ".")
    return Nothing

  where
    v = fromFlagOrDefault normal (initVerbosity flags)

    -- desugar if cabal version lower than 2.0
    desugar = case cabalVersion flags of
      Flag x -> x                   < CabalSpecV2_0
      NoFlag -> defaultCabalVersion < CabalSpecV2_0

-- | Given a version, return an API-compatible (according to PVP) version range.
--
-- If the boolean argument denotes whether to use a desugared
-- representation (if 'True') or the new-style @^>=@-form (if
-- 'False').
--
-- Example: @pvpize True (mkVersion [0,4,1])@ produces the version range @>= 0.4 && < 0.5@ (which is the
-- same as @0.4.*@).
pvpize :: Bool -> Version -> VersionRange
pvpize False  v = majorBoundVersion v
pvpize True   v = orLaterVersion v'
           `intersectVersionRanges`
           earlierVersion (incVersion 1 v')
  where
    v' = alterVersion (take 2) v

    -- Increment the nth version component (counting from 0).
    incVersion :: Int -> Version -> Version
    incVersion n = alterVersion (incVersion' n)
      where
        incVersion' 0 []     = [1]
        incVersion' 0 (v'':_)  = [v'' + 1]
        incVersion' m []     = replicate m 0 ++ [1]
        incVersion' m (v'':vs) = v'' : incVersion' (m-1) vs

versionParser :: Parsec String () String
versionParser = do
  skipMany (noneOf "1234567890")
  many $ choice
    [ oneOf "1234567890"
    , oneOf "."
    ]

filePathToPkgName :: FilePath -> P.PackageName
filePathToPkgName = PN.mkPackageName . Prelude.last . splitDirectories

currentDirPkgName :: Interactive m => m P.PackageName
currentDirPkgName = filePathToPkgName <$> getCurrentDirectory

mkPackageNameDep :: PackageName -> Dependency
mkPackageNameDep pkg = mkDependency pkg anyVersion (NES.singleton LMainLibName)

-- when cabal-version < 1.18, extra-doc-files is not supported
-- so whatever the user wants as doc files should be dumped into
-- extra-src-files.
--
fixupDocFiles :: PkgDescription -> PkgDescription
fixupDocFiles pkgDesc
  | _pkgCabalVersion pkgDesc < CabalSpecV1_18 = pkgDesc
    { _pkgExtraSrcFiles =_pkgExtraSrcFiles pkgDesc
      <> fromMaybe mempty (_pkgExtraDocFiles pkgDesc)
    , _pkgExtraDocFiles = Nothing
    }
  | otherwise = pkgDesc
