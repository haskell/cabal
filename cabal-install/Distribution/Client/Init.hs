-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Init
-- Copyright   :  (c) Brent Yorgey 2009
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Implementation of the 'cabal init' command, which creates an initial .cabal
-- file for a project.
--
-----------------------------------------------------------------------------

module Distribution.Client.Init (

    -- * Commands
    initCabal
  , incVersion

  ) where

import Prelude ()
import Distribution.Client.Compat.Prelude hiding (empty)

import Distribution.Deprecated.ReadP (readP_to_E)

import System.IO
  ( hSetBuffering, stdout, BufferMode(..) )
import System.Directory
  ( getCurrentDirectory, doesDirectoryExist, doesFileExist, copyFile
  , getDirectoryContents, createDirectoryIfMissing )
import System.FilePath
  ( (</>), (<.>), takeBaseName, takeExtension, equalFilePath )
import Data.Time
  ( getCurrentTime, utcToLocalTime, toGregorian, localDay, getCurrentTimeZone )

import Data.List
  ( groupBy, (\\) )
import Data.Function
  ( on )
import qualified Data.Map as M
import qualified Data.Set as Set
import Control.Monad
  ( (>=>), join, forM_, mapM, mapM_ )
import Control.Arrow
  ( (&&&), (***) )

import Text.PrettyPrint hiding (mode, cat)

import Distribution.Version
  ( Version, mkVersion, alterVersion, versionNumbers, majorBoundVersion
  , orLaterVersion, earlierVersion, intersectVersionRanges, VersionRange )
import Distribution.Verbosity
  ( Verbosity )
import Distribution.ModuleName
  ( ModuleName )  -- And for the Text instance
import qualified Distribution.ModuleName as ModuleName
  ( fromString, toFilePath )
import Distribution.InstalledPackageInfo
  ( InstalledPackageInfo, exposed )
import qualified Distribution.Package as P
import Distribution.Types.LibraryName
  ( LibraryName(..) )
import Language.Haskell.Extension ( Language(..) )

import Distribution.Client.Init.Types
  ( InitFlags(..), BuildType(..), PackageType(..), Category(..)
  , displayPackageType )
import Distribution.Client.Init.Licenses
  ( bsd2, bsd3, gplv2, gplv3, lgpl21, lgpl3, agplv3, apache20, mit, mpl20, isc )
import Distribution.Client.Init.Heuristics
  ( guessPackageName, guessAuthorNameMail, guessMainFileCandidates,
    SourceFileEntry(..),
    scanForModules, neededBuildPrograms )

import Distribution.License
  ( License(..), knownLicenses, licenseToSPDX )
import qualified Distribution.SPDX as SPDX

import Distribution.ReadE
  ( runReadE )
import Distribution.Simple.Setup
  ( Flag(..), flagToMaybe )
import Distribution.Simple.Utils
  ( dropWhileEndLE )
import Distribution.Simple.Configure
  ( getInstalledPackages )
import Distribution.Simple.Compiler
  ( PackageDBStack, Compiler )
import Distribution.Simple.Program
  ( ProgramDb )
import Distribution.Simple.PackageIndex
  ( InstalledPackageIndex, moduleNameIndex )
import Distribution.Deprecated.Text
  ( display, Text(..) )
import Distribution.Pretty
  ( prettyShow )
import Distribution.Parsec
  ( eitherParsec )

import Distribution.Solver.Types.PackageIndex
  ( elemByPackageName )

import Distribution.Client.IndexUtils
  ( getSourcePackages )
import Distribution.Client.Types
  ( SourcePackageDb(..) )
import Distribution.Client.Setup
  ( RepoContext(..) )

initCabal :: Verbosity
          -> PackageDBStack
          -> RepoContext
          -> Compiler
          -> ProgramDb
          -> InitFlags
          -> IO ()
initCabal verbosity packageDBs repoCtxt comp progdb initFlags = do

  installedPkgIndex <- getInstalledPackages verbosity comp packageDBs progdb
  sourcePkgDb <- getSourcePackages verbosity repoCtxt

  hSetBuffering stdout NoBuffering

  initFlags' <- extendFlags installedPkgIndex sourcePkgDb initFlags

  case license initFlags' of
    Flag PublicDomain -> return ()
    _                 -> writeLicense initFlags'
  writeSetupFile initFlags'
  writeChangeLog initFlags'
  createDirectories (sourceDirs initFlags')
  createLibHs initFlags'
  createDirectories (applicationDirs initFlags')
  createMainHs initFlags'
  -- If a test suite was requested and this is not an executable-only
  -- package, then create the "test" directory.
  when (eligibleForTestSuite initFlags') $ do
    createDirectories (testDirs initFlags')
    createTestHs initFlags'
  success <- writeCabalFile initFlags'

  when success $ generateWarnings initFlags'

---------------------------------------------------------------------------
--  Flag acquisition  -----------------------------------------------------
---------------------------------------------------------------------------

-- | Fill in more details by guessing, discovering, or prompting the
--   user.
extendFlags :: InstalledPackageIndex -> SourcePackageDb -> InitFlags -> IO InitFlags
extendFlags pkgIx sourcePkgDb =
      getSimpleProject
  >=> getLibOrExec
  >=> getCabalVersion
  >=> getPackageName sourcePkgDb
  >=> getVersion
  >=> getLicense
  >=> getAuthorInfo
  >=> getHomepage
  >=> getSynopsis
  >=> getCategory
  >=> getExtraSourceFiles
  >=> getAppDir
  >=> getSrcDir
  >=> getGenTests
  >=> getTestDir
  >=> getLanguage
  >=> getGenComments
  >=> getModulesBuildToolsAndDeps pkgIx

-- | Combine two actions which may return a value, preferring the first. That
--   is, run the second action only if the first doesn't return a value.
infixr 1 ?>>
(?>>) :: IO (Maybe a) -> IO (Maybe a) -> IO (Maybe a)
f ?>> g = do
  ma <- f
  if isJust ma
    then return ma
    else g

-- | Witness the isomorphism between Maybe and Flag.
maybeToFlag :: Maybe a -> Flag a
maybeToFlag = maybe NoFlag Flag

defaultCabalVersion :: Version
defaultCabalVersion = mkVersion [1,10]

displayCabalVersion :: Version -> String
displayCabalVersion v = case versionNumbers v of
  [1,10] -> "1.10   (legacy)"
  [2,0]  -> "2.0    (+ support for Backpack, internal sub-libs, '^>=' operator)"
  [2,2]  -> "2.2    (+ support for 'common', 'elif', redundant commas, SPDX)"
  [2,4]  -> "2.4    (+ support for '**' globbing)"
  _      -> display v

-- | Ask if a simple project with sensible defaults should be created.
getSimpleProject :: InitFlags -> IO InitFlags
getSimpleProject flags = do
  simpleProj <-     return (flagToMaybe $ simpleProject flags)
                ?>> maybePrompt flags
                    (promptYesNo
                      "Should I generate a simple project with sensible defaults"
                      (Just True))
  return $ case maybeToFlag simpleProj of
    Flag True ->
      flags { interactive = Flag False
            , simpleProject = Flag True
            , packageType = Flag LibraryAndExecutable
            , cabalVersion = Flag (mkVersion [2,4])
            }
    simpleProjFlag@_ ->
      flags { simpleProject = simpleProjFlag }


-- | Ask which version of the cabal spec to use.
getCabalVersion :: InitFlags -> IO InitFlags
getCabalVersion flags = do
  cabVer <-     return (flagToMaybe $ cabalVersion flags)
            ?>> maybePrompt flags (either (const defaultCabalVersion) id `fmap`
                                  promptList "Please choose version of the Cabal specification to use"
                                  [mkVersion [1,10], mkVersion [2,0], mkVersion [2,2], mkVersion [2,4]]
                                  (Just defaultCabalVersion) displayCabalVersion False)
            ?>> return (Just defaultCabalVersion)

  return $  flags { cabalVersion = maybeToFlag cabVer }


-- | Get the package name: use the package directory (supplied, or the current
--   directory by default) as a guess. It looks at the SourcePackageDb to avoid
--   using an existing package name.
getPackageName :: SourcePackageDb -> InitFlags -> IO InitFlags
getPackageName sourcePkgDb flags = do
  guess    <-     traverse guessPackageName (flagToMaybe $ packageDir flags)
              ?>> Just `fmap` (getCurrentDirectory >>= guessPackageName)

  let guess' | isPkgRegistered guess = Nothing
             | otherwise = guess

  pkgName' <-     return (flagToMaybe $ packageName flags)
              ?>> maybePrompt flags (prompt "Package name" guess')
              ?>> return guess'

  chooseAgain <- if isPkgRegistered pkgName'
                    then promptYesNo promptOtherNameMsg (Just True)
                    else return False

  if chooseAgain
    then getPackageName sourcePkgDb flags
    else return $ flags { packageName = maybeToFlag pkgName' }

  where
    isPkgRegistered (Just pkg) = elemByPackageName (packageIndex sourcePkgDb) pkg
    isPkgRegistered Nothing    = False

    promptOtherNameMsg = "This package name is already used by another " ++
                         "package on hackage. Do you want to choose a " ++
                         "different name"

-- | Package version: use 0.1.0.0 as a last resort, but try prompting the user
--  if possible.
getVersion :: InitFlags -> IO InitFlags
getVersion flags = do
  let v = Just $ mkVersion [0,1,0,0]
  v' <-     return (flagToMaybe $ version flags)
        ?>> maybePrompt flags (prompt "Package version" v)
        ?>> return v
  return $ flags { version = maybeToFlag v' }

-- | Choose a license.
getLicense :: InitFlags -> IO InitFlags
getLicense flags = do
  lic <-     return (flagToMaybe $ license flags)
         ?>> fmap (fmap (either UnknownLicense id))
                  (maybePrompt flags
                    (promptList "Please choose a license" listedLicenses
                     (Just BSD3) displayLicense True))

  case checkLicenseInvalid lic of
    Just msg -> putStrLn msg >> getLicense flags
    Nothing  -> return $ flags { license = maybeToFlag lic }

  where
    displayLicense l | needSpdx  = prettyShow (licenseToSPDX l)
                     | otherwise = display l

    checkLicenseInvalid (Just (UnknownLicense t))
      | needSpdx  = case eitherParsec t :: Either String SPDX.License of
                      Right _ -> Nothing
                      Left _  -> Just "\nThe license must be a valid SPDX expression."
      | otherwise = if any (not . isAlphaNum) t
                    then Just promptInvalidOtherLicenseMsg
                    else Nothing
    checkLicenseInvalid _ = Nothing

    promptInvalidOtherLicenseMsg = "\nThe license must be alphanumeric. " ++
                                   "If your license name has many words, " ++
                                   "the convention is to use camel case (e.g. PublicDomain). " ++
                                   "Please choose a different license."

    listedLicenses =
      knownLicenses \\ [GPL Nothing, LGPL Nothing, AGPL Nothing
                       , Apache Nothing, OtherLicense]

    needSpdx = maybe False (>= mkVersion [2,2]) $ flagToMaybe (cabalVersion flags)

-- | The author's name and email. Prompt, or try to guess from an existing
--   darcs repo.
getAuthorInfo :: InitFlags -> IO InitFlags
getAuthorInfo flags = do
  (authorName, authorEmail)  <-
    (flagToMaybe *** flagToMaybe) `fmap` guessAuthorNameMail
  authorName'  <-     return (flagToMaybe $ author flags)
                  ?>> maybePrompt flags (promptStr "Author name" authorName)
                  ?>> return authorName

  authorEmail' <-     return (flagToMaybe $ email flags)
                  ?>> maybePrompt flags (promptStr "Maintainer email" authorEmail)
                  ?>> return authorEmail

  return $ flags { author = maybeToFlag authorName'
                 , email  = maybeToFlag authorEmail'
                 }

-- | Prompt for a homepage URL.
getHomepage :: InitFlags -> IO InitFlags
getHomepage flags = do
  hp  <- queryHomepage
  hp' <-     return (flagToMaybe $ homepage flags)
         ?>> maybePrompt flags (promptStr "Project homepage URL" hp)
         ?>> return hp

  return $ flags { homepage = maybeToFlag hp' }

-- | Right now this does nothing, but it could be changed to do some
--   intelligent guessing.
queryHomepage :: IO (Maybe String)
queryHomepage = return Nothing     -- get default remote darcs repo?

-- | Prompt for a project synopsis.
getSynopsis :: InitFlags -> IO InitFlags
getSynopsis flags = do
  syn <-     return (flagToMaybe $ synopsis flags)
         ?>> maybePrompt flags (promptStr "Project synopsis" Nothing)

  return $ flags { synopsis = maybeToFlag syn }

-- | Prompt for a package category.
--   Note that it should be possible to do some smarter guessing here too, i.e.
--   look at the name of the top level source directory.
getCategory :: InitFlags -> IO InitFlags
getCategory flags = do
  cat <-     return (flagToMaybe $ category flags)
         ?>> fmap join (maybePrompt flags
                         (promptListOptional "Project category" [Codec ..]))
  return $ flags { category = maybeToFlag cat }

-- | Try to guess extra source files (don't prompt the user).
getExtraSourceFiles :: InitFlags -> IO InitFlags
getExtraSourceFiles flags = do
  extraSrcFiles <-     return (extraSrc flags)
                   ?>> Just `fmap` guessExtraSourceFiles flags

  return $ flags { extraSrc = extraSrcFiles }

defaultChangeLog :: FilePath
defaultChangeLog = "CHANGELOG.md"

-- | Try to guess things to include in the extra-source-files field.
--   For now, we just look for things in the root directory named
--   'readme', 'changes', or 'changelog', with any sort of
--   capitalization and any extension.
guessExtraSourceFiles :: InitFlags -> IO [FilePath]
guessExtraSourceFiles flags = do
  dir <-
    maybe getCurrentDirectory return . flagToMaybe $ packageDir flags
  files <- getDirectoryContents dir
  let extraFiles = filter isExtra files
  if any isLikeChangeLog extraFiles
    then return extraFiles
    else return (defaultChangeLog : extraFiles)

  where
    isExtra = likeFileNameBase ("README" : changeLogLikeBases)
    isLikeChangeLog = likeFileNameBase changeLogLikeBases
    likeFileNameBase candidates = (`elem` candidates) . map toUpper . takeBaseName
    changeLogLikeBases = ["CHANGES", "CHANGELOG"]

-- | Ask whether the project builds a library or executable.
getLibOrExec :: InitFlags -> IO InitFlags
getLibOrExec flags = do
  pkgType <-     return (flagToMaybe $ packageType flags)
           ?>> maybePrompt flags (either (const Executable) id `fmap`
                                   promptList "What does the package build"
                                   [Executable, Library, LibraryAndExecutable]
                                   Nothing displayPackageType False)
           ?>> return (Just Executable)

  -- If this package contains an executable, get the main file name.
  mainFile <- if pkgType == Just Library then return Nothing else
                    getMainFile flags

  return $ flags { packageType = maybeToFlag pkgType
                 , mainIs = maybeToFlag mainFile
                 }


-- | Try to guess the main file of the executable, and prompt the user to choose
-- one of them. Top-level modules including the word 'Main' in the file name
-- will be candidates, and shorter filenames will be preferred.
getMainFile :: InitFlags -> IO (Maybe FilePath)
getMainFile flags =
  return (flagToMaybe $ mainIs flags)
  ?>> do
    candidates <- guessMainFileCandidates flags
    let showCandidate = either (++" (does not yet exist, but will be created)") id
        defaultFile = listToMaybe candidates
    maybePrompt flags (either id (either id id) `fmap`
                       promptList "What is the main module of the executable"
                       candidates
                       defaultFile showCandidate True)
      ?>> return (fmap (either id id) defaultFile)

-- | Ask if a test suite should be generated for the library.
getGenTests :: InitFlags -> IO InitFlags
getGenTests flags = do
  genTests <-     return (flagToMaybe $ initializeTestSuite flags)
                  -- Only generate a test suite if the package contains a library.
              ?>> if (packageType flags) == Flag Executable then return (Just False) else return Nothing
              ?>> maybePrompt flags
                  (promptYesNo
                    "Should I generate a test suite for the library"
                    (Just True))
  return $ flags { initializeTestSuite = maybeToFlag genTests }

-- | Ask for the test root directory.
getTestDir :: InitFlags -> IO InitFlags
getTestDir flags = do
  dirs <- return (testDirs flags)
              -- Only need testDirs when test suite generation is enabled.
          ?>> if not (eligibleForTestSuite flags) then return (Just []) else return Nothing
          ?>> fmap (fmap ((:[]) . either id id)) (maybePrompt
                   flags
                   (promptList "Test directory" ["test"] (Just "test") id True))

  return $ flags { testDirs = dirs }

-- | Ask for the base language of the package.
getLanguage :: InitFlags -> IO InitFlags
getLanguage flags = do
  lang <-     return (flagToMaybe $ language flags)
          ?>> maybePrompt flags
                (either UnknownLanguage id `fmap`
                  promptList "What base language is the package written in"
                  [Haskell2010, Haskell98]
                  (Just Haskell2010) display True)
          ?>> return (Just Haskell2010)

  if invalidLanguage lang
    then putStrLn invalidOtherLanguageMsg >> getLanguage flags
    else return $ flags { language = maybeToFlag lang }

  where
    invalidLanguage (Just (UnknownLanguage t)) = any (not . isAlphaNum) t
    invalidLanguage _ = False

    invalidOtherLanguageMsg = "\nThe language must be alphanumeric. " ++
                              "Please enter a different language."

-- | Ask whether to generate explanatory comments.
getGenComments :: InitFlags -> IO InitFlags
getGenComments flags = do
  genComments <-     return (not <$> flagToMaybe (noComments flags))
                 ?>> maybePrompt flags (promptYesNo promptMsg (Just False))
                 ?>> return (Just False)
  return $ flags { noComments = maybeToFlag (fmap not genComments) }
  where
    promptMsg = "Add informative comments to each field in the cabal file (y/n)"

-- | Ask for the application root directory.
getAppDir :: InitFlags -> IO InitFlags
getAppDir flags = do
  appDirs <- return (applicationDirs flags)
             -- No application dir if this is a 'Library'.
             ?>> if (packageType flags) == Flag Library then return (Just []) else return Nothing
             ?>> fmap (:[]) `fmap` guessAppDir flags
             ?>> fmap (>>= fmap ((:[]) . either id id)) (maybePrompt
                      flags
                      (promptListOptional'
                       ("Application " ++ mainFile ++ "directory")
                       ["src-exe", "app"] id))

  return $ flags { applicationDirs = appDirs }

  where
    mainFile = case mainIs flags of
      Flag mainPath -> "(" ++ mainPath ++ ") "
      _             -> ""

-- | Try to guess app directory. Could try harder; for the
--   moment just looks to see whether there is a directory called 'app'.
guessAppDir :: InitFlags -> IO (Maybe String)
guessAppDir flags = do
  dir      <- maybe getCurrentDirectory return . flagToMaybe $ packageDir flags
  appIsDir <- doesDirectoryExist (dir </> "app")
  return $ if appIsDir
             then Just "app"
             else Nothing

-- | Ask for the source (library) root directory.
getSrcDir :: InitFlags -> IO InitFlags
getSrcDir flags = do
  srcDirs <- return (sourceDirs flags)
             -- source dir if this is an 'Executable'.
             ?>> if (packageType flags) == Flag Executable then return (Just []) else return Nothing
             ?>> fmap (:[]) `fmap` guessSourceDir flags
             ?>> fmap (>>= fmap ((:[]) . either id id)) (maybePrompt
                      flags
                      (promptListOptional' "Library source directory"
                       ["src", "lib", "src-lib"] id))

  return $ flags { sourceDirs = srcDirs }

-- | Try to guess source directory. Could try harder; for the
--   moment just looks to see whether there is a directory called 'src'.
guessSourceDir :: InitFlags -> IO (Maybe String)
guessSourceDir flags = do
  dir      <-
    maybe getCurrentDirectory return . flagToMaybe $ packageDir flags
  srcIsDir <- doesDirectoryExist (dir </> "src")
  return $ if srcIsDir
             then Just "src"
             else Nothing

-- | Check whether a potential source file is located in one of the
--   source directories.
isSourceFile :: Maybe [FilePath] -> SourceFileEntry -> Bool
isSourceFile Nothing        sf = isSourceFile (Just ["."]) sf
isSourceFile (Just srcDirs) sf = any (equalFilePath (relativeSourcePath sf)) srcDirs

-- | Get the list of exposed modules and extra tools needed to build them.
getModulesBuildToolsAndDeps :: InstalledPackageIndex -> InitFlags -> IO InitFlags
getModulesBuildToolsAndDeps pkgIx flags = do
  dir <- maybe getCurrentDirectory return . flagToMaybe $ packageDir flags

  sourceFiles0 <- scanForModules dir

  let sourceFiles = filter (isSourceFile (sourceDirs flags)) sourceFiles0

  Just mods <-      return (exposedModules flags)
           ?>> (return . Just . map moduleName $ sourceFiles)

  tools <-     return (buildTools flags)
           ?>> (return . Just . neededBuildPrograms $ sourceFiles)

  deps <-      return (dependencies flags)
           ?>> Just <$> importsToDeps flags
                        (fromString "Prelude" :  -- to ensure we get base as a dep
                           (   nub   -- only need to consider each imported package once
                             . filter (`notElem` mods)  -- don't consider modules from
                                                        -- this package itself
                             . concatMap imports
                             $ sourceFiles
                           )
                        )
                        pkgIx

  exts <-     return (otherExts flags)
          ?>> (return . Just . nub . concatMap extensions $ sourceFiles)

  -- If we're initializing a library and there were no modules discovered
  -- then create an empty 'MyLib' module.
  -- This gets a little tricky when 'sourceDirs' == 'applicationDirs' because
  -- then the executable needs to set 'other-modules: MyLib' or else the build
  -- fails.
  let (finalModsList, otherMods) = case (packageType flags, mods) of

        -- For an executable leave things as they are.
        (Flag Executable, _) -> (mods, otherModules flags)

        -- If a non-empty module list exists don't change anything.
        (_, (_:_)) -> (mods, otherModules flags)

        -- Library only: 'MyLib' in 'other-modules' only.
        (Flag Library, _) -> ([myLibModule], Nothing)

        -- For a 'LibraryAndExecutable' we need to have special handling.
        -- If we don't have a module list (Nothing or empty), then create a Lib.
        (_, []) ->
          if sourceDirs flags == applicationDirs flags
          then ([myLibModule], Just [myLibModule])
          else ([myLibModule], Nothing)

  return $ flags { exposedModules = Just finalModsList
                 , otherModules   = otherMods
                 , buildTools     = tools
                 , dependencies   = deps
                 , otherExts      = exts
                 }

importsToDeps :: InitFlags -> [ModuleName] -> InstalledPackageIndex -> IO [P.Dependency]
importsToDeps flags mods pkgIx = do

  let modMap :: M.Map ModuleName [InstalledPackageInfo]
      modMap  = M.map (filter exposed) $ moduleNameIndex pkgIx

      modDeps :: [(ModuleName, Maybe [InstalledPackageInfo])]
      modDeps = map (id &&& flip M.lookup modMap) mods

  message flags "\nGuessing dependencies..."
  nub . catMaybes <$> mapM (chooseDep flags) modDeps

-- Given a module and a list of installed packages providing it,
-- choose a dependency (i.e. package + version range) to use for that
-- module.
chooseDep :: InitFlags -> (ModuleName, Maybe [InstalledPackageInfo])
          -> IO (Maybe P.Dependency)

chooseDep flags (m, Nothing)
  = message flags ("\nWarning: no package found providing " ++ display m ++ ".")
    >> return Nothing

chooseDep flags (m, Just [])
  = message flags ("\nWarning: no package found providing " ++ display m ++ ".")
    >> return Nothing

    -- We found some packages: group them by name.
chooseDep flags (m, Just ps)
  = case pkgGroups of
      -- if there's only one group, i.e. multiple versions of a single package,
      -- we make it into a dependency, choosing the latest-ish version (see toDep).
      [grp] -> Just <$> toDep grp
      -- otherwise, we refuse to choose between different packages and make the user
      -- do it.
      grps  -> do message flags ("\nWarning: multiple packages found providing "
                                 ++ display m
                                 ++ ": " ++ intercalate ", " (map (display . P.pkgName . head) grps))
                  message flags "You will need to pick one and manually add it to the Build-depends: field."
                  return Nothing
  where
    pkgGroups = groupBy ((==) `on` P.pkgName) (map P.packageId ps)

    desugar = maybe True (< mkVersion [2]) $ flagToMaybe (cabalVersion flags)

    -- Given a list of available versions of the same package, pick a dependency.
    toDep :: [P.PackageIdentifier] -> IO P.Dependency

    -- If only one version, easy.  We change e.g. 0.4.2  into  0.4.*
    toDep [pid] = return $ P.Dependency (P.pkgName pid) (pvpize desugar . P.pkgVersion $ pid) (Set.singleton LMainLibName) --TODO sublibraries

    -- Otherwise, choose the latest version and issue a warning.
    toDep pids  = do
      message flags ("\nWarning: multiple versions of " ++ display (P.pkgName . head $ pids) ++ " provide " ++ display m ++ ", choosing the latest.")
      return $ P.Dependency (P.pkgName . head $ pids)
                            (pvpize desugar . maximum . map P.pkgVersion $ pids)
                            (Set.singleton LMainLibName) --TODO take into account sublibraries

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
  where v' = alterVersion (take 2) v

-- | Increment the nth version component (counting from 0).
incVersion :: Int -> Version -> Version
incVersion n = alterVersion (incVersion' n)
  where
    incVersion' 0 []     = [1]
    incVersion' 0 (v:_)  = [v+1]
    incVersion' m []     = replicate m 0 ++ [1]
    incVersion' m (v:vs) = v : incVersion' (m-1) vs

-- | Returns true if this package is eligible for test suite initialization.
eligibleForTestSuite :: InitFlags -> Bool
eligibleForTestSuite flags =
  Flag True == initializeTestSuite flags
  && Flag Executable /= packageType flags

---------------------------------------------------------------------------
--  Prompting/user interaction  -------------------------------------------
---------------------------------------------------------------------------

-- | Run a prompt or not based on the interactive flag of the
--   InitFlags structure.
maybePrompt :: InitFlags -> IO t -> IO (Maybe t)
maybePrompt flags p =
  case interactive flags of
    Flag False -> return Nothing
    _          -> Just `fmap` p

-- | Create a prompt with optional default value that returns a
--   String.
promptStr :: String -> Maybe String -> IO String
promptStr = promptDefault' Just id

-- | Create a yes/no prompt with optional default value.
--
promptYesNo :: String -> Maybe Bool -> IO Bool
promptYesNo =
    promptDefault' recogniseYesNo showYesNo
  where
    recogniseYesNo s | s == "y" || s == "Y" = Just True
                     | s == "n" || s == "N" = Just False
                     | otherwise            = Nothing
    showYesNo True  = "y"
    showYesNo False = "n"

-- | Create a prompt with optional default value that returns a value
--   of some Text instance.
prompt :: Text t => String -> Maybe t -> IO t
prompt = promptDefault'
           (either (const Nothing) Just . runReadE (readP_to_E id parse))
           display

-- | Create a prompt with an optional default value.
promptDefault' :: (String -> Maybe t)       -- ^ parser
               -> (t -> String)             -- ^ pretty-printer
               -> String                    -- ^ prompt message
               -> Maybe t                   -- ^ optional default value
               -> IO t
promptDefault' parser pretty pr def = do
  putStr $ mkDefPrompt pr (pretty `fmap` def)
  inp <- getLine
  case (inp, def) of
    ("", Just d)  -> return d
    _  -> case parser inp of
            Just t  -> return t
            Nothing -> do putStrLn $ "Couldn't parse " ++ inp ++ ", please try again!"
                          promptDefault' parser pretty pr def

-- | Create a prompt from a prompt string and a String representation
--   of an optional default value.
mkDefPrompt :: String -> Maybe String -> String
mkDefPrompt pr def = pr ++ "?" ++ defStr def
  where defStr Nothing  = " "
        defStr (Just s) = " [default: " ++ s ++ "] "

promptListOptional :: (Text t, Eq t)
                   => String            -- ^ prompt
                   -> [t]               -- ^ choices
                   -> IO (Maybe (Either String t))
promptListOptional pr choices = promptListOptional' pr choices display

promptListOptional' :: Eq t
                   => String            -- ^ prompt
                   -> [t]               -- ^ choices
                   -> (t -> String)     -- ^ show an item
                   -> IO (Maybe (Either String t))
promptListOptional' pr choices displayItem =
    fmap rearrange
  $ promptList pr (Nothing : map Just choices) (Just Nothing)
               (maybe "(none)" displayItem) True
  where
    rearrange = either (Just . Left) (fmap Right)

-- | Create a prompt from a list of items.
promptList :: Eq t
           => String            -- ^ prompt
           -> [t]               -- ^ choices
           -> Maybe t           -- ^ optional default value
           -> (t -> String)     -- ^ show an item
           -> Bool              -- ^ whether to allow an 'other' option
           -> IO (Either String t)
promptList pr choices def displayItem other = do
  putStrLn $ pr ++ ":"
  let options1 = map (\c -> (Just c == def, displayItem c)) choices
      options2 = zip ([1..]::[Int])
                     (options1 ++ [(False, "Other (specify)") | other])
  mapM_ (putStrLn . \(n,(i,s)) -> showOption n i ++ s) options2
  promptList' displayItem (length options2) choices def other
 where showOption n i | n < 10 = " " ++ star i ++ " " ++ rest
                      | otherwise = " " ++ star i ++ rest
                  where rest = show n ++ ") "
                        star True = "*"
                        star False = " "

promptList' :: (t -> String) -> Int -> [t] -> Maybe t -> Bool -> IO (Either String t)
promptList' displayItem numChoices choices def other = do
  putStr $ mkDefPrompt "Your choice" (displayItem `fmap` def)
  inp <- getLine
  case (inp, def) of
    ("", Just d) -> return $ Right d
    _  -> case readMaybe inp of
            Nothing -> invalidChoice inp
            Just n  -> getChoice n
 where invalidChoice inp = do putStrLn $ inp ++ " is not a valid choice."
                              promptList' displayItem numChoices choices def other
       getChoice n | n < 1 || n > numChoices = invalidChoice (show n)
                   | n < numChoices ||
                     (n == numChoices && not other)
                                  = return . Right $ choices !! (n-1)
                   | otherwise    = Left `fmap` promptStr "Please specify" Nothing

---------------------------------------------------------------------------
--  File generation  ------------------------------------------------------
---------------------------------------------------------------------------

writeLicense :: InitFlags -> IO ()
writeLicense flags = do
  message flags "\nGenerating LICENSE..."
  year <- show <$> getYear
  let authors = fromMaybe "???" . flagToMaybe . author $ flags
  let licenseFile =
        case license flags of
          Flag BSD2
            -> Just $ bsd2 authors year

          Flag BSD3
            -> Just $ bsd3 authors year

          Flag (GPL (Just v)) | v == mkVersion [2]
            -> Just gplv2

          Flag (GPL (Just v)) | v == mkVersion [3]
            -> Just gplv3

          Flag (LGPL (Just v)) | v == mkVersion [2,1]
            -> Just lgpl21

          Flag (LGPL (Just v)) | v == mkVersion [3]
            -> Just lgpl3

          Flag (AGPL (Just v)) | v == mkVersion [3]
            -> Just agplv3

          Flag (Apache (Just v)) | v == mkVersion [2,0]
            -> Just apache20

          Flag MIT
            -> Just $ mit authors year

          Flag (MPL v) | v == mkVersion [2,0]
            -> Just mpl20

          Flag ISC
            -> Just $ isc authors year

          _ -> Nothing

  case licenseFile of
    Just licenseText -> writeFileSafe flags "LICENSE" licenseText
    Nothing -> message flags "Warning: unknown license type, you must put a copy in LICENSE yourself."

getYear :: IO Integer
getYear = do
  u <- getCurrentTime
  z <- getCurrentTimeZone
  let l = utcToLocalTime z u
      (y, _, _) = toGregorian $ localDay l
  return y

writeSetupFile :: InitFlags -> IO ()
writeSetupFile flags = do
  message flags "Generating Setup.hs..."
  writeFileSafe flags "Setup.hs" setupFile
 where
  setupFile = unlines
    [ "import Distribution.Simple"
    , "main = defaultMain"
    ]

writeChangeLog :: InitFlags -> IO ()
writeChangeLog flags = when ((defaultChangeLog `elem`) $ fromMaybe [] (extraSrc flags)) $ do
  message flags ("Generating "++ defaultChangeLog ++"...")
  writeFileSafe flags defaultChangeLog changeLog
 where
  changeLog = unlines
    [ "# Revision history for " ++ pname
    , ""
    , "## " ++ pver ++ " -- YYYY-mm-dd"
    , ""
    , "* First version. Released on an unsuspecting world."
    ]
  pname = maybe "" display $ flagToMaybe $ packageName flags
  pver = maybe "" display $ flagToMaybe $ version flags



writeCabalFile :: InitFlags -> IO Bool
writeCabalFile flags@(InitFlags{packageName = NoFlag}) = do
  message flags "Error: no package name provided."
  return False
writeCabalFile flags@(InitFlags{packageName = Flag p}) = do
  let cabalFileName = display p ++ ".cabal"
  message flags $ "Generating " ++ cabalFileName ++ "..."
  writeFileSafe flags cabalFileName (generateCabalFile cabalFileName flags)
  return True

-- | Write a file \"safely\", backing up any existing version (unless
--   the overwrite flag is set).
writeFileSafe :: InitFlags -> FilePath -> String -> IO ()
writeFileSafe flags fileName content = do
  moveExistingFile flags fileName
  writeFile fileName content

-- | Create directories, if they were given, and don't already exist.
createDirectories :: Maybe [String] -> IO ()
createDirectories mdirs = case mdirs of
  Just dirs -> forM_ dirs (createDirectoryIfMissing True)
  Nothing   -> return ()

-- | Create MyLib.hs file, if its the only module in the liste.
createLibHs :: InitFlags -> IO ()
createLibHs flags = when ((exposedModules flags) == Just [myLibModule]) $ do
  let modFilePath = ModuleName.toFilePath myLibModule ++ ".hs"
  case sourceDirs flags of
    Just (srcPath:_) -> writeLibHs flags (srcPath </> modFilePath)
    _                -> writeLibHs flags modFilePath

-- | Write a MyLib.hs file if it doesn't already exist.
writeLibHs :: InitFlags -> FilePath -> IO ()
writeLibHs flags libPath = do
  dir <- maybe getCurrentDirectory return (flagToMaybe $ packageDir flags)
  let libFullPath = dir </> libPath
  exists <- doesFileExist libFullPath
  unless exists $ do
    message flags $ "Generating " ++ libPath ++ "..."
    writeFileSafe flags libFullPath myLibHs

myLibModule :: ModuleName
myLibModule = ModuleName.fromString "MyLib"

-- | Default MyLib.hs file.  Used when no Lib.hs exists.
myLibHs :: String
myLibHs = unlines
  [ "module MyLib (someFunc) where"
  , ""
  , "someFunc :: IO ()"
  , "someFunc = putStrLn \"someFunc\""
  ]

-- | Create Main.hs, but only if we are init'ing an executable and
--   the mainIs flag has been provided.
createMainHs :: InitFlags -> IO ()
createMainHs flags =
  if hasMainHs flags then
    case applicationDirs flags of
      Just (appPath:_) -> writeMainHs flags (appPath </> mainFile)
      _ -> writeMainHs flags mainFile
  else return ()
  where
    Flag mainFile = mainIs flags

--- | Write a main file if it doesn't already exist.
writeMainHs :: InitFlags -> FilePath -> IO ()
writeMainHs flags mainPath = do
  dir <- maybe getCurrentDirectory return (flagToMaybe $ packageDir flags)
  let mainFullPath = dir </> mainPath
  exists <- doesFileExist mainFullPath
  unless exists $ do
      message flags $ "Generating " ++ mainPath ++ "..."
      writeFileSafe flags mainFullPath (mainHs flags)

-- | Check that a main file exists.
hasMainHs :: InitFlags -> Bool
hasMainHs flags = case mainIs flags of
  Flag _ -> (packageType flags == Flag Executable
             || packageType flags == Flag LibraryAndExecutable)
  _ -> False

-- | Default Main.(l)hs file.  Used when no Main.(l)hs exists.
--
--   If we are initializing a new 'LibraryAndExecutable' then import 'MyLib'.
mainHs :: InitFlags -> String
mainHs flags = (unlines . map prependPrefix) $ case packageType flags of
  Flag LibraryAndExecutable ->
    [ "module Main where"
    , ""
    , "import qualified MyLib (someFunc)"
    , ""
    , "main :: IO ()"
    , "main = do"
    , "  putStrLn \"Hello, Haskell!\""
    , "  MyLib.someFunc"
    ]
  _ ->
    [ "module Main where"
    , ""
    , "main :: IO ()"
    , "main = putStrLn \"Hello, Haskell!\""
    ]
  where
    prependPrefix "" = ""
    prependPrefix line
      | isLiterate = "> " ++ line
      | otherwise  = line
    isLiterate = case mainIs flags of
      Flag mainPath -> takeExtension mainPath == ".lhs"
      _             -> False

testFile :: String
testFile = "MyLibTest.hs"

-- | Create MyLibTest.hs, but only if we are init'ing a library and
--   the initializeTestSuite flag has been set.
createTestHs :: InitFlags -> IO ()
createTestHs flags =
  when (eligibleForTestSuite flags) $
    case testDirs flags of
      Just (testPath:_) -> writeTestHs flags (testPath </> testFile)
      _ -> writeMainHs flags testFile

--- | Write a test file.
writeTestHs :: InitFlags -> FilePath -> IO ()
writeTestHs flags testPath = do
  dir <- maybe getCurrentDirectory return (flagToMaybe $ packageDir flags)
  let testFullPath = dir </> testPath
  exists <- doesFileExist testFullPath
  unless exists $ do
      message flags $ "Generating " ++ testPath ++ "..."
      writeFileSafe flags testFullPath testHs

-- | Default MyLibTest.hs file.
testHs :: String
testHs = unlines
  [ "module Main (main) where"
  , ""
  , "main :: IO ()"
  , "main = putStrLn \"Test suite not yet implemented.\""
  ]


-- | Move an existing file, if there is one, and the overwrite flag is
--   not set.
moveExistingFile :: InitFlags -> FilePath -> IO ()
moveExistingFile flags fileName =
  unless (overwrite flags == Flag True) $ do
    e <- doesFileExist fileName
    when e $ do
      newName <- findNewName fileName
      message flags $ "Warning: " ++ fileName ++ " already exists, backing up old version in " ++ newName
      copyFile fileName newName

findNewName :: FilePath -> IO FilePath
findNewName oldName = findNewName' 0
  where
    findNewName' :: Integer -> IO FilePath
    findNewName' n = do
      let newName = oldName <.> ("save" ++ show n)
      e <- doesFileExist newName
      if e then findNewName' (n+1) else return newName

-- | Generate a .cabal file from an InitFlags structure.  NOTE: this
--   is rather ad-hoc!  What we would REALLY like is to have a
--   standard low-level AST type representing .cabal files, which
--   preserves things like comments, and to write an *inverse*
--   parser/pretty-printer pair between .cabal files and this AST.
--   Then instead of this ad-hoc code we could just map an InitFlags
--   structure onto a low-level AST structure and use the existing
--   pretty-printing code to generate the file.
generateCabalFile :: String -> InitFlags -> String
generateCabalFile fileName c = trimTrailingWS $
  (++ "\n") .
  renderStyle style { lineLength = 79, ribbonsPerLine = 1.1 } $
  -- Starting with 2.2 the `cabal-version` field needs to be the first line of the PD
  (if specVer < mkVersion [1,12]
   then field "cabal-version" (Flag $ orLaterVersion specVer) -- legacy
   else field "cabal-version" (Flag $ specVer))
              Nothing -- NB: the first line must be the 'cabal-version' declaration
              False
  $$
  (if minimal c /= Flag True
    then showComment (Just $ "Initial package description '" ++ fileName ++ "' generated "
                          ++ "by 'cabal init'.  For further documentation, see "
                          ++ "http://haskell.org/cabal/users-guide/")
         $$ text ""
    else empty)
  $$
  vcat [ field  "name"          (packageName   c)
                (Just "The name of the package.")
                True

       , field  "version"       (version       c)
                (Just $ "The package version.  See the Haskell package versioning policy (PVP) for standards guiding when and how versions should be incremented.\nhttps://pvp.haskell.org\n"
                ++ "PVP summary:      +-+------- breaking API changes\n"
                ++ "                  | | +----- non-breaking API additions\n"
                ++ "                  | | | +--- code changes with no API change")
                True

       , fieldS "synopsis"      (synopsis      c)
                (Just "A short (one-line) description of the package.")
                True

       , fieldS "description"   NoFlag
                (Just "A longer description of the package.")
                True

       , fieldS "homepage"      (homepage     c)
                (Just "URL for the project homepage or repository.")
                False

       , fieldS "bug-reports"   NoFlag
                (Just "A URL where users can report bugs.")
                True

       , fieldS  "license"      licenseStr
                (Just "The license under which the package is released.")
                True

       , case (license c) of
           Flag PublicDomain -> empty
           _ -> fieldS "license-file" (Flag "LICENSE")
                       (Just "The file containing the license text.")
                       True

       , fieldS "author"        (author       c)
                (Just "The package author(s).")
                True

       , fieldS "maintainer"    (email        c)
                (Just "An email address to which users can send suggestions, bug reports, and patches.")
                True

       , case (license c) of
           Flag PublicDomain -> empty
           _ -> fieldS "copyright"     NoFlag
                       (Just "A copyright notice.")
                       True

       , fieldS "category"      (either id display `fmap` category c)
                Nothing
                True

       , fieldS "build-type"    (if specVer >= mkVersion [2,2] then NoFlag else Flag "Simple")
                Nothing
                False

       , fieldS "extra-source-files" (listFieldS (extraSrc c))
                (Just "Extra files to be distributed with the package, such as examples or a README.")
                True

       , case packageType c of
           Flag Executable -> executableStanza
           Flag Library    -> libraryStanza
           Flag LibraryAndExecutable -> libraryStanza $+$ executableStanza
           _               -> empty

       , if eligibleForTestSuite c then testSuiteStanza else empty
       ]
 where
   specVer = fromMaybe defaultCabalVersion $ flagToMaybe (cabalVersion c)

   licenseStr | specVer < mkVersion [2,2] = prettyShow `fmap` license c
              | otherwise                 = go `fmap` license c
     where
       go (UnknownLicense s) = s
       go l                  = prettyShow (licenseToSPDX l)

   generateBuildInfo :: BuildType -> InitFlags -> Doc
   generateBuildInfo buildType c' = vcat
     [ fieldS "other-modules" (listField otherMods)
              (Just $ case buildType of
                 LibBuild    -> "Modules included in this library but not exported."
                 ExecBuild -> "Modules included in this executable, other than Main.")
              True

     , fieldS "other-extensions" (listField (otherExts c'))
              (Just "LANGUAGE extensions used by modules in this package.")
              True

     , fieldS "build-depends" ((++ myLibDep) <$> listField (dependencies c'))
              (Just "Other library packages from which modules are imported.")
              True

     , fieldS "hs-source-dirs" (listFieldS (case buildType of
                                            LibBuild  -> sourceDirs c'
                                            ExecBuild -> applicationDirs c'))
              (Just "Directories containing source files.")
              True

     , fieldS "build-tools" (listFieldS (buildTools c'))
              (Just "Extra tools (e.g. alex, hsc2hs, ...) needed to build the source.")
              False

     , field  "default-language" (language c')
              (Just "Base language which the package is written in.")
              True
     ]
     -- Hack: Can't construct a 'Dependency' which is just 'packageName'(?).
     where
       myLibDep = if exposedModules c' == Just [myLibModule] && buildType == ExecBuild
                      then case packageName c' of
                             Flag pkgName -> ", " ++ P.unPackageName pkgName
                             _ -> ""
                      else ""

       -- Only include 'MyLib' in 'other-modules' of the executable.
       otherModsFromFlag = otherModules c'
       otherMods = if buildType == LibBuild && otherModsFromFlag == Just [myLibModule]
                   then Nothing
                   else otherModsFromFlag

   listField :: Text s => Maybe [s] -> Flag String
   listField = listFieldS . fmap (map display)

   listFieldS :: Maybe [String] -> Flag String
   listFieldS = Flag . maybe "" (intercalate ", ")

   field :: Text t => String -> Flag t -> Maybe String -> Bool -> Doc
   field s f = fieldS s (fmap display f)

   fieldS :: String        -- ^ Name of the field
          -> Flag String   -- ^ Field contents
          -> Maybe String  -- ^ Comment to explain the field
          -> Bool          -- ^ Should the field be included (commented out) even if blank?
          -> Doc
   fieldS _ NoFlag _    inc | not inc || (minimal c == Flag True) = empty
   fieldS _ (Flag "") _ inc | not inc || (minimal c == Flag True) = empty
   fieldS s f com _ = case (isJust com, noComments c, minimal c) of
                        (_, _, Flag True) -> id
                        (_, Flag True, _) -> id
                        (True, _, _)      -> (showComment com $$) . ($$ text "")
                        (False, _, _)     -> ($$ text "")
                      $
                      comment f <<>> text s <<>> colon
                                <<>> text (replicate (20 - length s) ' ')
                                <<>> text (fromMaybe "" . flagToMaybe $ f)
   comment NoFlag    = text "-- "
   comment (Flag "") = text "-- "
   comment _         = text ""

   showComment :: Maybe String -> Doc
   showComment (Just t) = vcat
                        . map (text . ("-- "++)) . lines
                        . renderStyle style {
                            lineLength = 76,
                            ribbonsPerLine = 1.05
                          }
                        . vcat
                        . map (fcat . map text . breakLine)
                        . lines
                        $ t
   showComment Nothing  = text ""

   breakLine  [] = []
   breakLine  cs = case break (==' ') cs of (w,cs') -> w : breakLine' cs'
   breakLine' [] = []
   breakLine' cs = case span (==' ') cs of (w,cs') -> w : breakLine cs'

   trimTrailingWS :: String -> String
   trimTrailingWS = unlines . map (dropWhileEndLE isSpace) . lines

   executableStanza :: Doc
   executableStanza = text "\nexecutable" <+>
             text (maybe "" display . flagToMaybe $ packageName c) $$
             nest 2 (vcat
             [ fieldS "main-is" (mainIs c) (Just ".hs or .lhs file containing the Main module.") True

             , generateBuildInfo ExecBuild c
             ])

   libraryStanza :: Doc
   libraryStanza = text "\nlibrary" $$ nest 2 (vcat
             [ fieldS "exposed-modules" (listField (exposedModules c))
                      (Just "Modules exported by the library.")
                      True

             , generateBuildInfo LibBuild c
             ])

   testSuiteStanza :: Doc
   testSuiteStanza = text "\ntest-suite" <+>
     text (maybe "" ((++"-test") . display) . flagToMaybe $ packageName c) $$
     nest 2 (vcat
             [ field  "default-language" (language c)
               (Just "Base language which the package is written in.")
               True

             , fieldS "type" (Flag "exitcode-stdio-1.0")
               (Just "The interface type and version of the test suite.")
               True

             , fieldS "hs-source-dirs" (listFieldS (testDirs c))
               (Just "The directory where the test specifications are found.")
               True

             , fieldS "main-is" (Flag testFile)
               (Just "The entrypoint to the test suite.")
               True

             , fieldS "build-depends" (listField (dependencies c))
               (Just "Test dependencies.")
               True
             ])

-- | Generate warnings for missing fields etc.
generateWarnings :: InitFlags -> IO ()
generateWarnings flags = do
  message flags ""
  when (synopsis flags `elem` [NoFlag, Flag ""])
       (message flags "Warning: no synopsis given. You should edit the .cabal file and add one.")

  message flags "You may want to edit the .cabal file and add a Description field."

-- | Possibly generate a message to stdout, taking into account the
--   --quiet flag.
message :: InitFlags -> String -> IO ()
message (InitFlags{quiet = Flag True}) _ = return ()
message _ s = putStrLn s
