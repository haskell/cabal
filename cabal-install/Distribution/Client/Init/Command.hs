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
--
-----------------------------------------------------------------------------

module Distribution.Client.Init.Command (

    -- * Commands
    initCabal
  , incVersion

  ) where

import Prelude ()
import Distribution.Client.Compat.Prelude hiding (empty)

import System.IO
  ( hSetBuffering, stdout, BufferMode(..) )
import System.Directory
  ( getCurrentDirectory, doesDirectoryExist, getDirectoryContents )
import System.FilePath
  ( (</>), takeBaseName, equalFilePath )

import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Control.Monad
  ( (>=>) )
import Control.Arrow
  ( (&&&), (***) )

import Distribution.CabalSpecVersion
  ( CabalSpecVersion (..), showCabalSpecVersion )
import Distribution.Version
  ( Version, mkVersion, alterVersion, majorBoundVersion
  , orLaterVersion, earlierVersion, intersectVersionRanges, VersionRange )
import Distribution.ModuleName
  ( ModuleName )  -- And for the Text instance
import Distribution.InstalledPackageInfo
  ( InstalledPackageInfo, exposed )
import qualified Distribution.Package as P
import qualified Distribution.SPDX as SPDX
import Language.Haskell.Extension ( Language(..) )

import Distribution.Client.Init.Defaults
  ( defaultApplicationDir, defaultCabalVersion, myLibModule, defaultSourceDir )
import Distribution.Client.Init.FileCreators
  ( writeLicense, writeChangeLog, createDirectories, createLibHs, createMainHs
  , createTestSuiteIfEligible, writeCabalFile )
import Distribution.Client.Init.Prompt
  ( prompt, promptYesNo, promptStr, promptList, maybePrompt
  , promptListOptional )
import Distribution.Client.Init.Utils
  ( eligibleForTestSuite,  message )
import Distribution.Client.Init.Types
  ( InitFlags(..), PackageType(..), Category(..)
  , displayPackageType )
import Distribution.Client.Init.Heuristics
  ( guessPackageName, guessAuthorNameMail, guessMainFileCandidates,
    SourceFileEntry(..),
    scanForModules, neededBuildPrograms )

import Distribution.Simple.Flag
  ( maybeToFlag )
import Distribution.Simple.Setup
  ( Flag(..), flagToMaybe )
import Distribution.Simple.Configure
  ( getInstalledPackages )
import Distribution.Simple.Compiler
  ( PackageDBStack, Compiler )
import Distribution.Simple.Program
  ( ProgramDb )
import Distribution.Simple.PackageIndex
  ( InstalledPackageIndex, moduleNameIndex )

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
    Flag SPDX.NONE -> return ()
    _              -> writeLicense initFlags'
  writeChangeLog initFlags'
  createDirectories (sourceDirs initFlags')
  createLibHs initFlags'
  createDirectories (applicationDirs initFlags')
  createMainHs initFlags'
  createTestSuiteIfEligible initFlags'
  success <- writeCabalFile initFlags'

  when success $ generateWarnings initFlags'

---------------------------------------------------------------------------
--  Flag acquisition  -----------------------------------------------------
---------------------------------------------------------------------------

-- | Fill in more details in InitFlags by guessing, discovering, or prompting
-- the user.
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
            , cabalVersion = Flag defaultCabalVersion
            }
    simpleProjFlag@_ ->
      flags { simpleProject = simpleProjFlag }


-- | Get the version of the cabal spec to use.
--
-- The spec version can be specified by the InitFlags cabalVersion field. If
-- none is specified then the user is prompted to pick from a list of
-- supported versions (see code below).
getCabalVersion :: InitFlags -> IO InitFlags
getCabalVersion flags = do
  cabVer <-     return (flagToMaybe $ cabalVersion flags)
            ?>> maybePrompt flags (either (const defaultCabalVersion) id `fmap`
                                  promptList "Please choose version of the Cabal specification to use"
                                  [CabalSpecV1_10, CabalSpecV2_0, CabalSpecV2_2, CabalSpecV2_4, CabalSpecV3_0]
                                  (Just defaultCabalVersion) displayCabalVersion False)
            ?>> return (Just defaultCabalVersion)

  return $  flags { cabalVersion = maybeToFlag cabVer }

  where
    displayCabalVersion :: CabalSpecVersion -> String
    displayCabalVersion v = case v of
      CabalSpecV1_10 -> "1.10   (legacy)"
      CabalSpecV2_0  -> "2.0    (+ support for Backpack, internal sub-libs, '^>=' operator)"
      CabalSpecV2_2  -> "2.2    (+ support for 'common', 'elif', redundant commas, SPDX)"
      CabalSpecV2_4  -> "2.4    (+ support for '**' globbing)"
      CabalSpecV3_0  -> "3.0    (+ set notation for ==, common stanzas in ifs, more redundant commas, better pkgconfig-depends)"
      _              -> showCabalSpecVersion v



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

-- | Choose a license for the package.
--
-- The license can come from Initflags (license field), if it is not present
-- then prompt the user from a predefined list of licenses.
getLicense :: InitFlags -> IO InitFlags
getLicense flags = do
  elic <- return (fmap Right $ flagToMaybe $ license flags)
      ?>> maybePrompt flags (promptList "Please choose a license" listedLicenses (Just SPDX.NONE) prettyShow True)

  case elic of
      Nothing          -> return flags { license = NoFlag }
      Just (Right lic) -> return flags { license = Flag lic }
      Just (Left str)  -> case eitherParsec str of
          Right lic -> return flags { license = Flag lic }
          -- on error, loop
          Left err -> do
              putStrLn "The license must be a valid SPDX expression."
              putStrLn err
              getLicense flags
  where
    -- perfectly we'll have this and writeLicense (in FileCreators)
    -- in a single file
    listedLicenses =
      SPDX.NONE :
      map (\lid -> SPDX.License (SPDX.ELicense (SPDX.ELicenseId lid) Nothing))
      [ SPDX.BSD_2_Clause
      , SPDX.BSD_3_Clause
      , SPDX.Apache_2_0
      , SPDX.MIT
      , SPDX.MPL_2_0
      , SPDX.ISC

      , SPDX.GPL_2_0_only
      , SPDX.GPL_3_0_only
      , SPDX.LGPL_2_1_only
      , SPDX.LGPL_3_0_only
      , SPDX.AGPL_3_0_only

      , SPDX.GPL_2_0_or_later
      , SPDX.GPL_3_0_or_later
      , SPDX.LGPL_2_1_or_later
      , SPDX.LGPL_3_0_or_later
      , SPDX.AGPL_3_0_or_later
      ]

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

-- | Prompt for a homepage URL for the package.
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

-- | Ask for the test suite root directory.
getTestDir :: InitFlags -> IO InitFlags
getTestDir flags = do
  dirs <- return (testDirs flags)
              -- Only need testDirs when test suite generation is enabled.
          ?>> if not (eligibleForTestSuite flags) then return (Just []) else return Nothing
          ?>> fmap (fmap ((:[]) . either id id)) (maybePrompt
                   flags
                   (promptList "Test directory" ["test"] (Just "test") id True))

  return $ flags { testDirs = dirs }

-- | Ask for the Haskell base language of the package.
getLanguage :: InitFlags -> IO InitFlags
getLanguage flags = do
  lang <-     return (flagToMaybe $ language flags)
          ?>> maybePrompt flags
                (either UnknownLanguage id `fmap`
                  promptList "What base language is the package written in"
                  [Haskell2010, Haskell98]
                  (Just Haskell2010) prettyShow True)
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
  appDirs <-
    return (applicationDirs flags)
    ?>> noAppDirIfLibraryOnly
    ?>> guessAppDir flags
    ?>> promptUserForApplicationDir
    ?>> setDefault
  return $ flags { applicationDirs = appDirs }

  where
    -- If the packageType==Library, then there is no application dir.
    noAppDirIfLibraryOnly :: IO (Maybe [String])
    noAppDirIfLibraryOnly =
      if (packageType flags) == Flag Library
      then return (Just [])
      else return Nothing

    -- Set the default application directory.
    setDefault :: IO (Maybe [String])
    setDefault = pure (Just [defaultApplicationDir])

    -- Prompt the user for the application directory (defaulting to "app").
    -- Returns 'Nothing' if in non-interactive mode, otherwise will always
    -- return a 'Just' value ('Just []' if no separate application directory).
    promptUserForApplicationDir :: IO (Maybe [String])
    promptUserForApplicationDir = fmap (either (:[]) id) <$> maybePrompt
      flags
      (promptList
       ("Application " ++ mainFile ++ "directory")
       [[defaultApplicationDir], ["src-exe"], []]
        (Just [defaultApplicationDir])
       showOption True)

    showOption :: [String] -> String
    showOption [] = "(none)"
    showOption (x:_) = x

    -- The name
    mainFile :: String
    mainFile = case mainIs flags of
      Flag mainPath -> "(" ++ mainPath ++ ") "
      _             -> ""

-- | Try to guess app directory. Could try harder; for the
--   moment just looks to see whether there is a directory called 'app'.
guessAppDir :: InitFlags -> IO (Maybe [String])
guessAppDir flags = do
  dir      <- maybe getCurrentDirectory return . flagToMaybe $ packageDir flags
  appIsDir <- doesDirectoryExist (dir </> "app")
  return $ if appIsDir
             then Just ["app"]
             else Nothing

-- | Ask for the source (library) root directory.
getSrcDir :: InitFlags -> IO InitFlags
getSrcDir flags = do
  srcDirs <-
    return (sourceDirs flags)
    ?>> noSourceDirIfExecutableOnly
    ?>> guessSourceDir flags
    ?>> promptUserForSourceDir
    ?>> setDefault

  return $ flags { sourceDirs = srcDirs }

  where
    -- If the packageType==Executable, then there is no source dir.
    noSourceDirIfExecutableOnly :: IO (Maybe [String])
    noSourceDirIfExecutableOnly =
      if (packageType flags) == Flag Executable
      then return (Just [])
      else return Nothing

    -- Set the default source directory.
    setDefault :: IO (Maybe [String])
    setDefault = pure (Just [defaultSourceDir])

    -- Prompt the user for the source directory (defaulting to "app").
    -- Returns 'Nothing' if in non-interactive mode, otherwise will always
    -- return a 'Just' value ('Just []' if no separate application directory).
    promptUserForSourceDir :: IO (Maybe [String])
    promptUserForSourceDir = fmap (either (:[]) id) <$> maybePrompt
      flags
      (promptList
       ("Library source directory")
       [[defaultSourceDir], ["lib"], ["src-lib"], []]
        (Just [defaultSourceDir])
       showOption True)

    showOption :: [String] -> String
    showOption [] = "(none)"
    showOption (x:_) = x


-- | Try to guess source directory. Could try harder; for the
--   moment just looks to see whether there is a directory called 'src'.
guessSourceDir :: InitFlags -> IO (Maybe [String])
guessSourceDir flags = do
  dir      <-
    maybe getCurrentDirectory return . flagToMaybe $ packageDir flags
  srcIsDir <- doesDirectoryExist (dir </> "src")
  return $ if srcIsDir
             then Just ["src"]
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

-- | Given a list of imported modules, retrieve the list of dependencies that
-- provide those modules.
importsToDeps :: InitFlags -> [ModuleName] -> InstalledPackageIndex -> IO [P.Dependency]
importsToDeps flags mods pkgIx = do

  let modMap :: M.Map ModuleName [InstalledPackageInfo]
      modMap  = M.map (filter exposed) $ moduleNameIndex pkgIx

      modDeps :: [(ModuleName, Maybe [InstalledPackageInfo])]
      modDeps = map (id &&& flip M.lookup modMap) mods

  message flags "\nGuessing dependencies..."
  nub . catMaybes <$> traverse (chooseDep flags) modDeps

-- Given a module and a list of installed packages providing it,
-- choose a dependency (i.e. package + version range) to use for that
-- module.
chooseDep :: InitFlags -> (ModuleName, Maybe [InstalledPackageInfo])
          -> IO (Maybe P.Dependency)

chooseDep flags (m, Nothing)
  = message flags ("\nWarning: no package found providing " ++ prettyShow m ++ ".")
    >> return Nothing

chooseDep flags (m, Just [])
  = message flags ("\nWarning: no package found providing " ++ prettyShow m ++ ".")
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
                                 ++ prettyShow m
                                 ++ ": " ++ intercalate ", " (fmap (prettyShow . P.pkgName . NE.head) grps))
                  message flags "You will need to pick one and manually add it to the Build-depends: field."
                  return Nothing
  where
    pkgGroups = NE.groupBy ((==) `on` P.pkgName) (map P.packageId ps)

    desugar = maybe True (< CabalSpecV2_0) $ flagToMaybe (cabalVersion flags)

    -- Given a list of available versions of the same package, pick a dependency.
    toDep :: NonEmpty P.PackageIdentifier -> IO P.Dependency

    -- If only one version, easy.  We change e.g. 0.4.2  into  0.4.*
    toDep (pid:|[]) = return $ P.Dependency (P.pkgName pid) (pvpize desugar . P.pkgVersion $ pid) P.mainLibSet --TODO sublibraries

    -- Otherwise, choose the latest version and issue a warning.
    toDep pids  = do
      message flags ("\nWarning: multiple versions of " ++ prettyShow (P.pkgName . NE.head $ pids) ++ " provide " ++ prettyShow m ++ ", choosing the latest.")
      return $ P.Dependency (P.pkgName . NE.head $ pids)
                            (pvpize desugar . maximum . fmap P.pkgVersion $ pids)
                            P.mainLibSet --TODO take into account sublibraries

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

-- | Generate warnings for missing fields etc.
generateWarnings :: InitFlags -> IO ()
generateWarnings flags = do
  message flags ""
  when (synopsis flags `elem` [NoFlag, Flag ""])
       (message flags "Warning: no synopsis given. You should edit the .cabal file and add one.")

  message flags "You may want to edit the .cabal file and add a Description field."
