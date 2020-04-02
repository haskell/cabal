-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Init.FileCreators
-- Copyright   :  (c) Brent Yorgey 2009
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Functions to create files during 'cabal init'.
--
-----------------------------------------------------------------------------

module Distribution.Client.Init.FileCreators (

    -- * Commands
    writeLicense
  , writeChangeLog
  , createDirectories
  , createLibHs
  , createMainHs
  , createTestSuiteIfEligible
  , writeCabalFile
  ) where

import Prelude ()
import Distribution.Client.Compat.Prelude hiding (empty)

import System.FilePath
  ( (</>), (<.>), takeExtension )

import Control.Monad
  ( forM_ )
import Data.Time
  ( getCurrentTime, utcToLocalTime, toGregorian, localDay, getCurrentTimeZone )
import System.Directory
  ( getCurrentDirectory, doesFileExist, copyFile
  , createDirectoryIfMissing )

import Text.PrettyPrint hiding (mode, cat)

import Distribution.Client.Init.Defaults
  ( defaultCabalVersion, myLibModule )
import Distribution.Client.Init.Licenses
  ( bsd2, bsd3, gplv2, gplv3, lgpl21, lgpl3, agplv3, apache20, mit, mpl20, isc )
import Distribution.Client.Init.Utils
  ( eligibleForTestSuite, message )
import Distribution.Client.Init.Types
  ( InitFlags(..), BuildType(..), PackageType(..) )

import Distribution.Deprecated.Text
  ( display, Text(..) )
import Distribution.License
  ( License(..), licenseToSPDX )
import qualified Distribution.ModuleName as ModuleName
  ( toFilePath )
import qualified Distribution.Package as P
  ( unPackageName )
import Distribution.Simple.Setup
  ( Flag(..), flagToMaybe )
import Distribution.Simple.Utils
  ( dropWhileEndLE )
import Distribution.Pretty
  ( prettyShow )
import Distribution.Version
  ( mkVersion, orLaterVersion )


---------------------------------------------------------------------------
--  File generation  ------------------------------------------------------
---------------------------------------------------------------------------

-- | Write the LICENSE file, as specified in the InitFlags license field.
--
-- For licences that contain the author's name(s), the values are taken
-- from the 'authors' field of 'InitFlags', and if not specified will
-- be the string "???".
--
-- If the license type is unknown no license file will be created and
-- a warning will be raised.
writeLicense :: InitFlags -> IO ()
writeLicense flags = do
  message flags "\nGenerating LICENSE..."
  year <- show <$> getCurrentYear
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

-- | Returns the current calendar year.
getCurrentYear :: IO Integer
getCurrentYear = do
  u <- getCurrentTime
  z <- getCurrentTimeZone
  let l = utcToLocalTime z u
      (y, _, _) = toGregorian $ localDay l
  return y

defaultChangeLog :: FilePath
defaultChangeLog = "CHANGELOG.md"

-- | Writes the changelog to the current directory.
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

-- | Creates and writes the initialized .cabal file.
--
-- Returns @False@ if no package name is specified, @True@ otherwise.
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
    mainFile = case mainIs flags of
      Flag x -> x
      NoFlag -> error "createMainHs: no mainIs"

-- | Write a main file if it doesn't already exist.
writeMainHs :: InitFlags -> FilePath -> IO ()
writeMainHs flags mainPath = do
  dir <- maybe getCurrentDirectory return (flagToMaybe $ packageDir flags)
  let mainFullPath = dir </> mainPath
  exists <- doesFileExist mainFullPath
  unless exists $ do
      message flags $ "Generating " ++ mainPath ++ "..."
      writeFileSafe flags mainFullPath (mainHs flags)

-- | Returns true if a main file exists.
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

-- | Create a test suite for the package if eligible.
createTestSuiteIfEligible :: InitFlags -> IO ()
createTestSuiteIfEligible flags =
  when (eligibleForTestSuite flags) $ do
    createDirectories (testDirs flags)
    createTestHs flags

-- | The name of the test file to generate (if --tests is specified).
testFile :: String
testFile = "MyLibTest.hs"

-- | Create MyLibTest.hs, but only if we are init'ing a library and
--   the initializeTestSuite flag has been set.
--
-- It is up to the caller to verify that the package is eligible
-- for test suite initialization (see eligibleForTestSuite).
createTestHs :: InitFlags -> IO ()
createTestHs flags =
  case testDirs flags of
    Just (testPath:_) -> writeTestHs flags (testPath </> testFile)
    _ -> writeMainHs flags testFile

-- | Write a test file.
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


-- | Given a file path find a new name for the file that does not
--   already exist.
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
