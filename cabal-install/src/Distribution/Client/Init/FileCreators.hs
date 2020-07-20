{-# LANGUAGE OverloadedStrings #-}
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

  -- * For testing
  , generateCabalFile
  ) where

import Prelude ()
import Distribution.Client.Compat.Prelude hiding (empty)

import System.FilePath
  ( (</>), (<.>), takeExtension )

import Distribution.Types.Dependency
import Distribution.Types.VersionRange

import Data.Time
  ( getCurrentTime, utcToLocalTime, toGregorian, localDay, getCurrentTimeZone )
import System.Directory
  ( getCurrentDirectory, doesFileExist, copyFile
  , createDirectoryIfMissing )

import Text.PrettyPrint hiding ((<>), mode, cat)

import Distribution.Client.Init.Defaults
  ( defaultCabalVersion, myLibModule )
import Distribution.Client.Init.Licenses
  ( bsd2, bsd3, gplv2, gplv3, lgpl21, lgpl3, agplv3, apache20, mit, mpl20, isc )
import Distribution.Client.Init.Utils
  ( eligibleForTestSuite, message )
import Distribution.Client.Init.Types
  ( InitFlags(..), BuildType(..), PackageType(..) )

import Distribution.CabalSpecVersion
import Distribution.Compat.Newtype
  ( Newtype )
import Distribution.Fields.Field
  ( FieldName )
import Distribution.License
  ( licenseFromSPDX )
import qualified Distribution.ModuleName as ModuleName
  ( toFilePath )
import Distribution.FieldGrammar.Newtypes
  ( SpecVersion(..) )
import Distribution.PackageDescription.FieldGrammar
  ( formatDependencyList, formatExposedModules, formatHsSourceDirs,
    formatOtherExtensions, formatOtherModules, formatExtraSourceFiles )
import Distribution.Simple.Flag
  ( maybeToFlag )
import Distribution.Simple.Setup
  ( Flag(..), flagToMaybe )
import Distribution.Simple.Utils
  ( toUTF8BS )
import Distribution.Fields.Pretty
  ( PrettyField(..), showFields' )

import qualified Distribution.SPDX as SPDX


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
  let isSimpleLicense :: SPDX.License -> Maybe SPDX.LicenseId
      isSimpleLicense (SPDX.License (SPDX.ELicense (SPDX.ELicenseId lid) Nothing)) = Just lid
      isSimpleLicense _                                                            = Nothing
  let licenseFile =
        case flagToMaybe (license flags) >>= isSimpleLicense of
          Just SPDX.BSD_2_Clause  -> Just $ bsd2 authors year
          Just SPDX.BSD_3_Clause  -> Just $ bsd3 authors year
          Just SPDX.Apache_2_0    -> Just apache20
          Just SPDX.MIT           -> Just $ mit authors year
          Just SPDX.MPL_2_0       -> Just mpl20
          Just SPDX.ISC           -> Just $ isc authors year

          -- GNU license come in "only" and "or-later" flavours
          -- license file used are the same.
          Just SPDX.GPL_2_0_only  -> Just gplv2
          Just SPDX.GPL_3_0_only  -> Just gplv3
          Just SPDX.LGPL_2_1_only -> Just lgpl21
          Just SPDX.LGPL_3_0_only -> Just lgpl3
          Just SPDX.AGPL_3_0_only -> Just agplv3

          Just SPDX.GPL_2_0_or_later  -> Just gplv2
          Just SPDX.GPL_3_0_or_later  -> Just gplv3
          Just SPDX.LGPL_2_1_or_later -> Just lgpl21
          Just SPDX.LGPL_3_0_or_later -> Just lgpl3
          Just SPDX.AGPL_3_0_or_later -> Just agplv3

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
  pname = maybe "" prettyShow $ flagToMaybe $ packageName flags
  pver = maybe "" prettyShow $ flagToMaybe $ version flags

-- | Creates and writes the initialized .cabal file.
--
-- Returns @False@ if no package name is specified, @True@ otherwise.
writeCabalFile :: InitFlags -> IO Bool
writeCabalFile flags@(InitFlags{packageName = NoFlag}) = do
  message flags "Error: no package name provided."
  return False
writeCabalFile flags@(InitFlags{packageName = Flag p}) = do
  let cabalFileName = prettyShow p ++ ".cabal"
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
  Just dirs -> for_ dirs (createDirectoryIfMissing True)
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
    prependPrefix :: String -> String
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


-- | Generate a .cabal file from an InitFlags structure.
generateCabalFile :: String -> InitFlags -> String
generateCabalFile fileName c =
    showFields' annCommentLines postProcessFieldLines 4 $ catMaybes
  [ fieldP "cabal-version" (Flag . SpecVersion $ specVer)
      []
      False

  , field "name" (packageName c)
      ["Initial package description '" ++ fileName ++ "' generated by",
       "'cabal init'. For further documentation, see:",
       "  http://haskell.org/cabal/users-guide/",
       "",
       "The name of the package."]
      True

  , field  "version"       (version       c)
           ["The package version.",
            "See the Haskell package versioning policy (PVP) for standards",
            "guiding when and how versions should be incremented.",
            "https://pvp.haskell.org",
            "PVP summary:      +-+------- breaking API changes",
            "                  | | +----- non-breaking API additions",
            "                  | | | +--- code changes with no API change"]
           True

  , fieldS "synopsis"      (synopsis      c)
           ["A short (one-line) description of the package."]
           True

  , fieldS "description"   NoFlag
           ["A longer description of the package."]
           True

  , fieldS "homepage"      (homepage     c)
           ["URL for the project homepage or repository."]
           False

  , fieldS "bug-reports"   NoFlag
           ["A URL where users can report bugs."]
           True

  , fieldS  "license"      licenseStr
                ["The license under which the package is released."]
                True

  , case license c of
      NoFlag         -> Nothing
      Flag SPDX.NONE -> Nothing
      _ -> fieldS "license-file" (Flag "LICENSE")
                  ["The file containing the license text."]
                  True

  , fieldS "author"        (author       c)
           ["The package author(s)."]
           True

  , fieldS "maintainer"    (email        c)
           ["An email address to which users can send suggestions, bug reports, and patches."]
           True

  , fieldS "copyright"     NoFlag
           ["A copyright notice."]
           True

  , fieldS "category"      (either id prettyShow `fmap` category c)
           []
           True

  , fieldS "build-type"    (if specVer >= CabalSpecV2_2 then NoFlag else Flag "Simple")
           []
           False

  , fieldPAla "extra-source-files" formatExtraSourceFiles (maybeToFlag (extraSrc c))
           ["Extra files to be distributed with the package, such as examples or a README."]
           True
  ]
  ++
  (case packageType c of
     Flag Executable -> [executableStanza]
     Flag Library    -> [libraryStanza]
     Flag LibraryAndExecutable -> [libraryStanza, executableStanza]
     _               -> [])
  ++
  if eligibleForTestSuite c then [testSuiteStanza] else []

 where
   specVer :: CabalSpecVersion
   specVer = fromMaybe defaultCabalVersion $ flagToMaybe (cabalVersion c)

   licenseStr | specVer < CabalSpecV2_2 = prettyShow . licenseFromSPDX <$> license c
              | otherwise               = prettyShow                   <$> license c

   generateBuildInfo :: BuildType -> InitFlags -> [PrettyField FieldAnnotation]
   generateBuildInfo buildType c' = catMaybes
     [ fieldPAla "other-modules" formatOtherModules (maybeToFlag otherMods)
       [ case buildType of
                 LibBuild    -> "Modules included in this library but not exported."
                 ExecBuild -> "Modules included in this executable, other than Main."]
       True

     , fieldPAla "other-extensions" formatOtherExtensions (maybeToFlag (otherExts c))
       ["LANGUAGE extensions used by modules in this package."]
       True

     , fieldPAla "build-depends" formatDependencyList (maybeToFlag buildDependencies)
       ["Other library packages from which modules are imported."]
       True

     , fieldPAla "hs-source-dirs" formatHsSourceDirs
       (maybeToFlag (case buildType of
                                              LibBuild -> sourceDirs c
                                              ExecBuild -> applicationDirs c))
       ["Directories containing source files."]
       True

     , fieldS "build-tools" (listFieldS $ buildTools c)
       ["Extra tools (e.g. alex, hsc2hs, ...) needed to build the source."]
       False

     , field "default-language" (language c)
       ["Base language which the package is written in."]
       True
     ]
     -- Hack: Can't construct a 'Dependency' which is just 'packageName'(?).
     where
       buildDependencies :: Maybe [Dependency]
       buildDependencies = (++ myLibDep) <$> dependencies c'

       myLibDep :: [Dependency]
       myLibDep = if exposedModules c' == Just [myLibModule] && buildType == ExecBuild
                      then case packageName c' of
                             Flag pkgName ->
                               [mkDependency pkgName anyVersion mainLibSet]
                             _ -> []
                  else []

       -- Only include 'MyLib' in 'other-modules' of the executable.
       otherModsFromFlag = otherModules c'
       otherMods = if buildType == LibBuild && otherModsFromFlag == Just [myLibModule]
                   then Nothing
                   else otherModsFromFlag

   listFieldS :: Maybe [String] -> Flag String
   listFieldS Nothing = NoFlag
   listFieldS (Just []) = NoFlag
   listFieldS (Just xs) = Flag . intercalate ", " $ xs

   -- | Construct a 'PrettyField' from a field that can be automatically
   --   converted to a 'Doc' via 'display'.
   field :: Pretty t
         => FieldName
         -> Flag t
         -> [String]
         -> Bool
         -> Maybe (PrettyField FieldAnnotation)
   field fieldName fieldContentsFlag = fieldS fieldName (prettyShow <$> fieldContentsFlag)

   -- | Construct a 'PrettyField' from a 'String' field.
   fieldS :: FieldName   -- ^ Name of the field
          -> Flag String -- ^ Field contents
          -> [String]    -- ^ Comment to explain the field
          -> Bool        -- ^ Should the field be included (commented out) even if blank?
          -> Maybe (PrettyField FieldAnnotation)
   fieldS fieldName fieldContentsFlag = fieldD fieldName (text <$> fieldContentsFlag)

   -- | Construct a 'PrettyField' from a Flag which can be 'pretty'-ied.
   fieldP :: Pretty a
          => FieldName
          -> Flag a
          -> [String]
          -> Bool
          -> Maybe (PrettyField FieldAnnotation)
   fieldP fieldName fieldContentsFlag fieldComments includeField =
     fieldPAla fieldName Identity fieldContentsFlag fieldComments includeField

   -- | Construct a 'PrettyField' from a flag which can be 'pretty'-ied, wrapped in newtypeWrapper.
   fieldPAla
     :: (Pretty b, Newtype a b)
     => FieldName
     -> (a -> b)
     -> Flag a
     -> [String]
     -> Bool
     -> Maybe (PrettyField FieldAnnotation)
   fieldPAla fieldName newtypeWrapper fieldContentsFlag fieldComments includeField =
     fieldD fieldName (pretty . newtypeWrapper <$> fieldContentsFlag) fieldComments includeField

   -- | Construct a 'PrettyField' from a 'Doc' Flag.
   fieldD :: FieldName   -- ^ Name of the field
          -> Flag Doc    -- ^ Field contents
          -> [String]    -- ^ Comment to explain the field
          -> Bool        -- ^ Should the field be included (commented out) even if blank?
          -> Maybe (PrettyField FieldAnnotation)
   fieldD fieldName fieldContentsFlag fieldComments includeField =
     case fieldContentsFlag of
       NoFlag ->
         -- If there is no content, optionally produce a commented out field.
         fieldSEmptyContents fieldName fieldComments includeField

       Flag fieldContents ->
         if isEmpty fieldContents
         then
           -- If the doc is empty, optionally produce a commented out field.
           fieldSEmptyContents fieldName fieldComments includeField
         else
           -- If the doc is not empty, produce a field.
           Just $ case (noComments c, minimal c) of
             -- If the "--no-comments" flag is set, strip comments.
             (Flag True, _) ->
               fieldSWithContents fieldName fieldContents []
             -- If the "--minimal" flag is set, strip comments.
             (_, Flag True) ->
               fieldSWithContents fieldName fieldContents []
             -- Otherwise, include comments.
             (_, _) ->
               fieldSWithContents fieldName fieldContents fieldComments

   -- | Optionally produce a field with no content (depending on flags).
   fieldSEmptyContents :: FieldName
                       -> [String]
                       -> Bool
                       -> Maybe (PrettyField FieldAnnotation)
   fieldSEmptyContents fieldName fieldComments includeField
     | not includeField || (minimal c == Flag True) =
         Nothing
     | otherwise =
         Just (PrettyField (commentedOutWithComments fieldComments) fieldName empty)

   -- | Produce a field with content.
   fieldSWithContents :: FieldName
                      -> Doc
                      -> [String]
                      -> PrettyField FieldAnnotation
   fieldSWithContents fieldName fieldContents fieldComments =
     PrettyField (withComments (map ("-- " ++) fieldComments)) fieldName fieldContents

   executableStanza :: PrettyField FieldAnnotation
   executableStanza = PrettySection annNoComments (toUTF8BS "executable") [exeName] $ catMaybes
     [ fieldS "main-is" (mainIs c)
       [".hs or .lhs file containing the Main module."]
       True
     ]
     ++
     generateBuildInfo ExecBuild c
     where
       exeName = text (maybe "" prettyShow . flagToMaybe $ packageName c)

   libraryStanza :: PrettyField FieldAnnotation
   libraryStanza = PrettySection annNoComments (toUTF8BS "library") [] $ catMaybes
     [ fieldPAla "exposed-modules" formatExposedModules (maybeToFlag (exposedModules c))
       ["Modules exported by the library."]
       True
     ]
     ++
     generateBuildInfo LibBuild c


   testSuiteStanza :: PrettyField FieldAnnotation
   testSuiteStanza = PrettySection annNoComments (toUTF8BS "test-suite") [testSuiteName] $ catMaybes
     [ field "default-language" (language c)
       ["Base language which the package is written in."]
       True

     , fieldS "type" (Flag "exitcode-stdio-1.0")
       ["The interface type and version of the test suite."]
       True

     , fieldPAla "hs-source-dirs" formatHsSourceDirs
       (maybeToFlag (testDirs c))
       ["Directories containing source files."]
       True

     , fieldS "main-is" (Flag testFile)
       ["The entrypoint to the test suite."]
       True

     , fieldPAla  "build-depends" formatDependencyList (maybeToFlag (dependencies c))
       ["Test dependencies."]
       True
     ]
     where
       testSuiteName =
         text (maybe "" ((++"-test") . prettyShow) . flagToMaybe $ packageName c)

-- | Annotations for cabal file PrettyField.
data FieldAnnotation = FieldAnnotation
  { annCommentedOut :: Bool
    -- ^ True iif the field and its contents should be commented out.
  , annCommentLines :: [String]
    -- ^ Comment lines to place before the field or section.
  }

-- | A field annotation instructing the pretty printer to comment out the field
--   and any contents, with no comments.
commentedOutWithComments :: [String] -> FieldAnnotation
commentedOutWithComments = FieldAnnotation True . map ("-- " ++)

-- | A field annotation with the specified comment lines.
withComments :: [String] -> FieldAnnotation
withComments = FieldAnnotation False

-- | A field annotation with no comments.
annNoComments :: FieldAnnotation
annNoComments = FieldAnnotation False []

postProcessFieldLines :: FieldAnnotation -> [String] -> [String]
postProcessFieldLines ann
  | annCommentedOut ann = map ("-- " ++)
  | otherwise = id
