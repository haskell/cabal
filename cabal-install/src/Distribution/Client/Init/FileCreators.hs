{-# LANGUAGE LambdaCase #-}
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
module Distribution.Client.Init.FileCreators
( -- * Commands
  writeProject
, writeLicense
, writeChangeLog
, prepareLibTarget
, prepareExeTarget
, prepareTestTarget
) where

import Prelude hiding (writeFile)
import Distribution.Client.Compat.Prelude hiding (head, empty, writeFile)

import qualified Data.Set as Set (member)

import Distribution.Client.Utils (getCurrentYear, removeExistingFile)
import Distribution.Client.Init.Defaults
import Distribution.Client.Init.Licenses
  ( bsd2, bsd3, gplv2, gplv3, lgpl21, lgpl3, agplv3, apache20, mit, mpl20, isc )
import Distribution.Client.Init.Types hiding (putStrLn, putStr, message)
import qualified Distribution.Client.Init.Types as T
import Distribution.Fields.Pretty (PrettyField(..), showFields')
import qualified Distribution.SPDX as SPDX
import Distribution.Types.PackageName
import Distribution.Client.Init.Format
import Distribution.CabalSpecVersion (showCabalSpecVersion)

import System.FilePath ((</>), (<.>))

-- -------------------------------------------------------------------- --
--  File generation

writeProject :: ProjectSettings -> IO ()
writeProject (ProjectSettings opts pkgDesc libTarget exeTarget testTarget)
    | null pkgName = do
      message opts "\nError: no package name given, so no .cabal file can be generated\n"
    | otherwise = do

      -- clear prompt history a bit"
      message opts
        $ "\nUsing cabal specification: "
        ++ showCabalSpecVersion (_optCabalSpec opts)
        ++ "\n"

      writeLicense opts pkgDesc
      writeChangeLog opts pkgDesc

      let pkgFields = mkPkgDescription opts pkgDesc

      libStanza <- prepareLibTarget opts libTarget
      exeStanza <- prepareExeTarget opts exeTarget
      testStanza <- prepareTestTarget opts testTarget

      writeCabalFile opts $ pkgFields ++ [libStanza, exeStanza, testStanza]

      when (null $ _pkgSynopsis pkgDesc) $
        message opts "\nWarning: no synopsis given. You should edit the .cabal file and add one."

      message opts "You may want to edit the .cabal file and add a Description field."
  where
    pkgName = unPackageName $ _optPkgName opts


prepareLibTarget
    :: WriteOpts
    -> Maybe LibTarget
    -> IO (PrettyField FieldAnnotation)
prepareLibTarget _ Nothing = return PrettyEmpty
prepareLibTarget opts (Just libTarget) = do
    void $ writeDirectoriesSafe opts srcDirs
    -- avoid writing when conflicting exposed paths may
    -- exist.
    when (expMods == (myLibModule :| [])) $ do
      writeFileSafe opts libPath myLibHs

    return $ mkLibStanza opts libTarget
  where
    expMods = _libExposedModules libTarget
    srcDirs = _libSourceDirs libTarget
    libPath = case srcDirs of
      path:_ -> path </> _hsFilePath myLibFile
      _ -> _hsFilePath myLibFile

prepareExeTarget
    :: WriteOpts
    -> Maybe ExeTarget
    -> IO (PrettyField FieldAnnotation)
prepareExeTarget _ Nothing = return PrettyEmpty
prepareExeTarget opts (Just exeTarget) = do
    void $ writeDirectoriesSafe opts appDirs
    void $ writeFileSafe opts mainPath mainHs
    return $ mkExeStanza opts exeTarget
  where
    exeMainIs = _exeMainIs exeTarget
    pkgType = _optPkgType opts
    appDirs = _exeApplicationDirs exeTarget
    mainFile = _hsFilePath exeMainIs
    mainPath = case appDirs of
      appPath:_ -> appPath </> mainFile
      _ -> mainFile

    mainHs = unlines . mkLiterate exeMainIs $
      if pkgType == LibraryAndExecutable
      then myLibExeHs
      else myExeHs

prepareTestTarget
    :: WriteOpts
    -> Maybe TestTarget
    -> IO (PrettyField FieldAnnotation)
prepareTestTarget _ Nothing = return PrettyEmpty
prepareTestTarget opts (Just testTarget) = do
    void $ writeDirectoriesSafe opts testDirs'
    void $ writeFileSafe opts testPath myTestHs
    return $ mkTestStanza opts testTarget
  where
    testDirs' = _testDirs testTarget
    testMainIs = _hsFilePath $ _testMainIs testTarget
    testPath = case testDirs' of
      p:_ -> p </> testMainIs
      _ -> testMainIs

writeCabalFile
    :: WriteOpts
    -> [PrettyField FieldAnnotation]
      -- ^ .cabal fields
    -> IO ()
writeCabalFile opts fields =
    writeFileSafe opts cabalFileName cabalContents
  where
    cabalContents = showFields'
      annCommentLines
      postProcessFieldLines
      4 fields

    cabalFileName = pkgName ++ ".cabal"
    pkgName = unPackageName $ _optPkgName opts

-- | Write the LICENSE file.
--
-- For licenses that contain the author's name(s), the values are taken
-- from the 'authors' field of 'InitFlags', and if not specified will
-- be the string "???".
--
-- If the license type is unknown no license file will be prepared and
-- a warning will be raised.
--
writeLicense :: WriteOpts -> PkgDescription -> IO ()
writeLicense writeOpts pkgDesc = do
  year <- show <$> getCurrentYear
  case licenseFile year (_pkgAuthor pkgDesc) of
    Just licenseText -> do
      message writeOpts "\nCreating LICENSE..."
      writeFileSafe writeOpts "LICENSE" licenseText
    Nothing -> message writeOpts "Warning: unknown license type, you must put a copy in LICENSE yourself."
  where
    getLid (SPDX.License (SPDX.ELicense (SPDX.ELicenseId lid) Nothing)) =
      Just lid
    getLid _ = Nothing

    licenseFile year auth = case getLid $ _pkgLicense pkgDesc of
      Just SPDX.BSD_2_Clause -> Just $ bsd2 auth year
      Just SPDX.BSD_3_Clause -> Just $ bsd3 auth year
      Just SPDX.Apache_2_0 -> Just apache20
      Just SPDX.MIT -> Just $ mit auth year
      Just SPDX.MPL_2_0 -> Just mpl20
      Just SPDX.ISC -> Just $ isc auth year
      Just SPDX.GPL_2_0_only -> Just gplv2
      Just SPDX.GPL_3_0_only -> Just gplv3
      Just SPDX.LGPL_2_1_only -> Just lgpl21
      Just SPDX.LGPL_3_0_only -> Just lgpl3
      Just SPDX.AGPL_3_0_only -> Just agplv3
      Just SPDX.GPL_2_0_or_later -> Just gplv2
      Just SPDX.GPL_3_0_or_later -> Just gplv3
      Just SPDX.LGPL_2_1_or_later -> Just lgpl21
      Just SPDX.LGPL_3_0_or_later -> Just lgpl3
      Just SPDX.AGPL_3_0_or_later -> Just agplv3
      _ -> Nothing

-- | Writes the changelog to the current directory.
--
writeChangeLog :: WriteOpts -> PkgDescription -> IO ()
writeChangeLog opts pkgDesc
  | Just docs <- _pkgExtraDocFiles pkgDesc
  , defaultChangelog `Set.member` docs = go
  | defaultChangelog `elem` _pkgExtraSrcFiles pkgDesc = go
  | otherwise = return ()
 where
  changeLog = unlines
    [ "# Revision history for " ++ prettyShow (_pkgName pkgDesc)
    , ""
    , "## " ++ prettyShow (_pkgVersion pkgDesc) ++ " -- YYYY-mm-dd"
    , ""
    , "* First version. Released on an unsuspecting world."
    ]

  go = do
    message opts ("Creating " ++ defaultChangelog ++"...")
    writeFileSafe opts defaultChangelog changeLog

-- -------------------------------------------------------------------- --
-- Utilities

-- | Possibly generate a message to stdout, taking into account the
--   --quiet flag.
message :: Interactive m => WriteOpts -> String -> m ()
message opts = T.message (_optVerbosity opts)

-- | Write a file \"safely\" if it doesn't exist, backing up any existing version when
--   the overwrite flag is set.
writeFileSafe :: WriteOpts -> FilePath -> String -> IO ()
writeFileSafe opts fileName content = do
    exists <- doesFileExist fileName

    let action
          | doOverwrite = "Overwriting"
          | exists = "Creating fresh"
          | otherwise = "Creating"

    go exists

    message opts $ action ++ " file " ++ fileName ++ "..."
    writeFile fileName content
  where
    doOverwrite = _optOverwrite opts

    go exists
      | exists, doOverwrite = do
        removeExistingFile fileName
      | exists, not doOverwrite = do
        newName <- findNewPath fileName
        message opts $ concat
          [ "Warning: "
          , fileName
          , " already exists. Backing up old version in "
          , newName
          ]

        copyFile fileName newName
        removeExistingFile fileName
      | otherwise = return ()

writeDirectoriesSafe :: WriteOpts -> [String] -> IO ()
writeDirectoriesSafe opts dirs = for_ dirs $ \dir -> do
    exists <- doesDirectoryExist dir

    let action
          | doOverwrite = "Overwriting"
          | exists = "Creating fresh"
          | otherwise = "Creating"

    go dir exists

    message opts $ action ++ " directory ./" ++ dir ++ "..."
    createDirectory dir
  where
    doOverwrite = _optOverwrite opts

    go dir exists
      | exists, doOverwrite = do
        removeDirectory dir
      | exists, not doOverwrite = do
        newDir <- findNewPath dir
        message opts $ concat
          [ "Warning: "
          , dir
          , " already exists. Backing up old version in "
          , newDir
          ]

        renameDirectory dir newDir
      | otherwise = return ()

findNewPath :: Interactive m => FilePath -> m FilePath
findNewPath dir = go (0 :: Int)
  where
    go n = do
      let newDir = dir <.> ("save" ++ show n)
      e <- doesDirectoryExist newDir
      if e then go (succ n) else return newDir
