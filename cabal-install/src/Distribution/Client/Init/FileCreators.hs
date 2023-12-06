{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------

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
module Distribution.Client.Init.FileCreators
  ( -- * Commands
    writeProject
  , writeLicense
  , writeChangeLog
  , prepareLibTarget
  , prepareExeTarget
  , prepareTestTarget
  ) where

import Distribution.Client.Compat.Prelude hiding (empty, head, readFile, writeFile)
import Prelude hiding (readFile, writeFile)

import qualified Data.Set as Set (member)

import Distribution.CabalSpecVersion (showCabalSpecVersion)
import Distribution.Client.Init.Defaults
import Distribution.Client.Init.Format
import Distribution.Client.Init.Licenses
  ( agplv3
  , apache20
  , bsd2
  , bsd3
  , gplv2
  , gplv3
  , isc
  , lgpl21
  , lgpl3
  , mit
  , mpl20
  )
import Distribution.Client.Init.Types hiding (message, putStr, putStrLn)
import qualified Distribution.Client.Init.Types as T
import Distribution.Fields.Pretty (PrettyField (..), showFields')
import qualified Distribution.SPDX as SPDX
import Distribution.Types.PackageName

import Distribution.FieldGrammar.Newtypes
import Distribution.License (licenseToSPDX)
import System.FilePath ((<.>), (</>))

-- -------------------------------------------------------------------- --
--  File generation

writeProject :: Interactive m => ProjectSettings -> m ()
writeProject (ProjectSettings opts pkgDesc libTarget exeTarget testTarget)
  | null pkgName = do
      message opts T.Error "no package name given, so no .cabal file can be generated\n"
  | otherwise = do
      -- clear prompt history a bit"
      message opts T.Info $
        "Using cabal specification: "
          ++ showCabalSpecVersion (_optCabalSpec opts)

      writeLicense opts pkgDesc
      writeChangeLog opts pkgDesc

      let pkgFields = mkPkgDescription opts pkgDesc
          commonStanza = mkCommonStanza opts

      libStanza <- prepareLibTarget opts libTarget
      exeStanza <- prepareExeTarget opts exeTarget
      testStanza <- prepareTestTarget opts testTarget

      (reusedCabal, cabalContents) <-
        writeCabalFile opts $
          pkgFields ++ [commonStanza, libStanza, exeStanza, testStanza]

      when (null $ _pkgSynopsis pkgDesc) $
        message opts T.Warning "No synopsis given. You should edit the .cabal file and add one."

      message opts T.Info "You may want to edit the .cabal file and add a Description field."

      when reusedCabal $ do
        existingCabal <- readFile $ unPackageName (_optPkgName opts) ++ ".cabal"
        when (existingCabal /= cabalContents) $
          message opts T.Warning "A .cabal file was found and not updated, if updating is desired please use the '--overwrite' option."

      -- clear out last line for presentation.
      T.putStrLn ""
  where
    pkgName = unPackageName $ _optPkgName opts

prepareLibTarget
  :: Interactive m
  => WriteOpts
  -> Maybe LibTarget
  -> m (PrettyField FieldAnnotation)
prepareLibTarget _ Nothing = return PrettyEmpty
prepareLibTarget opts (Just libTarget) = do
  void $ writeDirectoriesSafe opts $ filter (/= ".") srcDirs
  -- avoid writing when conflicting exposed paths may
  -- exist.
  when (expMods == (myLibModule :| [])) . void $
    writeFileSafe opts libPath myLibHs

  return $ mkLibStanza opts libTarget
  where
    expMods = _libExposedModules libTarget
    srcDirs = _libSourceDirs libTarget
    libPath = case srcDirs of
      path : _ -> path </> _hsFilePath myLibFile
      _ -> _hsFilePath myLibFile

prepareExeTarget
  :: Interactive m
  => WriteOpts
  -> Maybe ExeTarget
  -> m (PrettyField FieldAnnotation)
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
      appPath : _ -> appPath </> mainFile
      _ -> mainFile

    mainHs =
      unlines . mkLiterate exeMainIs $
        if pkgType == LibraryAndExecutable
          then myLibExeHs
          else myExeHs

prepareTestTarget
  :: Interactive m
  => WriteOpts
  -> Maybe TestTarget
  -> m (PrettyField FieldAnnotation)
prepareTestTarget _ Nothing = return PrettyEmpty
prepareTestTarget opts (Just testTarget) = do
  void $ writeDirectoriesSafe opts testDirs'
  void $ writeFileSafe opts testPath myTestHs
  return $ mkTestStanza opts testTarget
  where
    testDirs' = _testDirs testTarget
    testMainIs = _hsFilePath $ _testMainIs testTarget
    testPath = case testDirs' of
      p : _ -> p </> testMainIs
      _ -> testMainIs

writeCabalFile
  :: Interactive m
  => WriteOpts
  -> [PrettyField FieldAnnotation]
  -- ^ .cabal fields
  -> m (Bool, String)
writeCabalFile opts fields = do
  let cabalContents =
        showFields'
          annCommentLines
          postProcessFieldLines
          4
          fields

  reusedCabal <- writeFileSafe opts cabalFileName cabalContents
  return (reusedCabal, cabalContents)
  where
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
writeLicense :: Interactive m => WriteOpts -> PkgDescription -> m ()
writeLicense writeOpts pkgDesc = do
  year <- show <$> getCurrentYear
  case licenseFile year (_pkgAuthor pkgDesc) of
    Just licenseText ->
      void $ writeFileSafe writeOpts "LICENSE" licenseText
    Nothing -> message writeOpts T.Warning "unknown license type, you must put a copy in LICENSE yourself."
  where
    getLid (Left (SPDX.License (SPDX.ELicense (SPDX.ELicenseId lid) Nothing))) = Just lid
    getLid (Right l) = getLid . Left $ licenseToSPDX l
    getLid _ = Nothing

    licenseFile year auth = case getLid . getSpecLicense $ _pkgLicense pkgDesc of
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
writeChangeLog :: Interactive m => WriteOpts -> PkgDescription -> m ()
writeChangeLog opts pkgDesc
  | Just docs <- _pkgExtraDocFiles pkgDesc
  , defaultChangelog `Set.member` docs =
      go
  | defaultChangelog `elem` _pkgExtraSrcFiles pkgDesc = go
  | otherwise = return ()
  where
    changeLog =
      unlines
        [ "# Revision history for " ++ prettyShow (_pkgName pkgDesc)
        , ""
        , "## " ++ prettyShow (_pkgVersion pkgDesc) ++ " -- YYYY-mm-dd"
        , ""
        , "* First version. Released on an unsuspecting world."
        ]

    go =
      void $ writeFileSafe opts defaultChangelog changeLog

-- -------------------------------------------------------------------- --
-- Utilities

data WriteAction = Overwrite | Fresh | Existing deriving (Eq)

instance Show WriteAction where
  show Overwrite = "Overwriting"
  show Fresh = "Creating fresh"
  show Existing = "Using existing"

-- | Possibly generate a message to stdout, taking into account the
--   --quiet flag.
message :: Interactive m => WriteOpts -> T.Severity -> String -> m ()
message opts = T.message (_optVerbosity opts)

-- | Write a file \"safely\" if it doesn't exist, backing up any existing version when
--   the overwrite flag is set.
writeFileSafe :: Interactive m => WriteOpts -> FilePath -> String -> m Bool
writeFileSafe opts fileName content = do
  exists <- doesFileExist fileName

  let action
        | exists && doOverwrite = Overwrite
        | not exists = Fresh
        | otherwise = Existing

  go exists

  message opts T.Info $ show action ++ " file " ++ fileName ++ "..."
  return $ action == Existing
  where
    doOverwrite = _optOverwrite opts

    go exists
      | not exists = do
          writeFile fileName content
      | exists && doOverwrite = do
          newName <- findNewPath fileName
          message opts T.Info $
            concat
              [ fileName
              , " already exists. Backing up old version in "
              , newName
              ]

          copyFile fileName newName -- backups the old file
          removeExistingFile fileName -- removes the original old file
          writeFile fileName content -- writes the new file
      | otherwise = return ()

writeDirectoriesSafe :: Interactive m => WriteOpts -> [String] -> m Bool
writeDirectoriesSafe opts dirs = fmap or $ for dirs $ \dir -> do
  exists <- doesDirectoryExist dir

  let action
        | exists && doOverwrite = Overwrite
        | not exists = Fresh
        | otherwise = Existing

  go dir exists

  message opts T.Info $ show action ++ " directory ./" ++ dir ++ "..."
  return $ action == Existing
  where
    doOverwrite = _optOverwrite opts

    go dir exists
      | not exists = do
          createDirectory dir
      | exists && doOverwrite = do
          newDir <- findNewPath dir
          message opts T.Info $
            concat
              [ dir
              , " already exists. Backing up old version in "
              , newDir
              ]

          renameDirectory dir newDir -- backups the old directory
          createDirectory dir -- creates the new directory
      | otherwise = return ()

findNewPath :: Interactive m => FilePath -> m FilePath
findNewPath dir = go (0 :: Int)
  where
    go n = do
      let newDir = dir <.> ("save" ++ show n)
      e <- doesDirectoryExist newDir
      if e then go (succ n) else return newDir
