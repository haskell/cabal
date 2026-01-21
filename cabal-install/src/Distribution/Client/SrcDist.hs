{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- | Utilities to implement cabal @v2-sdist@.
module Distribution.Client.SrcDist
  ( allPackageSourceFiles
  , packageDirToSdist
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Control.Monad.State.Lazy (StateT, evalStateT, gets, modify)
import Control.Monad.Trans (liftIO)
import Control.Monad.Writer.Lazy (WriterT, execWriterT, tell)

import Distribution.Client.Errors
import Distribution.Client.Utils (tryReadAddSourcePackageDesc)
import Distribution.Package (Package (packageId), packageName, unPackageName)
import Distribution.PackageDescription.Configuration (flattenPackageDescription)
import Distribution.Simple.PreProcess (knownSuffixHandlers)
import Distribution.Simple.SrcDist (listPackageSources, listPackageSourcesWithDie)
import Distribution.Simple.Utils (dieWithException, tryFindPackageDesc)
import Distribution.Types.GenericPackageDescription (GenericPackageDescription)
import Distribution.Utils.Path
  ( FileOrDir (File)
  , Pkg
  , SymbolicPath
  , getSymbolicPath
  , makeSymbolicPath
  )

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import System.Directory (canonicalizePath)
import System.FilePath

-- | List all source files of a given add-source dependency. Exits with error if
-- something is wrong (e.g. there is no .cabal file in the given directory).
--
-- Used in sandbox and projectbuilding.
-- TODO: when sandboxes are removed, move to ProjectBuilding.
allPackageSourceFiles :: Verbosity -> FilePath -> IO [FilePath]
allPackageSourceFiles verbosity packageDir = do
  let err = "Error reading source files of package."
  gpd <- tryReadAddSourcePackageDesc verbosity packageDir err
  let pd = flattenPackageDescription gpd
  srcs <-
    listPackageSourcesWithDie
      verbosity
      (\_ _ -> return [])
      (Just $ makeSymbolicPath packageDir)
      pd
      knownSuffixHandlers
  return $ map getSymbolicPath srcs

-- | Create a tarball for a package in a directory
packageDirToSdist
  :: Verbosity
  -> GenericPackageDescription
  -- ^ read in GPD
  -> FilePath
  -- ^ directory containing that GPD
  -> IO BSL.ByteString
  -- ^ resulting sdist tarball
packageDirToSdist verbosity gpd dir = do
  let prefix = prettyShow (packageId gpd)

  mbWorkDir <- Just . makeSymbolicPath <$> canonicalizePath dir
  cabalFilePath <-
    getSymbolicPath
      <$> tryFindPackageDesc verbosity mbWorkDir
  files' <- listPackageSources verbosity mbWorkDir (flattenPackageDescription gpd) knownSuffixHandlers

  let insertMapping
        :: SymbolicPath Pkg File
        -> Map FilePath FilePath
        -> Map FilePath FilePath
      insertMapping file =
        let
          value = normalise (getSymbolicPath file)

          -- Replace the file name of the package description with one that
          -- matches the actual package name.
          -- See related issue #6299.
          key =
            prefix
              </> if value == cabalFilePath
                then unPackageName (packageName gpd) <.> "cabal"
                else value
         in
          Map.insert key value

  let files :: Map FilePath FilePath
      files = foldr insertMapping Map.empty files'

  let entriesM :: StateT (Set.Set FilePath) (WriterT [Tar.Entry] IO) ()
      entriesM = do
        modify (Set.insert prefix)
        case Tar.toTarPath True prefix of
          Left err -> liftIO $ dieWithException verbosity $ ErrorPackingSdist err
          Right path -> tell [Tar.directoryEntry path]

        for_ (Map.toAscList files) $ \(tarFile, srcFile) -> do
          let fileDir = takeDirectory tarFile
          needsEntry <- gets (Set.notMember fileDir)

          when needsEntry $ do
            modify (Set.insert fileDir)
            case Tar.toTarPath True fileDir of
              Left err -> liftIO $ dieWithException verbosity $ ErrorPackingSdist err
              Right path -> tell [Tar.directoryEntry path]

          contents <- liftIO . fmap BSL.fromStrict . BS.readFile $ dir </> srcFile
          case Tar.toTarPath False tarFile of
            Left err -> liftIO $ dieWithException verbosity $ ErrorPackingSdist err
            Right path -> tell [(Tar.fileEntry path contents){Tar.entryPermissions = Tar.ordinaryFilePermissions}]

  entries <- execWriterT (evalStateT entriesM mempty)
  let
    -- Pretend our GZip file is made on Unix.
    normalize bs = BSL.concat [pfx, "\x03", rest']
      where
        (pfx, rest) = BSL.splitAt 9 bs
        rest' = BSL.tail rest
    -- The Unix epoch, which is the default value, is
    -- unsuitable because it causes unpacking problems on
    -- Windows; we need a post-1980 date. One gigasecond
    -- after the epoch is during 2001-09-09, so that does
    -- nicely. See #5596.
    setModTime :: Tar.Entry -> Tar.Entry
    setModTime entry = entry{Tar.entryTime = 1000000000}
  return . normalize . GZip.compress . Tar.write $ fmap setModTime entries
