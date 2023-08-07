{-# LANGUAGE OverloadedStrings #-}

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
import System.FilePath (normalise, takeDirectory, (</>))

import Distribution.Client.Utils (tryFindAddSourcePackageDesc)
import Distribution.Package (Package (packageId))
import Distribution.PackageDescription.Configuration (flattenPackageDescription)
import Distribution.Simple.PackageDescription (readGenericPackageDescription)
import Distribution.Simple.PreProcess (knownSuffixHandlers)
import Distribution.Simple.SrcDist (listPackageSourcesWithDie)
import Distribution.Simple.Utils (die', dieWithException)
import Distribution.Types.GenericPackageDescription (GenericPackageDescription)

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Set as Set

-- | List all source files of a given add-source dependency. Exits with error if
-- something is wrong (e.g. there is no .cabal file in the given directory).
--
-- Used in sandbox and projectbuilding.
-- TODO: when sandboxes are removed, move to ProjectBuilding.
allPackageSourceFiles :: Verbosity -> FilePath -> IO [FilePath]
allPackageSourceFiles verbosity packageDir = do
  pd <- do
    let err = "Error reading source files of package."
    desc <- tryFindAddSourcePackageDesc verbosity packageDir err
    flattenPackageDescription `fmap` readGenericPackageDescription verbosity desc

  listPackageSourcesWithDie verbosity (\_ _ -> return []) packageDir pd knownSuffixHandlers

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
  -- let thisDie :: Verbosity -> String -> IO a
  --    thisDie v s = die' v $ "sdist of " <> prettyShow (packageId gpd) ++ ": " ++ s

  files' <- listPackageSourcesWithDie verbosity dieWithException dir (flattenPackageDescription gpd) knownSuffixHandlers
  let files :: [FilePath]
      files = nub $ sort $ map normalise files'

  let entriesM :: StateT (Set.Set FilePath) (WriterT [Tar.Entry] IO) ()
      entriesM = do
        let prefix = prettyShow (packageId gpd)
        modify (Set.insert prefix)
        case Tar.toTarPath True prefix of
          Left err -> liftIO $ die' verbosity ("Error packing sdist: " ++ err)
          Right path -> tell [Tar.directoryEntry path]

        for_ files $ \file -> do
          let fileDir = takeDirectory (prefix </> file)
          needsEntry <- gets (Set.notMember fileDir)

          when needsEntry $ do
            modify (Set.insert fileDir)
            case Tar.toTarPath True fileDir of
              Left err -> liftIO $ die' verbosity ("Error packing sdist: " ++ err)
              Right path -> tell [Tar.directoryEntry path]

          contents <- liftIO . fmap BSL.fromStrict . BS.readFile $ dir </> file
          case Tar.toTarPath False (prefix </> file) of
            Left err -> liftIO $ die' verbosity ("Error packing sdist: " ++ err)
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
