{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- FOURMOLU_DISABLE -}
module Distribution.Client.Compat.Tar
  ( extractTarGzFile
  , createTarGzFile
  , createTarGzFileMulti
#if MIN_VERSION_tar(0,6,0)
  , Tar.Entry
  , Tar.Entries
  , Tar.GenEntries (..)
  , Tar.GenEntryContent (..)
  , Tar.entryContent
#else
  , Tar.Entries (..)
  , Tar.Entry (..)
  , Tar.EntryContent (..)
#endif
  ) where
{- FOURMOLU_ENABLE -}

import Distribution.Client.Compat.Prelude
import Prelude ()

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Check as Tar
#if MIN_VERSION_tar(0,6,0)
#else
import qualified Codec.Archive.Tar.Entry as Tar
#endif
import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy as BS
import qualified Distribution.Client.GZipUtils as GZipUtils

instance (Exception a, Exception b) => Exception (Either a b) where
  toException (Left e) = toException e
  toException (Right e) = toException e

  fromException e =
    case fromException e of
      Just e' -> Just (Left e')
      Nothing -> case fromException e of
        Just e' -> Just (Right e')
        Nothing -> Nothing

{- FOURMOLU_DISABLE -}
extractTarGzFile
  :: FilePath
  -- ^ Destination directory
  -> FilePath
  -- ^ Expected subdir (to check for tarbombs)
  -> FilePath
  -- ^ Tarball
  -> IO ()
extractTarGzFile dir expected tar =
#if MIN_VERSION_tar(0,6,0)
  Tar.unpackAndCheck
    ( \x ->
        SomeException <$> Tar.checkEntryTarbomb expected x
          <|> SomeException <$> Tar.checkEntrySecurity x
    )
    dir
#else
  Tar.unpack dir
    . Tar.checkTarbomb expected
#endif
  . Tar.read
  . GZipUtils.maybeDecompress
  =<< BS.readFile tar

createTarGzFile
  :: FilePath
  -- ^ Full Tarball path
  -> FilePath
  -- ^ Base directory
  -> FilePath
  -- ^ Directory to archive, relative to base dir
  -> IO ()
createTarGzFile tar base dir =
#if MIN_VERSION_tar(0,7,0)
  BS.writeFile tar . GZip.compress =<< Tar.write' =<< Tar.pack' base [dir]
#else
  BS.writeFile tar . GZip.compress . Tar.write =<< Tar.pack base [dir]
#endif

-- | Like 'createTarGzFile', but packs several directories into a single
-- tarball.
--
-- Each directory to archive is given as a pair of a base directory and a
-- directory path relative to that base; the entries keep their relative
-- path as their name in the archive. This avoids having to gather all the
-- directories to archive into a single staging directory first.
createTarGzFileMulti
  :: FilePath
  -- ^ Full tarball path
  -> [(FilePath, FilePath)]
  -- ^ Base directory and directory to archive, relative to the base, for
  -- each directory to include in the tarball
  -> IO ()
createTarGzFileMulti tar dirs =
#if MIN_VERSION_tar(0,7,0)
  BS.writeFile tar . GZip.compress =<< Tar.write' . concat =<< traverse (\(base, dir) -> Tar.pack' base [dir]) dirs
#else
  BS.writeFile tar . GZip.compress . Tar.write . concat =<< traverse (\(base, dir) -> Tar.pack base [dir]) dirs
#endif


{- FOURMOLU_ENABLE -}
