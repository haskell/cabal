{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{- FOURMOLU_DISABLE -}
module Distribution.Client.Compat.Tar
  ( extractTarGzFile
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
{- FOURMOLU_ENABLE -}
