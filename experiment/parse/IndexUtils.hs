-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.IndexUtils
-- Copyright   :  (c) Duncan Coutts 2008
-- License     :  BSD-like
--
-- Maintainer  :  duncan@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Extra utils related to the package indexes.
-----------------------------------------------------------------------------
module IndexUtils (
  readPackageIndexFile,
  parseRepoIndex,
  ) where

import qualified Codec.Archive.Tar as Tar

import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy (ByteString)
import System.FilePath (takeExtension)

-- | Read a compressed \"00-index.tar.gz\" file into a 'PackageIndex'.
--
-- This is supposed to be an \"all in one\" way to easily get at the info in
-- the hackage package index.
--
-- It takes a function to map a 'GenericPackageDescription' into any more
-- specific instance of 'Package' that you might want to use. In the simple
-- case you can just use @\_ p -> p@ here.
--
readPackageIndexFile :: FilePath -> IO [ByteString]
readPackageIndexFile indexFile =
      either fail return
    . parseRepoIndex
  =<< BS.readFile indexFile

-- | Parse an uncompressed \"00-index.tar\" repository index file represented
-- as a 'ByteString'.
--
parseRepoIndex :: ByteString
               -> Either String [ByteString]
parseRepoIndex = foldlTarball (\pkgs -> maybe pkgs (:pkgs) . extractPkg) []

extractPkg :: Tar.Entry -> Maybe ByteString
extractPkg entry =
  case Tar.entryContent entry of
    Tar.NormalFile content _ | takeExtension (Tar.entryPath entry) == ".cabal"
      -> Just content
    _ -> Nothing

foldlTarball :: (a -> Tar.Entry -> a) -> a
             -> ByteString -> Either String a
foldlTarball f z = either Left (Right . foldl f z) . check [] . Tar.read
  where
    check _  (Tar.Fail err)  = Left $ show err
    check ok Tar.Done        = Right ok
    check ok (Tar.Next e es) = check (e:ok) es
