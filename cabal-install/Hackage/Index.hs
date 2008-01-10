-----------------------------------------------------------------------------
-- |
-- Module      :  Hackage.Index
-- Copyright   :  (c) David Himmelstrup 2005, Bjorn Bringert 2007
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Reading the local package index.
-----------------------------------------------------------------------------
module Hackage.Index (getKnownPackages) where

import Hackage.Types
import Hackage.Tar

import Prelude hiding (catch)
import Control.Exception (catch, Exception(IOException))
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BS.Char8
import Data.ByteString.Lazy (ByteString)
import System.FilePath ((</>), takeExtension, splitDirectories, normalise)
import System.IO.Error (isDoesNotExistError)

import Distribution.PackageDescription (parsePackageDescription, ParseResult(..))
import Distribution.Package (PackageIdentifier(..))
import Distribution.Version (readVersion)
import Distribution.Verbosity (Verbosity)
import Distribution.Simple.Utils (warn)

getKnownPackages :: Verbosity -> [Repo] -> IO [PkgInfo]
getKnownPackages verbosity repos
    = fmap concat $ mapM (readRepoIndex verbosity) repos

readRepoIndex :: Verbosity -> Repo -> IO [PkgInfo]
readRepoIndex verbosity repo =
    do let indexFile = repoCacheDir repo </> "00-index.tar"
       fmap (parseRepoIndex repo) (BS.readFile indexFile)
          `catch` (\e -> do case e of
                              IOException ioe | isDoesNotExistError ioe ->
                                warn verbosity "The package list does not exist. Run 'cabal update' to download it."
                              _ -> warn verbosity (show e)
                            return [])

parseRepoIndex :: Repo -> ByteString -> [PkgInfo]
parseRepoIndex repo s =
    do (hdr, content) <- readTarArchive s
       if takeExtension (tarFileName hdr) == ".cabal"
         then case splitDirectories (normalise (tarFileName hdr)) of
                [pkgname,vers,_] ->
                  let descr = case parsePackageDescription (BS.Char8.unpack content) of
                        ParseOk _ d -> d
                        _           -> error $ "Couldn't read cabal file "
                                            ++ show (tarFileName hdr)
                   in case readVersion vers of
                        Just ver ->
                         return $ PkgInfo {
                                 pkgInfoId = PackageIdentifier pkgname ver,
                                 pkgRepo = repo,
                                 pkgDesc = descr
                               }
                        _ -> []
                _ -> []
         else []
