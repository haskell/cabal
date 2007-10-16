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

import Hackage.Config
import Hackage.Types
import Hackage.Tar

import Prelude hiding (catch)
import Control.Exception (catch, Exception(IOException))
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.ByteString.Lazy.Char8 (ByteString)
import System.FilePath ((</>), takeExtension)
import System.IO (hPutStrLn, stderr)
import System.IO.Error (isDoesNotExistError)

import Distribution.PackageDescription (parsePackageDescription, ParseResult(..))



getKnownPackages :: ConfigFlags -> IO [PkgInfo]
getKnownPackages cfg
    = fmap concat $ mapM (readRepoIndex cfg) $ configRepos cfg

readRepoIndex :: ConfigFlags -> Repo -> IO [PkgInfo]
readRepoIndex cfg repo =
    do let indexFile = repoCacheDir cfg repo </> "00-index.tar"
       fmap (parseRepoIndex repo) (BS.readFile indexFile)
          `catch` (\e -> do case e of
                              IOException ioe | isDoesNotExistError ioe ->
                                hPutStrLn stderr "The package list does not exist. Run 'cabal update' to download it."
                              _ -> hPutStrLn stderr ("Error: " ++ show e)
                            return [])

parseRepoIndex :: Repo -> ByteString -> [PkgInfo]
parseRepoIndex repo s =
    do (hdr, content) <- readTarArchive s
       if takeExtension (tarFileName hdr) == ".cabal"
         then case parsePackageDescription (BS.unpack content) of
                    ParseOk _ descr -> return $ PkgInfo { 
                                                         pkgRepo = repo,
                                                         pkgDesc = descr
                                                        }
                    _               -> error $ "Couldn't read cabal file " ++ show (tarFileName hdr)
         else fail "Not a .cabal file"