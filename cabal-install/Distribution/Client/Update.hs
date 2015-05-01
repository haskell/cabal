-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Update
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
--
-----------------------------------------------------------------------------
module Distribution.Client.Update
    ( update
    ) where

import Distribution.Client.Types
         ( Repo(..), RemoteRepo(..), LocalRepo(..) )
import Distribution.Client.HttpUtils
         ( DownloadResult(..), HttpTransport(..) )
import Distribution.Client.FetchUtils
         ( downloadIndex )
import Distribution.Client.IndexUtils
         ( updateRepoIndexCache )
import Distribution.Client.JobControl
         ( newParallelJobControl, spawnJob, collectJob )

import Distribution.Simple.Utils
         ( writeFileAtomic, warn, notice )
import Distribution.Verbosity
         ( Verbosity )

import qualified Data.ByteString.Lazy       as BS
import Distribution.Client.GZipUtils (maybeDecompress)
import System.FilePath (dropExtension)
import Data.Either (lefts)

-- | 'update' downloads the package list from all known servers
update :: HttpTransport -> Verbosity -> [Repo] -> IO ()
update _ verbosity [] =
  warn verbosity $ "No remote package servers have been specified. Usually "
                ++ "you would have one specified in the config file."
update transport verbosity repos = do
  jobCtrl <- newParallelJobControl
  let remoteRepos = lefts (map repoKind repos)
  case remoteRepos of
    [] -> return ()
    [remoteRepo] ->
        notice verbosity $ "Downloading the latest package list from "
                        ++ remoteRepoName remoteRepo
    _ -> notice verbosity . unlines
            $ "Downloading the latest package lists from: "
            : map (("- " ++) . remoteRepoName) remoteRepos
  mapM_ (spawnJob jobCtrl . updateRepo transport verbosity) repos
  mapM_ (\_ -> collectJob jobCtrl) repos

updateRepo :: HttpTransport -> Verbosity -> Repo -> IO ()
updateRepo transport verbosity repo = case repoKind repo of
  Right LocalRepo -> return ()
  Left remoteRepo -> do
    downloadResult <- downloadIndex transport verbosity remoteRepo (repoLocalDir repo)
    case downloadResult of
      FileAlreadyInCache -> return ()
      FileDownloaded indexPath -> do
        writeFileAtomic (dropExtension indexPath) . maybeDecompress
                                                =<< BS.readFile indexPath
        updateRepoIndexCache verbosity repo
