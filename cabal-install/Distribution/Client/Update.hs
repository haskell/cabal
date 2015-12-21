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
{-# LANGUAGE RecordWildCards #-}
module Distribution.Client.Update
    ( update
    ) where

import Distribution.Client.Types
         ( Repo(..), RemoteRepo(..), maybeRepoRemote )
import Distribution.Client.HttpUtils
         ( DownloadResult(..) )
import Distribution.Client.FetchUtils
         ( downloadIndex )
import Distribution.Client.IndexUtils
         ( updateRepoIndexCache, Index(..) )
import Distribution.Client.JobControl
         ( newParallelJobControl, spawnJob, collectJob )
import Distribution.Client.Setup
         ( RepoContext(..) )
import Distribution.Verbosity
         ( Verbosity )

import Distribution.Simple.Utils
         ( writeFileAtomic, warn, notice )

import qualified Data.ByteString.Lazy       as BS
import Distribution.Client.GZipUtils (maybeDecompress)
import System.FilePath (dropExtension)
import Data.Maybe (catMaybes)
import Data.Time (getCurrentTime)

import qualified Hackage.Security.Client as Sec

-- | 'update' downloads the package list from all known servers
update :: Verbosity -> RepoContext -> IO ()
update verbosity repoCtxt | null (repoContextRepos repoCtxt) = do
  warn verbosity $ "No remote package servers have been specified. Usually "
                ++ "you would have one specified in the config file."
update verbosity repoCtxt = do
  jobCtrl <- newParallelJobControl
  let repos       = repoContextRepos repoCtxt
      remoteRepos = catMaybes (map maybeRepoRemote repos)
  case remoteRepos of
    [] -> return ()
    [remoteRepo] ->
        notice verbosity $ "Downloading the latest package list from "
                        ++ remoteRepoName remoteRepo
    _ -> notice verbosity . unlines
            $ "Downloading the latest package lists from: "
            : map (("- " ++) . remoteRepoName) remoteRepos
  mapM_ (spawnJob jobCtrl . updateRepo verbosity repoCtxt) repos
  mapM_ (\_ -> collectJob jobCtrl) repos

updateRepo :: Verbosity -> RepoContext -> Repo -> IO ()
updateRepo verbosity repoCtxt repo = do
  transport <- repoContextGetTransport repoCtxt
  case repo of
    RepoLocal{..} -> return ()
    RepoRemote{..} -> do
      downloadResult <- downloadIndex transport verbosity repoRemote repoLocalDir
      case downloadResult of
        FileAlreadyInCache -> return ()
        FileDownloaded indexPath -> do
          writeFileAtomic (dropExtension indexPath) . maybeDecompress
                                                  =<< BS.readFile indexPath
          updateRepoIndexCache verbosity (RepoIndex repoCtxt repo)
    RepoSecure{} -> repoContextWithSecureRepo repoCtxt repo $ \repoSecure -> do
      ce <- if repoContextIgnoreExpiry repoCtxt
              then Just `fmap` getCurrentTime
              else return Nothing
      updated <- Sec.uncheckClientErrors $ Sec.checkForUpdates repoSecure ce
      -- Update cabal's internal index as well so that it's not out of sync
      -- (If all access to the cache goes through hackage-security this can go)
      case updated of
        Sec.NoUpdates  ->
          return ()
        Sec.HasUpdates ->
          updateRepoIndexCache verbosity (RepoIndex repoCtxt repo)
