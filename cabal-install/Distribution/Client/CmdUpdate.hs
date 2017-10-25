{-# LANGUAGE CPP, NamedFieldPuns, RecordWildCards, ViewPatterns #-}

-- | cabal-install CLI command: update
--
module Distribution.Client.CmdUpdate (
    updateCommand,
    updateAction,
  ) where

import Distribution.Client.ProjectOrchestration
import Distribution.Client.ProjectConfig
         ( ProjectConfig(..)
         , projectConfigWithSolverRepoContext )
import Distribution.Client.Types
         ( Repo(..), RemoteRepo(..), isRepoRemote )
import Distribution.Client.HttpUtils
         ( DownloadResult(..) )
import Distribution.Client.FetchUtils
         ( downloadIndex )
import Distribution.Client.JobControl
         ( newParallelJobControl, spawnJob, collectJob )
import Distribution.Client.Setup
         ( GlobalFlags, ConfigFlags(..), ConfigExFlags, InstallFlags, UpdateFlags (updateIndexState)
         , applyFlagDefaults, defaultUpdateFlags, RepoContext(..) )
import Distribution.Simple.Setup
         ( HaddockFlags, fromFlagOrDefault, fromFlag )
import Distribution.Simple.Utils
         ( die', notice, wrapText, writeFileAtomic, noticeNoWrap, intercalate )
import Distribution.Verbosity
         ( Verbosity, normal, lessVerbose )
import Distribution.Client.IndexUtils.Timestamp
import Distribution.Client.IndexUtils
         ( updateRepoIndexCache, Index(..), writeIndexTimestamp
         , currentIndexTimestamp )
import Distribution.Text
         ( Text(..), display, simpleParse )

import qualified Distribution.Compat.ReadP  as ReadP
import qualified Text.PrettyPrint          as Disp

import Control.Monad (unless, when)
import qualified Data.ByteString.Lazy       as BS
import Distribution.Client.GZipUtils (maybeDecompress)
import System.FilePath (dropExtension)
import Data.Time (getCurrentTime)
import Distribution.Simple.Command
         ( CommandUI(..), usageAlternatives )
import qualified Distribution.Client.Setup as Client

import qualified Hackage.Security.Client as Sec

updateCommand :: CommandUI (ConfigFlags, ConfigExFlags, InstallFlags, HaddockFlags)
updateCommand = Client.installCommand {
  commandName         = "new-update",
  commandSynopsis     = "Updates list of known packages.",
  commandUsage        = usageAlternatives "new-update" [ "[FLAGS]" ],
  commandDescription  = Just $ \_ -> wrapText $
        "For all known remote repositories, download the package list.",
  commandNotes        = Just $ \pname ->
        "Examples:\n"
     ++ "  " ++ pname ++ " new-update\n"
     ++ "    Download the package list for all known remote repositories.\n\n"
     ++ "Note: this command is part of the new project-based system (aka "
     ++ "nix-style\nlocal builds). These features are currently in beta. "
     ++ "Please see\n"
     ++ "http://cabal.readthedocs.io/en/latest/nix-local-build-overview.html "
     ++ "for\ndetails and advice on what you can expect to work. If you "
     ++ "encounter problems\nplease file issues at "
     ++ "https://github.com/haskell/cabal/issues and if you\nhave any time "
     ++ "to get involved and help with testing, fixing bugs etc then\nthat "
     ++ "is very much appreciated.\n"
  }

data UpdateRequest = UpdateRequest
  { _updateRequestRepoName :: String
  , _updateRequestRepoState :: IndexState
  } deriving (Show)

instance Text UpdateRequest where
  disp (UpdateRequest n s) = Disp.text n Disp.<> Disp.char ',' Disp.<> disp s
  parse = parseWithState ReadP.+++ parseHEAD
    where parseWithState = do
            name <- ReadP.many1 (ReadP.satisfy (\c -> c /= ','))
            _ <- ReadP.char ','
            state <- parse
            return (UpdateRequest name state)
          parseHEAD = do
            name <- ReadP.manyTill (ReadP.satisfy (\c -> c /= ',')) ReadP.eof
            return (UpdateRequest name IndexStateHead)

updateAction :: (ConfigFlags, ConfigExFlags, InstallFlags, HaddockFlags)
             -> [String] -> GlobalFlags -> IO ()
updateAction (applyFlagDefaults -> (configFlags, configExFlags, installFlags, haddockFlags))
             extraArgs globalFlags = do

  ProjectBaseContext {
    projectConfig
  } <- establishProjectBaseContext verbosity cliConfig

  projectConfigWithSolverRepoContext verbosity (projectConfigShared projectConfig) (projectConfigBuildOnly projectConfig)
    $ \repoCtxt -> do
    let repos       = filter isRepoRemote $ repoContextRepos repoCtxt
        repoName    = remoteRepoName . repoRemote
        parseArg :: String -> IO UpdateRequest
        parseArg s = case simpleParse s of
          Just r -> pure r
          Nothing -> die' verbosity $ "'new-update' unable to parse repo: \"" ++ s ++ "\""
    updateRepoRequests <- mapM parseArg extraArgs

    unless (null updateRepoRequests) $ do
      let remoteRepoNames = map repoName repos
          unknownRepos = [r | (UpdateRequest r _) <- updateRepoRequests
                            , not (r `elem` remoteRepoNames)]
      unless (null unknownRepos) $
        die' verbosity $ "'new-update' repo(s): \"" ++ intercalate "\", \"" unknownRepos
                      ++ "\" can not be found in known remote repo(s): " ++ intercalate ", " remoteRepoNames

    let reposToUpdate = case updateRepoRequests of
          [] -> repos
          updateRequests -> let repoNames = map _updateRequestRepoName updateRequests
                            in filter (\r-> repoName r `elem` repoNames) repos

    case reposToUpdate of
      [] -> return ()
      [remoteRepo] ->
        notice verbosity $ "Downloading the latest package list from "
                        ++ repoName remoteRepo
      _ -> notice verbosity . unlines
              $ "Downloading the latest package lists from: "
              : map (("- " ++) . repoName) repos
    jobCtrl <- newParallelJobControl (length reposToUpdate)
    mapM_ (spawnJob jobCtrl . updateRepo verbosity defaultUpdateFlags repoCtxt) reposToUpdate
    mapM_ (\_ -> collectJob jobCtrl) reposToUpdate

  where
    verbosity = fromFlagOrDefault normal (configVerbosity configFlags)
    cliConfig = commandLineFlagsToProjectConfig
                  globalFlags configFlags configExFlags
                  installFlags haddockFlags

updateRepo :: Verbosity -> UpdateFlags -> RepoContext -> Repo -> IO ()
updateRepo verbosity updateFlags repoCtxt repo = do
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
      let index = RepoIndex repoCtxt repo
      -- NB: This may be a nullTimestamp if we've never updated before
      current_ts <- currentIndexTimestamp (lessVerbose verbosity) repoCtxt repo
      -- NB: always update the timestamp, even if we didn't actually
      -- download anything
      writeIndexTimestamp index (fromFlag (updateIndexState updateFlags))
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
          updateRepoIndexCache verbosity index
      -- TODO: This will print multiple times if there are multiple
      -- repositories: main problem is we don't have a way of updating
      -- a specific repo.  Once we implement that, update this.
      when (current_ts /= nullTimestamp) $
        noticeNoWrap verbosity $
          "To revert to previous state run:\n" ++
          "    cabal new-update '" ++ remoteRepoName (repoRemote repo) ++ "," ++ display current_ts ++ "'\n"
 
