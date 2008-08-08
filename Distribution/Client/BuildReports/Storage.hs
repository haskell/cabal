-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Reporting
-- Copyright   :  (c) David Waern 2008
-- License     :  BSD-like
--
-- Maintainer  :  david.waern@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Anonymous build report data structure, printing and parsing
--
-----------------------------------------------------------------------------
module Distribution.Client.BuildReports.Storage (

    -- * Storing and retrieving build reports
    storeAnonymous,
    storeLocal,
--    retrieve,

    -- * 'InstallPlan' support
    fromInstallPlan,
  ) where

import qualified Distribution.Client.BuildReports.Anonymous as BuildReport
import Distribution.Client.BuildReports.Anonymous (BuildReport)

import Distribution.Client.Types
         ( ConfiguredPackage(..), AvailablePackage(..)
         , AvailablePackageSource(..), Repo(..), RemoteRepo(..) )
import qualified Distribution.Client.InstallPlan as InstallPlan
import Distribution.Client.InstallPlan
         ( InstallPlan, PlanPackage )
import Distribution.Client.Config
         ( defaultLogsDir )

import Distribution.System
         ( OS, Arch )
import Distribution.Compiler
         ( CompilerId )
import Distribution.Simple.Utils
         ( comparing, equating )

import Data.List
         ( groupBy, sortBy )
import Data.Maybe
         ( catMaybes )
import System.FilePath
         ( (</>) )

storeAnonymous :: [(BuildReport, Repo)] -> IO ()
storeAnonymous reports = sequence_
  [ appendFile file (concatMap format reports')
  | (repo, reports') <- separate reports
  , let file = repoLocalDir repo </> "build-reports.log" ]
  --TODO: make this concurrency safe, either lock the report file or make sure
  -- the writes for each report are atomic (under 4k and flush at boundaries)

  where
    format r = '\n' : BuildReport.show r ++ "\n"
    separate :: [(BuildReport, Repo)]
             -> [(Repo, [BuildReport])]
    separate = map (\rs@((_,repo,_):_) -> (repo, [ r | (r,_,_) <- rs ]))
             . map concat
             . groupBy (equating (repoName . head))
             . sortBy (comparing (repoName . head))
             . groupBy (equating repoName)
             . onlyRemote
    repoName (_,_,rrepo) = remoteRepoName rrepo

    onlyRemote :: [(BuildReport, Repo)] -> [(BuildReport, Repo, RemoteRepo)]
    onlyRemote rs =
      [ (report, repo, remoteRepo)
      | (report, repo@Repo { repoKind = Left remoteRepo }) <- rs ]

storeLocal :: [(BuildReport, Repo)] -> IO ()
storeLocal reports = do
  logsDir <- defaultLogsDir
  let file = logsDir </> "build.log"
  appendFile file (concatMap (format . fst) reports)
  --TODO: make this concurrency safe, either lock the report file or make sure
  -- the writes for each report are atomic (under 4k and flush at boundaries)

  where
    format r = '\n' : BuildReport.show r ++ "\n"


-- ------------------------------------------------------------
-- * InstallPlan support
-- ------------------------------------------------------------

fromInstallPlan :: InstallPlan -> [(BuildReport, Repo)]
fromInstallPlan plan = catMaybes
                     . map (fromPlanPackage os' arch' comp)
                     . InstallPlan.toList
                     $ plan
  where os'   = InstallPlan.planOS plan
        arch' = InstallPlan.planArch plan
        comp  = InstallPlan.planCompiler plan

fromPlanPackage :: OS -> Arch -> CompilerId
                -> InstallPlan.PlanPackage
                -> Maybe (BuildReport, Repo)
fromPlanPackage os' arch' comp planPackage = case planPackage of

  InstallPlan.Installed pkg@(ConfiguredPackage (AvailablePackage {
                          packageSource = RepoTarballPackage repo }) _ _) result
    -> Just $ (BuildReport.new os' arch' comp pkg (Right result), repo)

  InstallPlan.Failed pkg@(ConfiguredPackage (AvailablePackage {
                       packageSource = RepoTarballPackage repo }) _ _) result
    -> Just $ (BuildReport.new os' arch' comp pkg (Left result), repo)

  _ -> Nothing
