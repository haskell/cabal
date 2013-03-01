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
import qualified Distribution.Client.InstallPlan as InstallPlan
import Distribution.Client.InstallPlan
         ( InstallPlan )

import Distribution.Simple.InstallDirs
         ( PathTemplate, fromPathTemplate
         , initialPathTemplateEnv, substPathTemplate )
import Distribution.System
         ( Platform(Platform) )
import Distribution.Compiler
         ( CompilerId )
import Distribution.Simple.Utils
         ( comparing, equating )

import Data.List
         ( groupBy, sortBy )
import Data.Maybe
         ( catMaybes )
import System.FilePath
         ( (</>), takeDirectory )
import System.Directory
         ( createDirectoryIfMissing )

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

storeLocal :: [PathTemplate] -> [(BuildReport, Repo)] -> Platform -> IO ()
storeLocal templates reports platform = sequence_
  [ do createDirectoryIfMissing True (takeDirectory file)
       appendFile file output
       --TODO: make this concurrency safe, either lock the report file or make
       --      sure the writes for each report are atomic
  | (file, reports') <- groupByFileName
                          [ (reportFileName template report, report)
                          | template <- templates
                          , (report, _repo) <- reports ]
  , let output = concatMap format reports'
  ]
  where
    format r = '\n' : BuildReport.show r ++ "\n"

    reportFileName template report =
        fromPathTemplate (substPathTemplate env template)
      where env = initialPathTemplateEnv
                    (BuildReport.package  report)
                    (BuildReport.compiler report)
                    platform

    groupByFileName = map (\grp@((filename,_):_) -> (filename, map snd grp))
                    . groupBy (equating  fst)
                    . sortBy  (comparing fst)

-- ------------------------------------------------------------
-- * InstallPlan support
-- ------------------------------------------------------------

fromInstallPlan :: InstallPlan -> [(BuildReport, Repo)]
fromInstallPlan plan = catMaybes
                     . map (fromPlanPackage platform comp)
                     . InstallPlan.toList
                     $ plan
  where platform = InstallPlan.planPlatform plan
        comp     = InstallPlan.planCompiler plan

fromPlanPackage :: Platform -> CompilerId
                -> InstallPlan.PlanPackage
                -> Maybe (BuildReport, Repo)
fromPlanPackage (Platform arch os) comp planPackage = case planPackage of

  InstallPlan.Installed pkg@(ConfiguredPackage (SourcePackage {
                          packageSource = RepoTarballPackage repo _ _ }) _ _ _) result
    -> Just $ (BuildReport.new os arch comp pkg (Right result), repo)

  InstallPlan.Failed pkg@(ConfiguredPackage (SourcePackage {
                       packageSource = RepoTarballPackage repo _ _ }) _ _ _) result
    -> Just $ (BuildReport.new os arch comp pkg (Left result), repo)

  _ -> Nothing
