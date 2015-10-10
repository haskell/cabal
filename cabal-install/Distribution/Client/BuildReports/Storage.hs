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
    fromPlanningFailure,
  ) where

import qualified Distribution.Client.BuildReports.Anonymous as BuildReport
import Distribution.Client.BuildReports.Anonymous (BuildReport)

import Distribution.Client.Types
import qualified Distribution.Client.InstallPlan as InstallPlan
import qualified Distribution.Client.ComponentDeps as CD
import Distribution.Client.InstallPlan
         ( InstallPlan )

import Distribution.Package
         ( PackageId, packageId )
import Distribution.PackageDescription
         ( FlagAssignment )
import Distribution.Simple.InstallDirs
         ( PathTemplate, fromPathTemplate
         , initialPathTemplateEnv, substPathTemplate )
import Distribution.System
         ( Platform(Platform) )
import Distribution.Compiler
         ( CompilerId(..), CompilerInfo(..)  )
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

storeAnonymous :: [(BuildReport, Maybe Repo)] -> IO ()
storeAnonymous reports = sequence_
  [ appendFile file (concatMap format reports')
  | (repo, reports') <- separate reports
  , let file = repoLocalDir repo </> "build-reports.log" ]
  --TODO: make this concurrency safe, either lock the report file or make sure
  -- the writes for each report are atomic (under 4k and flush at boundaries)

  where
    format r = '\n' : BuildReport.show r ++ "\n"
    separate :: [(BuildReport, Maybe Repo)]
             -> [(Repo, [BuildReport])]
    separate = map (\rs@((_,repo,_):_) -> (repo, [ r | (r,_,_) <- rs ]))
             . map concat
             . groupBy (equating (repoName . head))
             . sortBy (comparing (repoName . head))
             . groupBy (equating repoName)
             . onlyRemote
    repoName (_,_,rrepo) = remoteRepoName rrepo

    onlyRemote :: [(BuildReport, Maybe Repo)]
               -> [(BuildReport, Repo, RemoteRepo)]
    onlyRemote rs =
      [ (report, repo, remoteRepo)
      | (report, Just repo@Repo { repoKind = Left remoteRepo }) <- rs ]

storeLocal :: CompilerInfo -> [PathTemplate] -> [(BuildReport, Maybe Repo)]
           -> Platform -> IO ()
storeLocal cinfo templates reports platform = sequence_
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
                    -- ToDo: In principle, we can support $pkgkey, but only
                    -- if the configure step succeeds.  So add a Maybe field
                    -- to the build report, and either use that or make up
                    -- a fake identifier if it's not available.
                    (error "storeLocal: package key not available")
                    cinfo
                    platform

    groupByFileName = map (\grp@((filename,_):_) -> (filename, map snd grp))
                    . groupBy (equating  fst)
                    . sortBy  (comparing fst)

-- ------------------------------------------------------------
-- * InstallPlan support
-- ------------------------------------------------------------

fromInstallPlan :: Platform -> CompilerId
                -> InstallPlan
                -> [(BuildReport, Maybe Repo)]
fromInstallPlan platform comp plan =
     catMaybes
   . map (fromPlanPackage platform comp)
   . InstallPlan.toList
   $ plan

fromPlanPackage :: Platform -> CompilerId
                -> InstallPlan.PlanPackage
                -> Maybe (BuildReport, Maybe Repo)
fromPlanPackage (Platform arch os) comp planPackage = case planPackage of
  InstallPlan.Installed (ReadyPackage (ConfiguredPackage srcPkg flags _ _) deps)
                         _ result
    -> Just $ ( BuildReport.new os arch comp
                                (packageId srcPkg) flags
                                (map packageId (CD.nonSetupDeps deps))
                                (Right result)
              , extractRepo srcPkg)

  InstallPlan.Failed (ConfiguredPackage srcPkg flags _ deps) result
    -> Just $ ( BuildReport.new os arch comp
                                (packageId srcPkg) flags
                                (map confSrcId (CD.nonSetupDeps deps))
                                (Left result)
              , extractRepo srcPkg )

  _ -> Nothing

  where
    extractRepo (SourcePackage { packageSource = RepoTarballPackage repo _ _ })
                  = Just repo
    extractRepo _ = Nothing

fromPlanningFailure :: Platform -> CompilerId
    -> [PackageId] -> FlagAssignment -> [(BuildReport, Maybe Repo)]
fromPlanningFailure (Platform arch os) comp pkgids flags =
  [ (BuildReport.new os arch comp pkgid flags [] (Left PlanningFailed), Nothing)
  | pkgid <- pkgids ]
