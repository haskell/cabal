{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-----------------------------------------------------------------------------

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
module Distribution.Client.BuildReports.Storage
  ( -- * Storing and retrieving build reports
    storeAnonymous
  , storeLocal
  --    retrieve,

    -- * 'InstallPlan' support
  , fromInstallPlan
  , fromPlanningFailure
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Client.BuildReports.Anonymous (BuildReport, newBuildReport, showBuildReport)
import qualified Distribution.Client.BuildReports.Anonymous as BuildReport

import Distribution.Client.InstallPlan
  ( InstallPlan
  )
import qualified Distribution.Client.InstallPlan as InstallPlan
import Distribution.Client.Types

import qualified Distribution.Solver.Types.ComponentDeps as CD
import Distribution.Solver.Types.SourcePackage

import Distribution.Compiler
  ( CompilerId (..)
  , CompilerInfo (..)
  )
import Distribution.Package
  ( PackageId
  , packageId
  )
import Distribution.PackageDescription
  ( FlagAssignment
  )
import Distribution.Simple.InstallDirs
  ( PathTemplate
  , fromPathTemplate
  , initialPathTemplateEnv
  , substPathTemplate
  )
import Distribution.Simple.Utils
  ( equating
  )
import Distribution.System
  ( Platform (Platform)
  )

import qualified Data.List as L
import Data.List.NonEmpty
  ( groupBy
  )
import System.Directory
  ( createDirectoryIfMissing
  )
import System.FilePath
  ( takeDirectory
  , (</>)
  )

storeAnonymous :: [(BuildReport, Maybe Repo)] -> IO ()
storeAnonymous reports =
  sequence_
    [ appendFile file (concatMap format reports')
    | (repo, reports') <- separate reports
    , let file = repoLocalDir repo </> "build-reports.log"
    ]
  where
    -- TODO: make this concurrency safe, either lock the report file or make sure
    -- the writes for each report are atomic (under 4k and flush at boundaries)

    format r = '\n' : showBuildReport r ++ "\n"
    separate
      :: [(BuildReport, Maybe Repo)]
      -> [(Repo, [BuildReport])]
    separate =
      map (\rs@((_, repo, _) : _) -> (repo, [r | (r, _, _) <- rs]))
        . map (concatMap toList)
        . L.groupBy (equating (repoName' . head))
        . sortBy (comparing (repoName' . head))
        . groupBy (equating repoName')
        . onlyRemote

    repoName' (_, _, rrepo) = remoteRepoName rrepo

    onlyRemote
      :: [(BuildReport, Maybe Repo)]
      -> [(BuildReport, Repo, RemoteRepo)]
    onlyRemote rs =
      [ (report, repo, remoteRepo)
      | (report, Just repo) <- rs
      , Just remoteRepo <- [maybeRepoRemote repo]
      ]

storeLocal
  :: CompilerInfo
  -> [PathTemplate]
  -> [(BuildReport, Maybe Repo)]
  -> Platform
  -> IO ()
storeLocal cinfo templates reports platform =
  sequence_
    [ do
      createDirectoryIfMissing True (takeDirectory file)
      appendFile file output
    | -- TODO: make this concurrency safe, either lock the report file or make
    --      sure the writes for each report are atomic
    (file, reports') <-
      groupByFileName
        [ (reportFileName template report, report)
        | template <- templates
        , (report, _repo) <- reports
        ]
    , let output = concatMap format reports'
    ]
  where
    format r = '\n' : showBuildReport r ++ "\n"

    reportFileName template report =
      fromPathTemplate (substPathTemplate env template)
      where
        env =
          initialPathTemplateEnv
            (BuildReport.package report)
            -- TODO: In principle, we can support $pkgkey, but only
            -- if the configure step succeeds.  So add a Maybe field
            -- to the build report, and either use that or make up
            -- a fake identifier if it's not available.
            (error "storeLocal: package key not available")
            cinfo
            platform

    groupByFileName =
      map (\grp@((filename, _) : _) -> (filename, map snd grp))
        . L.groupBy (equating fst)
        . sortBy (comparing fst)

-- ------------------------------------------------------------

-- * InstallPlan support

-- ------------------------------------------------------------

fromInstallPlan
  :: Platform
  -> CompilerId
  -> InstallPlan
  -> BuildOutcomes
  -> [(BuildReport, Maybe Repo)]
fromInstallPlan platform comp plan buildOutcomes =
  mapMaybe
    ( \pkg ->
        fromPlanPackage
          platform
          comp
          pkg
          (InstallPlan.lookupBuildOutcome pkg buildOutcomes)
    )
    . InstallPlan.toList
    $ plan

fromPlanPackage
  :: Platform
  -> CompilerId
  -> InstallPlan.PlanPackage
  -> Maybe BuildOutcome
  -> Maybe (BuildReport, Maybe Repo)
fromPlanPackage
  (Platform arch os)
  comp
  (InstallPlan.Configured (ConfiguredPackage _ srcPkg flags _ deps))
  (Just buildResult) =
    Just
      ( newBuildReport
          os
          arch
          comp
          (packageId srcPkg)
          flags
          (map packageId (CD.nonSetupDeps deps))
          buildResult
      , extractRepo srcPkg
      )
    where
      extractRepo (SourcePackage{srcpkgSource = RepoTarballPackage repo _ _}) =
        Just repo
      extractRepo _ = Nothing
fromPlanPackage _ _ _ _ = Nothing

fromPlanningFailure
  :: Platform
  -> CompilerId
  -> [PackageId]
  -> FlagAssignment
  -> [(BuildReport, Maybe Repo)]
fromPlanningFailure (Platform arch os) comp pkgids flags =
  [ (newBuildReport os arch comp pkgid flags [] (Left PlanningFailed), Nothing)
  | pkgid <- pkgids
  ]
