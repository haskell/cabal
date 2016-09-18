{-# LANGUAGE BangPatterns, RecordWildCards, NamedFieldPuns,
             DeriveGeneric, DeriveDataTypeable, GeneralizedNewtypeDeriving,
             ScopedTypeVariables #-}

module Distribution.Client.ProjectPlanOutput (
    writePlanExternalRepresentation,
  ) where

import           Distribution.Client.ProjectPlanning.Types
import           Distribution.Client.DistDirLayout
import           Distribution.Client.Types

import qualified Distribution.Client.InstallPlan as InstallPlan
import qualified Distribution.Client.Utils.Json as J
import qualified Distribution.Simple.InstallDirs as InstallDirs

import qualified Distribution.Solver.Types.ComponentDeps as ComponentDeps

import           Distribution.Package
import           Distribution.InstalledPackageInfo (InstalledPackageInfo)
import qualified Distribution.PackageDescription as PD
import           Distribution.Text
import           Distribution.Simple.Utils
import qualified Paths_cabal_install as Our (version)

import           Data.Monoid
import qualified Data.ByteString.Builder as BB
import           System.FilePath


-----------------------------------------------------------------------------
-- Writing plan.json files
--

-- | Write out a representation of the elaborated install plan.
--
-- This is for the benefit of debugging and external tools like editors.
--
writePlanExternalRepresentation :: DistDirLayout
                                -> ElaboratedInstallPlan
                                -> ElaboratedSharedConfig
                                -> IO ()
writePlanExternalRepresentation distDirLayout elaboratedInstallPlan
                                elaboratedSharedConfig =
    writeFileAtomic (distProjectCacheFile distDirLayout "plan.json") $
        BB.toLazyByteString
      . J.encodeToBuilder
      $ encodePlanAsJson distDirLayout elaboratedInstallPlan elaboratedSharedConfig

-- | Renders a subset of the elaborated install plan in a semi-stable JSON
-- format.
--
encodePlanAsJson :: DistDirLayout -> ElaboratedInstallPlan -> ElaboratedSharedConfig -> J.Value
encodePlanAsJson distDirLayout elaboratedInstallPlan elaboratedSharedConfig =
    --TODO: [nice to have] include all of the sharedPackageConfig and all of
    --      the parts of the elaboratedInstallPlan
    J.object [ "cabal-version"     J..= jdisplay Our.version
             , "cabal-lib-version" J..= jdisplay cabalVersion
             , "install-plan"      J..= installPlanToJ elaboratedInstallPlan
             ]
  where
    installPlanToJ :: ElaboratedInstallPlan -> [J.Value]
    installPlanToJ = map planPackageToJ . InstallPlan.toList

    planPackageToJ :: ElaboratedPlanPackage -> J.Value
    planPackageToJ pkg =
      case pkg of
        InstallPlan.PreExisting ipi -> installedPackageInfoToJ ipi
        InstallPlan.Configured elab -> elaboratedPackageToJ False elab
        InstallPlan.Installed  elab -> elaboratedPackageToJ True  elab

    installedPackageInfoToJ :: InstalledPackageInfo -> J.Value
    installedPackageInfoToJ ipi =
      -- Pre-existing packages lack configuration information such as their flag
      -- settings or non-lib components. We only get pre-existing packages for
      -- the global/core packages however, so this isn't generally a problem.
      -- So these packages are never local to the project.
      --
      J.object
        [ "type"       J..= J.String "pre-existing"
        , "id"         J..= jdisplay (installedUnitId ipi)
        , "depends" J..= map jdisplay (installedDepends ipi)
        ]

    elaboratedPackageToJ :: Bool -> ElaboratedConfiguredPackage -> J.Value
    elaboratedPackageToJ isInstalled elab =
      J.object $
        [ "type"       J..= J.String (if isInstalled then "installed"
                                                     else "configured")
        , "id"         J..= (jdisplay . installedUnitId) elab
        , "flags"      J..= J.object [ fn J..= v
                                     | (PD.FlagName fn,v) <-
                                            elabFlagAssignment elab ]
        , "style"      J..= J.String (style2str (elabLocalToProject elab) (elabBuildStyle elab))
        ] ++
        (case elabBuildStyle elab of
            BuildInplaceOnly ->
                ["dist-dir"   J..= J.String dist_dir]
            BuildAndInstall ->
                -- TODO: install dirs?
                []
            ) ++
        case elabPkgOrComp elab of
          ElabPackage pkg ->
            let components = J.object $
                  [ comp2str c J..= (J.object $
                    [ "depends"     J..= map (jdisplay . confInstId) ldeps
                    , "exe-depends" J..= map (jdisplay . confInstId) edeps ] ++
                    bin_file c)
                  | (c,(ldeps,edeps))
                      <- ComponentDeps.toList $
                         ComponentDeps.zip (pkgLibDependencies pkg)
                                           (pkgExeDependencies pkg) ]
            in ["components" J..= components]
          ElabComponent comp ->
            ["depends"     J..= map (jdisplay . confInstId) (elabLibDependencies elab)
            ,"exe-depends" J..= map jdisplay (elabExeDependencies elab)
            ,"component-name" J..= J.String (comp2str (compSolverName comp))
            ] ++
            bin_file (compSolverName comp)
     where
      dist_dir = distBuildDirectory distDirLayout
                    (elabDistDirParams elaboratedSharedConfig elab)

      bin_file c = case c of
        ComponentDeps.ComponentExe s   -> bin_file' s
        ComponentDeps.ComponentTest s  -> bin_file' s
        ComponentDeps.ComponentBench s -> bin_file' s
        _ -> []
      bin_file' s =
        ["bin-file" J..= J.String bin]
       where
        bin = if elabBuildStyle elab == BuildInplaceOnly
               then dist_dir </> "build" </> s </> s
               else InstallDirs.bindir (elabInstallDirs elab) </> s

    -- TODO: maybe move this helper to "ComponentDeps" module?
    --       Or maybe define a 'Text' instance?
    comp2str :: ComponentDeps.Component -> String
    comp2str c = case c of
        ComponentDeps.ComponentLib     -> "lib"
        ComponentDeps.ComponentSubLib s -> "lib:"   <> s
        ComponentDeps.ComponentExe s   -> "exe:"   <> s
        ComponentDeps.ComponentTest s  -> "test:"  <> s
        ComponentDeps.ComponentBench s -> "bench:" <> s
        ComponentDeps.ComponentSetup   -> "setup"

    style2str :: Bool -> BuildStyle -> String
    style2str True  _                = "local"
    style2str False BuildInplaceOnly = "inplace"
    style2str False BuildAndInstall  = "global"

    jdisplay :: Text a => a -> J.Value
    jdisplay = J.String . display

