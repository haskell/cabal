{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Distribution.Client.CmdTarget
  ( targetCommand
  , targetAction
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import qualified Data.Map as Map
import Distribution.Client.CmdBuild (selectComponentTarget, selectPackageTargets)
import Distribution.Client.CmdErrorMessages
import Distribution.Client.InstallPlan
import qualified Distribution.Client.InstallPlan as InstallPlan
import Distribution.Client.NixStyleOptions
  ( NixStyleFlags (..)
  , defaultNixStyleFlags
  )
import Distribution.Client.ProjectOrchestration
import Distribution.Client.ProjectPlanning
import Distribution.Client.Setup
  ( ConfigFlags (..)
  , GlobalFlags
  )
import Distribution.Client.TargetProblem
  ( TargetProblem'
  )
import Distribution.Package
import Distribution.Simple.Command
  ( CommandUI (..)
  , usageAlternatives
  )
import Distribution.Simple.Flag (fromFlagOrDefault)
import Distribution.Simple.Utils
  ( wrapText
  )
import Distribution.Verbosity
  ( normal
  )
import Text.PrettyPrint
import qualified Text.PrettyPrint as Pretty

-------------------------------------------------------------------------------
-- Command
-------------------------------------------------------------------------------

targetCommand :: CommandUI (NixStyleFlags ())
targetCommand =
  CommandUI
    { commandName = "v2-target"
    , commandSynopsis = "Target disclosure."
    , commandUsage = usageAlternatives "v2-target" ["[TARGETS]"]
    , commandDescription = Just $ \_ ->
        wrapText $
          "Reveal the targets of build plan. "
            ++ "If no [TARGETS] are given 'all' will be used for selecting a build plan.\n\n"
            ++ "A [TARGETS] item can be one of these target forms;\n"
            ++ "- a package target (e.g. [pkg:]package)\n"
            ++ "- a component target (e.g. [package:][ctype:]component)\n"
            ++ "- all packages (e.g. all)\n"
            ++ "- components of a particular type (e.g. package:ctypes or all:ctypes)\n"
            ++ "- a module target: (e.g. [package:][ctype:]module)\n"
            ++ "- a filepath target: (e.g. [package:][ctype:]filepath)\n"
            ++ "- a script target: (e.g. path/to/script)\n\n"
            ++ "The ctypes can be one of: "
            ++ "libs or libraries, "
            ++ "exes or executables, "
            ++ "tests, "
            ++ "benches or benchmarks, "
            ++ " and flibs or foreign-libraries."
    , commandNotes = Just $ \pname ->
        "Examples:\n"
          ++ "  "
          ++ pname
          ++ " v2-target all\n"
          ++ "    Targets of the package in the current directory "
          ++ "or all packages in the project\n"
          ++ "  "
          ++ pname
          ++ " v2-target pkgname\n"
          ++ "    Targets of the package named pkgname in the project\n"
          ++ "  "
          ++ pname
          ++ " v2-target ./pkgfoo\n"
          ++ "    Targets of the package in the ./pkgfoo directory\n"
          ++ "  "
          ++ pname
          ++ " v2-target cname\n"
          ++ "    Targets of the component named cname in the project\n"
          ++ "  "
    , commandDefaultFlags = defaultNixStyleFlags ()
    , commandOptions = const []
    }

-------------------------------------------------------------------------------
-- Action
-------------------------------------------------------------------------------

targetAction :: NixStyleFlags () -> [String] -> GlobalFlags -> IO ()
targetAction flags@NixStyleFlags{..} ts globalFlags = do
  ProjectBaseContext
    { distDirLayout
    , cabalDirLayout
    , projectConfig
    , localPackages
    } <-
    establishProjectBaseContext verbosity cliConfig OtherCommand

  (_, elaboratedPlan, _, _, _) <-
    rebuildInstallPlan
      verbosity
      distDirLayout
      cabalDirLayout
      projectConfig
      localPackages
      Nothing

  targetSelectors <-
    either (reportTargetSelectorProblems verbosity) return
      =<< readTargetSelectors localPackages Nothing targetStrings

  targets :: TargetsMap <-
    either (reportBuildTargetProblems verbosity) return $
      resolveTargets
        selectPackageTargets
        selectComponentTarget
        elaboratedPlan
        Nothing
        targetSelectors

  printTargetForms targets elaboratedPlan
  where
    verbosity = fromFlagOrDefault normal (configVerbosity configFlags)
    targetStrings = if null ts then ["all"] else ts
    cliConfig =
      commandLineFlagsToProjectConfig
        globalFlags
        flags
        mempty

reportBuildTargetProblems :: Verbosity -> [TargetProblem'] -> IO a
reportBuildTargetProblems verbosity problems =
  reportTargetProblems verbosity "target" problems

printTargetForms :: TargetsMap -> ElaboratedInstallPlan -> IO ()
printTargetForms targets elaboratedPlan = do
  putStrLn . render . nest 1 . vcat . ((text "-" <+>) . text <$>) . sort $
    catMaybes
      [ targetForm ct pkgs
      | (u :: UnitId, xs) <- Map.toAscList targets
      , (ct :: ComponentTarget, _) <- xs
      , let pkgs = filter ((== u) . elabUnitId) localPkgs
      ]
  where
    localPkgs =
      [x | Configured x@ElaboratedConfiguredPackage{elabLocalToProject = True} <- InstallPlan.toList elaboratedPlan]

    targetForm _ [] = Nothing
    targetForm ct (x : _) =
      let pkgId@PackageIdentifier{pkgName = n} = elabPkgSourceId x
       in Just . render $ pretty n Pretty.<> colon Pretty.<> text (showComponentTarget pkgId ct)
